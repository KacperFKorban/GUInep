package guinep

import guinep.*
import guinep.model.*
import guinep.serialization.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*
import zio.json.*
import zio.json.ast.Json.*
import scala.util.chaining.*

private[guinep] object webgen {

  def genWeb(
    funs: Seq[(String, Fun)],
    config: GuinepWebConfig
  ): Unit = {
    val ws = WebServer(funs, config)
    val runtime = Runtime.default
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.run(
        ws.run
          .race(Console.readLine("Press ENTER to stop...").*>(Console.printLine("Stopping...")))
      )
    }
  }

  class WebServer(
    val funs: Seq[(String, Fun)],
    val config: GuinepWebConfig
  ) extends HtmlGen {
    val funsMap = funs.toMap

    val routes = Routes(
      Method.GET / PathCodec.empty ->
        handler(Response.html(generateHtml)),
      Method.GET / string("name") ->
        handler(Response.html(generateHtml)),
      Method.POST / string("name") ->
        handler { (name: String, req: Request) =>
          (for {
            str <- req.body.asString
            obj <- ZIO.fromEither(str.fromJson[Obj])
            fun = funsMap(name)
            given Map[String, FormElement] = fun.form.namedFormElements
            inputsValuesMap <- ZIO.fromEither(fun.form.inputs.toList.parseJSONValue(obj))
            inputsValues = fun.form.inputs.toList.sortByArgs(inputsValuesMap)
            result = fun.run(inputsValues)
          } yield Response.text(result)).onError(e => ZIO.debug(e.toString))
        }
    ).sandbox @@ Middleware.serveResources(Path.empty / "assets")

    val app: HttpApp[Any] = routes.toHttpApp

    def run =
      Server
        .serve(app)
        .provide(
          ZLayer.succeed(config.httpServerConfig),
          Server.live 
        )
  }
}
