package guinep.internal

import guinep.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*
import zio.json.*
import zio.json.ast.Json.*
import scala.util.chaining.*

def genWeb(scripts: Map[String, Script]): Unit = {
  val ws = WebServer(scripts)
  val runtime = Runtime.default
  Unsafe.unsafe { implicit unsafe =>
    runtime.unsafe.run(ws.run)
  }
}

class WebServer(val scripts: Map[String, Script]) extends HtmlGen {
  val app: HttpApp[Any] = Routes(
    Method.GET / PathCodec.empty ->
      handler(Response.redirect(URL.root / "scripts", isPermanent = true)),
    Method.GET / "scripts" ->
      handler(Response.html(generateHtml)),
    Method.GET / "scripts" / string("name") ->
      handler(Response.html(generateHtml)),
    Method.POST / "run" ->
      handler { (req: Request) =>
        (for {
          str <- req.body.asString
          obj <- ZIO.fromEither(str.fromJson[Obj])
          scriptName <- ZIO.fromEither(obj.get("script@name").get.asString.toRight("Missing script name"))
          script = scripts(scriptName)
          inputsValuesMap <- ZIO.fromEither(script.inputs.toList.parseJSONValue(obj))
          inputsValues = script.inputs.toList.sortByArgs(inputsValuesMap)
          result = script.run(inputsValues)
        } yield Response.text(result)).onError(e => ZIO.debug(e.toString))
      }
  ).sandbox.toHttpApp

  def run =
    Server.serve(app).provide(Server.defaultWithPort(8090))
}
