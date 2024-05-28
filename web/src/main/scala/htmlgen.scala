package guinep

import guinep.*
import guinep.model.*
import guinep.serialization.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*

private[guinep] trait HtmlGen {
  val funs: Seq[(String, Fun)]
  val config: GuinepWebConfig
  def generateHtml =
    html(
      head(
        title("GUInep"),
        link(relAttr := "stylesheet", href := "/assets/default.css"),
        script(srcAttr := "/assets/default.js"),
        script(Dom.raw(jsToChangeFormBasedOnPath)),
      ),
      body(
        onLoadAttr := "initFormBasedOnPath();",
        div(
          classAttr := List("sidebar"), {
            for
              (name, _) <- funs.toSeq
            yield {
              a(
                href := s"/$name",
                name
              )
            }
          }
        ),
        div(
          classAttr := List("main-content"),
          div(
            classAttr := List("form-container"),
            form(
              id := "guinepForm",
              methodAttr := "post",
              actionAttr := "/run"
            ),
            div(id := "result", classAttr := List("result"))
          )
        )
      )
    )

  // JavaScript to change the form inputs based on the URL path
  def jsToChangeFormBasedOnPath: String =
    s"""
    function initFormBasedOnPath() {
      const funName = window.location.pathname.replace("/", "");
      changeForm(funName, ${funsToJsonArray}, ${configJson});
    }
    """

  def funsToJsonArray: String =
    funs
      .toSeq
      .map { case (name, fun) =>
        s"""|["$name",
            |${fun.form.formElementsJSONRepr},
            |${fun.form.namedFormElementsJSONRepr}
            |]""".stripMargin
      }
      .mkString("[", ",", "]")
  
  def configJson: String =
    s"""|{ "requireNonNullableInputs": ${config.requireNonNullableInputs} }""".stripMargin
}
