package guinep.internal

import guinep.*
import zio.*
import zio.http.*
import zio.http.template.*
import io.netty.handler.codec.http.QueryStringDecoder

def genWeb(scripts: Map[String, Script]): Unit = {
  val ws = WebServer(scripts)
  val runtime = Runtime.default
  Unsafe.unsafe { implicit unsafe =>
    runtime.unsafe.run(ws.run)
  }
}

class WebServer(val scripts: Map[String, Script]) {
  val app: HttpApp[Any] = Routes(
    Method.GET / "scripts" ->
      handler(Response.html(generateHtml)),
    Method.GET / "scripts" / string("name") ->
      handler(Response.html(generateHtml)),
    Method.POST / "run" ->
      handler { (req: Request) =>
        for {
          form <- req.body.asURLEncodedForm
          _ <- ZIO.debug(form)
          formMap = form.formData.map(fd => fd.name -> fd).toMap
          scriptName = formMap.get("name").get.stringValue.get
          inputsData = formMap.removed("name").toSeq.map(_._2)
          script = scripts(scriptName)
          inputsValues = inputsData.map { (v: FormField) => v.stringValue.get }.toList
          result = script.run(inputsValues)
        } yield Response.text(result)
      }
  ).sandbox.toHttpApp

  def generateHtml =
    html(
      head(
        title("GUInep"),
        script(Dom.raw(jsToChangeFormBasedOnPath)),
      ),
      body(
        onLoadAttr := "initFormBasedOnPath();",
        div(
          classAttr := List("sidebar"), {
            for
              (scriptName, _) <- scripts.toSeq
            yield {
              a(
                href := s"/scripts/$scriptName",
                scriptName
              )
            }
          }
        ),
        div(
          classAttr := List("main-content"),
          form(
            id := "scriptForm",
            methodAttr := "post",
            actionAttr := "/run"
          )
        )
      )
    )

  // JavaScript to change the form inputs based on the URL path
  def jsToChangeFormBasedOnPath: String =
    s"""
    function initFormBasedOnPath() {
      const scriptName = window.location.pathname.replace("/scripts/", "");
      changeForm(scriptName);
    }
    
    function changeForm(scriptName) {
      const form = document.getElementById('scriptForm');
      form.innerHTML = ''; // Clear the form
      
      const scripts = ${scriptsToJsonArray};
      const selectedScript = scripts.find(script => script[0] === scriptName);
      
      if (selectedScript) {
        // add the script name as a hidden input
        const nameInput = document.createElement('input');
        nameInput.type = 'hidden';
        nameInput.name = "name";
        nameInput.value = scriptName;
        form.appendChild(nameInput);
        // add the script inputs as text inputs
        selectedScript[1].forEach(inputName => {
          const input = document.createElement('input');
          input.type = 'text';
          input.name = inputName;
          input.placeholder = inputName;
          form.appendChild(input);
        });
        const submitButton = document.createElement('input');
        submitButton.type = 'submit';
        submitButton.value = 'Submit';
        form.appendChild(submitButton);
      }
    }
    """

  def scriptsToJsonArray: String =
    scripts
      .toSeq
      .map { case (name, script) =>
        s"""["$name", [${script.inputs.map(i => s""""${i.name}"""").mkString(",")}]]"""
      }
      .mkString("[", ",", "]")

  def run = Server.serve(app).provide(Server.defaultWithPort(8090))
}
