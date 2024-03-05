package guinep.internal

import guinep.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*

def genWeb(scripts: Map[String, Script]): Unit = {
  val ws = WebServer(scripts)
  val runtime = Runtime.default
  Unsafe.unsafe { implicit unsafe =>
    runtime.unsafe.run(ws.run)
  }
}

class WebServer(val scripts: Map[String, Script]) {
  val app: HttpApp[Any] = Routes(
    Method.GET / PathCodec.empty ->
      handler(Response.redirect(URL.root / "scripts", isPermanent = true)),
    Method.GET / "scripts" ->
      handler(Response.html(generateHtml)),
    Method.GET / "scripts" / string("name") ->
      handler(Response.html(generateHtml)),
    Method.POST / "run" ->
      handler { (req: Request) =>
        for {
          form <- req.body.asMultipartForm
          formMap = form.formData.map(fd => fd.name -> fd).toMap
          scriptName <- formMap.get("script@name").get.asText
          inputsData = formMap.removed("script@name").toSeq.map(_._2)
          script = scripts(scriptName)
          inputsValues <- ZIO.foreach(inputsData.toList) { (v: FormField) => v.asText }
          result = script.run(inputsValues)
        } yield Response.text(result)
      }
  ).sandbox.toHttpApp

  def run =
    Server.serve(app).provide(Server.defaultWithPort(8090))

  def generateHtml =
    html(
      head(
        title("GUInep"),
        style(
          """
          body { font-family: Arial, sans-serif; }
          .sidebar { position: fixed; left: 0; top: 0; width: 200px; height: 100vh; background-color: #f0f0f0; padding: 20px; }
          .sidebar a { display: block; padding: 10px; margin-bottom: 10px; background-color: #007bff; color: white; text-decoration: none; text-align: center; border-radius: 5px; }
          .sidebar a:hover { background-color: #0056b3; }
          .main-content { margin-left: 220px; padding: 20px; display: flex; justify-content: center; padding-top: 20px; }
          .form-container { width: 300px; }
          .form-input { margin-bottom: 10px; }
          label { display: block; margin-bottom: 5px; }
          input[type=text] { width: 100%; padding: 8px; margin-bottom: 20px; box-sizing: border-box; }
          input[type=submit] { background-color: #4CAF50; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; }
          input[type=submit]:hover { background-color: #45a049; }
          .result { margin-top: 20px; font-weight: bold; }
          """
        ),
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
          div(
            classAttr := List("form-container"),
            form(
              id := "scriptForm",
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
      const scriptName = window.location.pathname.replace("/scripts/", "");
      changeForm(scriptName);
    }
    
    function changeForm(scriptName) {
      const form = document.getElementById('scriptForm');
      form.innerHTML = ''; // Clear the form
      
      const scripts = ${scriptsToJsonArray};
      const selectedScript = scripts.find(script => script[0] === scriptName);
      
      if (selectedScript) {
        // add name title to the top of the form
        const nameTitle = document.createElement('h1');
        nameTitle.innerText = scriptName;
        form.appendChild(nameTitle);
        // add the script name as a hidden input
        const nameInput = document.createElement('input');
        nameInput.type = 'hidden';
        nameInput.name = "script@name";
        nameInput.value = scriptName;
        form.appendChild(nameInput);
        const br = document.createElement('br');
        form.appendChild(br);
        // add the script inputs as text inputs
        selectedScript[1].forEach(inputName => {
          const input = document.createElement('input');
          input.type = 'text';
          input.name = inputName;
          input.placeholder = inputName;
          form.appendChild(input);
          form.appendChild(br.cloneNode());
        });
        const submitButton = document.createElement('input');
        submitButton.type = 'submit';
        submitButton.value = 'Submit';
        form.appendChild(submitButton);
      }
    }
    document.addEventListener("DOMContentLoaded", function() {
      document.getElementById("scriptForm").addEventListener("submit", function(e) {
        e.preventDefault(); // Prevent the default form submission and redirect
        const formData = new FormData(e.target);
        fetch("/run", {
          method: "POST",
          body: formData
        })
        .then(response => response.text())
        .then(data => {
          document.getElementById("result").textContent = 'Result: ' + data;
        })
        .catch(error => console.error('Error:', error));
      });
    });
    """

  def scriptsToJsonArray: String =
    scripts
      .toSeq
      .map { case (name, script) =>
        s"""["$name", [${script.inputs.map(i => s""""${i.name}"""").mkString(",")}]]"""
      }
      .mkString("[", ",", "]")
}
