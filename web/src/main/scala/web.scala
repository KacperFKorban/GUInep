package guinep.internal

import guinep.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*
import zio.json.*
import scala.util.chaining.*
import zio.json.ast.*
import zio.json.ast.Json.*

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
        (for {
          str <- req.body.asString
          obj <- ZIO.fromEither(str.fromJson[Obj])
          scriptName <- ZIO.fromEither(obj.get("script@name").get.asString.toRight("Missing script name"))
          script = scripts(scriptName)
          inputsValues <- ZIO.fromEither(script.inputs.toList.parseJSONValue(obj))
          _ <- ZIO.debug(inputsValues)
          result = script.run(inputsValues)
        } yield Response.text(result)).onError(e => ZIO.debug(e.toString))
      }
  ).sandbox.toHttpApp

  def run =
    Server.serve(app).provide(Server.defaultWithPort(8090))

  def sequenceEither[A, B](eithers: List[Either[A, B]]): Either[A, List[B]] =
    eithers.foldLeft(Right(List.empty): Either[A, List[B]]) { (acc, e) =>
      acc.flatMap(a => e.map(b => a :+ b))
    }

  extension (formElements: List[FormElement])
    def parseJSONValue(value: Obj): Either[String, List[Any]] =
      formElements
        .map { element =>
          value.get(element.name).toRight(s"Missing value for ${element.name}").flatMap(element.parseJSONValue)
        }
        .pipe(sequenceEither)

  extension (formElement: FormElement)
    def parseJSONValue(value: Json): Either[String, Any] = formElement match
      case FormElement.FieldSet(name, elements) =>
        for {
          m <- value.asObject.toRight(s"Invalid object: $value")
          res <- elements.parseJSONValue(m)
        } yield res
      case FormElement.TextInput(_) => Right(value)
      case FormElement.NumberInput(_) => value.asNumber.toRight(s"Invalid number: $value")
      case FormElement.CheckboxInput(_) => value.asBoolean.toRight(s"Invalid boolean: $value")
      case FormElement.Dropdown(_, options) =>
        for {
          v <- value.asString.toRight(s"Invalid string: $value")
          res <- options.find(_._1 == v).map(_._2).toRight(s"Invalid option: $value")
        } yield res
      case FormElement.TextArea(_, _, _) => Right(value)
      case _ => Left(s"Unsupported form element: $formElement")

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
          input:not([type=submit]) { width: 100%; padding: 8px; margin-bottom: 20px; box-sizing: border-box; }
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

      function addFormElement(form, formElem) {
        const br = document.createElement('br');
        if (formElem.type == 'fieldset') {
          const fieldset = document.createElement('fieldset');
          fieldset.name = formElem.name;
          const legend = document.createElement('legend');
          legend.innerText = formElem.name;
          fieldset.appendChild(legend);
          formElem.elements.forEach(subElem => addFormElement(fieldset, subElem));
          form.appendChild(fieldset);
          form.appendChild(br.cloneNode());
        } else {
          const label = document.createElement('label');
          label.innerText = formElem.name;
          label.for = formElem.name;
          form.appendChild(label);
          const input = document.createElement('input');
          input.type = formElem.type;
          input.name = formElem.name;
          input.id = formElem.name;
          input.placeholder = formElem.name;
          form.appendChild(input);
          form.appendChild(br.cloneNode());
        }
      }

      if (selectedScript) {
        // add name title to the top of the form
        const nameTitle = document.createElement('h1');
        nameTitle.innerText = scriptName;
        form.appendChild(nameTitle);
        // add the script name as a hidden input
        const nameInput = document.createElement('input');
        nameInput.type = 'hidden';
        nameInput.name = 'script@name';
        nameInput.value = scriptName;
        form.appendChild(nameInput);
        const br = document.createElement('br');
        form.appendChild(br);
        // add the script inputs as text inputs
        selectedScript[1].forEach(formElem => {
          addFormElement(form, formElem);
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

        const form = this;
        const jsonData = {};

        const processElement = (element, parentObj) => {
          if (element.tagName === 'FIELDSET') {
            const name = element.name;
            parentObj[name] = {};
            Array.from(element.elements).forEach(innerEl => {
              processElement(innerEl, parentObj[name]);
            });
          } else if (element.tagName === 'INPUT' && element.type !== 'submit') {
            const name = element.name;
            const value = element.value;
            if (parentObj[name]) { // Handle arrays for multiple values with the same name
              if (!Array.isArray(parentObj[name])) {
                parentObj[name] = [parentObj[name]];
              }
              parentObj[name].push(value);
            } else if (element.type === 'checkbox') {
              parentObj[name] = element.checked;
            } else {
              parentObj[name] = value;
            }
          }
        };

        Array.from(form.elements).forEach(el => {
          if ((el.tagName === 'INPUT' && el.type !== 'submit') || el.tagName === 'FIELDSET') {
            processElement(el, jsonData);
          }
        });

        fetch("/run", {
          method: "POST",
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(jsonData)
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
        s"""["$name", [${script.inputs.map(_.toJSONRepr).mkString(",")}]]"""
      }
      .mkString("[", ",", "]")
}
