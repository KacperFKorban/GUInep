package guinep

import guinep.internal.*
import scalatags.Text.all.*

trait HtmlGen {
  def generateHtml(scripts: Map[String, Script]): String =
    html(
      head(
        // h1("Dynamic Form Based on URL Path"),
        script(jsToChangeFormBasedOnPath(scripts)), // Include JS directly
        // script(src := "/path/to/your/script.js"),
        // link(rel := "stylesheet", href := "/path/to/your/styles.css")
      ),
      body(
        onload := "initFormBasedOnPath();", // Call the initialization function when the page loads
        div(cls := "sidebar")(
          for
            (scriptName, _) <- scripts.toSeq
          yield {
            a(href := s"/scripts/$scriptName")(scriptName)
          }
        ),
        div(cls := "main-content")(
          form(
            id := "scriptForm",
            method := "post",
            action := "/run"
          )
        )
      )
    ).render

  // JavaScript to change the form inputs based on the URL path
  def jsToChangeFormBasedOnPath(scripts: Map[String, Script]): String =
    s"""
    function initFormBasedOnPath() {
      const scriptName = window.location.pathname.replace("/scripts/", "");
      changeForm(scriptName);
    }
    
    function changeForm(scriptName) {
      const form = document.getElementById('scriptForm');
      form.innerHTML = ''; // Clear the form
      
      const scripts = ${scriptsToJsonArray(scripts)};
      const selectedScript = scripts.find(script => script[0] === scriptName);
      
      if (selectedScript) {
        // add the script name as a hidden input
        const nameInput = document.createElement('input');
        nameInput.type = 'text';
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

  def scriptsToJsonArray(scripts: Map[String, Script]): String =
    scripts
      .toSeq
      .map { case (name, script) =>
        s"""["$name", [${script.inputs.map(i => "\"" + i + "\"").mkString(",")}]]"""
      }
      .mkString("[", ",", "]")
}
