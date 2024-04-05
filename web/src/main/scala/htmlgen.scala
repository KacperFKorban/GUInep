package guinep

import guinep.*
import guinep.model.*
import zio.*
import zio.http.*
import zio.http.template.*
import zio.http.codec.*

private[guinep] trait HtmlGen {
  val funs: Seq[(String, Fun)]
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
          .main-content { margin-left: 232px; padding: 40px; display: flex; justify-content: center; padding-top: 20px; }
          .form-container { width: 600px; }
          label { display: inline-block; margin-right: 10px; vertical-align: middle; }
          input:not([type=submit]) { display: inline-block; padding: 8px; margin-bottom: 10px; box-sizing: border-box; }
          input[type=submit] { background-color: #4CAF50; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; }
          input[type=submit]:hover { background-color: #45a049; }
          select { display: inline-block; margin-bottom: 10px; padding: 8px; box-sizing: border-box; }
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
      changeForm(funName);
    }

    function changeForm(funName) {
      const form = document.getElementById('guinepForm');
      form.innerHTML = ''; // Clear the form
      form.funName = funName;

      const funs = ${funsToJsonArray};
      const selectedFun = funs.find(fun => fun[0] === funName);

      function setSubFormOnSelect(subForm, formElement, selectedOption, namedLookup) {
        const selectedOptionForm = formElement.options.find(option => option.name === selectedOption);
        subForm.innerHTML = '';
        if (selectedOptionForm.value.elements.length > 0) {
          subForm.style.visibility = 'visible';
          selectedOptionForm.value.elements.forEach(subFormElem => addFormElement(subForm, subFormElem, namedLookup));
        } else {
          subForm.style.visibility = 'hidden';
        }
      }

      function addFormElement(form, formElem, namedLookup) {
        const br = document.createElement('br');
        if (formElem.type === 'fieldset') {
          const fieldset = document.createElement('fieldset');
          fieldset.name = formElem.name;
          const legend = document.createElement('legend');
          legend.innerText = formElem.name;
          fieldset.appendChild(legend);
          formElem.elements.forEach(subElem => addFormElement(fieldset, subElem, namedLookup));
          form.appendChild(fieldset);
          form.appendChild(br.cloneNode());
        } else if (formElem.type === 'dropdown') {
          const fieldset = document.createElement('fieldset');
          fieldset.name = formElem.name;
          const legend = document.createElement('legend');
          legend.innerText = formElem.name;
          fieldset.appendChild(legend);
          const nameSelect = document.createElement('select');
          nameSelect.name = 'name';
          formElem.options.forEach(option => {
            const optionElem = document.createElement('option');
            optionElem.value = option.name;
            optionElem.innerText = option.name;
            nameSelect.appendChild(optionElem);
          });
          fieldset.appendChild(nameSelect);
          fieldset.appendChild(br.cloneNode());
          const valueFieldSet = document.createElement('fieldset');
          valueFieldSet.name = 'value';
          fieldset.appendChild(valueFieldSet);
          nameSelect.onchange = (selection) => setSubFormOnSelect(valueFieldSet, formElem, selection.target.value, namedLookup);
          setSubFormOnSelect(valueFieldSet, formElem, formElem.options[0].name, namedLookup);
          form.appendChild(fieldset);
          form.appendChild(br.cloneNode());
        } else if (formElem.type == 'namedref') {
          const formElemFromLookup = namedLookup[formElem.ref];
          formElemFromLookup.name = formElem.name;
          addFormElement(form, formElemFromLookup, namedLookup);
        } else if (formElem.type == 'float') {
          const label = document.createElement('label');
          label.innerText = formElem.name + ': ';
          label.for = formElem.name;
          form.appendChild(label);
          const input = document.createElement('input');
          input.type = 'number';
          input.step = 'any';
          input.name = formElem.name;
          input.id = formElem.name;
          input.placeholder = formElem.name;
          form.appendChild(input);
          form.appendChild(br.cloneNode());
        } else if (formElem.type == 'char') {
          const label = document.createElement('label');
          label.innerText = formElem.name + ': ';
          label.for = formElem.name;
          form.appendChild(label);
          const input = document.createElement('input');
          input.type = 'text';
          input.maxLength = '1';
          input.name = formElem.name;
          input.id = formElem.name;
          input.placeholder = formElem.name;
          form.appendChild(input);
          form.appendChild(br.cloneNode());
        } else {
          const label = document.createElement('label');
          label.innerText = formElem.name + ': ';
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

      if (selectedFun) {
        // add name title to the top of the form
        const nameTitle = document.createElement('h1');
        nameTitle.innerText = funName;
        form.appendChild(nameTitle);
        // add the function inputs as text inputs
        const namedLookup = selectedFun[2];
        selectedFun[1].forEach(formElem => {
          addFormElement(form, formElem, namedLookup);
        });
        const submitButton = document.createElement('input');
        submitButton.type = 'submit';
        submitButton.value = 'Submit';
        form.appendChild(submitButton);
      }
    }

    document.addEventListener("DOMContentLoaded", function() {
      document.getElementById("guinepForm").addEventListener("submit", function(e) {
        e.preventDefault(); // Prevent the default form submission and redirect

        const form = this;
        const jsonData = {};
        const funName = form.funName;

        const processElement = (element, parentObj) => {
          if (element.tagName === 'FIELDSET') {
            const name = element.name;
            parentObj[name] = {};
            Array.from(element.children).forEach(innerEl => {
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
          } else if (element.tagName === 'SELECT') {
            const name = element.name;
            const value = element.value;
            parentObj[name] = value;
          }
        };

        Array.from(form.children).forEach(el => {
          if ((el.tagName === 'INPUT' && el.type !== 'submit') || el.tagName === 'FIELDSET') {
            processElement(el, jsonData);
          }
        });

        fetch(`/` + funName, {
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
}
