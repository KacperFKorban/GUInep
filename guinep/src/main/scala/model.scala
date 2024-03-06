package guinep.internal

case class Script(name: String, inputs: Seq[FormElement], run: List[Any] => String)

enum FormElement(val name: String):
  case FieldSet(override val name: String, elements: List[FormElement]) extends FormElement(name)
  case TextInput(override val name: String) extends FormElement(name)
  case NumberInput(override val name: String) extends FormElement(name)
  case CheckboxInput(override val name: String) extends FormElement(name)
  case Dropdown(override val name: String, options: List[(String, String)]) extends FormElement(name)
  case TextArea(override val name: String, rows: Option[Int] = None, cols: Option[Int] = None) extends FormElement(name)
  case DateInput(override val name: String) extends FormElement(name)
  case EmailInput(override val name: String) extends FormElement(name)
  case PasswordInput(override val name: String) extends FormElement(name)

  def toJSONRepr: String = this match
    case FormElement.FieldSet(name, elements) =>
      s"""{ "name": '$name', "type": 'fieldset', "elements": [${elements.map(_.toJSONRepr).mkString(",")}] }"""
    case FormElement.TextInput(name) =>
      s"""{ "name": '$name', "type": 'text' }"""
    case FormElement.NumberInput(name) =>
      s"""{ "name": '$name', "type": 'number' }"""
    case FormElement.CheckboxInput(name) =>
      s"""{ "name": '$name', "type": 'checkbox' }"""
    case FormElement.Dropdown(name, options) =>
      s"""{ "name": '$name', "type": 'dropdown', "options": [${options.map { case (k, v) => s"""{"key": "$k", "value": "$v"}""" }.mkString(",")}] }"""
    case FormElement.TextArea(name, rows, cols) =>
      s"""{ "name": '$name', "type": 'textarea', "rows": ${rows.getOrElse("")}, "cols": ${cols.getOrElse("")} }"""
    case FormElement.DateInput(name) =>
      s"""{ "name": '$name', "type": 'date' }"""
    case FormElement.EmailInput(name) =>
      s"""{ "name": '$name', "type": 'email' }"""
    case FormElement.PasswordInput(name) =>
      s"""{ "name": '$name', "type": 'password' }"""
