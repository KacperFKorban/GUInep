package guinep

import scala.quoted.*

private[guinep] object model {
  case class Fun(name: String, form: Form, run: List[Any] => String)

  case class Form(inputs: Seq[FormElement], namedFormElements: Map[String, FormElement]) {
    def formElementsJSONRepr =
      val elems = this.inputs.map(_.toJSONRepr).mkString(",")
      s"[$elems]"
    def namedFormElementsJSONRepr: String =
      val entries = this.namedFormElements.toList.map { (name, formElement) =>
        s""""$name": ${formElement.toJSONRepr}"""
      }
      .mkString(",")
      s"{$entries}"
  }
  object Form:
    given ToExpr[Form] with
      def apply(form: Form)(using Quotes): Expr[Form] = form match
        case Form(inputs, namedFormElements) =>
          '{ Form(${Expr(inputs)}, ${Expr(namedFormElements)}) }

  enum FormElement(val name: String):
    case TextInput(override val name: String) extends FormElement(name)
    case NumberInput(override val name: String) extends FormElement(name)
    case CheckboxInput(override val name: String) extends FormElement(name)
    case Dropdown(override val name: String, options: List[(String, FormElement)]) extends FormElement(name)
    case TextArea(override val name: String, rows: Option[Int] = None, cols: Option[Int] = None) extends FormElement(name)
    case DateInput(override val name: String) extends FormElement(name)
    case EmailInput(override val name: String) extends FormElement(name)
    case PasswordInput(override val name: String) extends FormElement(name)
    case FieldSet(override val name: String, elements: List[FormElement]) extends FormElement(name)
    case NamedRef(override val name: String, ref: String) extends FormElement(name)

    def constrOrd: Int = this match
      case TextInput(_) => 0
      case NumberInput(_) => 1
      case CheckboxInput(_) => 2
      case Dropdown(_, _) => 3
      case TextArea(_, _, _) => 4
      case DateInput(_) => 5
      case EmailInput(_) => 6
      case PasswordInput(_) => 7
      case FieldSet(_, _) => 8
      case NamedRef(_, _) => 9

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
        // TODO(kπ) this sortBy isn't 100% sure to be working (the only requirement is for the first constructor to not be recursive; this is a graph problem, sorta)
        s"""{ "name": '$name', "type": 'dropdown', "options": [${options.sortBy(_._2).map { case (k, v) => s"""{"name": "$k", "value": ${v.toJSONRepr}}""" }.mkString(",")}] }"""
      case FormElement.TextArea(name, rows, cols) =>
        s"""{ "name": '$name', "type": 'textarea', "rows": ${rows.getOrElse("")}, "cols": ${cols.getOrElse("")} }"""
      case FormElement.DateInput(name) =>
        s"""{ "name": '$name', "type": 'date' }"""
      case FormElement.EmailInput(name) =>
        s"""{ "name": '$name', "type": 'email' }"""
      case FormElement.PasswordInput(name) =>
        s"""{ "name": '$name', "type": 'password' }"""
      case FormElement.NamedRef(name, ref) =>
        s"""{ "name": '$name', "ref": '$ref', "type": 'namedref' }"""

  object FormElement:
    given ToExpr[FormElement] with
      def apply(formElement: FormElement)(using Quotes): Expr[FormElement] = formElement match
        case FormElement.FieldSet(name, elements) =>
          '{ FormElement.FieldSet(${Expr(name)}, ${Expr(elements)}) }
        case FormElement.TextInput(name) =>
          '{ FormElement.TextInput(${Expr(name)}) }
        case FormElement.NumberInput(name) =>
          '{ FormElement.NumberInput(${Expr(name)}) }
        case FormElement.CheckboxInput(name) =>
          '{ FormElement.CheckboxInput(${Expr(name)}) }
        case FormElement.Dropdown(name, options) =>
          '{ FormElement.Dropdown(${Expr(name)}, ${Expr(options)}) }
        case FormElement.TextArea(name, rows, cols) =>
          '{ FormElement.TextArea(${Expr(name)}, ${Expr(rows)}, ${Expr(cols)}) }
        case FormElement.DateInput(name) =>
          '{ FormElement.DateInput(${Expr(name)}) }
        case FormElement.EmailInput(name) =>
          '{ FormElement.EmailInput(${Expr(name)}) }
        case FormElement.PasswordInput(name) =>
          '{ FormElement.PasswordInput(${Expr(name)}) }
        case FormElement.NamedRef(name, ref) =>
          '{ FormElement.NamedRef(${Expr(name)}, ${Expr(ref)}) }

    given Ordering[FormElement] = new Ordering[FormElement] {
      def compare(x: FormElement, y: FormElement): Int =
        if x.constrOrd < y.constrOrd then -1
        else if x.constrOrd > y.constrOrd then 1
        else (x, y) match
          case (FormElement.FieldSet(_, elems1), FormElement.FieldSet(_, elems2)) =>
            elems1.size - elems2.size
          case (FormElement.Dropdown(_, opts1), FormElement.Dropdown(_, opts2)) =>
            opts1.size - opts2.size
          case _ => 0
  }
}
