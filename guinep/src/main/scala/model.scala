package guinep

import scala.quoted.*

private[guinep] object model {
  case class Fun(name: String, form: Form, run: List[Any] => String)

  case class Form(inputs: Seq[FormElement], namedFormElements: Map[String, FormElement])
  object Form:
    given ToExpr[Form] with
      def apply(form: Form)(using Quotes): Expr[Form] = form match
        case Form(inputs, namedFormElements) =>
          '{ Form(${Expr(inputs)}, ${Expr(namedFormElements)}) }

  object Types {
    enum FloatingType:
      case Double
      case Float

    object FloatingType:
      given ToExpr[FloatingType] with
        def apply(x: FloatingType)(using Quotes): Expr[FloatingType] = x match
          case FloatingType.Double =>
            '{ FloatingType.Double }
          case FloatingType.Float =>
            '{ FloatingType.Float }

    enum IntType:
      case Int
      case Long
      case Byte
      case Short

    object IntType:
      given ToExpr[IntType] with
        def apply(x: IntType)(using Quotes): Expr[IntType] = x match
          case IntType.Int =>
            '{ IntType.Int }
          case IntType.Long => 
            '{ IntType.Long }
          case IntType.Byte =>
            '{ IntType.Byte }
          case IntType.Short =>
            '{ IntType.Short }

    enum ListType:
      case List
      case Seq
      case Vector
      case Array

    object ListType:
      given ToExpr[ListType] with
        def apply(x: ListType)(using Quotes): Expr[ListType] = x match
          case ListType.List =>
            '{ ListType.List }
          case ListType.Seq =>
            '{ ListType.Seq }
          case ListType.Vector =>
            '{ ListType.Vector }
  }

  enum FormElement(val name: String):
    case TextInput(override val name: String) extends FormElement(name)
    case CharInput(override val name: String) extends FormElement(name)
    case NumberInput(override val name: String, underlying: Types.IntType) extends FormElement(name)
    case FloatingNumberInput(override val name: String, underlying: Types.FloatingType) extends FormElement(name)
    case CheckboxInput(override val name: String) extends FormElement(name)
    case Dropdown(override val name: String, options: List[(String, FormElement)]) extends FormElement(name)
    case ListInput(override val name: String, element: FormElement, underlying: Types.ListType) extends FormElement(name)
    case TextArea(override val name: String, rows: Option[Int] = None, cols: Option[Int] = None) extends FormElement(name)
    case DateInput(override val name: String) extends FormElement(name)
    case EmailInput(override val name: String) extends FormElement(name)
    case PasswordInput(override val name: String) extends FormElement(name)
    case FieldSet(override val name: String, elements: List[FormElement]) extends FormElement(name)
    case NamedRef(override val name: String, ref: String) extends FormElement(name)

    def constrOrd: Int = this match
      case TextInput(_) => 0
      case CharInput(_) => 0
      case NumberInput(_, _) => 1
      case FloatingNumberInput(_, _) => 1
      case CheckboxInput(_) => 2
      case Dropdown(_, _) => 3
      case ListInput(_, _, _) => 3
      case TextArea(_, _, _) => 4
      case DateInput(_) => 5
      case EmailInput(_) => 6
      case PasswordInput(_) => 7
      case FieldSet(_, _) => 8
      case NamedRef(_, _) => 9

  object FormElement:
    given ToExpr[FormElement] with
      def apply(formElement: FormElement)(using Quotes): Expr[FormElement] = formElement match
        case FormElement.FieldSet(name, elements) =>
          '{ FormElement.FieldSet(${Expr(name)}, ${Expr(elements)}) }
        case FormElement.TextInput(name) =>
          '{ FormElement.TextInput(${Expr(name)}) }
        case FormElement.CharInput(name) =>
          '{ FormElement.CharInput(${Expr(name)}) }
        case FormElement.NumberInput(name, underlying) =>
          '{ FormElement.NumberInput(${Expr(name)}, ${Expr(underlying)}) }
        case FormElement.FloatingNumberInput(name, underlying) =>
          '{ FormElement.FloatingNumberInput(${Expr(name)}, ${Expr(underlying)}) }
        case FormElement.CheckboxInput(name) =>
          '{ FormElement.CheckboxInput(${Expr(name)}) }
        case FormElement.Dropdown(name, options) =>
          '{ FormElement.Dropdown(${Expr(name)}, ${Expr(options)}) }
        case FormElement.ListInput(name, element, underlying) =>
          '{ FormElement.ListInput(${Expr(name)}, ${Expr(element)}, ${Expr(underlying)}) }
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

    // This ordering is a hack to avoid placing recursive constructors as first options in a dropdown
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
