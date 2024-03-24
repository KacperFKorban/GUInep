package guinep
package tests

import guinep.model.*

class FormGenTests extends munit.FunSuite {
  import FormGenTests.*

  inline def checkGeneratedFormEquals(name: String, inline f: Any, expected: Form): Unit =
    test(s"generate correct form for function $name") {
      val infos = macros.funInfos(f)
      assert(infos.length == 1, s"Expected 1 form info, got: $infos")
      val info = infos.head
      assertEquals(info.form, expected)
    }

  checkGeneratedFormEquals(
    "upperCaseText",
    upperCaseText,
    Form(Seq(FormElement.TextInput("text")), Map.empty)
  )

  checkGeneratedFormEquals(
    "add",
    add,
    Form(
      Seq(
        FormElement.NumberInput("a"),
        FormElement.NamedRef("b", "scala.Int")
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "concat",
    concat,
    Form(
      Seq(
        FormElement.TextInput("a"),
        FormElement.NamedRef("b", "scala.Predef$.String")
      ),
      Map(
        "scala.Predef$.String" -> FormElement.TextInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "giveALongText",
    giveALongText,
    Form(
      Seq(
        FormElement.CheckboxInput("b")
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "addObj",
    addObj,
    Form(
      Seq(
        FormElement.FieldSet(
          "add",
          List(
            FormElement.NumberInput("a"),
            FormElement.NamedRef("b", "scala.Int")
          )
        )
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "greetMaybeName",
    greetMaybeName,
    Form(
      Seq(
        FormElement.Dropdown(
          "maybeName",
          List(
            "None" -> FormElement.FieldSet("value", Nil),
            "Some" -> FormElement.FieldSet(
              "value",
              List(FormElement.TextInput("value"))
            )
          )
        )
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "greetInLanguage",
    greetInLanguage,
    Form(
      Seq(
        FormElement.TextInput("name"),
        FormElement.Dropdown(
          "language",
          List(
            "English" -> FormElement.FieldSet("value", Nil),
            "Polish" -> FormElement.FieldSet("value", Nil)
          )
        )
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "nameWithPossiblePrefix",
    nameWithPossiblePrefix,
    Form(
      Seq(
        FormElement.TextInput("name"),
        FormElement.Dropdown(
          "maybePrefix",
          List(
            "JustString" -> FormElement.FieldSet("value", List(FormElement.NamedRef("value", "scala.Predef$.String"))),
            "NoString" -> FormElement.FieldSet("value", Nil)
          )
        )
      ),
      Map(
        "scala.Predef$.String" -> FormElement.TextInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "nameWithPossiblePrefix1",
    nameWithPossiblePrefix1,
    Form(
      Seq(
        FormElement.TextInput("name"),
        FormElement.Dropdown(
          "maybePrefix",
          List(
            "JustString" -> FormElement.FieldSet("value", List(FormElement.NamedRef("value", "scala.Predef$.String"))),
            "NoString" -> FormElement.FieldSet("value", Nil)
          )
        )
      ),
      Map(
        "scala.Predef$.String" -> FormElement.TextInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "roll20",
    roll20,
    Form(Seq.empty, Map.empty)
  )

  checkGeneratedFormEquals(
    "roll6",
    roll6(),
    Form(Seq.empty, Map.empty)
  )

  // TODO(kπ) Add test for WeirdGADT

  checkGeneratedFormEquals(
    "concatAll",
    concatAll,
    Form(
      Seq(
        FormElement.Dropdown(
          "elems",
          List(
            "::" -> FormElement.FieldSet(
              "value",
              List(FormElement.TextInput("head"), FormElement.NamedRef("next", "scala.collection.immutable.List[scala.Predef$.String]"))
            ),
            "Nil" -> FormElement.FieldSet("value", Nil)
          )
        )
      ),
      Map(
        "scala.collection.immutable.List[scala.Predef$.String]" -> FormElement.Dropdown(
          "value",
          List(
            "::" -> FormElement.FieldSet(
              "value",
              List(FormElement.TextInput("head"), FormElement.NamedRef("next", "scala.collection.immutable.List[scala.Predef$.String]"))
            ),
            "Nil" -> FormElement.FieldSet("value", Nil)
          )
        )
      )
    )
  )

  checkGeneratedFormEquals(
    "isInTree",
    isInTree,
    Form(
      Seq(
        FormElement.NumberInput("elem"),
        FormElement.Dropdown(
          "tree",
          List(
            "Leaf" -> FormElement.FieldSet("value", Nil),
            "Node" -> FormElement.FieldSet(
              "value",
              List(
                FormElement.NamedRef("left", "guinep.tests.FormGenTests$.IntTree"),
                FormElement.NamedRef("value", "scala.Int"),
                FormElement.NamedRef("right", "guinep.tests.FormGenTests$.IntTree")
              )
            )
          )
        )
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value"),
        "guinep.tests.FormGenTests$.IntTree" -> FormElement.Dropdown(
          "value",
          List(
            "Leaf" -> FormElement.FieldSet("value", Nil),
            "Node" -> FormElement.FieldSet(
              "value",
              List(
                FormElement.NamedRef("left", "guinep.tests.FormGenTests$.IntTree"),
                FormElement.NamedRef("value", "scala.Int"),
                FormElement.NamedRef("right", "guinep.tests.FormGenTests$.IntTree")
              )
            )
          )
        )
      
      )
    )
  )

  // TODO(kπ) Add test for isInTreeExt

  // TODO(kπ) Add test for addManyParamLists
}

object FormGenTests {
  def upperCaseText(text: String): String =
    text.toUpperCase

  def add(a: Int, b: Int) =
    a + b

  def concat(a: String, b: String) =
    a + b

  def giveALongText(b: Boolean): String =
    if b then "XXXXXXXXXXXXXXXXXXXXXX" else "-"

  case class Add(a: Int, b: Int)

  def addObj(add: Add) =
    add.a + add.b

  def greetMaybeName(maybeName: Option[String]): String =
    maybeName.fold("Hello!")(name => s"Hello, $name!")

  enum Language:
    case English, Polish

  def greetInLanguage(name: String, language: Language): String =
    language match
      case Language.English => s"Hello, $name!"
      case Language.Polish => s"Cześć, $name!"

  sealed trait MaybeString
  case class JustString(value: String) extends MaybeString
  case object NoString extends MaybeString

  def nameWithPossiblePrefix(name: String, maybePrefix: MaybeString): String =
    maybePrefix match
      case JustString(value) => s"$value $name"
      case NoString => name

  enum MaybeString1:
    case JustString(value: String)
    case NoString

  def nameWithPossiblePrefix1(name: String, maybePrefix: MaybeString1): String =
    maybePrefix match
      case MaybeString1.JustString(value) => s"$value $name"
      case MaybeString1.NoString => name

  def roll20: Int =
    scala.util.Random.nextInt(20) + 1

  def roll6(): Int =
    scala.util.Random.nextInt(6) + 1

  sealed trait WeirdGADT[+A]
  case class IntValue(value: Int) extends WeirdGADT[Int]
  case class SomeValue[+A](value: A) extends WeirdGADT[A]
  case class SomeOtherValue[+A, +B](value: A, value2: B) extends WeirdGADT[A]

  // This fails on unknown type params
  def printsWeirdGADT(g: WeirdGADT[String]): String = g match
    case SomeValue(value) => s"SomeValue($value)"
    case SomeOtherValue(value, value2) => s"SomeOtherValue($value, $value2)"

  def concatAll(elems: List[String]): String =
    elems.mkString

  enum IntTree:
    case Leaf
    case Node(left: IntTree, value: Int, right: IntTree)

  def isInTree(elem: Int, tree: IntTree): Boolean = tree match
    case IntTree.Leaf => false
    case IntTree.Node(left, value, right) =>
      value == elem || isInTree(elem, left) || isInTree(elem, right)

  // Can't be handled right now
  extension (elem: Int)
    def isInTreeExt(tree: IntTree): Boolean = tree match
    case IntTree.Leaf => false
    case IntTree.Node(left, value, right) =>
      value == elem || elem.isInTreeExt(left) || elem.isInTreeExt(right)

  def addManyParamLists(a: Int)(b: Int): Int =
    a + b
}
