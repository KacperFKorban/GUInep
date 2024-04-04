package guinep
package tests

import guinep.model.*

class FormGenTests extends munit.FunSuite {
  import TestsData.*

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
    "showDouble",
    showDouble,
    Form(
      Seq(
        FormElement.FloatingNumberInput("d")
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "multiplyShorts",
    multiplyShorts,
    Form(
      Seq(
        FormElement.NumberInput("a"),
        FormElement.NamedRef("b", "scala.Short")
      ),
      Map(
        "scala.Short" -> FormElement.NumberInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "divideFloats",
    divideFloats,
    Form(
      Seq(
        FormElement.FloatingNumberInput("a"),
        FormElement.NamedRef("b", "scala.Float")
      ),
      Map(
        "scala.Float" -> FormElement.FloatingNumberInput("value")
      )
    )
  )

  checkGeneratedFormEquals(
    "subtractLongs",
    subtractLongs,
    Form(
      Seq(
        FormElement.NumberInput("a"),
        FormElement.NamedRef("b", "scala.Long")
      ),
      Map(
        "scala.Long" -> FormElement.NumberInput("value")
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
                FormElement.NamedRef("left", "guinep.tests.TestsData$.IntTree"),
                FormElement.NamedRef("value", "scala.Int"),
                FormElement.NamedRef("right", "guinep.tests.TestsData$.IntTree")
              )
            )
          )
        )
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value"),
        "guinep.tests.TestsData$.IntTree" -> FormElement.Dropdown(
          "value",
          List(
            "Leaf" -> FormElement.FieldSet("value", Nil),
            "Node" -> FormElement.FieldSet(
              "value",
              List(
                FormElement.NamedRef("left", "guinep.tests.TestsData$.IntTree"),
                FormElement.NamedRef("value", "scala.Int"),
                FormElement.NamedRef("right", "guinep.tests.TestsData$.IntTree")
              )
            )
          )
        )
      
      )
    )
  )

  // TODO(kπ) Add test for WeirdGADT

  // TODO(kπ) Add test for isInTreeExt

  // TODO(kπ) Add test for addManyParamLists
}
