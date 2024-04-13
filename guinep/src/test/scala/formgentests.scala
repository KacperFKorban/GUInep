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
        FormElement.NumberInput("a", Types.IntType.Int),
        FormElement.NamedRef("b", "scala.Int")
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value", Types.IntType.Int)
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
            FormElement.NumberInput("a", Types.IntType.Int),
            FormElement.NamedRef("b", "scala.Int")
          )
        )
      ),
      Map(
        "scala.Int" -> FormElement.NumberInput("value", Types.IntType.Int)
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
        FormElement.ListInput("elems", FormElement.TextInput("elem"), Types.ListType.List)
      ),
      Map()
    )
  )

  checkGeneratedFormEquals(
    "showDouble",
    showDouble,
    Form(
      Seq(
        FormElement.FloatingNumberInput("d", Types.FloatingType.Double)
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "multiplyShorts",
    multiplyShorts,
    Form(
      Seq(
        FormElement.NumberInput("a", Types.IntType.Short),
        FormElement.NamedRef("b", "scala.Short")
      ),
      Map(
        "scala.Short" -> FormElement.NumberInput("value", Types.IntType.Short)
      )
    )
  )

  checkGeneratedFormEquals(
    "divideFloats",
    divideFloats,
    Form(
      Seq(
        FormElement.FloatingNumberInput("a", Types.FloatingType.Float),
        FormElement.NamedRef("b", "scala.Float")
      ),
      Map(
        "scala.Float" -> FormElement.FloatingNumberInput("value", Types.FloatingType.Float)
      )
    )
  )

  checkGeneratedFormEquals(
    "subtractLongs",
    subtractLongs,
    Form(
      Seq(
        FormElement.NumberInput("a", Types.IntType.Long),
        FormElement.NamedRef("b", "scala.Long")
      ),
      Map(
        "scala.Long" -> FormElement.NumberInput("value", Types.IntType.Long)
      )
    )
  )

  checkGeneratedFormEquals(
    "codeOfChar",
    codeOfChar,
    Form(
      Seq(
        FormElement.CharInput("c")
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "sumVector",
    sumVector,
    Form(
      Seq(
        FormElement.ListInput("v", FormElement.NumberInput("elem", Types.IntType.Byte), Types.ListType.Vector)
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "productSeq",
    productSeq,
    Form(
      Seq(
        FormElement.ListInput("s", FormElement.FloatingNumberInput("elem", Types.FloatingType.Float), Types.ListType.Seq)
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "showNullableInt",
    showNullableInt,
    Form(
      Seq(
        FormElement.Nullable("i", FormElement.NumberInput("i", Types.IntType.Int))
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "factorialBigInt",
    factorialBigInt,
    Form(
      Seq(
        FormElement.NumberInput("n", Types.IntType.BigInt)
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "inverseBigDecimal",
    inverseBigDecimal,
    Form(
      Seq(
        FormElement.FloatingNumberInput("bd", Types.FloatingType.BigDecimal)
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "sayBye",
    sayBye,
    Form(
      Seq(
        FormElement.HiddenInput("unit", "Unit")
      ),
      Map.empty
    )
  )

  checkGeneratedFormEquals(
    "isInTree",
    isInTree,
    Form(
      Seq(
        FormElement.NumberInput("elem", Types.IntType.Int),
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
        "scala.Int" -> FormElement.NumberInput("value", Types.IntType.Int),
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
