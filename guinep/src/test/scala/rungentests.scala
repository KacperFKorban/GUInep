package guinep
package tests

import guinep.model.* 

class RunGenTests extends munit.FunSuite {
  import TestsData.*

  inline def checkGeneratedRunResultSatisfies(name: String, inline f: Any, args: List[Any], assertResult: Any => Unit): Unit =
    test(s"generate run for function $name applied to ${args} is correct") {
      val infos = macros.funInfos(f)
      assert(infos.length == 1, s"Expected 1 run info, got: $infos")
      val info = infos.head
      assertResult(info.run(args))
    }

  inline def checkGeneratedRunResultEquals(name: String, inline f: Any, args: List[Any], expected: String): Unit =
    checkGeneratedRunResultSatisfies(name, f, args, result => assertEquals(result, expected))

  checkGeneratedRunResultEquals(
    "upperCaseText",
    upperCaseText,
    List("hello"),
    "HELLO"
  )

  checkGeneratedRunResultEquals(
    "add",
    add,
    List(1l, 2l),
    "3"
  )

  checkGeneratedRunResultEquals(
    "concat",
    concat,
    List("hello", "world"),
    "helloworld"
  )

  checkGeneratedRunResultEquals(
    "giveALongText",
    giveALongText,
    List(true),
    "XXXXXXXXXXXXXXXXXXXXXX"
  )

  checkGeneratedRunResultEquals(
    "giveALongText",
    giveALongText,
    List(false),
    "-"
  )

  checkGeneratedRunResultEquals(
    "addObj",
    addObj,
    List(Map("a" -> 1l, "b" -> 2l)),
    "3"
  )

  checkGeneratedRunResultEquals(
    "greetMaybeName",
    greetMaybeName,
    List(Map("name" -> "Some", "value" -> Map("value" -> "John"))),
    "Hello, John!"
  )

  checkGeneratedRunResultEquals(
    "greetMaybeName",
    greetMaybeName,
    List(Map("name" -> "None", "value" -> Map.empty)),
    "Hello!"
  )

  checkGeneratedRunResultEquals(
    "greetInLanguage",
    greetInLanguage,
    List("John", Map("name" -> "English", "value" -> Map.empty)),
    "Hello, John!"
  )

  checkGeneratedRunResultEquals(
    "greetInLanguage",
    greetInLanguage,
    List("John", Map("name" -> "Polish", "value" -> Map.empty)),
    "Cześć, John!"
  )

  checkGeneratedRunResultEquals(
    "nameWithPossiblePrefix",
    nameWithPossiblePrefix,
    List("John", Map("name" -> "JustString", "value" -> Map("value" -> "Mr."))),
    "Mr. John"
  )

  checkGeneratedRunResultEquals(
    "nameWithPossiblePrefix",
    nameWithPossiblePrefix,
    List("John", Map("name" -> "NoString", "value" -> Map.empty)),
    "John"
  )

  checkGeneratedRunResultEquals(
    "nameWithPossiblePrefix1",
    nameWithPossiblePrefix1,
    List("John", Map("name" -> "JustString", "value" -> Map("value" -> "Mr."))),
    "Mr. John"
  )

  checkGeneratedRunResultEquals(
    "nameWithPossiblePrefix1",
    nameWithPossiblePrefix1,
    List("John", Map("name" -> "NoString", "value" -> Map.empty)),
    "John"
  )

  checkGeneratedRunResultSatisfies(
    "roll20",
    roll20,
    List.empty,
    {
      case resultString: String =>
        val result = resultString.toInt
        assert(result >= 1 && result <= 20)
    }
  )

  checkGeneratedRunResultSatisfies(
    "roll6",
    roll6(),
    List.empty,
    {
      case resultString: String =>
        val result = resultString.toInt
        assert(result >= 1 && result <= 6)
    }
  )

  checkGeneratedRunResultEquals(
    "concatAll",
    concatAll,
    List(Map("name" -> "::", "value" -> Map("head" -> "hello", "next" -> Map("name" -> "::", "value" -> Map("head" -> "world", "next" -> Map("name" -> "Nil", "value" -> Map.empty)))))),
    "helloworld"
  )

  checkGeneratedRunResultEquals(
    "concatAll",
    concatAll,
    List(Map("name" -> "Nil", "value" -> Map.empty)),
    ""
  )

  checkGeneratedRunResultEquals(
    "concatAll",
    concatAll,
    List(Map("name" -> "::", "value" -> Map("head" -> "hello", "next" -> Map("name" -> "Nil", "value" -> Map.empty)))),
    "hello"
  )

  checkGeneratedRunResultEquals(
    "showDouble",
    showDouble,
    List(1.0d),
    "1.0"
  )

  checkGeneratedRunResultEquals(
    "multiplyShorts",
    multiplyShorts,
    List(1l, 2l),
    "2"
  )

  checkGeneratedRunResultEquals(
    "divideFloats",
    divideFloats,
    List(1.0d, 2.0d),
    "0.5"
  )

  checkGeneratedRunResultEquals(
    "subtractLongs",
    subtractLongs,
    List(2l, 1l),
    "1"
  )

  checkGeneratedRunResultEquals(
    "isInTree",
    isInTree,
    List(1l, Map("name" -> "Node", "value" -> Map("left" -> Map("name" -> "Leaf", "value" -> Map.empty), "value" -> 1l, "right" -> Map("name" -> "Leaf", "value" -> Map.empty)))),
    "true"
  )

  checkGeneratedRunResultEquals(
    "isInTree",
    isInTree,
    List(1l, Map("name" -> "Leaf", "value" -> Map.empty)),
    "false"
  )

  // TODO(kπ) Add test for WeirdGADT

  // TODO(kπ) Add test for isInTreeExt

  // TODO(kπ) Add test for addManyParamLists
}