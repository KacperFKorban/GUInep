package guinep.tests

object TestsData {
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

  def concatAll(elems: List[String]): String =
    elems.mkString

  def showDouble(d: Double): String =
    d.toString

  def multiplyShorts(a: Short, b: Short): Int =
    a * b

  def divideFloats(a: Float, b: Float): Float =
    a / b

  def subtractLongs(a: Long, b: Long): Long =
    a - b

  def codeOfChar(c: Char): Int =
    c.toInt

  def sumVector(v: Vector[Byte]): Byte =
    v.sum

  def productSeq(s: Seq[Float]): Float =
    s.product

  enum IntTree:
    case Leaf
    case Node(left: IntTree, value: Int, right: IntTree)

  def isInTree(elem: Int, tree: IntTree): Boolean = tree match
    case IntTree.Leaf => false
    case IntTree.Node(left, value, right) =>
      value == elem || isInTree(elem, left) || isInTree(elem, right)

  // This fails on unknown type params
  def printsWeirdGADT(g: WeirdGADT[String]): String = g match
    case SomeValue(value) => s"SomeValue($value)"
    case SomeOtherValue(value, value2) => s"SomeOtherValue($value, $value2)"

  // Can't be handled right now
  extension (elem: Int)
    def isInTreeExt(tree: IntTree): Boolean = tree match
    case IntTree.Leaf => false
    case IntTree.Node(left, value, right) =>
      value == elem || elem.isInTreeExt(left) || elem.isInTreeExt(right)

  def addManyParamLists(a: Int)(b: Int): Int =
    a + b
}
