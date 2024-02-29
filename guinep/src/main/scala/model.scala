package guinep.internal

enum FieldType:
  case String, Int

case class Script(name: String, inputs: Seq[FieldType], run: List[Any] => String)
