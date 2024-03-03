package guinep.internal

enum FieldType:
  case String, Int

case class Script(name: String, inputs: Seq[Input], run: List[Any] => String)

case class Input(name: String, fieldType: FieldType)
