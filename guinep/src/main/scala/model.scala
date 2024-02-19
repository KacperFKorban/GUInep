package guinep.internal

enum FieldType:
  case String, Int

case class Script(name: String, inputs: Seq[Input], run: List[String] => String)

case class Input(name: String, fieldType: FieldType)

trait Deserializer[T]:
  def deserialize(s: String): Either[String, T]

object Deserializer:
  implicit val stringDeserializer: Deserializer[String] = new Deserializer[String]:
    def deserialize(s: String): Either[String, String] = Right(s)

  implicit val intDeserializer: Deserializer[Int] = new Deserializer[Int]:
    def deserialize(s: String): Either[String, Int] = 
      s.toIntOption.toRight(s"Cannot convert $s to Int")
