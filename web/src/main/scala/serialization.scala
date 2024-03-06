package guinep

import guinep.*
import guinep.model.*
import zio.*
import zio.json.*
import zio.json.ast.*
import zio.json.ast.Json.*
import scala.util.chaining.*

private[guinep] object serialization:
  def sequenceEither[A, B](eithers: List[Either[A, B]]): Either[A, List[B]] =
    eithers.foldLeft(Right(List.empty): Either[A, List[B]]) { (acc, e) =>
      acc.flatMap(a => e.map(b => a :+ b))
    }

  extension (formElements: List[FormElement])
    def parseJSONValue(value: Obj): Either[String, Map[String, Any]] =
      formElements
        .map { element =>
          val v = value.get(element.name).toRight(s"Missing value for ${element.name}").flatMap(element.parseJSONValue)
          v.map(element.name -> _)
        }
        .pipe(sequenceEither).map(_.toMap)

    def sortByArgs(values: Map[String, Any]): List[Any] =
      formElements.map { element =>
        values(element.name)
      }

  extension (formElement: FormElement)
    def parseJSONValue(value: Json): Either[String, Any] = formElement match
      case FormElement.FieldSet(name, elements) =>
        for {
          m <- value.asObject.toRight(s"Invalid object: $value")
          res <- elements.parseJSONValue(m)
        } yield res
      case FormElement.TextInput(_) => value.asString.toRight(s"Invalid string: $value")
      case FormElement.NumberInput(_) => value.asString.flatMap(_.toIntOption).toRight(s"Invalid number: $value")
      case FormElement.CheckboxInput(_) => value.asBoolean.toRight(s"Invalid boolean: $value")
      case FormElement.Dropdown(_, options) =>
        for {
          v <- value.asString.toRight(s"Invalid string: $value")
          res <- options.find(_._1 == v).map(_._2).toRight(s"Invalid option: $value")
        } yield res
      case FormElement.TextArea(_, _, _) => Right(value)
      case _ => Left(s"Unsupported form element: $formElement")
