package guinep.testrun

import guinep.*

def personsAge(name: String): Int = name match {
  case "Bartek" => 20
  case _ => 0
}

def upperCaseText(text: String): String = text.toUpperCase

@main
def run: Unit =
  guinep(personsAge, upperCaseText)
