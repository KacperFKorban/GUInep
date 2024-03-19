# GUInep

> Guinep are small tropical fruits from the Caribbean and Central/Southern America. They are described to taste like a cross between Lime and Lychee. 

PoC library to turn Scala 3 functions into UI forms with a single line of code.

## Example

```scala
def upperCaseText(text: String): String =
  text.toUpperCase

@main
def run =
  guinep.web(upperCaseText)
```

This will start a local web server with auto generated endpoints for running all listed functions. And an auto generated front-end with a form for each function.

## Usage (with build tools)

### sbt

```scala
libraryDependencies ++= Seq(
  "io.github.kacperfkorban" %% "guinep-web" % "version_from_releases"
)
```

### scala-cli

```scala
//> using lib "io.github.kacperfkorban::guinep-web:version_from_releases"
```

## Usage (in code)

`GUInep` only exposes one function `guinep.web` which takes some number of functions as arguments.

```scala
def upperCaseText(text: String): String =
  text.toUpperCase

def rollDice(sides: Int): Int =
  scala.util.Random.nextInt(sides) + 1

@main
def run =
  guinep.web(
    upperCaseText,
    rollDice
  )
```

## Demo

![GUInep_classes_demo](https://github.com/KacperFKorban/GUInep/assets/39772805/556b6c1b-ea72-4089-8cbd-16f680484177)
