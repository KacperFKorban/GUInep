[![guinep Scala version support](https://index.scala-lang.org/kacperfkorban/guinep/guinep/latest.svg)](https://index.scala-lang.org/kacperfkorban/guinep/guinep)

# GUInep

> Guinep are small tropical fruits from the Caribbean and Central/Southern America. They are described to taste like a cross between Lime and Lychee. 

Automatic UI forms for Scala 3 functions

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

<img src="https://github.com/KacperFKorban/GUInep/assets/39772805/556b6c1b-ea72-4089-8cbd-16f680484177" style="max-width: 100%; max-height: 100%">

## Current limitations

Currently GUInep doesn't support:
- Functions with multiple parameter lists (and by extension - extension methods) - https://github.com/KacperFKorban/GUInep/issues/33
- Fancier GADTs with unobvious inheritance, type bounds and variance - https://github.com/KacperFKorban/GUInep/issues/30
- Union and intersection types - https://github.com/KacperFKorban/GUInep/issues/44
- Opaque types - https://github.com/KacperFKorban/GUInep/issues/49

## Contributing

If you have any ideas on how to improve GUInep, feel free to open an issue.

All bug reports and feature requests are highly appreciated.