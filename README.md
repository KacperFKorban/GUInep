## GUInep

> Guinep are small tropical fruits from the Caribbean and Central/Southern America. They are described to taste like a cross between Lime and Lychee. 

PoC library to turn Scala 3 functions into UI forms with a single line of code.

Code example:
```scala
def upperCaseText(text: String): String =
  text.toUpperCase

@main
def run =
  guinep.web(upperCaseText)
```

Demo:

![GUInep_classes_demo](https://github.com/KacperFKorban/GUInep/assets/39772805/556b6c1b-ea72-4089-8cbd-16f680484177)
