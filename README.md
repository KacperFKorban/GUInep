## GUInep

> Guinep are small tropical fruits from the Caribbean and Central/Southern America. They are described to taste like a cross between Lime and Lychee. 

PoC library to turn Scala functions into gui forms with a single line of code.

Code example:
```scala
import guinep.*

def upperCaseText(text: String): String =
  text.toUpperCase

@main
def run =
  guinep(upperCaseText)
```
