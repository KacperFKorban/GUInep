## guinep

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
