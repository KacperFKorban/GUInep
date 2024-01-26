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

Demo:

https://github.com/KacperFKorban/guinep/assets/39772805/b288b8e4-a068-4455-9fa5-073286b82af8
