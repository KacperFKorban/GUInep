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

https://github.com/KacperFKorban/guinep/assets/39772805/558c482a-3b91-4246-bbdb-cfda8b3b5d4c
