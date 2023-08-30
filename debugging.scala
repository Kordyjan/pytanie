package pytanie.debugging

import quoted.*

inline def typeOf[T](t: T): String = ${ typeOfImpl[T] }

def typeOfImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(TypeRepr.of[T].show)
