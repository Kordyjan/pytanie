package pytanie

import upickle.default.*
import deriving.Mirror
import ujson.Value

trait Variable[T]:
  def name: String & Singleton
  def nullable: Boolean
  def write(t: T): ujson.Value
  inline def graphqlType: String = name + (if nullable then "" else "!")

object Variable:
  inline def derived[T: Writer](using mirror: Mirror.Of[T]) = new Variable[T]:
    inline val nameImpl = compiletime.constValue[mirror.MirroredLabel]
    def name = nameImpl
    def nullable: Boolean = false
    def write(t: T): Value = writeJs(t)

  given Variable[String] with
    def name = "String"
    def nullable: Boolean = false
    def write(t: String): Value = t

  given Variable[Int] with
    def name = "Int"
    def nullable: Boolean = false
    def write(t: Int): Value = t

  given [T: Variable]: Variable[Option[T]] with
    def name = summon[Variable[T]].name
    def nullable: Boolean = true
    def write(t: Option[T]): Value = t match
      case Some(x) => summon[Variable[T]].write(x)
      case None => ujson.Null