package pytanie.test.utils

import scala.collection.mutable

sealed trait Trace[-T]:
  type Content
  def append(elem: T): Unit
  def content: Content

class LogingTrace[T] extends Trace[T]:
  type Content = Seq[T]
  private val buffer = mutable.ArrayBuffer[T]()
  def append(elem: T): Unit = buffer.addOne(elem)
  def content: Content = buffer.toVector

object NoTrace extends Trace[Any]:
  type Content = Nothing
  def append(elem: Any) = ()
  def content: Content = ???

inline def log[T](elem: T)(using trace: Trace[T]): Unit =
  trace.append(elem)
