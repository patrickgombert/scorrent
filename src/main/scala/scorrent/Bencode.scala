package scorrent;

import scala.collection.immutable.ListMap

object Bencode {
  def encode(input: Int) : String = {
    "i" + input + "e"
  }

  def encode(input: String) : String = {
    input.length + ":" + input
  }

  def encode(input: List[Any]) : String = {
    "l" + input.map(i => encode(i)).mkString + "e"
  }

  def encode(input: Map[String, Any]) : String = {
    "d" + input.toSeq.sortBy(_._1).map{ case (k, v) => encode(k) + encode(v) }.mkString + "e"
  }

  def encode(input: Any) : String = {
    input match {
      case _: Int => encode(input.asInstanceOf[Int])
      case _: String => encode(input.asInstanceOf[String])
      case _ => throw new IllegalArgumentException("encode only works with String, Int, List[Any] & Map[String, Any]")
    }
  }
}
