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

  def decode(input: String) : Any = {
    doDecode(input)._1
  }

  private def doDecode(input: String) : Tuple2[Any, String] = {
    input.head match {
      case 'i' => decodeInt(input.tail)
      case 'l' => decodeList(input.tail)
      case 'd' => decodeMap(input.tail)
      case _ => decodeString(input)
    }
  }

  private def decodeInt(input: String) : Tuple2[Int, String] = {
    val int = input.takeWhile(c => c != 'e')
    (int.toInt, input.substring(int.length + 1, input.length))
  }

  private def decodeList(input: String) : Tuple2[List[Any], String] = {
    doDecodeList((List[Any](), input))
  }

  private def doDecodeList(accumulator: Tuple2[List[Any], String]) : Tuple2[List[Any], String] = {
    if (accumulator._2.head == 'e') {
      (accumulator._1, accumulator._2.tail)
    } else {
      val res = doDecode(accumulator._2)
      doDecodeList((accumulator._1 :+ res._1, res._2))
    }
  }

  private def decodeMap(input: String) : Tuple2[Map[String, Any], String] = {
    doDecodeMap((ListMap[String, Any](), input))
  }

  private def doDecodeMap(accumulator: Tuple2[Map[String, Any], String]) : Tuple2[Map[String, Any], String] = {
    if (accumulator._2.head == 'e') {
      (accumulator._1, accumulator._2.tail)
    } else {
      val key = doDecode(accumulator._2)
      val value = doDecode(key._2)
      doDecodeMap((accumulator._1 + (key._1.asInstanceOf[String] -> value._1), value._2))
    }
  }

  private def decodeString(input: String) : Tuple2[String, String] = {
    val size = input.takeWhile(c => c != ':')
    val frontLength = size.length + 1
    val stringLength = frontLength + size.toInt
    val str = input.substring(frontLength, stringLength)
    if (stringLength == input.size) {
      (str, "")
    } else {
      (str, input.substring(stringLength, input.size))
    }
  }
}
