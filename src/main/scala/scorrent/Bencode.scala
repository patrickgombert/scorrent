package scorrent;

import scala.collection.immutable.ListMap

object Bencode {
  def encode(input: Int) : String = {
    "i" + input + "e"
  }

  def encode(input: String) : String = {
    input.getBytes("UTF-8").length + ":" + input
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

  def decode(input: Array[Byte]) : Any = {
    doDecode(input)._1
  }

  private def doDecode(input: Array[Byte]) : Tuple2[Any, Array[Byte]] = {
    input.head.toChar match {
      case 'i' => decodeInt(input.tail)
      case 'l' => decodeList(input.tail)
      case 'd' => decodeMap(input.tail)
      case _ => decodeString(input)
    }
  }

  private def decodeInt(input: Array[Byte]) : Tuple2[Int, Array[Byte]] = {
    val int = input.takeWhile(c => c != 'e'.toByte)
    val decodedInt = int.foldLeft("")(_ + _.toChar).toInt
    val rest = input.slice(int.length + 1, input.length)
    (decodedInt, rest)
  }

  private def decodeList(input: Array[Byte]) : Tuple2[List[Any], Array[Byte]] = {
    doDecodeList((List[Any](), input))
  }

  private def doDecodeList(accumulator: Tuple2[List[Any], Array[Byte]]) : Tuple2[List[Any], Array[Byte]] = {
    if (accumulator._2.head.toChar == 'e') {
      (accumulator._1, accumulator._2.tail)
    } else {
      val res = doDecode(accumulator._2)
      doDecodeList((accumulator._1 :+ res._1, res._2))
    }
  }

  private def decodeMap(input: Array[Byte]) : Tuple2[Map[String, Any], Array[Byte]] = {
    doDecodeMap((ListMap[String, Any](), input))
  }

  private def doDecodeMap(accumulator: Tuple2[Map[String, Any], Array[Byte]]) : Tuple2[Map[String, Any], Array[Byte]] = {
    if (accumulator._2.head.toChar == 'e') {
      (accumulator._1, accumulator._2.tail)
    } else {
      val key = doDecode(accumulator._2)
      val value = doDecode(key._2)
      doDecodeMap((accumulator._1 + (key._1.asInstanceOf[String] -> value._1), value._2))
    }
  }

  private def decodeString(input: Array[Byte]) : Tuple2[String, Array[Byte]] = {
    val size = input.takeWhile(c => c.toChar != ':')
    val frontLength = size.length + 1
    val stringLength = frontLength + size.foldLeft("")(_ + _.toChar).toInt
    val str = input.slice(frontLength, stringLength).foldLeft("")(_ + _.toChar)
    if (stringLength == input.size) {
      (str, Array[Byte]())
    } else {
      (str, input.slice(stringLength, input.size))
    }
  }
}
