package scorrent.util;

import java.nio.ByteBuffer

object Conversion {
  def intToByteArray(i: Int) : Array[Byte] = {
    ByteBuffer.allocate(4).putInt(i).array
  }

  def unsignedByteToInt(b: Byte) : Int = {
    b & 0xFF
  }

  def mapEntryOption[T](m: Map[String, Any], k: String) : Option[T] = {
    if (m.contains(k))
      Some(m(k).asInstanceOf[T])
    else
      None
  }
}
