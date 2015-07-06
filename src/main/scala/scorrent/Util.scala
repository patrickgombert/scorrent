package scorrent;

import java.nio.ByteBuffer

object Util {
  def intToByteArray(i: Int) : Array[Byte] = {
    ByteBuffer.allocate(4).putInt(i).array
  }
}
