package scorrent.tracker;

import scorrent.util.Conversion
import java.nio._

case class Peer (
  id: String,
  ip: String,
  port: Int
)

object Peer {
  def fromAny(input: Any) : List[Peer] = input match {
    case _:List[_] => fromList(input.asInstanceOf[List[Map[String, Any]]])
    case _:String  => fromString(input.asInstanceOf[String])
    case _         => throw new IllegalArgumentException("Tracker response did not have a valid peer list")
  }

  private def fromList(input: List[Map[String, Any]]) : List[Peer] = {
    input.map(m =>
      Peer(
        id = m("peer id").asInstanceOf[String],
        ip = m("ip").asInstanceOf[String],
        port = m("port").asInstanceOf[Int]
      )
    )
  }

  private def fromString(input: String) : List[Peer] = {
    input.getBytes("ISO-8859-1").grouped(6).toList.map { m =>
      val ip = m.slice(0, 4).map(Conversion.unsignedByteToInt(_).toString).mkString(".")
      val portbb = ByteBuffer.wrap(m.slice(4, 6))
      portbb.order(ByteOrder.BIG_ENDIAN)
      Peer(
        id = null,
        ip = ip,
        port = portbb.getShort.toInt
      )
    }
  }
}
