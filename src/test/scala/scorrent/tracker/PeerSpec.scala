package scorrent.tracker

import org.scalatest._
import java.nio._

class PeerSpec extends FlatSpec with ShouldMatchers {
  "fromAny" should "work for a list of maps" in {
    val peer1 = Map("peer id" -> "id1", "ip" -> "0.0.0.0", "port" -> 3000)
    val peer2 = Map("peer id" -> "id2", "ip" -> "192.1.1.1", "port" -> 3001)
    val mapPeers = List(peer1, peer2)

    val peers = Peer.fromAny(mapPeers)

    peers(0).id   should equal("id1")
    peers(0).ip   should equal("0.0.0.0")
    peers(0).port should equal(3000)
    peers(1).id   should equal("id2")
    peers(1).ip   should equal("192.1.1.1")
    peers(1).port should equal(3001)
  }

  "fromAny" should "work for a binary string of peers" in {
    val peer = Array(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 1.toByte).map(_.toChar).mkString
    val peers = Peer.fromAny(peer)

    peers(0).id   should equal(null)
    peers(0).ip   should equal("0.0.0.0")
    peers(0).port should equal(1)
  }
}
