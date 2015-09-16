package scorrent.peer;

import scorrent.util.Conversion
import java.io.{OutputStream, ByteArrayOutputStream, InputStream, ByteArrayInputStream}
import org.scalatest._

class PeerCommandSpec extends FlatSpec with ShouldMatchers {
  "sendHandshake" should "write the handshake to the stream" in {
    val infoHash = new Array[Byte](20)
    val outBytes = withOutputStream { peer =>
      peer.sendHandshake(infoHash)
    }

    outBytes.head          should equal(19.toByte)
    outBytes.slice(1, 20)  should equal("BitTorrent protocol".getBytes)
    outBytes.slice(20, 28) should equal(new Array[Byte](8))
    outBytes.slice(28, 48) should equal(infoHash)
    outBytes.slice(48, 56) should equal("-SC0001-".getBytes)
    outBytes.length        should equal(68)
  }

  val okHandshake = Array[Byte](19.toByte) ++ "BitTorrent protocol".getBytes ++ new Array[Byte](28) ++ "-SC0001-".getBytes ++ new Array[Byte](12)

  "receiveHandshake" should "return true for a successful handshake" in {
    val status = withInputStream(okHandshake, { peer =>
      peer.receiveHandshake
    })
    status match {
      case _ : OKPeerStatus => assert(true)
      case _ => fail()
    }
  }

  "receiveHandshake" should "set the infoHash and peerId" in {
    withInputStream(okHandshake, { peer =>
      val ret = peer.receiveHandshake
      peer.infoHash should equal(new Array[Byte](20))
      peer.peerId should equal("-SC0001-".getBytes ++ new Array[Byte](12))
      ret
    })
  }

  "sendKeepAlive" should "write a keepalive to the stream" in {
    val outBytes = withOutputStream { peer =>
      peer.sendKeepAlive
    }
    outBytes should equal(Conversion.intToByteArray(0))
  }

  "sendChoke" should "write a message with id 0" in {
    val outBytes = withOutputStream { p => p.sendChoke }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(1))
    outBytes(4)          should equal(0.toByte)
  }

  "sendUnchoke" should "write a message with id 1" in {
    val outBytes = withOutputStream { p => p.sendUnchoke }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(1))
    outBytes(4)          should equal(1.toByte)
  }

  "sendInterested" should "write a message with id 2" in {
    val outBytes = withOutputStream { p => p.sendInterested }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(1))
    outBytes(4)          should equal(2.toByte)
  }

  "sendNotInterested" should "write a message with id 3" in {
    val outBytes = withOutputStream { p => p.sendNotInterested }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(1))
    outBytes(4)          should equal(3.toByte)
  }

  "sendHave" should "write a message with id 4 and the index" in {
    val outBytes = withOutputStream { peer =>
      peer.sendHave(42)
    }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(5))
    outBytes(4)          should equal(4.toByte)
    outBytes.slice(5, 9) should equal(Conversion.intToByteArray(42))
  }

  "sendBitfield" should "write a message with id 5 and the bitfield" in {
    val outBytes = withOutputStream { peer =>
      peer.sendBitfield(Array[Byte](1.toByte, 0.toByte))
    }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(3))
    outBytes(4)          should equal(5.toByte)
    outBytes.slice(5, 7) should equal(Array[Byte](1.toByte, 0.toByte))
  }

  "sendRequest" should "write a message with id 6, the index, the begin offset and the length" in {
    val outBytes = withOutputStream { peer =>
      peer.sendRequest(1, 5, 20)
    }
    outBytes.slice(0, 4)   should equal(Conversion.intToByteArray(13))
    outBytes(4)            should equal(6.toByte)
    outBytes.slice(5, 9)   should equal(Conversion.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Conversion.intToByteArray(5))
    outBytes.slice(13, 17) should equal(Conversion.intToByteArray(20))
  }

  "sendPiece" should "write a message with id 7, the index, the begin offset and the data block" in {
    val outBytes = withOutputStream { peer =>
      peer.sendPiece(1, 3, Array[Byte](0.toByte, 5.toByte))
    }
    outBytes.slice(0, 4)   should equal(Conversion.intToByteArray(11))
    outBytes(4)            should equal(7.toByte)
    outBytes.slice(5, 9)   should equal(Conversion.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Conversion.intToByteArray(3))
    outBytes.slice(13, 15) should equal(Array[Byte](0.toByte, 5.toByte))
  }

  "sendCancel" should "write a message with id 8, the index, the begin offset and the length" in {
    val outBytes = withOutputStream { peer =>
      peer.sendCancel(1, 5, 20)
    }
    outBytes.slice(0, 4)   should equal(Conversion.intToByteArray(13))
    outBytes(4)            should equal(8.toByte)
    outBytes.slice(5, 9)   should equal(Conversion.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Conversion.intToByteArray(5))
    outBytes.slice(13, 17) should equal(Conversion.intToByteArray(20))
  }

  "sendPort" should "write a message with id 9 and the listen port" in {
    val outBytes = withOutputStream { peer =>
      peer.sendPort(3030)
    }
    outBytes.slice(0, 4) should equal(Conversion.intToByteArray(3))
    outBytes(4)          should equal(9.toByte)
    outBytes.slice(5, 9) should equal(Array[Byte](11.toByte, -42.toByte))
  }

  private def withOutputStream(context: (Peer) => Unit) : Array[Byte] = {
    val outputStream = new ByteArrayOutputStream
    val peer = new Peer(null, outputStream)
    context(peer)
    val ret = outputStream.toByteArray
    outputStream.close
    ret
  }

  private def withInputStream(bytes: Array[Byte], context: (Peer) => PeerStatus) : PeerStatus = {
    val inputStream = new ByteArrayInputStream(bytes)
    val peer = new Peer(inputStream, null)
    val ret = context(peer)
    inputStream.close
    ret
  }
}
