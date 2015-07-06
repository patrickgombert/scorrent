package scorrent;

import java.io.{OutputStream, ByteArrayOutputStream}
import org.scalatest._

class PeerCommandSpec extends FlatSpec with ShouldMatchers {
  "sendHandshake" should "write the handshake to the stream" in {
    val infoHash = new Array[Byte](20)
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendHandshake(outputStream, infoHash)
    }

    outBytes.head          should equal(19.toByte)
    outBytes.slice(1, 20)  should equal("BitTorrent protocol".getBytes)
    outBytes.slice(20, 28) should equal(new Array[Byte](8))
    outBytes.slice(28, 48) should equal(infoHash)
    outBytes.slice(48, 56) should equal("-SC0001-".getBytes)
    outBytes.length        should equal(68)
  }

  "sendKeepAlive" should "write a keepalive to the stream" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendKeepAlive(outputStream)
    }
    outBytes should equal(Util.intToByteArray(0))
  }

  "sendChoke" should "write a message with id 0" in {
    shouldSendId(0, PeerCommand.sendChoke)
  }

  "sendUnchoke" should "write a message with id 1" in {
    shouldSendId(1, PeerCommand.sendUnchoke)
  }

  "sendInterested" should "write a message with id 2" in {
    shouldSendId(2, PeerCommand.sendInterested)
  }

  "sendNotInterested" should "write a message with id 3" in {
    shouldSendId(3, PeerCommand.sendNotInterested)
  }

  "sendHave" should "write a message with id 4 and the index" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendHave(outputStream, 42)
    }
    outBytes.slice(0, 4) should equal(Util.intToByteArray(5))
    outBytes(4)          should equal(4.toByte)
    outBytes.slice(5, 9) should equal(Util.intToByteArray(42))
  }

  "sendBitfield" should "write a message with id 5 and the bitfield" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendBitfield(outputStream, Array[Byte](1.toByte, 0.toByte))
    }
    outBytes.slice(0, 4) should equal(Util.intToByteArray(3))
    outBytes(4)          should equal(5.toByte)
    outBytes.slice(5, 7) should equal(Array[Byte](1.toByte, 0.toByte))
  }

  "sendRequest" should "write a message with id 6, the index, the begin offset and the length" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendRequest(outputStream, 1, 5, 20)
    }
    outBytes.slice(0, 4)   should equal(Util.intToByteArray(13))
    outBytes(4)            should equal(6.toByte)
    outBytes.slice(5, 9)   should equal(Util.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Util.intToByteArray(5))
    outBytes.slice(13, 17) should equal(Util.intToByteArray(20))
  }

  "sendPiece" should "write a message with id 7, the index, the begin offset and the data block" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendPiece(outputStream, 1, 3, Array[Byte](0.toByte, 5.toByte))
    }
    outBytes.slice(0, 4)   should equal(Util.intToByteArray(11))
    outBytes(4)            should equal(7.toByte)
    outBytes.slice(5, 9)   should equal(Util.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Util.intToByteArray(3))
    outBytes.slice(13, 15) should equal(Array[Byte](0.toByte, 5.toByte))
  }

  "sendCancel" should "write a message with id 8, the index, the begin offset and the length" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendCancel(outputStream, 1, 5, 20)
    }
    outBytes.slice(0, 4)   should equal(Util.intToByteArray(13))
    outBytes(4)            should equal(8.toByte)
    outBytes.slice(5, 9)   should equal(Util.intToByteArray(1))
    outBytes.slice(9, 13)  should equal(Util.intToByteArray(5))
    outBytes.slice(13, 17) should equal(Util.intToByteArray(20))
  }

  "sendPort" should "write a message with id 9 and the listen port" in {
    val outBytes = withOutputStream { outputStream =>
      PeerCommand.sendPort(outputStream, 3030)
    }
    outBytes.slice(0, 4) should equal(Util.intToByteArray(3))
    outBytes(4)          should equal(9.toByte)
    outBytes.slice(5, 9) should equal(Array[Byte](11.toByte, -42.toByte))
  }

  private def shouldSendId(id: Int, command: (OutputStream) => Unit) {
    val outBytes = withOutputStream { o => command(o) }
    outBytes.slice(0, 4) should equal(Util.intToByteArray(1))
    outBytes(4)          should equal(id.toByte)
  }

  private def withOutputStream(context: (OutputStream) => Unit) : Array[Byte] = {
    val outputStream = new ByteArrayOutputStream
    context(outputStream)
    val ret = outputStream.toByteArray
    outputStream.close
    ret
  }
}
