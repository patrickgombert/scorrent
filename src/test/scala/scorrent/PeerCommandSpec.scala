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

  private def withOutputStream(context: (OutputStream) => Unit) : Array[Byte] = {
    val outputStream = new ByteArrayOutputStream
    context(outputStream)
    val ret = outputStream.toByteArray
    outputStream.close
    ret
  }
}
