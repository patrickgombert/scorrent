package scorrent;

import java.io.OutputStream
import scala.util.Random

object PeerCommand {
  def sendHandshake(out: OutputStream, infoHash: Array[Byte]) {
    var randBytes = new Array[Byte](12)
    Random.nextBytes(randBytes)
    val outBytes = Array[Byte](19.toByte) ++
                   "BitTorrent protocol".getBytes ++
                   new Array[Byte](8) ++
                   infoHash ++
                   "-SC0001-".getBytes ++
                   randBytes
    out.write(outBytes)
  }
}
