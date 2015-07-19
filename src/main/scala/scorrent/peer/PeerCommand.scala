package scorrent.peer;

import scorrent.util.Conversion
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

  def sendKeepAlive(out: OutputStream) {
    out.write(new Array[Byte](4))
  }

  def sendChoke(out: OutputStream) {
    sendIdMessage(out, 0)
  }

  def sendUnchoke(out: OutputStream) {
    sendIdMessage(out, 1)
  }

  def sendInterested(out: OutputStream) {
    sendIdMessage(out, 2)
  }

  def sendNotInterested(out: OutputStream) {
    sendIdMessage(out, 3)
  }

  def sendHave(out: OutputStream, index: Int) {
    sendIdMessage(out, 4, index)
  }

  def sendBitfield(out: OutputStream, bitfield: Array[Byte]) {
    sendIdMessage(out, 5, bitfield)
  }

  def sendRequest(out: OutputStream, index: Int, begin: Int, length: Int) {
    sendIdMessage(out, 6, index, begin, length)
  }

  def sendPiece(out: OutputStream, index: Int, begin: Int, block: Array[Byte]) {
    sendIdMessage(out, 7, index, begin, block)
  }

  def sendCancel(out: OutputStream, index: Int, begin: Int, length: Int) {
    sendIdMessage(out, 8, index, begin, length)
  }

  def sendPort(out: OutputStream, listenPort: Int) {
    sendIdMessage(out, 9, Conversion.intToByteArray(listenPort).slice(2, 4))
  }

  private def sendIdMessage(out: OutputStream, id: Int, args: Any*) {
    val argBytes = args.foldLeft(Array[Byte]())(_ ++ toBytes(_))
    val outBytes = (toBytes(argBytes.length + 1) :+ id.toByte) ++ argBytes
    out.write(outBytes)
  }

  private def toBytes(arg: Any) : Array[Byte] = arg match {
    case i: Int => Conversion.intToByteArray(i)
    case v: Array[Byte] => v
  }

}
