package scorrent.peer

import java.net.Socket
import java.io.{InputStream, OutputStream}
import scorrent.util.Conversion
import scala.util.Random

class Peer(inputStream: InputStream, outputStream: OutputStream) {
  var amChoking = false
  var peerChoking = false

  def sendHandshake(infoHash: Array[Byte]) {
    var randBytes = new Array[Byte](12)
    Random.nextBytes(randBytes)
    val outBytes = Array[Byte](19.toByte) ++
                   "BitTorrent protocol".getBytes ++
                   new Array[Byte](8) ++
                   infoHash ++
                   "-SC0001-".getBytes ++
                   randBytes
    outputStream.write(outBytes)
  }

  def sendKeepAlive() {
    outputStream.write(new Array[Byte](4))
  }

  def sendChoke() {
    sendIdMessage(0)
  }

  def sendUnchoke() {
    sendIdMessage(1)
  }

  def sendInterested() {
    sendIdMessage(2)
  }

  def sendNotInterested() {
    sendIdMessage(3)
  }

  def sendHave(index: Int) {
    sendIdMessage(4, index)
  }

  def sendBitfield(bitfield: Array[Byte]) {
    sendIdMessage(5, bitfield)
  }

  def sendRequest(index: Int, begin: Int, length: Int) {
    sendIdMessage(6, index, begin, length)
  }

  def sendPiece(index: Int, begin: Int, block: Array[Byte]) {
    sendIdMessage(7, index, begin, block)
  }

  def sendCancel(index: Int, begin: Int, length: Int) {
    sendIdMessage(8, index, begin, length)
  }

  def sendPort(listenPort: Int) {
    sendIdMessage(9, Conversion.intToByteArray(listenPort).slice(2, 4))
  }

  private def sendIdMessage(id: Int, args: Any*) {
    val argBytes = args.foldLeft(Array[Byte]())(_ ++ toBytes(_))
    val outBytes = (toBytes(argBytes.length + 1) :+ id.toByte) ++ argBytes
    outputStream.write(outBytes)
  }

  private def toBytes(arg: Any) : Array[Byte] = arg match {
    case i: Int => Conversion.intToByteArray(i)
    case v: Array[Byte] => v
  }
}
