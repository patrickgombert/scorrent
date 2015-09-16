package scorrent.peer

import java.net.Socket
import java.io.{InputStream, OutputStream}
import scorrent.util.Conversion
import scala.util.Random

sealed trait PeerStatus

case class OKPeerStatus() extends PeerStatus
case class FailedPeerStatus(reason: String) extends PeerStatus

class Peer(inputStream: InputStream, outputStream: OutputStream) {
  var amChoking = false
  var peerChoking = false
  var infoHash = Array[Byte]()
  var peerId = Array[Byte]()

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

  def receiveHandshake() : PeerStatus = {
    val rawHandshake = new Array[Byte](68)
    val bytesRead = inputStream.read(rawHandshake, 0, 68)
    if (bytesRead == 68) {
      val protocolLength = rawHandshake.head
      val protocol = new String(rawHandshake.tail.take(protocolLength))
      if (protocol == "BitTorrent protocol") {
        infoHash = rawHandshake.slice(28, 48)
        peerId = rawHandshake.slice(48, 68)
        OKPeerStatus()
      } else {
        FailedPeerStatus("Not using BitTorrent protocol")
      }
    } else {
      FailedPeerStatus("Failed to receive handshake")
    }
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

  def close() {
    inputStream.close
    outputStream.close
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

