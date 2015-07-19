package scorrent.tracker;

import java.net._
import java.io._

object Request {
  def announce(announce: String, infoHash: String, peerId: String, port: Int, event: String, trackerId: Option[String], downloaded: Long, uploaded: Long, left: Long, onComplete: (TrackerResponse) => Unit, getFn: (String) => TrackerResponse = getRequest) {
    val params = buildParams(infoHash, peerId, port, event, trackerId, downloaded, uploaded, left)
    onComplete(getFn(announce + "?" + params))
  }

  private def buildParams(infoHash: String, peerId: String, port: Int, event: String, trackerId: Option[String], downloaded: Long, uploaded: Long, left: Long) : String = {
    val escapedInfoHash = URLEncoder.encode(infoHash, "ISO-8859-1")
    val params = Map("info_hash" -> escapedInfoHash,
                     "peer_id" -> peerId,
                     "port" -> port,
                     "event" -> event,
                     "downloaded" -> downloaded,
                     "uploaded" -> uploaded,
                     "left" -> left,
                     "compact" -> 0,
                     "no_peer_id" -> 1,
                     "numwant" -> 30)
    val str = params.map(e => e._1 + "=" + e._2.toString).mkString("&")
    trackerId match {
      case Some(id) => str + "&trackerid=" + id
      case None => str
    }
  }

  private def getRequest(requestUrl: String) : TrackerResponse = {
    try {
      val url = new URL(requestUrl)
      val connection = url.openConnection.asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      val reader = new InputStreamReader(connection.getInputStream)
      if (connection.getResponseCode == HttpURLConnection.HTTP_OK) {
        var result = Array[Byte]()
        while (!(reader.ready)) { }
        var b : Int = reader.read
        while (b != -1) {
          result = result :+ b.toByte
          b = reader.read
        }
        reader.close
        TrackerResponse.fromByteArray(result)
      } else {
        FailedResponse(failureReason = "foo")
      }
    } catch {
      case e : Throwable =>
        FailedResponse(failureReason = e.getMessage)
    }
  }
}
