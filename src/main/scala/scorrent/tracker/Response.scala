package scorrent.tracker;

import scorrent.bencode.Bencode

sealed trait TrackerResponse

case class FailedResponse (
  failureReason: String
) extends TrackerResponse

case class SuccessResponse (
  warningMessage: Option[String],
  interval: Int,
  minInterval: Option[Int],
  trackerId: Option[String],
  complete: Int,
  incomplete: Int,
  peers: List[Peer]
) extends TrackerResponse

object TrackerResponse {
  def fromByteArray(input: Array[Byte]) : TrackerResponse = {
    val response = Bencode.decode(input).asInstanceOf[Map[String, Any]]
    if (response.contains("failure reason")) {
      FailedResponse(failureReason = response("failure reason").asInstanceOf[String])
    } else {
      SuccessResponse(
        warningMessage =
          if (response.contains("warning message"))
            Some(response("warning message").asInstanceOf[String])
          else
            None,
        interval = response("interval").asInstanceOf[Int],
        minInterval =
          if (response.contains("min interval"))
            Some(response("min interval").asInstanceOf[Int])
          else
            None,
        trackerId =
          if (response.contains("tracker id"))
            Some(response("tracker id").asInstanceOf[String])
          else
            None,
        complete = response("complete").asInstanceOf[Int],
        incomplete = response("incomplete").asInstanceOf[Int],
        peers = Peer.fromAny(response("peers"))
      )
    }
  }
}
