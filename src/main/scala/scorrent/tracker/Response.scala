package scorrent.tracker;

import scorrent.bencode.Bencode
import scorrent.util.Conversion

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
        warningMessage = Conversion.mapEntryOption[String](response, "warning message"),
        interval = response("interval").asInstanceOf[Int],
        minInterval = Conversion.mapEntryOption[Int](response, "min interval"),
        trackerId = Conversion.mapEntryOption[String](response, "tracker id"),
        complete = response("complete").asInstanceOf[Int],
        incomplete = response("incomplete").asInstanceOf[Int],
        peers = Peer.fromAny(response("peers"))
      )
    }
  }
}
