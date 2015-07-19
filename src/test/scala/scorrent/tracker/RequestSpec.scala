package scorrent.tracker;

import org.scalatest._

class RequestSpec extends FlatSpec with ShouldMatchers {
  val announce = "announ.ce"
  val infoHash = "ahash"
  val peerId = "peerId"
  val port = 3000
  val event = "anevent"
  val trackerId = Some("trackerId")
  val downloaded = 1
  val uploaded = 2
  val left = 3
  lazy val noOpOnComplete = (_: TrackerResponse) => ()
  lazy val noOpGetFn = (_: String) => FailedResponse(failureReason = "No Op")

  def invoke(onComplete: (TrackerResponse) => Unit, getFn: (String) => TrackerResponse) {
    Request.announce(announce, infoHash, peerId, port, event, trackerId, downloaded, uploaded, left, onComplete, getFn)
  }

  "announce" should "invoke onComplete" in {
    var invoked = false
    val onComplete = (_: TrackerResponse) => invoked = true
    invoke(onComplete, noOpGetFn)
    invoked should equal(true)
  }

  "announce" should "build a request" in {
    var request = ""
    val getFn = (str: String) => {
      request = str
      FailedResponse(failureReason = "No Op")
    }
    invoke(noOpOnComplete, getFn)
    request should startWith(announce)
    request should include("info_hash=ahash&")
    request should include("peer_id=peerId&")
    request should include("port=3000&")
    request should include("event=anevent&")
    request should include("downloaded=1&")
    request should include("uploaded=2&")
    request should include("left=3&")
    request should include("compact=0&")
    request should include("no_peer_id=1&")
    request should include("numwant=30&")
    request should endWith("trackerid=trackerId")
  }
}
