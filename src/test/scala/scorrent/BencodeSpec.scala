package scorrent;

import org.scalatest._

class BencodeSpec extends FlatSpec with ShouldMatchers {
  "encode" should "encode an integer" in {
    Bencode.encode(555) should equal("i555e")
  }

  "encode" should "encode a string" in {
    Bencode.encode("foo") should equal("3:foo")
  }

  "encode" should "encode a list" in {
    val list = List[Any](1, "foo")
    Bencode.encode(list) should equal("li1e3:fooe")
  }

  "encode" should "encode a map in lexicographical order" in {
    val map = Map[String, Any]("foo" -> 1, "bar" -> 2)
    Bencode.encode(map) should equal("d3:bari2e3:fooi1ee")
  }
}
