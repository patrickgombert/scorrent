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

  "decode" should "decode an integer" in {
    Bencode.decode("i555e".getBytes) should equal(555)
  }

  "decode" should "decode a string" in {
    Bencode.decode("3:foo".getBytes) should equal("foo")
  }

  "decode" should "decode a list" in {
    val expected = List[Any](1, "foo")
    Bencode.decode("li1e3:fooe".getBytes) should equal(expected)
  }

  "decode" should "decode a map" in {
    val expected = Map[String, Any]("bar" -> 2, "foo" -> 1)
    Bencode.decode("d3:bari2e3:fooi1ee".getBytes) should equal(expected)
  }
}
