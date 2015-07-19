package scorrent.metainfo;

import org.scalatest._
import java.nio.file.{Paths, Files}

class MetaInfoSpec extends FlatSpec with ShouldMatchers {
  "fromFile" should "build the metainfo from a file" in {
    val path = Paths.get(getClass.getResource("/example.torrent").toURI)
    val metaInfo = MetaInfo.fromBytes(Files.readAllBytes(path))

    metaInfo.announceList should equal(Set("http://torrent.ubuntu.com:6969/announce", "http://ipv6.torrent.ubuntu.com:6969/announce"))
    metaInfo.creationDate should equal(Some(1429786237))
    metaInfo.comment should equal(Some("Ubuntu CD releases.ubuntu.com"))
    metaInfo.createdBy should equal(None)
    metaInfo.encoding should equal(None)

    val infoDict = metaInfo.info

    infoDict.pieceLength should equal(524288)
    infoDict.pieceSHAs.size should equal(4396)
    infoDict.priv should equal(false)
    infoDict.files.size should equal(1)

    val file = infoDict.files.head

    file.length should equal(1150844928)
    file.path should equal("ubuntu-15.04-desktop-amd64.iso")
    file.md5sum should equal(None)
  }
}
