package scorrent.metainfo

import java.nio.file.{Paths, Files}
import scorrent.bencode.Bencode
import scorrent.util.Conversion

case class MetaInfo (
  info: MetaInfoDict,
  announceList: Set[String],
  creationDate: Option[Int],
  comment: Option[String],
  createdBy: Option[String],
  encoding: Option[String]
)

object MetaInfo {
  def fromFile(filePath: String) : MetaInfo = {
    val path = Paths.get(filePath)
    fromBytes(Files.readAllBytes(path))
  }

  def fromBytes(bytes: Array[Byte]) : MetaInfo = {
    val m = Bencode.decode(bytes).asInstanceOf[Map[String, Any]]
    val announce = m("announce").asInstanceOf[String]
    MetaInfo(
      info = MetaInfoDict.fromMap(m("info").asInstanceOf[Map[String, Any]]),
      announceList =
        if (m.contains("announce-list"))
          (m("announce-list").asInstanceOf[List[List[String]]].flatten :+ announce).toSet
        else
          Set(announce),
      creationDate = Conversion.mapEntryOption[Int](m, "creation date"),
      comment = Conversion.mapEntryOption[String](m, "comment"),
      createdBy = Conversion.mapEntryOption[String](m, "created by"),
      encoding = Conversion.mapEntryOption[String](m, "encoding")
    )
  }
}
