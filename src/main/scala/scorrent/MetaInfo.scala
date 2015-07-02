package scorrent

import java.nio.file.{Paths, Files}

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
      creationDate =
        if (m.contains("creation date"))
          Some(m("creation date").asInstanceOf[Int])
        else
          None,
      comment =
        if (m.contains("comment"))
          Some(m("comment").asInstanceOf[String])
        else
          None,
      createdBy =
        if (m.contains("created by"))
          Some(m("created by").asInstanceOf[String])
        else
          None,
      encoding =
        if (m.contains("encoding"))
          Some(m("encoding").asInstanceOf[String])
        else
          None
    )
  }
}
