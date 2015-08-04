package scorrent.metainfo

import scorrent.util.Conversion

case class MetaInfoDict (
  files: List[MetaInfoFile],
  pieceLength: Int,
  pieceSHAs: List[Array[Byte]],
  priv: Boolean
)

object MetaInfoDict {
  def fromMap(m: Map[String, Any]) : MetaInfoDict = {
    MetaInfoDict(
      files = filesFromInfoMap(m),
      pieceLength = m("piece length").asInstanceOf[Int],
      pieceSHAs = m("pieces").asInstanceOf[String].getBytes.grouped(20).toList,
      priv = m.get("private").getOrElse(0).asInstanceOf[Int] == 1
    )
  }

  private def filesFromInfoMap(m: Map[String, Any]) : List[MetaInfoFile] = {
    if (m.contains("files"))
      m("files").asInstanceOf[List[Map[String, Any]]].map(file =>
        MetaInfoFile(
          length = file("length").asInstanceOf[Int],
          path = file("path").asInstanceOf[String],
          md5sum = Conversion.mapEntryOption[String](m, "md5sum")
        )
      )
    else
      List(
        MetaInfoFile(
          length = m("length").asInstanceOf[Int],
          path = m("name").asInstanceOf[String],
          md5sum = Conversion.mapEntryOption[String](m, "md5sum")
        )
      )
  }
}
