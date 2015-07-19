package scorrent.metainfo

case class MetaInfoFile (
  length: Int,
  path: String,
  md5sum: Option[String]
)
