package scorrent

case class MetaInfoFile (
  length: Int,
  path: String,
  md5sum: Option[String]
)
