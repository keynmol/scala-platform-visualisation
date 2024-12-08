//> using dep com.lihaoyi::upickle::4.0.2

import upickle.default.*

object protocol:
  enum Platform derives ReadWriter:
    case JS1, JVM, Native05, Native04, JS06

  enum ScalaVersion derives ReadWriter:
    case Scala212, Scala213, Scala3

  case class Project(
      organization: String,
      repository: String,
      stars: Option[Int] = None
  ) derives ReadWriter

  case class ArtifactName(groupId: String, name: String) derives ReadWriter
  
  type Result = Map[ScalaVersion, List[(Project, ArtifactName, Set[Platform])]]

