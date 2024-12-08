//> using dep com.lihaoyi::requests::0.9.0
//> using dep com.lihaoyi::upickle::4.0.2
//> using dep com.lihaoyi::os-lib::0.11.3
//> using dep com.lihaoyi::pprint::latest.release
//> using dep com.outr::scribe::3.15.2
//> using option -Wunused:all
//> using file ../shared/protocol.scala

import upickle.default.*

def url(path: String) = "https://index.scala-lang.org" + path

os.makeDir.all(os.pwd / ".cache")

def cached(path: String, force: Boolean = false) =
  val cachePath = os.pwd / ".cache" / path.replaceAll("[^0-9a-zA-Z]", "_")
  val skipCache = force || sys.env.contains("NO_CACHE") || !os.exists(cachePath)

  if skipCache then
    val result = requests.get.stream(url(path))
    os.makeDir.all(os.pwd / ".cache")

    os.write.over(cachePath, result)
  end if

  os.read.bytes(cachePath)

def cachedJson[T: ReadWriter](path: String) =
  try read[T](cached(path))
  catch
    case ex: ujson.IncompleteParseException =>
      read[T](cached(path, force = true))

import protocol.*

enum Moniker(val name: String):
  case Java(canonicalName: String) extends Moniker(canonicalName)
  case Scala(
      canonicalName: String,
      platform: Platform,
      scala: ScalaVersion
  ) extends Moniker(canonicalName)

import Platform.*, ScalaVersion.*

case class ProjectId(organization: String, repository: String)
    derives ReadWriter

case class Artifact(groupId: String, artifactId: String, version: String)
    derives ReadWriter:
  lazy val moniker: Option[Moniker] =
    def parseScala(artifact: String, sv: ScalaVersion): Option[Moniker.Scala] =
      artifact match
        case s"${artifact}_native0.5" =>
          Some(Moniker.Scala(artifact, Native05, sv))
        case s"${artifact}_native0.4" =>
          Some(Moniker.Scala(artifact, Native04, sv))
        case s"${artifact}_sjs1"   => Some(Moniker.Scala(artifact, JS1, sv))
        case s"${artifact}_sjs0.6" => Some(Moniker.Scala(artifact, JS06, sv))
        case s"${artifact}"        => Some(Moniker.Scala(artifact, JVM, sv))
        case _                     => None

    artifactId match
      case s"${artifact}_3"        => parseScala(artifact, Scala3)
      case s"${artifact}_2.13"     => parseScala(artifact, Scala213)
      case s"${artifact}_2.12"     => parseScala(artifact, Scala212)
      case s"${artifact}_2.${old}" => None // "let's ignore"
      case other                   => Some(Moniker.Java(other))
  end moniker

  lazy val name = moniker.map(m => ArtifactName(groupId, m.name))

val projects =
  cachedJson[List[ProjectId]]("/api/v1/projects")

val result = collection.mutable.Map
  .empty[ScalaVersion, List[(Project, ArtifactName, Set[Platform])]]
  .withDefault(_ => List.empty)

projects.zipWithIndex
  .tapEach((id, i) =>
    scribe.info(s"Processing $id ($i out of ${projects.length}")
  )
  .map(_._1)
  .map(id =>
    cachedJson[Project](s"/api/v1/projects/${id.organization}/${id.repository}")
  )
  .filter(_.stars.exists(_ > 100))
  .map: proj =>
    proj -> cachedJson[List[Artifact]](
      s"/api/v1/projects/${proj.organization}/${proj.repository}/artifacts"
    )
  .foreach: (project, artifacts) =>
    artifacts
      .flatMap(a =>
        a.moniker
          .collect { case m: Moniker.Scala => m }
          .map(m => (m, a.groupId))
      )
      .groupMapReduce(m =>
        (m._1.scala, ArtifactName(m._2, m._1.canonicalName)),
      )((m, _) => Set(m.platform))(_ ++ _)
      .foreach:
        case ((sv, artifact), platforms) =>
          result.update(sv, (project, artifact, platforms) :: result(sv))

os.write.over(
  os.pwd / "result.json",
  write[Result](
    result.toMap
      .mapValues(
        _.sortBy(k =>
          (
            -k._1.stars.getOrElse(0),
            k._1.organization,
            k._1.repository,
            k._2.groupId,
            k._2.name
          )
        )
      )
      .toMap
  )
)
