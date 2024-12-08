//> using platform scala-js
//> using jsVersion 1.17.0
//> using dep org.scala-js::scalajs-dom::2.8.0
//> using scala 3.5.2
//> using file ../shared/protocol.scala

import scalajs.js
import js.annotation.*

@js.native @JSImport("/scripts/result.json?raw", JSImport.Default)
val rawData: String = js.native

import org.scalajs.dom.*
import protocol.ScalaVersion
import util.chaining.*
import protocol.*

@main def hello =
  val data = upickle.default.read[protocol.Result](rawData)

  val index =
    collection.mutable.Map.empty[Int, (ScalaVersion, Project, ArtifactName)]

  val container = document.getElementById("content")

  val searchInput = document.getElementById("search-input")
  searchInput.addEventListener(
    "input",
    ev =>
      val text = searchInput.asInstanceOf[HTMLInputElement].value.toLowerCase()
      if text.nonEmpty then
        val pred = (sv: ScalaVersion, p: Project, a: ArtifactName) =>
          p.repository.contains(text) || a.name.contains(
            text
          ) || p.organization.contains(text) || a.groupId.contains(text)

        val matches = index.filter((id, v) => pred.tupled(v)).map(_._1)

        document
          .getElementsByClassName("el")
          .foreach: el =>
            el.classList.remove("selected")

        matches.foreach: id =>
          document
            .getElementById(s"el-$id")
            .classList
            .add("selected")
      else
        document
          .getElementsByClassName("el")
          .foreach: el =>
            el.classList.remove("selected")
  )

  data.keySet.toList
    .sortBy(_.toString())
    .reverse
    .foreach: sv =>

      val tooltip = document.createElement("div")
      tooltip.classList.add("tooltip")
      container.appendChild(
        document.createElement("h1").tap(_.innerText = sv.toString())
      )
      container.appendChild(tooltip)
      val box = document.createElement("div")
      container.appendChild(box)
      box.setAttribute(
        "style",
        """                margin: auto;
                max-width: 80%;
                display: flex;
                flex-wrap: wrap;
                gap: 2px;
"""
      )
      data(sv).foreach: (proj, artifact, platform) =>
        val id = index.size
        index.update(
          index.size,
          (
            sv,
            proj.copy(
              organization = proj.organization.toLowerCase(),
              repository = proj.repository.toLowerCase()
            ),
            artifact.copy(
              groupId = artifact.groupId.toLowerCase(),
              name = artifact.name.toLowerCase()
            )
          )
        )
        val el = document.createElement("div")
        el.setAttribute("id", s"el-$id")
        var text =
          s"Github: ${proj.organization}/${proj.repository}\nMaven: ${artifact.groupId}:${artifact.name}"
        el.classList.add("el")
        if platform
            .contains(Platform.Native05) || platform.contains(Platform.Native04)
        then
          el.classList.add("yes")
          val newValue = {
            platform
              .find(_ == Platform.Native05)
              .map(_ => "Scala Native 0.5")
              .orElse(
                platform
                  .find(_ == Platform.Native04)
                  .map(_ => "Scala Native 0.4")
              ).getOrElse("")
          }
          text += s"\n Published for $newValue"
        else
          el.classList.add("no")
          text += s"\n Not published for Scala Native"

        el.setAttribute(
          "data-tippy-content",
          text
        )

        el.addEventListener(
          "mouseover",
          ev =>
            tooltip.innerText = el.getAttribute("data-tippy-content")
            el.classList.toggle("highlight")
        )
        el.addEventListener(
          "mouseout",
          ev =>
            tooltip.innerText = ""

            el.classList.toggle("highlight")
        )

        box.appendChild(el)

end hello

@JSImport("tippy.js", JSImport.Default)
@js.native
def tippy(arg1: scala.scalajs.js.Any): scala.scalajs.js.Any = js.native
