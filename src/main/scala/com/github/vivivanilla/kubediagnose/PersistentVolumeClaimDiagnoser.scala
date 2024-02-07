package com.github.vivivanilla.kubediagnose

import cats.effect.IO
import io.kubernetes.client.openapi.ApiClient
import io.kubernetes.client.openapi.apis.CoreV1Api
import io.kubernetes.client.openapi.models.{
  V1PersistentVolume,
  V1PersistentVolumeClaim,
  V1Pod
}
import io.kubernetes.client.util.Yaml

object PersistentVolumeClaimDiagnoser {
  def apply(pvc: V1PersistentVolumeClaim)(implicit client: ApiClient) =
    new PersistentVolumeClaimDiagnoser(pvc, client)

  def byName(name: String, namespace: String)(implicit
      client: ApiClient
  ): IO[PersistentVolumeClaimDiagnoser] =
    for {
      api <- IO(new CoreV1Api(client))
      pvc <- IO.blocking(
        api.readNamespacedPersistentVolumeClaim(name, namespace, null)
      )
    } yield apply(pvc)(client)

  def fromPod(
      pod: V1Pod
  )(implicit client: ApiClient): IO[Seq[PersistentVolumeClaimDiagnoser]] = {
    val pvcNames = pod.getSpec.getVolumes.convertToScala
      .map(volume => Option(volume.getPersistentVolumeClaim).toSeq)
      .flatten
      .map(_.getClaimName)
    val namespace = pod.getMetadata.getNamespace

    IO.parSequenceN(1)(pvcNames.map(pvc => byName(pvc, namespace)(client)))
  }
}

class PersistentVolumeClaimDiagnoser(
    val pvc: V1PersistentVolumeClaim,
    val client: ApiClient
) {
  val name = pvc.getMetadata.getName
  val namespace = pvc.getMetadata.getName

  def diagnose: IO[Markdown] =
    for {
      status <- showStatus.recover { case e =>
        MarkdownAlert(
          "WARNING",
          MarkdownParagraph(
            s"Cannot read PersistentVolumeClaim status: ${e.getMessage}"
          )
        )
      }
    } yield MarkdownBlocks(
      Seq(
        MarkdownHeading(s"PersistentVolumeClaim ${namespace}/${name}", 2),
        MarkdownParagraph("Specification:"),
        showSpec,
        MarkdownParagraph("Status:"),
        status
      )
    )

  def status: String =
    Option(pvc.getStatus.getPhase).getOrElse("")

  def showSpec: Markdown = {
    val metadata = pvc.getMetadata.managedFields(null)
    MarkdownCode(Yaml.dump(pvc.metadata(metadata)), Some("yaml"))
  }

  def showStatus: IO[Markdown] = {
    val conditions = pvc.getStatus.getConditions.convertToScala
      .map(Condition.fromPersistentVolumeClaimCondition)
    val tableRowsIO = conditions.map(condition =>
      for {
        probeTime <- condition.showProbeTime
        transitionTime <- condition.showTransitionTime
      } yield Seq(
        probeTime,
        transitionTime,
        condition.kind.getOrElse("-"),
        condition.status.getOrElse("-"),
        condition.message.getOrElse("-"),
        condition.reason.getOrElse("-")
      )
    )
    val tableRows = IO.parSequenceN(1)(tableRowsIO)

    tableRows.map(rows =>
      MarkdownTable(
        Seq(
          "Last probe",
          "Last transition",
          "Type",
          "Status",
          "Message",
          "Reason"
        ) +: rows
      )
    )
  }

  def showPersistentVolumeSpec: IO[Markdown] = for {
    pv <- persistentVolume.attempt
  } yield pv match {
    case Left(e) =>
      MarkdownAlert(
        "WARNING",
        MarkdownParagraph(s"Could not read PersistentVolume: ${e}")
      )
    case Right(pv) => MarkdownCode(Yaml.dump(pv), Some("yaml"))
  }

  def persistentVolume: IO[V1PersistentVolume] =
    for {
      api <- IO(new CoreV1Api(client))
      volumeName <- IO.fromOption(Option(pvc.getSpec.getVolumeName))(
        new Exception("No Volume assigned to VolumeClaim")
      )
      volume <- IO.blocking(api.readPersistentVolume(name, null))
    } yield volume
}

extension (pvcList: Seq[PersistentVolumeClaimDiagnoser]) {
  def showStatus: Markdown = {
    val rows = pvcList.map(pvc =>
      Seq(
        pvc.name,
        pvc.namespace,
        Option(pvc.pvc.getStatus.getPhase).getOrElse("-"),
        Option(pvc.pvc.getStatus.getResizeStatus).getOrElse("-")
      )
    )
    MarkdownTable(Seq("Name", "Namespace", "Phase", "Resize status") +: rows)
  }
}
