package com.github.vivivanilla.kubediagnose

import cats.effect.IO
import io.kubernetes.client.openapi.models.{
  V1PersistentVolumeClaimCondition,
  V1PodCondition
}

import java.time.OffsetDateTime

object Condition {
  def fromPersistentVolumeClaimCondition(
      pvcc: V1PersistentVolumeClaimCondition
  ): Condition =
    Condition(
      Option(pvcc.getLastProbeTime),
      Option(pvcc.getLastTransitionTime),
      Option(pvcc.getMessage),
      Option(pvcc.getReason),
      Option(pvcc.getStatus),
      Option(pvcc.getType)
    )

  def fromPodCondition(pc: V1PodCondition): Condition =
    Condition(
      Option(pc.getLastProbeTime),
      Option(pc.getLastTransitionTime),
      Option(pc.getMessage),
      Option(pc.getReason),
      Option(pc.getStatus),
      Option(pc.getType)
    )
}

case class Condition(
    probeTime: Option[OffsetDateTime],
    transitionTime: Option[OffsetDateTime],
    message: Option[String],
    reason: Option[String],
    status: Option[String],
    kind: Option[String]
) {
  def showProbeTime: IO[String] = probeTime match {
    case Some(time) => time.relativeToNow.map(_.humanReadable)
    case None       => IO.pure("-")
  }

  def showTransitionTime: IO[String] = transitionTime match {
    case Some(time) => time.relativeToNow.map(_.humanReadable)
    case None       => IO.pure("-")
  }
}
