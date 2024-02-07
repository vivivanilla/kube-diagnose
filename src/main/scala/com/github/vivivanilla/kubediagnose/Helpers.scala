package com.github.vivivanilla.kubediagnose

import cats.effect.IO

import scala.jdk.CollectionConverters.*

extension [A](list: java.util.List[A])
  def convertToScala: Seq[A] =
    Option(list).map(_.asScala).toSeq.flatten

extension (date: java.time.OffsetDateTime)
  def relativeToNow: IO[java.time.Duration] = IO(
    java.time.Duration.between(java.time.OffsetDateTime.now(), date)
  )

extension (duration: java.time.Duration)
  def humanReadable: String = duration.toString
    .replaceAll("PT", "")
    .replaceAll("(\\d[HMS])(?!$)", "$1 ")
    .toLowerCase
