package com.github.vivivanilla.kubediagnose

import cats.effect.IO
import io.kubernetes.client.openapi.ApiClient
import io.kubernetes.client.openapi.apis.CoreV1Api
import io.kubernetes.client.openapi.models.CoreV1Event
import io.kubernetes.client.util.Yaml

object EventsDiagnoser {
  def forInvolvedObject(
      apiVersion: String,
      kind: String,
      name: String,
      namespace: String
  )(implicit client: ApiClient): IO[EventsDiagnoser] =
    for {
      api <- IO(new CoreV1Api(client))
      eventList <- IO.blocking(
        api.listEventForAllNamespaces(
          null, null, null, null, null, null, null, null, null, null, null
        )
      )
      events <- IO(eventList.getItems).map(_.convertToScala)
    } yield {
      val relevantEvents = events.filter(event => {
        val obj = event.getInvolvedObject
        obj != null
        && obj.getApiVersion == apiVersion
        && obj.getKind == kind
        && obj.getName == name
        && obj.getNamespace == namespace
      })
      new EventsDiagnoser(relevantEvents, client)
    }

  def allWithWarning(
      namespace: Option[String]
  )(implicit client: ApiClient): IO[EventsDiagnoser] =
    for {
      api <- IO(new CoreV1Api(client))
      eventList <- IO.blocking(
        api.listEventForAllNamespaces(
          null, null, null, null, null, null, null, null, null, null, null
        )
      )
      events <- IO(eventList.getItems).map(_.convertToScala)
    } yield {
      val relevantEvents = events.filter(event =>
        event.getType == "Warning"
          && (namespace match {
            case Some(namespace) =>
              event.getInvolvedObject != null && event.getInvolvedObject.getNamespace == namespace
            case None => true
          })
      )
      new EventsDiagnoser(relevantEvents, client)
    }
}

class EventsDiagnoser(events: Seq[CoreV1Event], client: ApiClient) {
  def diagnoseSingleObject: IO[Markdown] = {
    val tableRows = IO.parTraverseN(1)(events)(event =>
      for {
        lastTimestamp <- event.getLastTimestamp.relativeToNow.map(
          _.abs.humanReadable
        )
      } yield Seq(
        lastTimestamp,
        nullToMinus(event.getType),
        nullToMinus(event.getMessage),
        nullToMinus(event.getReason)
      )
    )

    tableRows.map(rows =>
      MarkdownTable(
        Seq("Last occurence", "Type", "Reason", "Message", "Reason") +: rows
      )
    )
  }

  def nullToMinus(in: String): String = if (in == null) "-" else in
}
