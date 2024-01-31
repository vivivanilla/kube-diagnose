package com.github.vivivanilla.kubediagnose

import cats.implicits._
import cats.effect._
import com.monovore.decline._
import com.monovore.decline.effect._

object Main
    extends CommandIOApp(
      name = "kube-diagnose",
      header =
        "This is a small utility to diagnose issues of workloads running on Kubernetes. It outputs Markdown-formatted diagnostic information for all pods found in an error state with a single command. This is useful for deployment pipelines or other environments where interactive debugging is not possible."
    ) {

  case class AllNamespaces()
  case class Namespaces(namespaces: List[String])

  val allNamespacesFlag =
    Opts
      .flag("all", short = "A", help = "Diagnose all namespaces.")
      .map(_ => AllNamespaces())

  val namespaceOpts =
    Opts
      .options[String](
        "namespace",
        short = "n",
        metavar = "namespace",
        help = "Namespace to diagnose."
      )
      .withDefault(cats.data.NonEmptyList("default", List.empty))
      .map(namespaces => Namespaces(namespaces.toList))

  override def main: Opts[IO[ExitCode]] =
    (allNamespacesFlag orElse namespaceOpts).map {
      case Namespaces(namespaces) =>
        for {
          diagnoser <- IO(new Diagnose())
          diagnoses <- IO.parSequenceN(1)(
            namespaces.map(diagnoser.diagnoseNamespace)
          )
          _ <- IO(println(MarkdownBlocks(diagnoses).rendered))
        } yield ExitCode.Success
      case AllNamespaces() => IO.pure(ExitCode.Success)
    }
}
