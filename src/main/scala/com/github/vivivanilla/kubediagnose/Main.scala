package com.github.vivivanilla.kubediagnose

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import io.kubernetes.client.util.Config

object Main
    extends CommandIOApp(
      name = "kube-diagnose",
      header =
        "This is a small utility to diagnose issues of workloads running on Kubernetes. It outputs Markdown-formatted diagnostic information for all pods found in an error state with a single command. This is useful for deployment pipelines or other environments where interactive debugging is not possible."
    ) {

  case class AllNamespaces()
  case class Namespaces(namespaces: List[String])
  case class DiagnoseServiceAccount(name: String, namespace: String)

  val allNamespacesFlag =
    Opts
      .flag("all", short = "A", help = "Diagnose all namespaces.")
      .map(_ => AllNamespaces())

  val namespacesOpts =
    Opts
      .options[String](
        "namespace",
        short = "n",
        metavar = "namespace",
        help = "Namespace to diagnose."
      )
      .withDefault(cats.data.NonEmptyList("default", List.empty))

  val serviceAccountOpts = Opts.option[String](
    "service-account",
    metavar = "sa",
    help = "ServiceAccount to diagnose"
  )

  val persistentVolumeClaimOpts = Opts.option[String](
    "persistent-volume-claim",
    metavar = "pvc",
    help = "PersistentVolumeClaim to diagnose."
  )

  case class DiagnoseServiceAccountCmd(name: String, namespace: String)

  case class DiagnosePersistentVolumeClaimCmd(name: String, namespace: String)

  val diagnoseServiceAccountCmd: Opts[DiagnoseServiceAccountCmd] =
    (serviceAccountOpts, namespacesOpts.map(_.head))
      .mapN(DiagnoseServiceAccountCmd.apply)

  val diagnosePersistentVolumeClaimCmd: Opts[DiagnosePersistentVolumeClaimCmd] =
    (persistentVolumeClaimOpts, namespacesOpts.map(_.head))
      .mapN(DiagnosePersistentVolumeClaimCmd.apply)

  override def main: Opts[IO[ExitCode]] =
    (diagnoseServiceAccountCmd orElse diagnosePersistentVolumeClaimCmd orElse allNamespacesFlag orElse namespacesOpts
      .map(ns => Namespaces(ns.toList)))
      .map {
        case Namespaces(namespaces) =>
          for {
            diagnoser <- IO(new Diagnose())
            diagnoses <- IO.parSequenceN(1)(
              namespaces.map(diagnoser.diagnoseNamespace)
            )
            _ <- IO(println(MarkdownBlocks(diagnoses).rendered))
          } yield ExitCode.Success
        case AllNamespaces() => IO.pure(ExitCode.Success)
        case DiagnoseServiceAccountCmd(name, namespace) =>
          for {
            client <- IO.blocking(Config.defaultClient())
            diagnoser <- ServiceAccountDiagnoser(name, namespace)(client)
            diagnoses <- diagnoser.diagnose
            _ <- IO(println(diagnoses.rendered))
          } yield ExitCode.Success
        case DiagnosePersistentVolumeClaimCmd(name, namespace) =>
          for {
            client <- IO.blocking(Config.defaultClient())
            diagnoser <- PersistentVolumeClaimDiagnoser.byName(name, namespace)(
              client
            )
            diagnoses <- diagnoser.diagnose
            _ <- IO(println(diagnoses.rendered))
          } yield ExitCode.Success
      }
}
