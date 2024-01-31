package com.github.vivivanilla.kubediagnose

import io.kubernetes.client.util.Config
import io.kubernetes.client.openapi.apis.{CoreV1Api, RbacAuthorizationV1Api}
import io.kubernetes.client.openapi.ApiException
import cats.effect.IO
import scala.util.{Try, Success, Failure}
import io.kubernetes.client.openapi.models.{
  V1Pod,
  V1ContainerStatus,
  V1Secret,
  V1ConfigMap,
  V1RoleBinding,
  V1Role,
  V1ServiceAccount
}
import scala.jdk.CollectionConverters.*
import io.kubernetes.client.util.Yaml;

class Diagnose() {
  val client = Config.defaultClient()

  def diagnoseNamespace(namespace: String) = {
    val api = new CoreV1Api(client)
    val pods = IO.blocking(
      Try(
        api.listNamespacedPod(
          namespace,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null,
          null
        )
      ).map(_.getItems.asScala.toSeq)
    )

    for {
      pods <- pods
      overview = pods match {
        case Failure(e) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(
              s"Could not list pods in namespace `${namespace}`: *${e}*"
            )
          )
        case Success(pods) => printPodListOverview(pods)
      }
      pods <- IO.fromTry(pods)
      podDiagnoses <- IO
        .parSequenceN(1)(
          unhealthyPods(pods).map(pod =>
            diagnosePod(namespace, pod.getMetadata.getName)
          )
        )
        .map(MarkdownBlocks(_))
      configMapDiagnoses <- IO
        .parSequenceN(1)(
          pods
            .flatMap(referencedConfigMaps)
            .toSet
            .toSeq
            .map(configmap => diagnoseConfigMap(namespace, configmap))
        )
        .map(MarkdownBlocks(_))
      secretDiagnoses <- IO
        .parSequenceN(1)(
          pods
            .flatMap(referencedSecrets)
            .toSet
            .toSeq
            .map(secret => diagnoseSecret(namespace, secret))
        )
        .map(MarkdownBlocks(_))
      serviceAccountDiagnoses <- IO
        .parSequenceN(1)(
          pods
            .flatMap(pod => Option(pod.getSpec.getServiceAccountName).toSeq)
            .toSet
            .toSeq
            .map(serviceAccount =>
              diagnoseServiceAccount(namespace, serviceAccount)
            )
        )
        .map(MarkdownBlocks(_))
    } yield MarkdownBlocks(
      Seq(
        MarkdownHeading(s"Namespace ${namespace}", 1),
        overview,
        podDiagnoses,
        configMapDiagnoses,
        secretDiagnoses,
        serviceAccountDiagnoses
      )
    )
  }

  def diagnosePod(namespace: String, name: String) = {
    val api = new CoreV1Api(client)
    val pod = IO.blocking(
      Try(
        api.readNamespacedPod(name, namespace, null)
      )
    )

    for {
      pod <- pod
      getAttempt = pod match {
        case Failure(e) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(
              s"Could not read pod `${name}` in namespace `${namespace}`."
            )
          )
        case Success(pod) => MarkdownEmpty()
      }
      pod <- IO.fromTry(pod)
      status = printPodStatus(pod)
      containers = printContainerStates(pod)
      spec = printPodSpec(pod)
      log <- diagnosePodLog(pod)
    } yield MarkdownBlocks(
      Seq(
        MarkdownHeading(s"Pod ${namespace}/${name}", 2),
        getAttempt,
        status,
        containers,
        spec,
        log
      )
    )
  }

  def diagnosePodLog(pod: V1Pod) = {
    val api = new CoreV1Api(client)
    val name = pod.getMetadata.getName
    val namespace = pod.getMetadata.getNamespace

    IO.parSequenceN(1)(
      unhealthyContainers(pod)
        .map(_.getName)
        .map(diagnoseContainerLog(namespace, name, _))
    ).map(MarkdownBlocks(_))
  }

  def diagnoseContainerLog(
      namespace: String,
      podName: String,
      containerName: String
  ): IO[Markdown] = {
    val api = new CoreV1Api(client)

    for {
      log <- IO.blocking(
        Try(
          api.readNamespacedPodLog(
            podName,
            namespace,
            containerName,
            false,
            null,
            null,
            null,
            false,
            null,
            null,
            null
          )
        )
      )
      diagnostic <- log match {
        case Failure(e) =>
          IO.pure(
            MarkdownAlert(
              "WARNING",
              MarkdownParagraph(
                s"Could not read log of container `${containerName}`."
              )
            )
          )
        case Success(log) =>
          Option(log) match {
            case Some(log) => IO.pure(MarkdownCode(log, None))
            case None =>
              for {
                log <- IO.blocking(
                  Try(
                    api.readNamespacedPodLog(
                      podName,
                      namespace,
                      containerName,
                      false,
                      null,
                      null,
                      null,
                      true,
                      null,
                      null,
                      null
                    )
                  )
                )
              } yield log match {
                case Failure(e) =>
                  MarkdownAlert(
                    "WARNING",
                    MarkdownParagraph(
                      s"Could not read log of previous container `${containerName}`."
                    )
                  )
                case Success(log) =>
                  Option(log) match {
                    case Some(log) => MarkdownCode(log, None)
                    case None =>
                      MarkdownAlert(
                        "WARNING",
                        MarkdownParagraph(
                          s"Container `${containerName}` did not write any log output."
                        )
                      )
                  }
              }
          }
      }
    } yield MarkdownBlocks(
      Seq(
        MarkdownParagraph(s"Log output of container `${containerName}`:"),
        diagnostic
      )
    )
  }

  def diagnoseConfigMap(namespace: String, name: String): IO[Markdown] = {
    val api = new CoreV1Api(client)

    for {
      req <- IO
        .blocking(api.readNamespacedConfigMap(name, namespace, null))
        .attempt
      diagnostic = req match {
        case Left(e: ApiException) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(
              s"Cannot read ConfigMap `${name}`: ${e.getMessage}"
            )
          )
        case Right(configMap) => printConfigMap(configMap)
      }
    } yield MarkdownBlocks(
      Seq(MarkdownHeading(s"ConfigMap ${name}", 2), diagnostic)
    )
  }

  def diagnoseSecret(namespace: String, name: String): IO[Markdown] = {
    val api = new CoreV1Api(client)

    for {
      req <- IO
        .blocking(api.readNamespacedSecret(name, namespace, null))
        .attempt
      diagnostic = req match {
        case Left(e: ApiException) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(s"Cannot read Secret `${name}`: ${e.getMessage}")
          )
        case Right(secret) => printSecret(secret)
      }
    } yield MarkdownBlocks(
      Seq(MarkdownHeading(s"Secret ${name}", 2), diagnostic)
    )
  }

  def diagnoseServiceAccount(namespace: String, name: String): IO[Markdown] = {
    val api = new CoreV1Api(client)

    for {
      req <- IO
        .blocking(api.readNamespacedServiceAccount(name, namespace, null))
        .attempt
      permissions <- diagnoseServiceAccountPermissions(name, namespace).attempt
    } yield {
      val spec = req match {
        case Left(e: ApiException) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(
              s"Cannot read ServiceAccount ${name}: ${e.getMessage}"
            )
          )
        case Right(serviceAccount) => printServiceAccount(serviceAccount)
      }
      val permissionDiagnostics = permissions match {
        case Left(e: ApiException) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(
              s"Could not find permissions for ServiceAccount ${name}: ${e.getMessage}"
            )
          )
        case Right(permissions) =>
          MarkdownBlocks(Seq(MarkdownParagraph("Permissions:"), permissions))
      }
      MarkdownBlocks(
        Seq(
          MarkdownHeading(s"ServiceAccount ${name}", 2),
          spec,
          permissionDiagnostics
        )
      )
    }
  }

  def diagnoseServiceAccountPermissions(
      namespace: String,
      name: String
  ): IO[Markdown] = {
    for {
      roleBindings <- findRoleBindingsForServiceAccount(namespace, name)
      roles <- IO.parSequenceN(1)(roleBindings.map(findRoleForRoleBinding))
    } yield {
      val rules = roles
        .flatMap(role => Option(role.getRules).map(_.asScala).toSeq.flatten)
        .toSet
        .toSeq
      val rows = rules.map(rule =>
        Seq(
          javaListToString(rule.getVerbs),
          javaListToString(rule.getApiGroups),
          javaListToString(rule.getResources),
          javaListToString(rule.getResourceNames),
          javaListToString(rule.getNonResourceURLs)
        )
      )
      MarkdownTable(
        Seq(
          "Verbs",
          "ApiGroups",
          "Resources",
          "ResourceNames",
          "NonResourceURLs"
        ) +: rows
      )
    }
  }

  def findRoleBindingsForServiceAccount(
      namespace: String,
      name: String
  ): IO[Seq[V1RoleBinding]] = {
    val api = new RbacAuthorizationV1Api(client)

    def filterRoleBinding(roleBinding: V1RoleBinding) =
      Option(roleBinding.getSubjects)
        .map(_.asScala)
        .toSeq
        .flatten
        .filter(subject =>
          subject.getName == name && subject.getNamespace == namespace
        )
        .nonEmpty

    for {
      roleBindingList <- IO.blocking(
        api.listRoleBindingForAllNamespaces(null, null, null, null, null, null,
          null, null, null, null, null)
      )
      referencedRoleBindings = Option(roleBindingList.getItems())
        .map(_.asScala)
        .toSeq
        .flatten
        .filter(filterRoleBinding)
    } yield referencedRoleBindings
  }

  def findRoleForRoleBinding(roleBinding: V1RoleBinding): IO[V1Role] = {
    val api = new RbacAuthorizationV1Api(client)
    val roleRef = roleBinding.getRoleRef()

    if (
      roleRef.getApiGroup != "rbac.authorization.k8s.io" || roleRef.getKind != "Role"
    ) IO.raiseError(new Exception("roleRef malformed"))
    else
      IO.blocking(
        api.readNamespacedRole(
          roleRef.getName,
          roleBinding.getMetadata.getNamespace,
          null
        )
      )
  }

  def printPodListOverview(pods: Seq[V1Pod]): Markdown =
    MarkdownTable(
      Seq("Name", "Status", "Ready", "Restarts") +: (for {
        pod <- pods
      } yield Seq(
        pod.getMetadata.getName,
        nullToEmpty(pod.getStatus.getPhase),
        s"${readyContainers(pod)} / ${numContainers(pod)}",
        s"${restartedContainers(pod)}"
      ))
    )

  def printPodSpec(pod: V1Pod): Markdown = {
    val metadata = pod.getMetadata.managedFields(null)
    val manifest = new V1Pod()
      .apiVersion(pod.getApiVersion)
      .kind(pod.getKind)
      .metadata(metadata)
      .spec(pod.getSpec)
    MarkdownBlocks(
      Seq(
        MarkdownParagraph("Manifest:"),
        MarkdownCode(Yaml.dump(manifest), Some("yaml"))
      )
    )
  }

  def printPodStatus(pod: V1Pod): Markdown = {
    val reason = Option(pod.getStatus.getReason) match {
      case Some(reason) =>
        MarkdownAlert(
          "WARNING",
          MarkdownParagraph(s"`${reason}`: ${pod.getStatus.getMessage}")
        )
      case None => MarkdownEmpty()
    }

    val conditions =
      Option(pod.getStatus.getConditions).map(_.asScala.toSeq) match {
        case Some(conditions) =>
          MarkdownTable(
            Seq("Type", "Status", "Reason", "Message") +: conditions.map(
              condition =>
                Seq(
                  nullToEmpty(condition.getType),
                  nullToEmpty(condition.getStatus),
                  nullToEmpty(condition.getReason),
                  nullToEmpty(condition.getMessage)
                )
            )
          )
        case None => MarkdownEmpty()
      }

    MarkdownBlocks(Seq(MarkdownParagraph("Status:"), reason, conditions))
  }

  def printContainerStates(pod: V1Pod): Markdown = {
    def containerRow(kind: String, container: V1ContainerStatus) = Seq(
      kind,
      container.getName,
      container.getRestartCount.toString,
      containerState(container)
    )

    def containerState(container: V1ContainerStatus): String = Option(
      container.getLastState
    ) match {
      case Some(state) => {
        val running = Option(state.getRunning).map(state =>
          s"Running since ${state.getStartedAt}"
        )
        val waiting = Option(state.getWaiting).map(state =>
          s"Waiting because of ${state.getReason}: ${state.getMessage}"
        )
        val terminated = Option(state.getTerminated).map(state =>
          s"Terminated with code `${state.getExitCode}` because of ${state.getReason}: ${state.getMessage}"
        )
        running
          .orElse(waiting)
          .orElse(terminated)
          .getOrElse("No state information")
      }
      case None => "No state information"
    }

    val status = pod.getStatus
    val initContainers = Option(status.getInitContainerStatuses)
      .map(_.asScala)
      .toSeq
      .flatten
      .map(containerRow("InitContainer", _))
    val containers = Option(status.getContainerStatuses)
      .map(_.asScala)
      .toSeq
      .flatten
      .map(containerRow("Container", _))
    val ephemeralContainers = Option(status.getEphemeralContainerStatuses)
      .map(_.asScala)
      .toSeq
      .flatten
      .map(containerRow("EphemeralContainer", _))

    MarkdownBlocks(
      Seq(
        MarkdownParagraph("Containers:"),
        MarkdownTable(
          Seq(
            "Type",
            "Name",
            "Restarts",
            "State"
          ) +: (initContainers ++ containers ++ ephemeralContainers)
        )
      )
    )
  }

  def printConfigMap(configMap: V1ConfigMap): Markdown = {
    val metadata = configMap.getMetadata.managedFields(null)

    MarkdownCode(Yaml.dump(configMap.metadata(metadata)), Some("yaml"))
  }

  def printSecret(secret: V1Secret): Markdown = {
    val data =
      Option(secret.getData).map(_.asScala.mapValues(_ => redacted).toMap)
    val stringData = Option(secret.getStringData).map(
      _.asScala.mapValues(_ => "redacted").toMap
    )
    val metadata = secret.getMetadata.managedFields(null)

    MarkdownCode(
      Yaml.dump(
        secret
          .data(data.map(_.asJava).getOrElse(null))
          .stringData(stringData.map(_.asJava).getOrElse(null))
          .metadata(metadata)
      ),
      Some("yaml")
    )
  }

  def printServiceAccount(serviceAccount: V1ServiceAccount): Markdown = {
    val metadata = serviceAccount.getMetadata.managedFields(null)

    MarkdownCode(Yaml.dump(serviceAccount.metadata(metadata)), Some("yaml"))
  }

  def numContainers(pod: V1Pod): Int = Option(
    pod.getStatus.getContainerStatuses
  ).map(_.asScala.size).getOrElse(0) + Option(
    pod.getStatus.getInitContainerStatuses
  ).map(_.asScala.size).getOrElse(0)

  def readyContainers(pod: V1Pod): Int = (Option(
    pod.getStatus.getContainerStatuses
  ).map(_.asScala).toSeq.flatten ++ Option(
    pod.getStatus.getInitContainerStatuses
  ).map(_.asScala).toSeq.flatten).filter(_.getReady).size

  def restartedContainers(pod: V1Pod): Int = (Option(
    pod.getStatus.getContainerStatuses
  ).map(_.asScala).toSeq.flatten ++ Option(
    pod.getStatus.getInitContainerStatuses
  ).map(_.asScala).toSeq.flatten).map(_.getRestartCount).max

  def unhealthyPods(pods: Seq[V1Pod]): Seq[V1Pod] = pods.filter(pod =>
    readyContainers(pod) != numContainers(pod) || restartedContainers(pod) > 0
  )

  def unhealthyContainers(pod: V1Pod): Seq[V1ContainerStatus] = {
    val status = pod.getStatus
    val initContainers =
      Option(status.getInitContainerStatuses).map(_.asScala).toSeq.flatten
    val containers =
      Option(status.getContainerStatuses).map(_.asScala).toSeq.flatten
    (initContainers ++ containers).filter(container =>
      !container.getReady || container.getRestartCount > 0
    )
  }

  def referencedConfigMaps(pod: V1Pod): Set[String] = {
    val volumes = Option(pod.getSpec.getVolumes).map(_.asScala).toSeq.flatten
    val projectedVolumes =
      volumes.map(vol => Option(vol.getProjected).toSeq).flatten
    val projectedVolumeSources = projectedVolumes
      .map(pv => Option(pv.getSources).map(_.asScala).toSeq.flatten)
      .flatten

    val initContainers =
      Option(pod.getSpec.getInitContainers).map(_.asScala).toSeq.flatten
    val containers =
      Option(pod.getSpec.getContainers).map(_.asScala).toSeq.flatten
    val ephemeralContainers =
      Option(pod.getSpec.getEphemeralContainers).map(_.asScala).toSeq.flatten
    val env = ((initContainers ++ containers).map(c =>
      Option(c.getEnv).map(_.asScala).toSeq.flatten
    ) ++ ephemeralContainers.map(c =>
      Option(c.getEnv).map(_.asScala).toSeq.flatten
    )).flatten
    val envSources = env.map(env => Option(env.getValueFrom).toSeq).flatten
    val envFrom = ((initContainers ++ containers).map(c =>
      Option(c.getEnvFrom).map(_.asScala).toSeq.flatten
    ) ++ ephemeralContainers.map(c =>
      Option(c.getEnvFrom).map(_.asScala).toSeq.flatten
    )).flatten

    val configMapsMountedAsVolume =
      volumes.map(vol => Option(vol.getConfigMap).toSeq).flatten.map(_.getName)
    val configMapsMountedAsProjectedVolume = projectedVolumeSources
      .map(src => Option(src.getConfigMap).toSeq)
      .flatten
      .map(_.getName)
    val configMapsFromEnv = envSources
      .map(src => Option(src.getConfigMapKeyRef).toSeq)
      .flatten
      .map(_.getName)
    val configMapsFromEnvFrom = envFrom
      .map(env => Option(env.getConfigMapRef).toSeq)
      .flatten
      .map(_.getName)

    (configMapsMountedAsVolume ++ configMapsMountedAsProjectedVolume ++ configMapsFromEnv ++ configMapsFromEnvFrom).toSet
  }

  def referencedSecrets(pod: V1Pod): Set[String] = {
    val volumes = Option(pod.getSpec.getVolumes).map(_.asScala).toSeq.flatten
    val projectedVolumes =
      volumes.map(vol => Option(vol.getProjected).toSeq).flatten
    val projectedVolumeSources = projectedVolumes
      .map(pv => Option(pv.getSources).map(_.asScala).toSeq.flatten)
      .flatten

    val initContainers =
      Option(pod.getSpec.getInitContainers).map(_.asScala).toSeq.flatten
    val containers =
      Option(pod.getSpec.getContainers).map(_.asScala).toSeq.flatten
    val ephemeralContainers =
      Option(pod.getSpec.getEphemeralContainers).map(_.asScala).toSeq.flatten
    val env = ((initContainers ++ containers).map(c =>
      Option(c.getEnv).map(_.asScala).toSeq.flatten
    ) ++ ephemeralContainers.map(c =>
      Option(c.getEnv).map(_.asScala).toSeq.flatten
    )).flatten
    val envSources = env.map(env => Option(env.getValueFrom).toSeq).flatten
    val envFrom = ((initContainers ++ containers).map(c =>
      Option(c.getEnvFrom).map(_.asScala).toSeq.flatten
    ) ++ ephemeralContainers.map(c =>
      Option(c.getEnvFrom).map(_.asScala).toSeq.flatten
    )).flatten

    val secretsMountedAsVolume = volumes
      .map(vol => Option(vol.getSecret).toSeq)
      .flatten
      .map(_.getSecretName)
    val secretsMountedAsProjectedVolume = projectedVolumeSources
      .map(src => Option(src.getSecret).toSeq)
      .flatten
      .map(_.getName)
    val secretsFromEnv = envSources
      .map(src => Option(src.getSecretKeyRef).toSeq)
      .flatten
      .map(_.getName)
    val secretsFromEnvFrom =
      envFrom.map(env => Option(env.getSecretRef).toSeq).flatten.map(_.getName)

    (secretsMountedAsVolume ++ secretsMountedAsProjectedVolume ++ secretsFromEnv ++ secretsFromEnvFrom).toSet
  }

  def nullToEmpty(in: String): String = if (in == null) "" else in

  def javaListToString(list: java.util.List[_]): String =
    Option(list).map(_.asScala).toSeq.flatten.map(_.toString).mkString(", ")

  val redacted: Array[Byte] =
    Array(0xad, 0xe7, 0x5a, 0x72, 0xd7, 0x9d).map(_.toByte)
}
