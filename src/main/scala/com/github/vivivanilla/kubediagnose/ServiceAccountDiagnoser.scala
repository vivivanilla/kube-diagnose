package com.github.vivivanilla.kubediagnose

import cats.effect.IO
import io.kubernetes.client.openapi.ApiClient
import io.kubernetes.client.openapi.apis.{CoreV1Api, RbacAuthorizationV1Api}
import io.kubernetes.client.openapi.models.{
  V1ClusterRole,
  V1ClusterRoleBinding,
  V1Pod,
  V1Role,
  V1RoleBinding,
  V1ServiceAccount
}
import io.kubernetes.client.util.Yaml

case class PolicyRule(
    namespace: Option[String],
    apiGroups: Seq[String],
    nonResourceUrls: Seq[String],
    resourceNames: Seq[String],
    resources: Seq[String],
    verbs: Seq[String]
)

object ServiceAccountDiagnoser {
  def apply(sa: V1ServiceAccount)(implicit
      client: ApiClient
  ): ServiceAccountDiagnoser =
    new ServiceAccountDiagnoser(sa, client)

  def apply(name: String, namespace: String)(implicit
      client: ApiClient
  ): IO[ServiceAccountDiagnoser] =
    for {
      api <- IO(new CoreV1Api(client))
      serviceAccount <- IO.blocking(
        api.readNamespacedServiceAccount(name, namespace, null)
      )
    } yield apply(serviceAccount)(client)

  def apply(
      pod: V1Pod
  )(implicit client: ApiClient): IO[ServiceAccountDiagnoser] =
    for {
      saName <- IO(pod.getSpec.getServiceAccountName)
      namespace <- IO(pod.getMetadata.getNamespace)
      diagnoser <- apply(saName, namespace)
    } yield diagnoser

}

class ServiceAccountDiagnoser(sa: V1ServiceAccount, val client: ApiClient) {
  val name = sa.getMetadata.getName
  val namespace = sa.getMetadata.getNamespace

  def diagnose: IO[Markdown] = for {
    permissions <- showPermissions.attempt
  } yield MarkdownBlocks(
    Seq(
      MarkdownHeading(s"ServiceAcount ${namespace}/${name}", 2),
      MarkdownParagraph("Specification:"),
      showSpec,
      MarkdownParagraph("Permissions:"),
      permissions match {
        case Left(e) =>
          MarkdownAlert(
            "WARNING",
            MarkdownParagraph(s"Cannot read permissions: ${e.getMessage}")
          )
        case Right(permissions) => permissions
      }
    )
  )

  def showSpec: Markdown = {
    val metadata = sa.getMetadata.managedFields(null)
    MarkdownCode(Yaml.dump(sa.metadata(metadata)), Some("yaml"))
  }

  def showPermissions: IO[Markdown] = {
    policyRules.map { policies =>
      MarkdownTable(
        Seq(
          "Namespace",
          "Verbs",
          "ApiGroups",
          "Resources",
          "ResourceNames",
          "NonResourceUrls"
        ) +: policies.map(policy =>
          Seq(
            policy.namespace.getOrElse("*"),
            policy.verbs.mkString(", "),
            policy.apiGroups
              .map {
                case ""  => "core"
                case any => any
              }
              .mkString(", "),
            policy.resources.mkString(", "),
            policy.resourceNames.mkString(", "),
            policy.nonResourceUrls.mkString(", ")
          )
        )
      )
    }
  }

  /** RoleBindings that reference the ServiceAccount
    */
  def roleBindings: IO[Seq[V1RoleBinding]] = {
    // returns true if RoleBinding referencecs this ServiceAccount, false otherwise
    def filterForServiceAccount(rb: V1RoleBinding) =
      rb.getSubjects.convertToScala
        .filter(subject =>
          (subject.getKind == "ServiceAccount" && subject.getName == name && rb.getMetadata.getNamespace == namespace)
            || (subject.getKind == "ServiceAccount" && subject.getName == name && subject.getNamespace == namespace)
            || (subject.getKind == "Group" && subject.getName == "system:serviceaccounts" && subject.getApiGroup == "rbac.authorization.k8s.io")
        )
        .nonEmpty

    for {
      api <- IO(new RbacAuthorizationV1Api(client))
      roleBindings <- IO
        .blocking(
          api.listRoleBindingForAllNamespaces(null, null, null, null, null,
            null, null, null, null, null, null)
        )
        .map(_.getItems.convertToScala.filter(filterForServiceAccount))
    } yield roleBindings
  }

  /** Roles and namespaced ClusterRoles associated with the ServiceAccount
    */
  def roles: IO[Seq[V1Role | (String, V1ClusterRole)]] = {
    def roleForRoleBinding(
        rb: V1RoleBinding
    ): IO[V1Role | (String, V1ClusterRole)] =
      for {
        api <- IO(new RbacAuthorizationV1Api(client))
        roleRef <- IO(rb.getRoleRef)
        roleName <- IO(roleRef.getName)
        roleNamespace <- IO(rb.getMetadata.getNamespace)
        roleKind <- IO(roleRef.getKind)
        role <- roleKind match {
          case "Role" =>
            IO.blocking(api.readNamespacedRole(roleName, roleNamespace, null))
          case "ClusterRole" =>
            IO.blocking(api.readClusterRole(roleName, null))
              .map((roleNamespace, _))
        }
      } yield role

    for {
      roleBindings <- roleBindings
      roles <- IO.parSequenceN(1)(roleBindings.map(roleForRoleBinding))
    } yield roles
  }

  /** ClusterRoleBindings referencing this ServiceAccount
    */
  def clusterRoleBindings: IO[Seq[V1ClusterRoleBinding]] = {
    def filterForServiceAccount(crb: V1ClusterRoleBinding) =
      crb.getSubjects.convertToScala
        .filter(subject =>
          subject.getKind == "ServiceAccount" && subject.getName == name && subject.getNamespace == namespace
        )
        .nonEmpty

    for {
      api <- IO(new RbacAuthorizationV1Api(client))
      clusterRoleBindings <- IO
        .blocking(
          api.listClusterRoleBinding(null, null, null, null, null, null, null,
            null, null, null, null)
        )
        .map(_.getItems.convertToScala.filter(filterForServiceAccount))
    } yield clusterRoleBindings
  }

  /** Cluster roles that are associated with this service account
    */
  def clusterRoles: IO[Seq[V1ClusterRole]] = {
    def clusterRoleForClusterRoleBinding(
        crb: V1ClusterRoleBinding
    ): IO[V1ClusterRole] =
      for {
        api <- IO(new RbacAuthorizationV1Api(client))
        roleRef <- IO(crb.getRoleRef)
        roleName <- IO(roleRef.getName)
        role <- IO.blocking(
          api.readClusterRole(roleName, null)
        )
      } yield role

    for {
      clusterRoleBindings <- clusterRoleBindings
      clusterRoles <- IO.parSequenceN(1)(
        clusterRoleBindings.map(clusterRoleForClusterRoleBinding)
      )
    } yield clusterRoles
  }

  /** Permission rules of the ServiceAccount
    */
  def policyRules: IO[Seq[PolicyRule]] = {
    for {
      roles <- roles
      clusterRoles <- clusterRoles
    } yield for {
      role <- roles ++ clusterRoles
      (namespace, policyRule) <- role match {
        case role: V1Role =>
          role.getRules.convertToScala.map(
            (Some(role.getMetadata.getNamespace), _)
          )
        case role: V1ClusterRole => role.getRules.convertToScala.map((None, _))
        case (namespace: String, role: V1ClusterRole) =>
          role.getRules.convertToScala.map((Some(namespace), _))
      }
    } yield PolicyRule(
      namespace,
      policyRule.getApiGroups.convertToScala,
      policyRule.getNonResourceURLs.convertToScala,
      policyRule.getResourceNames.convertToScala,
      policyRule.getResources.convertToScala,
      policyRule.getVerbs.convertToScala
    )
  }
}
