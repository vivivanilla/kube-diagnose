FROM sbtscala/scala-sbt:graalvm-community-21.0.2_1.9.8_3.3.1 AS build
ARG TARGETPLATFORM
ARG BUILDPLATFORM
COPY . /build
RUN cd /build \
 && sbt graalvm-native-image:packageBin

FROM gcr.io/distroless/base-debian12
COPY --from=build /build/target/graalvm-native-image/kube-diagnose /
ENTRYPOINT [ "/kube-diagnose" ]
