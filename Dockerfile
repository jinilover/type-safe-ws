FROM haskell:8.0.2 as builder
ARG BUILD_LOC=/opt/build

# Install dependecies needed to compile Haskell libraries
RUN apt-get update && apt-get install -y \
    libpq-dev \
    xz-utils \
    make

WORKDIR ${BUILD_LOC}

COPY . .

RUN stack build && \
    strip ${BUILD_LOC}/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/type-safe-ws-exe

FROM ubuntu:16.04
ARG RESRC_LOC=/opt/resources
ARG BUILD_LOC=/opt/build
ARG BIN_LOC=/opt/bin

RUN apt-get update && apt-get install -y \
  libpq-dev \
  libgmp-dev \
  curl \
  git

RUN mkdir -p ${RESRC_LOC}
RUN mkdir -p ${BIN_LOC}/.git

WORKDIR ${BIN_LOC}

COPY --from=builder ${BUILD_LOC}/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/type-safe-ws-exe .
COPY --from=builder ${BUILD_LOC}/.git ./.git
COPY --from=builder ${BUILD_LOC}/src/resources ${RESRC_LOC}

EXPOSE 9001

# -e RESRC_LOC=something must be passed in `docker run` because ARG is only for `docker build`
ENTRYPOINT ./type-safe-ws-exe ${RESRC_LOC}
