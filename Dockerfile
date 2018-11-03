FROM haskell:8.0.2 as builder

# Install dependecies needed to compile Haskell libraries
RUN apt-get update && apt-get install -y \
    libpq-dev \
    xz-utils \
    make

WORKDIR /opt/build

COPY . .

#RUN echo Y | apt install postgresql-server-dev-all

RUN stack build && \
    strip /opt/build/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/type-safe-ws-exe

#FROM fpco/haskell-scratch:integer-gmp
FROM ubuntu:16.04

WORKDIR /opt/bin

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libpq-dev \
  libgmp-dev

COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/type-safe-ws-exe .

RUN mkdir -p /opt/resources

COPY --from=builder /opt/build/src/resources /opt/resources

EXPOSE 9001

ENTRYPOINT ["/opt/bin/type-safe-ws-exe", "/opt/resources/"]
