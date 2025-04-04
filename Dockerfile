# alpine base image
FROM alpine:3.20 AS alpine-base

FROM alpine-base AS alpine-builder

RUN apk update && \
    apk upgrade && \
    apk add build-base opam

RUN opam init --bare -a -y --disable-sandboxing \
    && opam update

RUN opam switch create default ocaml-base-compiler.5.2.0

RUN opam install -y dune

WORKDIR /app

COPY dune-project dune hello.ml read2.ml ./

RUN opam exec dune build

FROM alpine-base AS alpine-runner

WORKDIR /app

COPY --from=alpine-builder /app/_build/default/hello.exe /app

CMD [ "/app/hello.exe" ]

# ubuntu base image 
FROM ubuntu:20.04 AS ubuntu-base

FROM ubuntu-base AS ubuntu-builder

# --disable-sandboxing is needed due to bwrap: No permissions to creating new namespace error
RUN apt-get update \
    && apt-get upgrade \
    && apt-get install -y opam \
    && opam init --bare -a -y --disable-sandboxing \
    && opam update

RUN opam switch create default ocaml-base-compiler.5.2.0

RUN opam install -y dune

WORKDIR /app

COPY dune-project dune hello.ml read2.ml ./

RUN opam exec dune build hello.exe

FROM ubuntu-base AS ubuntu-runner

WORKDIR /app

COPY --from=ubuntu-builder /app/_build/default/hello.exe /app

CMD [ "/app/hello.exe" ]