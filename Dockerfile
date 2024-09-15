FROM ubuntu:20.04 AS base

FROM base AS builder


# --disable-sandboxing is needed due to bwrap: No permissions to creating new namespace error
RUN apt-get update && apt-get install -y opam \
    && opam init --bare -a -y --disable-sandboxing \
    && opam update

RUN opam switch create default ocaml-base-compiler.5.2.0

RUN opam install -y dune

WORKDIR /app

COPY dune-project dune hello.ml read2.ml ./

# eval $(opam config env) applies dune to PATH but it only persists in a single RUN layer
RUN eval $(opam config env) \
    && dune build hello.exe

FROM base AS runner

WORKDIR /app

COPY --from=builder /app/_build/default/hello.exe /app

CMD [ "/app/hello.exe" ]