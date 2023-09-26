FROM ocaml/opam:alpine

WORKDIR /app
COPY . ./

RUN opam install . --deps-only --unlock-base
RUN eval $(opam config env)
RUN sudo chown -R opam:nogroup . && opam exec dune build

CMD dune exec bin/main.exe /var/rinha/source.rinha
