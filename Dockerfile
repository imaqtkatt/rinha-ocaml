FROM ocaml/opam

WORKDIR /app
COPY . ./

RUN opam install . --deps-only --unlock-base
RUN eval $(opam env)
RUN opam exec dune build

CMD dune exec bin/main.exe /var/rinha/source.rinha
