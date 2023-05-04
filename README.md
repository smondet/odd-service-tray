# odd-service-tray

## Development

Dependencies:

```sh
opam switch create ost414 \
     --formula='"ocaml-base-compiler" {>= "4.14" & < "4.15"}'
opam switch link ost414 .
opam install . --deps-only --with-test --with-doc
```

Build:

     opam exec -- dune build
     
Format & test:

     opam exec -- dune runtest
     opam exec -- dune fmt

Run

    opam exec -- dune exec bin/main.exe start
