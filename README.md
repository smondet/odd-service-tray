# odd-service-tray

## Development

Dependencies:

```sh
opam switch create ost500 5.0.0
opam switch link ost500 .
eval $(opam env)
opam install ./odd_service_tray.opam --deps-only --with-test
```

Build:

     dune build
     
Format & test:

     dune runtest
     dune fmt
