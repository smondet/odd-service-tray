(lang dune 3.7)

(name odd_service_tray)

(generate_opam_files true)

(source
 (github smondet/odd-service-tray))

(authors "Seb Mondet")

(maintainers "Seb Mondet")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name odd_service_tray)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base lablgtk3
  ppx_deriving
  ppx_fields_conv
  ppx_variants_conv
  ppx_base
  ppx_inline_test
  ppx_expect
  ;; We use `with-test` because `--with-dev` is not yet in the released `opam`:
  (merlin :with-test)
  (ocamlformat (and (= "0.24.1") :with-test)))
 (tags
  (desktop gtk)))

