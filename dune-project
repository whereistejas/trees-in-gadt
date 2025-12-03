(lang dune 3.20)

(name gadt)

(generate_opam_files true)

(source
 (github whereistejas/gadt))

(authors "Tejas Sanap <email@whereistejas.xyz>")

(maintainers "Tejas Sanap <email@whereistejas.xyz>")

(license MIT)

(package
 (name gadt)
 (synopsis "GADT library")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.4.0))
  (dune
   (>= 3.20.2))
  (base
   (>= 0.17))
  (ppx_inline_test
   (and
    :with-test
    (>= 0.16.1)))
  (ppx_assert
   (and
    :with-test
    (>= 0.16.0)))
  (ppx_expect
   (and
    :with-test
    (>= 0.16.0))))
 (depopts afl-persistent))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
