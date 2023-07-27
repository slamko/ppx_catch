(lang dune 3.6)

(name ppx_catch)

(generate_opam_files true)

(source
 (github slamko/ppx_catch))

(authors "Viacheslav Chepelyk-Kozhin")

(maintainers "Viacheslav Chepelyk-Kozhin")

(license AGPL-3.0-only)

(documentation https://github.com/slamko/ppx_catch)

(package
 (name ppx_catch)
 (synopsis "PPX rewriter that wraps exceptions into result")
 (depends ocaml dune)
 (tags
  (ppx functional-programming no-exception ppx-rewriter meta-programming)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
