(module base "cmzscheme.ss"
  (require "compile.ss"
           "prims.ss"
           "moreprims.ss")

  (provide (all-from "prims.ss")
           (all-from "moreprims.ss")
           
           ;; Operators:
           + - * / < > <= >= = expt quotient
           
           #%top
           #%app
           #%datum))
