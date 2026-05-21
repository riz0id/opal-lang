;; macro-hygiene.opal — exercise the module-body macro pipeline.
;;
;; This file demonstrates that the expander now drives an end-to-end
;; macro-expansion through a module body. It exercises:
;;
;;   * `define-syntax` — defines a compile-time transformer.
;;   * `lambda` — the transformer body.
;;   * `quote-syntax` — the transformer's output syntax.
;;   * `define` — value binding at the module level.
;;   * Macro invocation at the module body level.
;;   * Lifting of the macro's output into the module's namespace.
;;
;; The scope-sets machinery (intro scopes, use-site scopes, inside-edge
;; scopes, and the per-context DefinitionContext box) is wired up
;; behind the scenes for every macro call — but with the current
;; macro-transformer language, no template-construction primitives
;; exist (no `syntax/loc`, no quasiquoted templates, no
;; `define-syntax-rule`), so the macro can't substitute its argument
;; into the output. It returns a literal `(quote-syntax (define x 1))`
;; regardless of what argument it receives. The use-site / inside-edge
;; mechanism is still active end-to-end — it just can't be *observed*
;; behaviorally without a name collision the macro would have to
;; construct.
;;
;; ---
;; How to run:
;;
;;   $ nix-shell --run 'cabal repl opal'
;;   λ> import Opal.Expander
;;   λ> stx <- runExpandFile "/abs/path/to/examples/macro-hygiene.opal"
;;   λ> -- inspect `stx` for the resulting module form
;;
;; What to look for in the output:
;;
;;   * The post-expansion module body contains BOTH `(define x 1)` and
;;     `(define y 0)` — the first one was produced by the macro.
;;   * The expansion log shows `enter-macro #'((make-defn) (y))` and
;;     `exit-macro #'((define) (x) (1))`, confirming the transformer
;;     ran.
;;   * The final `ExpandState.expand_namespace.ns_phases[0]
;;     .defns_variables` lists both `x = 1` and `y = 0`.

(module macro-hygiene

  (export y)

  ;; A macro that, given an identifier, produces a binding for it.
  ;; The interesting case (currently aspirational — see file header)
  ;; is when the caller passes an identifier that the caller has
  ;; *also* bound in the surrounding context: without use-site scopes,
  ;; the macro's binder and the caller's binder are indistinguishable
  ;; and resolution becomes ambiguous (or worse, silently picks the
  ;; wrong one).
  ;;
  ;; Because Opal's transformer language has no template-construction
  ;; primitive yet, this transformer returns a literal `(define x 1)`
  ;; instead of substituting the caller's identifier. The expansion
  ;; pipeline still flows through the use-site scope machinery — the
  ;; behavior just can't be observed via a y/y collision.
  (define-syntax make-defn
    (lambda (stx)
      (quote-syntax (define x 1))))

  ;; The outer binding.
  (define y 0)

  ;; The macro call: `partialExpandModuleBegin` recognises the macro
  ;; use, dispatches the transformer, and re-feeds the result into
  ;; the same pass — so the produced `(define x 1)` gets lifted into
  ;; the module's namespace, not left as inert syntax.
  (make-defn y))
