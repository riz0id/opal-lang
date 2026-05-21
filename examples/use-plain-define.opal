;; use-plain-define.opal — exercise cross-file module import.
;;
;; Imports `plain-define` from `lib/define.opal` (resolved via the
;; expander's module search path) and uses it to bind `x` to 42.

(module use-plain-define-demo

  (import define)

  (plain-define x 42))
