(rule
 (targets serializable_t.ml serializable_t.mli)
 (deps serializable.atd)
 (action
  (run atdgen -t %{deps})))

(rule
 (targets serializable_j.ml serializable_j.mli)
 (deps serializable.atd)
 (action
  (run atdgen -j -j-std %{deps})))
