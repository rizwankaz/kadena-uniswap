(namespace "uniswap")

(module factory 'admin-keyset
  ;; Registry of supported tokens and pools
  (deftable pools:{object {pool:string}})

  ;; Create pool (calls init inside pool module)
  (defun create-pool (token-module:string token:string)
    (enforce (not (contains pools token)) "Pool already exists")
    (let ((pool-name (format "pool-{}" [token])))
      ;; call pool initializer
      (enforce-keyset 'admin-keyset)
      (pact `uniswap.{pool-name}.init-pool` [token-module token])
      ;; register pool
      (insert pools token { "pool": pool-name })
    )
  )

  ;; Get pool
  (defun get-pool (token:string):string
    (at "pool" (read pools token))
  )
)
