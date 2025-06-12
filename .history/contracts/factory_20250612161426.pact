(namespace "uniswap")

(module factory 'admin-keyset
  (deftable pools:{object { "pool": string }})

  (defun create-pool (token-module:string token:string)
    (enforce (not (contains pools token)) "Pool already exists")
    (let ((pool-name (format "pool-{}" [token])))
      ;; No backtick string interpolation; build full pact call manually
      (let ((module-name (format "uniswap.{}.init-pool" [pool-name])))
        (pact module-name [token-module token])
      )
      (insert pools token { "pool": pool-name })
    )
  )

  (defun get-pool (token:string):string
    (at "pool" (read pools token))
  )
)
