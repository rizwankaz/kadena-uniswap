(namespace "uniswap")

(module factory 'admin-keyset

  (defschema pools-schema
    pool: string
  )

  (deftable pools:{pools-schema})

  (defun create-pool (token-module:string token:string)
    (enforce (not (contains pools token)) "Pool already exists")
    (let ((pool-name (format "pool-{}" [token])))
      (let ((module-name (format "uniswap.{}.init-pool" [pool-name])))
        (pact module-name [token-module token])
      )
      (insert pools token { "pool": pool-name })
    )
  )

  (defun get-pool (token:string) string
    (at "pool" (read pools token))
  )
)
