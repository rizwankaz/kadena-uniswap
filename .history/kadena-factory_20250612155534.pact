(namespace "uniswap")

(module factory 'admin-keyset

  (deftable tokens:{object {}})

  ;; Register new token pools
  (defun create-pool (token:string)
    (enforce (not (contains tokens token)) "Pool already exists")
    (insert tokens token {})
    (exchange.init-pool token)
  )

  ;; View whether token registered
  (defun pool-exists (token:string):bool
    (contains tokens token)
  )
)
