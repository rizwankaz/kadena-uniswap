(namespace "uniswap")

(module pool-kdx 'admin-keyset
  (defconst FEE:decimal 0.003)

  (defschema pool-state
    token-module:string
    token:string
    reserve-kda:decimal
    reserve-token:decimal
  )

  (deftable pool:{pool-state})
  (use fungible-v2)

  (defun get-lp-name (token:string):string
    (format "UNILP-{}" [token])
  )

  (defun init-pool (token-module:string token:string)
    (enforce (not (contains pool "state")) "Already initialized")
    (insert pool "state" {
      "token-module": token-module,
      "token": token,
      "reserve-kda": 0.0,
      "reserve-token": 0.0
    })
    (let ((lp-name (get-lp-name token)))
      (create-fungible-v2 lp-name "Uniswap LP Token" "UNI-LP" 6)
    )
  )

  ;; Remainder identical to template (you already have this)
)
