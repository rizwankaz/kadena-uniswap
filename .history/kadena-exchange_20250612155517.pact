(namespace "uniswap") ;; assume we deploy in the "uniswap" namespace

(module exchange 'admin-keyset
  (defschema liquidity-pool
    token:string
    reserve-kda:decimal
    reserve-token:decimal
    total-liquidity:decimal
  )

  ;; key tables
  (deftable pools:{liquidity-pool})
  (deftable balances:{object {liquidity:decimal}})

  ;; Initialize pool for a token
  (defun init-pool (token:string)
    (enforce (not (contains pools token)) "Pool already exists")
    (insert pools token {"token": token, "reserve-kda": 0.0, "reserve-token": 0.0, "total-liquidity": 0.0})
  )

  ;; Add liquidity
  (defun add-liquidity (token:string kda:decimal token-amount:decimal)
    (enforce (> kda 0.0) "Must add KDA")
    (enforce (> token-amount 0.0) "Must add tokens")

    (with-read pools token
      { "reserve-kda": old-kda, "reserve-token": old-token, "total-liquidity": old-liq } ->

      ;; handle first deposit vs existing pool
      (if (= old-liq 0.0)
        ;; first liquidity provider
        (let ((liq kda))
          (insert balances (read-keyset "sender") { "liquidity": liq })
          (update pools token { "reserve-kda": kda, "reserve-token": token-amount, "total-liquidity": liq })
        )
        ;; existing pool
        (let* (
          (kda-ratio (/ kda old-kda))
          (expected-token (* old-token kda-ratio))
        )
          (enforce (>= token-amount expected-token) "Insufficient tokens")
          (let ((liq (* kda (/ old-liq old-kda))))
            (update balances (read-keyset "sender") { "liquidity": (+ (at "liquidity" (read balances (read-keyset "sender"))) liq) })
            (update pools token {
              "reserve-kda": (+ old-kda kda),
              "reserve-token": (+ old-token token-amount),
              "total-liquidity": (+ old-liq liq)
            })
          )
        )
      )
    )
  )

  ;; Remove liquidity
  (defun remove-liquidity (token:string amount:decimal)
    (enforce (> amount 0.0) "Amount must be > 0")

    (with-read pools token
      { "reserve-kda": r-kda, "reserve-token": r-token, "total-liquidity": total-liq } ->

      (let* (
        (user-liq (at "liquidity" (read balances (read-keyset "sender"))))
        (new-user-liq (- user-liq amount))
        (kda-out (* (/ amount total-liq) r-kda))
        (token-out (* (/ amount total-liq) r-token))
      )
        (enforce (>= user-liq amount) "Insufficient liquidity")
        (update balances (read-keyset "sender") { "liquidity": new-user-liq })
        (update pools token {
          "reserve-kda": (- r-kda kda-out),
          "reserve-token": (- r-token token-out),
          "total-liquidity": (- total-liq amount)
        })
      )
    )
  )

  ;; Swap KDA for tokens
  (defun swap-kda-for-token (token:string kda-in:decimal min-token-out:decimal)
    (enforce (> kda-in 0.0) "Input must be positive")

    (with-read pools token
      { "reserve-kda": r-kda, "reserve-token": r-token } ->

      ;; Uniswap V1 constant product formula with 0.3% fee
      (let* (
        (kda-in-fee (* kda-in 0.997))
        (num (* kda-in-fee r-token))
        (den (+ (* r-kda 1.0) kda-in-fee))
        (token-out (/ num den))
      )
        (enforce (>= token-out min-token-out) "Slippage exceeded")

        ;; update reserves
        (update pools token {
          "reserve-kda": (+ r-kda kda-in),
          "reserve-token": (- r-token token-out)
        })
      )
    )
  )

  ;; Swap token for KDA
  (defun swap-token-for-kda (token:string token-in:decimal min-kda-out:decimal)
    (enforce (> token-in 0.0) "Input must be positive")

    (with-read pools token
      { "reserve-kda": r-kda, "reserve-token": r-token } ->

      (let* (
        (token-in-fee (* token-in 0.997))
        (num (* token-in-fee r-kda))
        (den (+ (* r-token 1.0) token-in-fee))
        (kda-out (/ num den))
      )
        (enforce (>= kda-out min-kda-out) "Slippage exceeded")

        (update pools token {
          "reserve-kda": (- r-kda kda-out),
          "reserve-token": (+ r-token token-in)
        })
      )
    )
  )
)
