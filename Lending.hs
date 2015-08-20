-- file: ch03/Lending.hs
-- introduce local variables with 'let...in' block
-- local vars are bound to expressions, which are only evaluated if needed
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
-- can also use 'where' clause to apply to preceding code
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve = 100
          newBalance = balance - amount
-- use guards: Bool expressions moved through. 'otherwise' is bound to True
lend3 amount balance
    | amount <= 0       = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise         = Just newBalance
    where reserve = 100
          newBalance = balance - amount
