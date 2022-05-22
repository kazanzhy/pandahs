module Demo where

----------------------------------------------------------------------------------------------------------------
doubleFact :: Integer -> Integer
doubleFact n = if n <= 2 then n else n * doubleFact (n - 2)

----------------------------------------------------------------------------------------------------------------
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | abs n == 1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
----------------------------------------------------------------------------------------------------------------
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | abs n == 1 = 1
            | n > 0 = forward (n - 2) 0 1
            | n < 0 = revers (n + 1) 1 0

forward step prev curr = if step == 0 
                        then prev + curr 
                        else forward (step - 1) curr (prev + curr)

revers step prev curr = if step == 0 
                         then prev - curr 
                         else revers (step + 1) curr (prev - curr)
----------------------------------------------------------------------------------------------------------------

