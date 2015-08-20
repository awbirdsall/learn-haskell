-- lastButOne
lastButOne n = if null (drop 2 n)
    then head n
    else lastButOne (drop 1 n)
