-- file: ch03/Guard.hs
-- case construct matches patterns within an expression
fromMaybe defval wrapped =
    case wrapped of
        Nothing -> defval
        Just value -> value
