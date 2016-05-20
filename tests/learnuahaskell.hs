sh x = "this is x: " ++ show x

cap "" = " cap is null"
cap a@(x:_) = "cap is " ++ sh x

cyl r h = s+2*t
    where s=2*pi*r*h
          t=pi*r*r

initials fn ln = 
    let (f:_) = fn
        (l:_) = ln
    in [f] ++ ". " ++ [l] ++ "."

conv x = case x of 1 -> "one"
                   2 -> "two"
                   3 -> "three"
                   xx -> "idk"