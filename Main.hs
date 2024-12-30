module Main where

data Node = Node {
    value    :: Double,
    gradient :: Double,           -- gradient w.r.t root node
    children :: [(Node, Double)], -- (child, d(node)/d(child))
    name     :: String
} deriving Show

initNode :: Double -> [(Node, Double)] -> String -> Node
initNode value children name = Node {
    value    = value,
    gradient = 0,
    children = children,
    name     = name
}

scalar :: Double -> String -> Node
scalar value name = initNode value [] name

add :: Node -> Node -> Node
add x y = initNode (vx + vy) ch n where
    vx = value x
    vy = value y
    ch = [(x, 1), (y, 1)]
    n  = "(" ++ name x ++ " + " ++ name y ++ ")"

multiply :: Node -> Node -> Node
multiply x y = initNode (vx * vy) ch n where
    vx = value x
    vy = value y
    ch = [(x, vy), (y, vy)]
    n  = "(" ++ name x ++ " + " ++ name y ++ ")"

square :: Node -> Node
square x = initNode (vx ^ 2) ch n where
    vx = value x
    ch = [(x, 2*vx)]
    n  = "(" ++ name x ++ ")^2"

-- compute the gradient of each node of the graph with respect to root
backward :: Node -> Double -> Node
backward node gradient = Node {
    value    = value node,
    gradient = gradient,
    children = map (\(child, g) -> (backward child (gradient * g), g)) (children node),
    name     = name node
}

main :: IO ()
main = putStrLn ""
