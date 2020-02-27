module Hw1 where

type Mapping = [(String, String, String)]
data AST = EmptyAST | ASTNode String AST AST deriving (Show, Read)

writeExpression :: (AST, Mapping) -> String
evaluateAST :: (AST, Mapping) -> (AST, String)
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT writeExpression and evaluateAST FUNCTION ACCORDING TO GIVEN SIGNATURES -- 


writeExpression (ast, map) = (write map) ++ (lst ast)


lst EmptyAST = ""
lst (ASTNode a l r)
 | a == "plus" = "(" ++ (lst l) ++ "+" ++ (lst r) ++ ")"
 | a == "times" = "(" ++ (lst l) ++ "*" ++ (lst r) ++ ")"
 | a == "negate" = "(" ++ "-" ++ (lst l) ++ ")"
 | a == "cat" = "(" ++ (lst l) ++ "++" ++ (lst r) ++ ")"
 | a == "len" = "(" ++  "length " ++ (lst l) ++ (lst r) ++ ")"
 | a == "str" =   "\"" ++  (lst l) ++ (lst r) ++ "\""  
 | a == "num" =  (lst l)  ++ (lst r)
 |otherwise =  (lst l) ++ a ++ (lst r) 


wrt (x,y,z)
 | y == "str" = x ++ "="  ++ "\"" ++ z ++ "\"" 
 | y == "num" = x ++ "="  ++ z 
 |otherwise = "" 

gettup [] = ""
gettup (x:xs)
 |xs /= [] = (wrt x) ++ ";" ++  (gettup xs)
 |xs == [] = (wrt x) ++ (gettup xs)

write [] = ""
write x = "let " ++ (gettup x) ++ " in "  


lst2 (ASTNode a l r )
 | a == "plus" = show((read(lst2 l)::Integer) + (read(lst2 r)::Integer)) 
 | a == "times" =  show((read(lst2 l)::Integer) * (read(lst2 r)::Integer)) 
 | a == "negate" = show(- ( (read(lst2 l)::Integer)))
 | a == "len" = show(length (lst3 l) )
 | a == "num" = (lst2 l)
 | a == "str" = (lst2 l)
 | a == "cat" = (lst3 l) ++ (lst3 r) 
 | otherwise = a
  


lst3 (ASTNode a l r)
 |a == "cat" = (lst3 l) ++ (lst3 r)
 |a == "str" = (lst3 l)
 |otherwise = a


first (x, _, _) = x  
second (_, y, _) = y  
third (_, _, z) = z  


find v [] = v
find v (x:xs)
 |v == first x = third x
 |otherwise = find v xs

find2 v [] = v
find2 v (x:xs)
 |v == first x = second x
 |otherwise = find2 v xs


tak EmptyAST _ = EmptyAST
tak (ASTNode a l r ) ms
 |bi == a = (ASTNode bi lef rig)
 |ti == "num"  = (ASTNode "num" (ASTNode bi EmptyAST EmptyAST) EmptyAST)
 |otherwise = (ASTNode "str" (ASTNode bi EmptyAST EmptyAST) EmptyAST) where
 lef = tak l ms
 rig = tak r ms
 bi = find a ms
 ti = find2 a ms


evaluateAST (ast2 ,map2) = (tak ast2 map2 , ( lst2 (tak ast2 map2)))



