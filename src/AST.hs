module AST
  ( runProgram,
    Aexp (Num, L, Add, Sub, Mul),
    Bexp (BTrue, BFalse, Eq, LTE, BNot, And, Or),
    Com (Skip, Assn, Seq, If, While, Print),
  )
where

type Loc = String

data Aexp = Num Int | L Loc | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp

data Bexp = BTrue | BFalse | Eq Aexp Aexp | LTE Aexp Aexp | BNot Bexp | And Bexp Bexp | Or Bexp Bexp

data Com = Skip | Assn Loc Aexp | Seq Com Com | If Bexp Com Com | While Bexp Com | Print Aexp

-- Context stuff
type Ctx = (Store, Buffer)

type Buffer = [String]

type Store = [(Loc, Int)]

defaultCtx :: Ctx
defaultCtx = ([], [])

addToStore :: Store -> Loc -> Int -> Store
addToStore [] k v = [(k, v)]
addToStore ((oldK, oldV) : rest) k v
  | k == oldK = (k, v) : rest
  | otherwise = (oldK, oldV) : addToStore rest k v

addToCtx :: Ctx -> Loc -> Int -> Ctx
addToCtx (store, buf) loc val = (newStore, buf) where newStore = addToStore store loc val

lookupStore :: Store -> Loc -> Int
lookupStore [] var = error ("Unknown location " ++ var)
lookupStore ((k, v) : rest) var
  | k == var = v
  | otherwise = lookupStore rest var

lookupCtx :: Ctx -> Loc -> Int
lookupCtx (store, _) = lookupStore store

myPrint :: Ctx -> Int -> Ctx
myPrint (store, buf) expr = (store, show expr : buf)

-- evaluation

evalAexp :: Ctx -> Aexp -> Int
evalAexp ctx (L l) = lookupCtx ctx l
evalAexp _ (Num n) = n
evalAexp ctx (Add a b) = evalAexp ctx a + evalAexp ctx b
evalAexp ctx (Sub a b) = evalAexp ctx a - evalAexp ctx b
evalAexp ctx (Mul a b) = evalAexp ctx a * evalAexp ctx b

evalBexp :: Ctx -> Bexp -> Bool
evalBexp _ BTrue = True
evalBexp _ BFalse = False
evalBexp ctx (Eq a b) = evalAexp ctx a == evalAexp ctx b -- handles eq and neq
evalBexp ctx (LTE a b) = evalAexp ctx a <= evalAexp ctx b
evalBexp ctx (BNot e) = not $ evalBexp ctx e
evalBexp ctx (And a b) = evalBexp ctx a && evalBexp ctx b
evalBexp ctx (Or a b) = evalBexp ctx a || evalBexp ctx b

runCommands :: Ctx -> Com -> Ctx
runCommands ctx Skip = ctx
runCommands ctx (Print expr) = myPrint ctx $ evalAexp ctx expr
runCommands ctx (Assn l a) = addToCtx ctx l $ evalAexp ctx a
runCommands ctx (Seq a b) = runCommands (runCommands ctx a) b
runCommands ctx (If cond a b) = if evalBexp ctx cond then runCommands ctx a else runCommands ctx b
runCommands ctx (While cond body) = if evalBexp ctx cond then continuation else ctx
  where
    afterBody = runCommands ctx body
    continuation = runCommands afterBody (While cond body)

runProgram :: Com -> [String]
runProgram coms = snd $ runCommands defaultCtx coms
