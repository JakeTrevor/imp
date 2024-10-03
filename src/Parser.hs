{-# LANGUAGE LambdaCase #-}

module Parser (parseProgram) where

import AST
import Data.Functor (($>))
import Text.Parsec (Parsec, alphaNum, char, choice, digit, letter, many, many1, manyTill, oneOf, optionMaybe, spaces, string, try, (<|>))

--------------
-- handle Aexp = Num Int | L Loc | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
parseNum :: Parsec String () Aexp
parseNum = Num . read <$> many1 digit

parseLoc :: Parsec String () Aexp
parseLoc = L <$> (many1 letter <> many alphaNum)

parseOp :: Parsec String () (Aexp -> Aexp -> Aexp)
parseOp =
  ( \case
      '*' -> Mul
      '-' -> Sub
      '+' -> Add
      _ -> error "unreachable"
  )
    <$> (spaces *> oneOf "+-*")

parseBinOp :: Parsec String () Aexp
parseBinOp = do
  a <- spaces *> ( try parseNum <|> parseLoc)
  op <- parseOp
  op a <$> (spaces *> parseAexp)

parseAexp :: Parsec String () Aexp
parseAexp = choice $ map try [parseBinOp, parseLoc, parseNum]

--------------------
-- handle Bexp = BTrue | BFalse | Eq Aexp Aexp | LTE Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp

parseTrue :: Parsec String () Bexp
parseTrue = string "true" $> BTrue

parseFalse :: Parsec String () Bexp
parseFalse = string "false" $> BFalse

parseLiteral :: Parsec String () Bexp
parseLiteral = try parseTrue <|> parseFalse

parseEq :: Parsec String () Bexp
parseEq =
  Eq
    <$> parseAexp
    <*> (spaces *> string "==" *> spaces *> parseAexp)

parseLTE :: Parsec String () Bexp
parseLTE =
  LTE
    <$> parseAexp
    <*> (spaces *> string "<=" *> spaces *> parseAexp)

parseUnitBexp :: Parsec String () Bexp
parseUnitBexp = choice $ map try [parseLiteral, parseEq, parseLTE]

parseBNot :: Parsec String () Bexp
parseBNot = BNot <$> (char '!' *> spaces *> parseBexp)

parseAnd :: Parsec String () Bexp
parseAnd =
  And
    <$> parseUnitBexp
    <*> (spaces *> string "&&" *> spaces *> parseBexp)

parseOr :: Parsec String () Bexp
parseOr =
  Or
    <$> parseUnitBexp
    <*> (spaces *> string "||" *> spaces *> parseBexp)

parseBBinOp :: Parsec String () Bexp
parseBBinOp = try parseAnd <|> parseOr

parseBexp :: Parsec String () Bexp
parseBexp = choice $ map try [parseBBinOp, parseBNot, parseEq, parseLTE, parseLiteral]

--------------------

-- data Com = Skip | Assn Loc Aexp  | If Bexp Com Com | While Bexp Com | Print Aexp | Seq Com Com
parseSkip :: Parsec String () Com
parseSkip = (spaces *> string "skip" <* spaces) $> Skip

parseAssn :: Parsec String () Com
parseAssn =
  Assn
    <$> manyTill alphaNum (spaces *> char '=')
    <*> (spaces *> parseAexp)

parseIf :: Parsec String () Com
parseIf =
  If
    <$> (string "if" *> spaces *> parseBexp)
    <*> (spaces *> string "then" *> spaces *> parseSeq)
    <*> (spaces *> string "else" *> spaces *> parseSeq)
    <* spaces <* string "end"

parseWhile :: Parsec String () Com
parseWhile =
  While
    <$> (string "while" *> spaces *> parseBexp)
    <*> (spaces *> string "do" *> spaces *> parseSeq)
    <* spaces <* string "end"

parsePrint :: Parsec String () Com
parsePrint =
  Print <$> (string "print" *> spaces *> parseAexp)

parseUnitCom :: Parsec String () Com
parseUnitCom = choice $ map try [parseSkip, parseAssn, parseIf, parseWhile, parsePrint]

parseSeq :: Parsec String () Com
parseSeq = do
  com1 <- parseUnitCom
  res <- optionMaybe $ try (spaces *> char ';' *> spaces *> parseSeq)
  return $ case res of
    Just com2 -> Seq com1 com2
    Nothing -> com1

parseProgram :: Parsec String () Com
parseProgram = parseSeq