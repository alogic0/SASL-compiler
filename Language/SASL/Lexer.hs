{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Language.SASL.Lexer where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char hiding (Space)
--import Data.Maybe

type RE' = RE Char

data Token
    = Number Int
    | Op String
    | Identifier String
    | Text String
    --    | Key String         --key words
    | KeyDef | KeyIf | KeyThen | KeyElse | KeyWhere
    | KeyHd | KeyTl | KeyNil
    | LParen | RParen | LSquare | RSquare
    | Coma | Dot | SemiColon
    deriving Show

pOp :: RE' Token
pOp = foldr1 (<|>)
      $ map (Op <$>) ["+", "-", "*", "/", "=", "~=" ,"<", ">"
                     , "<=", ">=", "and", "or", "not"]
                   
pKw :: RE' Token
pKw = foldr1 (<|>) 
      $ zipWith (<$)
        [KeyDef, KeyIf, KeyThen, KeyElse, KeyWhere, KeyHd, KeyTl, KeyNil]
        ["def", "if", "then", "else", "where", "hd", "tl", "nil"] 

pSym :: RE' Token
pSym = foldr1 (<|>) 
      $ zipWith (<$)
        [LParen, RParen, LSquare, RSquare, Coma, Dot, SemiColon]
        (map sym ['(', ')', '[', ']', '[', ',', '.', ';'])

identifier :: RE' String
identifier = (:) <$> (psym isAlpha <|> sym '_') <*> many (psym isAlphaNum)

pText :: RE' Token
pText = Text <$> ("\"" *> few anySym <* "\"")

pToken :: RE' Token
pToken 
  = (Number <$> decimal)
  <|> pOp
  <|> pKw
  <|> pSym
  <|> pText
  <|> (Identifier <$> identifier)
  
tokens :: String -> [Token]
tokens "" = []
tokens s =
  let re = (Just <$> pToken) <|> (Nothing <$ some (psym isSpace)) in
  case findLongestPrefix re s of
    Just (Just tok, rest) -> tok : tokens rest
    Just (Nothing, rest) -> tokens rest -- whitespace
    Nothing -> error "lexical error"
