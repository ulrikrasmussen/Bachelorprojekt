module Parser(program, runParser) where

import Language

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.ParserCombinators.Parsec.Expr

lexeme :: Parser a -> Parser a
lexeme p = p <* many space

lexeme' :: Parser a -> Parser a
lexeme' p = p <* many1 space

parens :: Parser a -> Parser a
parens p = char '(' *> many space *> p <* char ')'

identifier :: Parser String
identifier = (:) <$> lower <*> many (alphaNum <|> char '\'')

expr :: Parser Expr
expr =   ZeroE <$ (lexeme $ char 'Z')
     <|> SuccE <$ (lexeme $ char 'S') <*> expr
     <|> VarE  <$> (lexeme identifier)
     <?> "Expression"

pat :: Parser Pat
pat  =   ZeroP <$ (lexeme $ char 'Z')
     <|> SuccP <$ (lexeme $ char 'S') <*> pat
     <|> VarP  <$> (lexeme identifier)
     <?> "Pattern"

joins :: Parser [Join]
joins = (lexeme varj) `sepBy` (lexeme $ char '&')
  where varj = VarJ <$> identifier <*> (parens $ pat `sepBy` (lexeme $ char ','))

defs :: Parser [Def]
defs = (lexeme reaction) `sepBy` (lexeme' $ string "or")
     <?> "Definition"
  where reaction = ReactionD <$> joins <* (lexeme $ string "|>") <*> proc

proc :: Parser Proc
proc =  (Proc . concat <$> atoms)
  where atoms = (((:[]) <$> atom)
                  <|> (concat <$> (lexeme $ parens atoms)))
                  `sepBy` (lexeme $ char '&')
        atom = (try defp)
              <|> (try matchp)
              <|> MsgP <$> identifier
                       <*> (lexeme (parens $ expr `sepBy` (lexeme $ char ',')))
              <|> InertA <$ (lexeme $ char '0')
        defp = DefA <$  (lexeme' $ string "def")
                    <*> defs
                    <*  (lexeme' $ string "in")
                    <*> proc
        matchp = MatchA <$  (lexeme' $ string "match")
                        <*> expr
                        <*  (lexeme' $ string "with")
                        <*> matchPair `sepBy` (lexeme $ char '|')
        matchPair = (,) <$> lexeme pat
                        <*  (lexeme $ string "->")
                        <*> proc

program :: Parser Proc
program = proc <* eof

test = parseFromFile (proc <* eof) "test.join"
