module Parser() where

import Language

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.ParserCombinators.Parsec.Expr

lexeme p = p <* many space
lexeme' p = p <* many1 space

parens p = char '(' *> many space *> p <* char ')'

identifier :: Parser String
identifier = (:) <$> lower <*> many (upper <|> lower)

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

joinP :: Parser Join
joinP = chainl1 (lexeme varj) (AndJ <$ (lexeme $ char '&'))
  where varj = VarJ <$> identifier <*> (parens $ pat `sepBy` (lexeme $ char ','))

def :: Parser Def
def = chainl (lexeme reaction) (OrD <$ (lexeme $ string "or")) EmptyD
    <?> "Definition"
  where reaction = ReactionD <$> joinP <* (lexeme $ string "|>") <*> proc

proc :: Parser Proc
proc = chainl1 proc' (AndP <$ (lexeme $ char '&')) <?> "Process"
  where proc' = (try defp)
              <|> (try matchp)
              <|> MsgP <$> identifier
                       <*> (lexeme (parens $ expr `sepBy` (lexeme $ char ',')))
              <|> InertP <$ (lexeme $ char '0')
        defp = DefP <$  (lexeme' $ string "def")
                    <*> def
                    <*  (lexeme' $ string "in")
                    <*> proc
        matchp = MatchP <$  (lexeme' $ string "match")
                        <*> expr
                        <*  (lexeme' $ string "with")
                        <*> matchPair `sepBy` (lexeme $ char '|')
        matchPair = (,) <$> lexeme pat
                        <*  (lexeme $ string "->")
                        <*> proc
test = parseFromFile (proc <* eof) "test.join"
