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

angles :: Parser a -> Parser a
angles p = char '<' *> many space *> p <* char '>'

brackets :: Parser a -> Parser a
brackets p = char '{' *> many space *> p <* char '}'

identifier :: Parser String
identifier = (:) <$> lower <*> many (alphaNum <|> char '\'')

expr :: Parser Expr
expr =   ZeroE <$ (lexeme $ char 'Z')
     <|> SuccE <$ (lexeme $ char 'S') <*> expr
     <|> VarE  <$> (lexeme identifier)
     <?> "Expression"

sexpr :: Parser SExpr
sexpr =   ZeroS <$ (char 'Z')
      <|> SuccS <$ (lexeme $ char 'S') <*> sexpr
      <|> (do ident <- identifier
              (CallS ident <$> (parens $ lexeme sexpr `sepBy` (lexeme $ char ','))
               <|> (pure $ VarS ident)))

pat :: Parser Pat
pat  =   ZeroP <$ (lexeme $ char 'Z')
     <|> SuccP <$ (lexeme $ char 'S') <*> pat
     <|> VarP  <$> (lexeme identifier)
     <?> "Pattern"

joins :: Parser [Join]
joins = (lexeme construction) `sepBy` (lexeme $ char '&')
  where construction = do
          ident <- identifier
          (VarJ ident <$> (angles $ pats)
           <|> SyncJ ident <$> (parens $ pats))
        pats = pat `sepBy` (lexeme $ char ',')

defs :: Parser [Def]
defs = (lexeme reaction) `sepBy` (lexeme' $ string "or")
     <?> "Definition"
  where reaction = ReactionD <$> joins <* (lexeme $ string "|>") <*> proc

proc :: Parser Proc
proc =  (Proc . concat <$> atoms)
  where atoms = (((:[]) <$> lexeme atom)
                  <|> (concat <$> (lexeme $ parens atoms))) `sepBy` (lexeme $ char '&')
        atom = defp
            <|> matchp
            <|> MsgA <$> identifier
                     <*> (angles $ lexeme expr `sepBy` (lexeme $ char ','))
            <|> InertA <$ char '0'
            <|> InstrA <$> (brackets $ lexeme instr `sepBy` (lexeme $ char ';'))
        defp = DefA <$  try (lexeme' $ string "def")
                    <*> defs
                    <*  (lexeme' $ string "in")
                    <*> proc
        matchp = MatchA <$  try (lexeme' $ string "match")
                        <*> lexeme expr
                        <*  (lexeme' $ string "with")
                        <*> matchPair `sepBy` (lexeme $ char '|')
        matchPair = (,) <$> lexeme pat
                        <*  (lexeme $ string "->")
                        <*> proc

instr :: Parser Instr
instr =  LetI <$ (lexeme' $ string "let") <*> lexeme pat <* (lexeme $ char '=') <*> sexpr
     <|> RunI <$ (try . lexeme' $ string "run") <*> proc
     <|> DoI <$ (lexeme' $ string "do")
             <*> identifier
             <*> parens (lexeme sexpr `sepBy` (lexeme $ char ','))
     <|> MatchI <$ (lexeme' $ string "match")
                <*> lexeme sexpr
                <* (lexeme' $ string "with")
                <*> lexeme matchPair `sepBy` (lexeme $ char '|')
     <|> ReturnI <$ (lexeme' $ string "return")
                 <*>(lexeme' $ parens (lexeme sexpr `sepBy` (lexeme $ char ',')))
                 <* (lexeme' $ string "to")
                 <*> identifier
  where matchPair = (,) <$> lexeme pat
                        <* (lexeme $ string "->")
                        <*> (brackets $ lexeme instr `sepBy` (lexeme $ char ';'))

program :: Parser Proc
program = lexeme proc <* eof

test = parseFromFile program "test.join"
