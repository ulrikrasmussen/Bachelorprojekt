module Parser(program, runParser) where

import Language

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.ParserCombinators.Parsec.Expr

-- |'lexeme p' parses p followed by zero or more spaces
lexeme :: Parser a -> Parser a
lexeme p = p <* many space

-- |'lexeme1 p' parses p followed by one or more spaces
lexeme1 :: Parser a -> Parser a
lexeme1 p = p <* many1 space

-- |'parens p' parses p inside parenthesis, allowing any number of spaces after
-- the opening parenthesis.
parens :: Parser a -> Parser a
parens p = char '(' *> many space *> p <* char ')'

-- |'angles p' parses p inside angle brackets, allowing any number of spaces
-- after the opening bracket.
angles :: Parser a -> Parser a
angles p = char '<' *> many space *> p <* char '>'

-- |'braces p' parses p inside braces, allowing any number of spaces after the
-- opening brace.
braces :: Parser a -> Parser a
braces p = char '{' *> many space *> p <* char '}'

-- |Parses an identifier, which consists of one lowercase letter followed by
-- zero or more alphanumeric letters (mixed case) or plings.
identifier :: Parser String
identifier = (:) <$> lower <*> many (alphaNum <|> char '\'') <?> "identifier"

-- |Parses an expression.
expr :: Parser Expr
expr =   ZeroE <$ (lexeme $ char 'Z')
     <|> SuccE <$ (lexeme $ char 'S') <*> expr
     <|> VarE  <$> (lexeme identifier)
     <?> "expression"

-- |Parses a sugared expression. This is the exact same parser as 'expr', but
-- also includes synchronous calls.
sexpr :: Parser SExpr
sexpr =   ZeroS <$ (char 'Z')
      <|> SuccS <$ (lexeme $ char 'S') <*> sexpr
      <|> (do ident <- identifier
              (CallS ident <$> (parens $ lexeme sexpr `sepBy` (lexeme $ char ','))
               <|> (pure $ VarS ident)))
      <?> "sugared expression"

-- |Parses patterns (like expressions, but linear).
pat :: Parser Pat
pat  =   ZeroP <$ (lexeme $ char 'Z')
     <|> SuccP <$ (lexeme $ char 'S') <*> pat
     <|> VarP  <$> (lexeme identifier)
     <?> "pattern"

-- |Parses one or more join expressions, separated by '&'.
joins :: Parser [Join]
joins = (lexeme construction) `sepBy1` (lexeme $ char '&')
  where construction = do
          ident <- identifier
          (VarJ ident <$> (angles $ pats)
           <|> SyncJ ident <$> (parens $ pats))
        pats = pat `sepBy` (lexeme $ char ',')


-- |Parses one or more definitions (reactions of the form J |> P) separated by
-- 'or'.
defs :: Parser [Def]
defs = (lexeme def) `sepBy1` (lexeme1 $ string "or")
     <?> "Definition"
  where def = ReactionD <$> (try joins) <* (lexeme $ string "|>") <*> proc
            <|> LocationD <$> identifier 
                <* (lexeme $ char '[') <*> defs <* (lexeme1 $ string "in") <*> proc <* char ']'


                -- (lexeme $ string "[" *> defs) <*> 
                -- (lexeme1 $ string "in" *> proc <* (string "]"))

-- |Parses a process, which is one or more atoms, separated by '&'
proc :: Parser Proc
proc =  (Proc . concat <$> atoms)
  where atoms = (((:[]) <$> lexeme atom)
                  <|> (concat <$> (lexeme $ parens atoms))) `sepBy` (lexeme $ char '&')
        atom = defp
            <|> matchp
            <|> MsgA <$> identifier
                     <*> (angles $ lexeme expr `sepBy` (lexeme $ char ','))
            <|> InertA <$ char '0'
            <|> InstrA <$> (braces $ lexeme instr `sepBy` (lexeme $ char ';'))
            <?> "atom"
        -- A 'def' parser. We use lookahead on the 'def' keyword to allow identifiers to be
        -- prefixed by 'def'.
        defp = DefA <$  try (lexeme1 $ string "def")
                    <*> defs
                    <*  (lexeme1 $ string "in")
                    <*> proc
        -- A 'match' parser. We use lookahead on the 'match' keyword to allow identifiers
        -- to be prefixed by 'match'.
        matchp = MatchA <$  try (lexeme1 $ string "match")
                        <*> lexeme expr
                        <*  (lexeme1 $ string "with")
                        <*> matchPair `sepBy` (lexeme $ char '|')
        matchPair = (,) <$> lexeme pat
                        <*  (lexeme $ string "->")
                        <*> proc

-- |Parses an instruction.
instr :: Parser Instr
instr =  LetI <$ (lexeme1 $ string "let") <*> lexeme pat <* (lexeme $ char '=') <*> sexpr
     -- We use lookahead on the keyword 'run' to distinguish between that
     -- keyword and the keyword 'return'.
     <|> RunI <$ (try . lexeme1 $ string "run") <*> proc
     <|> DoI <$ (lexeme1 $ string "do")
             <*> identifier
             <*> parens (lexeme sexpr `sepBy` (lexeme $ char ','))
     <|> MatchI <$ (lexeme1 $ string "match")
                <*> lexeme sexpr
                <* (lexeme1 $ string "with")
                <*> lexeme matchPair `sepBy` (lexeme $ char '|')
     <|> ReturnI <$ (lexeme1 $ string "return")
                 <*>(lexeme1 $ parens (lexeme sexpr `sepBy` (lexeme $ char ',')))
                 <* (lexeme1 $ string "to")
                 <*> identifier
     <?> "instruction"
  where matchPair = (,) <$> lexeme pat
                        <* (lexeme $ string "->")
                        <*> (braces $ lexeme instr `sepBy` (lexeme $ char ';'))

-- |Parses a complete program, which consists of one or more atoms
program :: Parser Proc
program = lexeme proc <* eof
