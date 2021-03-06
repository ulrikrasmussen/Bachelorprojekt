module Parser(program, runParser, parseTest) where

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

-- |Parses a type constructor, which consists of one uppercase letter followed by
-- zero of more alphanumeric letters (mixed case) or plings.
constructor :: Parser String
constructor = (:) <$> upper <*> many (alphaNum <|> char '\'') <?> "constructor"

genericString :: ([a] -> a) -> a -> (Int -> a) -> Parser a
genericString cons nil int = do
 char '"'
 str <- many stringChar
 char '"'
 return $ mkJString str
 where
   stringChar = do
     x <- noneOf ['"']
     if x /= '\\'
       then return x
       else do e <- anyChar
               case e of
                 'n' -> return '\n'
                 't' -> return '\t'
                 '"' -> return '"'
                 '\\' -> return '\\'
                 x -> return x

   mkJString [] = nil
   mkJString (x:xs) = cons [int . fromEnum $ x, mkJString xs]

-- |Parses an expression.
expr :: Parser Expr
expr =   ConE  <$> (lexeme constructor)
               <*> ((parens $ lexeme expr `sepBy` (lexeme $ char ','))
                    <|> ((:[]) <$> expr)
                    <|> pure [])
     <|> VarE  <$> (lexeme identifier)
     <|> (IntE . read) <$> (lexeme $ many1 digit)
     <|> genericString (ConE "Cons") (ConE "Nil" []) IntE
     <?> "expression"

-- |Parses a sugared expression. This is the exact same parser as 'expr', but
-- also includes synchronous calls.
sexpr :: Parser SExpr
sexpr = ConS <$> (lexeme constructor)
             <*> ((parens $ lexeme sexpr `sepBy` (lexeme $ char ','))
                  <|> ((:[]) <$> sexpr)
                  <|> pure [])
        <|> (IntS . read) <$> (lexeme $ many1 digit)
        <|> (do ident <- identifier
                (CallS ident <$> (parens $ lexeme sexpr `sepBy` (lexeme $ char ','))
                 <|> (pure $ VarS ident)))
        <|> genericString (ConS "Cons") (ConS "Nil" []) IntS
        <?> "sugared expression"

-- |Parses patterns (like expressions, but linear).
pat :: Parser Pat
pat  =   ConP  <$> (lexeme constructor)
               <*> ((parens $ lexeme pat `sepBy` (lexeme $ char ','))
                    <|> ((:[]) <$> pat)
                    <|> (pure []))
     <|> VarP  <$> (lexeme identifier)
     <|> IntP . read  <$> (lexeme $ many1 digit)
     <|> genericString (ConP "Cons") (ConP "Nil" []) IntP
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
  where def = ReactionD <$> (try joins)
                        <*  (string "|>")
                        <*> (char '^' *> lexeme1 (read <$> many1 digit)
                             <|> pure 0 <* many1 space)
                        <*> proc
            <|> LocationD <$> identifier
                <* (lexeme $ char '[') <*> defs <* (lexeme1 $ string "in") <*> proc <* char ']'

-- |Parses a process, which is one or more atoms, separated by '&'
proc :: Parser Proc
proc =  (Proc . concat <$> atoms)
  where atoms = (((:[]) <$> lexeme atom)
                  <|> (concat <$> (lexeme $ parens atoms))) `sepBy` (lexeme $ char '&')
        atom = defp
            <|> matchp
            <|> DelayA 0 . Proc . (:[])
                <$> msga
            <|> InertA <$ char '0'
            <|> InstrA <$> (braces $ lexeme instr `sepBy` (lexeme $ char ';'))
            <|> DelayA <$> lexeme (read <$> many1 digit)
                       <*  (lexeme $ char ':')
                       <*> (Proc . (:[]) <$> msga)
            <?> "atom"
        msga = ((DelayA 0) . Proc . (:[])) <$> (MsgA <$> identifier
                          <*> (angles $ lexeme expr `sepBy` (lexeme $ char ',')))
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
instr =  LetI <$ (lexeme1 $ string "let")
              <*> ((:[]) <$> lexeme pat
                   <|> (lexeme . parens $ lexeme pat `sepBy` lexeme (char ',')))
              <*  (lexeme $ char '=')
              <*> sexpr
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
                 <*>((lexeme $ parens (lexeme sexpr `sepBy` (lexeme $ char ',')))
                    <|> (:[]) <$> lexeme sexpr)
                 <* (lexeme1 $ string "to")
                 <*> identifier
     <?> "instruction"
  where matchPair = (,) <$> lexeme pat
                        <* (lexeme $ string "->")
                        <*> (braces $ lexeme instr `sepBy` (lexeme $ char ';'))

-- |Parses a complete program, which consists of one or more atoms
program :: Parser Proc
program =  ( Proc . (:[]) . (DelayA 0)) <$> (many space *> (lexeme proc <* eof))
