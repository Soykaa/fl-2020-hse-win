module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import PrologAst

languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h:t)

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
brackets = Token.parens lexer
dot = Token.dot lexer

parseNullArgumentedAtom :: Parser Atom
parseNullArgumentedAtom = do
  h <- identifier
  return Atom {atomHead = h, atomArgs = []}

parseListOfID :: Parser [Either Atom String]
parseListOfID = do
  h <- fmap Left parseNullArgumentedAtom
  t <- many(fmap Left parseNullArgumentedAtom)
  return (h:t)

parseAseq :: Parser [Either Atom String]
parseAseq = do
  h <- parseAseqElem
  t <- (parseAseq <|> return [])
  return (h:t)


manyBrackets :: Parser a -> Parser a
manyBrackets x = brackets (manyBrackets x) <|> x


parseAseqElem :: Parser (Either Atom String)
parseAseqElem = fmap Left parseNullArgumentedAtom <|> 
                fmap Left (manyBrackets atom)


atom :: Parser Atom
atom = do
  h <- identifier
  t <- (parseAseq <|> return [])
  return Atom {atomHead = h, atomArgs = t}

{-
data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq, Show)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq, Show)-}


relation :: Parser Relation
relation = do 
  h <- atom
  b <- (do; dot; return Nothing) <|> (do; (reservedOp ":-"); x <- parseBody; dot; return (Just x))
  return Relation {relHead = h, relBody = b}

parseBody :: Parser RelationBody
parseBody =
  fmap (foldr1 Disj) $ sepBy parseConaseq (char ';')

parseConaseq :: Parser RelationBody
parseConaseq =
  fmap (foldr1 Conj) $ sepBy parseConelem (char ',')

parseConelem :: Parser RelationBody
parseConelem = fmap RAtom atom <|> brackets parseBody




parseModule :: Parser String
parseModule = undefined


typeExpr :: Parser Type
typeExpr = undefined


typ :: Parser TypeDef
typ = undefined


prog :: Parser PrologProgram
prog = do
  r <- many relation
  return Program {pModule = Nothing, types = [], rels = r}