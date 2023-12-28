{
{-# OPTIONS -w #-}

module Parser( parseProgram ) where

import Lexer
import Data.Either
import qualified Tokens

}

%name parse
%monad { Alex }
%error { parseError }
%tokentype { AlexTokenTag }
%lexer { lexwrap } { AlexTokenTag TokenEOF _ }

%token 

-- ********************
-- * Binary Operators *
-- ********************

'+' { AlexTokenTag AlexRawToken_PLUS   _ }

ID  { AlexTokenTag (AlexRawToken_ID  id) _ }
INT { AlexTokenTag (AlexRawToken_INT  i) _ }

-- ********************************
-- *                              *
-- * associativity and precedence *
-- *                              *
-- ********************************
%left '+'

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

program: tokens { Tokens.Tokens { Tokens.filename = "someFile.txt", Tokens.content = $1 } }

token: ID  { Tokens.TokID   (lexerIDVal  $1) (lexerLoc $1) }
     | INT { Tokens.TokInt  (lexerIntVal $1) (lexerLoc $1) }
     | '+' { Tokens.TokPlus                  (lexerLoc $1) }

tokens: token { [$1] } | token tokens { $1:$2 }

{

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

-- **************
-- *            *
-- * parseError *
-- *            *
-- **************
parseError :: AlexTokenTag -> Alex a
parseError t = alexError' (tokenLoc t)

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram :: FilePath -> String -> Either String Tokens.Tokens
parseProgram = runAlex' parse
}
