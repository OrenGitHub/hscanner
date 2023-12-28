{
{-# OPTIONS -w #-}

module Parser( parseProgram ) where

import Lexer
import Data.Either
import qualified Token

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

program: ids { IDs { filename = "someFile.txt", content = $1 } }

tokenID: ID { Token.Named (tokStrValue $1) (location $1) }

ids: tokenID { [MyLovelyID] } | tokenID '+' ids { $1:$3 }

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
parseProgram :: FilePath -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}
