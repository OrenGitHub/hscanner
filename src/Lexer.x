{

{-# OPTIONS -w  #-}

module Lexer
(
    runAlex'     ,
    location     ,
    alexError'   ,
    tokIntValue  ,
    tokStrValue  ,
    alexMonadScan,

    Alex        (..),
    AlexTokenTag(..),
    AlexPosn    (..),
    AlexRawToken(..),
)
where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Location
}

%wrapper "monadUserState"

-- ***********************
-- *                     *
-- * regular expressions *
-- *                     *
-- ***********************

-- ***********
-- *         *
-- * integer *
-- *         *
-- ***********
@DIGIT  = 0-9
@INT    = @DIGIT+

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
@LETTER = [A-Za-z_]
@LETTER_OR_DIGIT = @LETTER | @DIGIT
@ID = @LETTER(@LETTER_OR_DIGIT*)

-- ********************
-- *                  *
-- * binary operators *
-- *                  *
-- ********************
@PLUS   = \+

-- ***********
-- *         *
-- * comment *
-- *         *
-- ***********
@COMMENT_PYTHON = "#".*

-- ***************
-- *             *
-- * white space *
-- *             *
-- ***************
@WHITE_SPACE = $white+

-- **********
-- * tokens *
-- **********
tokens :-

-- ********************
-- *                  *
-- * binary operators *
-- *                  *
-- ********************
@PLUS { lex' AlexRawToken_PLUS        }
@ID   { lex  AlexRawToken_ID          }
@INT  { lex (AlexRawToken_INT . read) }

-- ****************************************
-- * WHITE SPACE and COMMENTS: do nothing *
-- ****************************************

@WHITE_SPACE    ;
@COMMENT        ;

{

data AlexUserState = AlexUserState { filepath :: FilePath } deriving ( Show )

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = filepath <$> alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  alexSetUserState (AlexUserState { filepath = fp })

-- *********
-- *       *
-- * Token *
-- *       *
-- *********
data AlexTokenTag
   = AlexTokenTag
     {
         tokenRaw :: AlexRawToken,
         tokenLoc :: Location
     }
     deriving ( Show )

-- *************
-- *           *
-- * Raw Token *
-- *           *
-- *************
data AlexRawToken
   = AlexRawToken_INT Int
   | AlexRawToken_ID String
   | AlexRawToken_PLUS
   | TokenEOF
   deriving ( Show )

-- ***********
-- * alexEOF *
-- ***********
alexEOF :: Alex AlexTokenTag
alexEOF = do
    ((AlexPn _ l c),_,_,_) <- alexGetInput
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = TokenEOF,
            tokenLoc = Location {
                lineStart = l,
                colStart = c,
                colEnd = c,
                filename = (filepath alexUserState)
            }
        }

-- *******
-- *     *
-- * lex *
-- *     *
-- *******
lex :: (String -> AlexRawToken) -> AlexInput -> Int -> Alex AlexTokenTag
lex f ((AlexPn _ l c),_,_,str) i = do
    alexUserState <- alexGetUserState
    return $
        AlexTokenTag
        {
            tokenRaw = (f (take i str)),
            tokenLoc = Location {
                lineStart = l,
                colStart = c,
                colEnd = c+i,
                filename = (filepath alexUserState)
            }
        }

-- *********************************************
-- * lex' for tokens WITHOUT associated values *
-- *********************************************
lex' :: AlexRawToken -> AlexInput -> Int -> Alex AlexTokenTag
lex' = lex . const

-- **************
-- * alexError' *
-- **************
alexError' :: Location -> Alex a
alexError' location = alexError $ "ERROR[" ++ show location ++ "]\n"

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location :: AlexTokenTag -> Location
location = tokenLoc

-- ***************
-- *             *
-- * tokIntValue *
-- *             *
-- ***************
tokIntValue :: AlexTokenTag -> Int
tokIntValue t = case (tokenRaw t) of { AlexRawToken_INT i -> i; _ -> 0; }

-- ***************
-- *             *
-- * tokStrValue *
-- *             *
-- ***************
tokStrValue :: AlexTokenTag -> String
tokStrValue t = case (tokenRaw t) of { AlexRawToken_ID s -> s; _ -> ""; }

-- ************
-- *          *
-- * runAlex' *
-- *          *
-- ************
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
