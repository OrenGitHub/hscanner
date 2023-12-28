module Main( main ) where

import Parser ( parseProgram )

import System.Environment ( getArgs )
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.Text.Lazy

main :: IO ()
main = do {

    (input:output:_) <- getArgs;
    program' <- fmap (parseProgram input) (readFile input);
    case program' of
        Left errorMsg -> Data.Text.Lazy.IO.writeFile output (pack errorMsg)
        Right program -> Data.Text.Lazy.IO.writeFile output (encodeToLazyText program)
}
