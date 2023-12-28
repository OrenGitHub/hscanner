module Token
where
import Location

data Token
   = TokenInt TokenIntContent
   | TokenID TokenIDContent
   | TokenPlus
   deriving ( Show )

data TokenIntContent
   = TokenIntContent
   {
       tokenIntValue :: Int,
       tokenIntLocation :: Location
   }
   deriving ( Show )

data TokenIDContent
   = TokenIDContent
   {
       tokenIDValue :: String,
       tokenIDLocation :: Location
   }
   deriving ( Show )
