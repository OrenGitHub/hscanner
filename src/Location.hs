module Location
where
data Location
   = Location
     {
         filename  :: FilePath,
         lineStart :: Int,
         colStart  :: Int,
         colEnd    :: Int
     }
     deriving ( Show )
