{-# OPTIONS -Wall #-} 
-- | Abstract Syntax Tree for Uniform Resource Identifiers
module Language.Drasil.URI.AST where

data URI = URL Scheme Authority Path Query Fragment
         | ISBN String
         
-- | URL scheme
data Scheme = HTTP --Just the two to start
            | FTP
            
-- | Authentication if necessary
data Authority = Full Username Password Host Port
               | Simple Host Port --Simplify for when authentication
                                  --isn't necessary.

                                  
type Path = String     -- ^ Type the full path excluding the first /
type Query = [String]  -- ^ Make sure [] doesn't print a "?"
                       ---Separate elements with "&"
type Fragment = String ---Make sure "" becomes "" not "#" when printing
type Username = String ---Again make sure "" doesn't print anything
type Password = String ---Don't print anything if "" or if Username=""
type Host = String     -- | Type the host address (ex. "www.github.com")
data Port = P Int      -- | Take an integer port number if applicable
          | NA         -- | Do nothing if port is not applicable
