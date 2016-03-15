{-# OPTIONS -Wall #-} 
module ASTURI where

data URI = URL Scheme Authority Path Query Fragment
         | ISBN String
         
data Scheme = HTTP --Just the two to start
            | FTP
            
data Authority = Full Username Password Host Port
               | Simple Host --Simplify the process when authentication
                             --isn't necessary.
               
type Path = String --Type the full path excluding the first /
type Query = [String] --Make sure [] doesn't print a "?"
                      --Separate elements with "&"
type Fragment = String --Make sure "" becomes "" not "#" when printing
type Username = String --Again make sure "" doesn't print anything
type Password = String --Don't print anything if "" or if Username=""
type Host = String --Type the host address (ex. "www.github.com")
data Port = P Int --Take an integer if applicable
          | NA    --Do nothing if port is not applicable