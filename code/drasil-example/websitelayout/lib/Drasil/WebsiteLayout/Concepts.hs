module Drasil.WebsiteLayout.Concepts where

import Language.Drasil


idwebsitelayout :: IdeaDict
idwebsitelayout      = mkIdea  "websitelayout"          (cn' "Website Layout")                 Nothing

idwebsite :: IdeaDict
idwebsite      = mkIdea  "website"          (cn' "Website")                 Nothing

iddrasil :: IdeaDict
iddrasil      = mkIdea  "drasil"          (cn' "Drasil")                 Nothing

acronyms :: [CI]
acronyms = [websitelayout, progName, theWebsite, drasil]

drasil :: CI
drasil  = commonIdeaWithDict "drasil" (pn "Drasil") "Drasil" [iddrasil]

theWebsite :: CI
theWebsite  = commonIdeaWithDict "thewebsite" (pn "the website") "the website" [idwebsite]

websitelayout :: CI
websitelayout  = commonIdeaWithDict "websitelayout" (pn "website layout") "Websitelayout" [idwebsitelayout]

progName :: CI
progName = commonIdeaWithDict "website" (pn "Website") "Website" [idwebsite]

