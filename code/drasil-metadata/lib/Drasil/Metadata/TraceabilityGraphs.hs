module Drasil.Metadata.TraceabilityGraphs (
    GraphInfo(..),
    NodeFamily(..),
    Label, Colour
) where

import Drasil.Database (UID)

-- * Types

-- | Type synonym for clarity.
type Colour = String
-- | Type synonym for clarity.
type Label = String

-- | A node family contains a list of 'UID's, their display labels, general subgraph label, and colour.
data NodeFamily = NF {
    -- | Node 'UID's.
    nodeUIDs :: [UID]
    -- | Display labels for nodes. We use the reference addresses from the 'UID's.
    , nodeLabels :: [Label]
    -- | Individual subgraph labels. These labels do not show on the
    -- final generated pdf or png files.
    , nfLabel :: Label
    -- | Gives the ability to change colours of bubbles within the graph.
    , nfColour :: Colour
}

-- | Holds all important and relevant information for generating a traceability graph.
-- Includes nodes, graph edges, and node family information.
data GraphInfo = GI {
    --------------- graph node families -------------------------------
    -- | Assumptions.
    assumpNF :: NodeFamily
    -- | Data definitions.
    , ddNF :: NodeFamily
    -- | General definitions.
    , gdNF :: NodeFamily
    -- | Theory models.
    , tmNF :: NodeFamily
    -- | Instance models.
    , imNF :: NodeFamily
    -- | Requirements (both functional and non-functional).
    , reqNF :: NodeFamily
    -- | Goal statement.
    , gsNF :: NodeFamily
    -- | Changes (both likely and unlikely).
    , chgNF :: NodeFamily

    -------------- graph edges  ---------------------------
    -- | Assumptions dependent on assumptions.
    , edgesAvsA     :: [(UID, [UID])]
    -- | Definitions, models, requirements, and changes dependent on assumptions.
    , edgesAvsAll   :: [(UID, [UID])]
    -- | Definitions and models that are dependent on other definitions and models.
    , edgesRefvsRef :: [(UID, [UID])]
    -- | Goals and requirements dependent on definitions, models, and other requirements.
    , edgesAllvsR   :: [(UID, [UID])]
    -- | Definitions, models, requirements, goals, and changes that are dependent on one another.
    , edgesAllvsAll :: [(UID, [UID])]

    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}
