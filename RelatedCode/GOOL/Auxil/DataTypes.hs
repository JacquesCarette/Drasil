module GOOL.Auxil.DataTypes (
    Label, Options(..),
    -- * Plot Elements
    Node(..),Section(..),Subsection(..),Story(..),
    -- * Control Mechanisms
    Event(..),NodeTransition(..),SectionTransition(..),

    -- * Constructing Datatypes
    createStory,createSectionTransition,createSectionTransitionAutoLabel,createSection,
    createSubsection,createNode,createNodeTransition,createNodeTransitionAutoLabel,createEvent
) where

import Data.List (nub)

import GOOL.Auxil.Helper (containsAll)

type Label = String                             -- ^ Every datatype is identifiable by a Label

-- | Holds the optional parameters specified in the Config file
data Options = Options {
    javalist :: Maybe String,
    cpplist :: Maybe String,
    objcstaticlist :: Maybe String,
    hsModule :: Maybe String,
    studentid :: Maybe Integer
    } deriving Show

---------- Story-specific:

-- | A Node represents a certain state of the Section
data Node = Node
    {nodeLabel :: Label                         -- ^ Returns the Label of the Node
    }
    deriving (Eq,Ord)

-- | An Event is some event in the game that is performed by the player
data Event = Event
    {eventLabel :: Label                        -- ^ Returns the Label of the Event
    }
    deriving (Eq,Ord)

-- | A NodeTransition is a transition from a Node to another Node, which is
--   \"fired\" by a certain Event. A NodeTransition has a \"direction\" from one node to another.
data NodeTransition = NodeTrans
    {nodeTransLabel :: Label,                   -- ^ Returns the Label of the NodeTransition
     nodeTransPreNode :: Node,                  -- ^ Returns the Node from which the transition is directing itself
     nodeTransPostNode :: Node,                 -- ^ Returns the Node to which the transition is directing itself
     nodeTransEvents :: [Event]                 -- ^ Returns the Event that causes the transition to \"fire\"
    }
    deriving (Eq,Ord)

-- | A Section contains a list of Nodes, Transitions, and Subsections, and it can be thought as a \"large-scale\" Node
data Section = Section
    {sectionLabel :: Label,                     -- ^ Returns the Label of the Section
     sectionNodes :: [Node],                    -- ^ Returns the list of Nodes that are contained within a Section
     sectionTrans :: [NodeTransition],          -- ^ Returns the list of Transitions that are contained within a Section
     sectionSubsects :: [Subsection]            -- ^ Returns the list of Subsections that are contained within a Section
    }
    deriving (Eq)

-- | A Subsection contains a list of Nodes and nested Subsections, and is a Section-within-a-Section.
-- Can be nested to unlimited depth.
data Subsection = Subsection
    {subsectionLabel :: Label,                  -- ^ Returns the Label of the Section
     subsectionNodes :: [Node],                 -- ^ Returns the list of Nodes that are contained within a Section
     subsectionSubsects :: [Subsection]         -- ^ Returns the list of Subsections that are contained within a Subsection
    }
    deriving (Eq)

-- | Gets all nodes contained in this subsection, including those within nested subsections
nestedSubsectionNodes :: Subsection -> [Node]
nestedSubsectionNodes s = subsectionNodes s ++ concatMap nestedSubsectionNodes (subsectionSubsects s)

-- | A SectionTransition is the same as a NodeTransition except that it is a transition from a Section to another Section,
--   meaning it holds the pre and post Section data
data SectionTransition = SectionTrans
    {sectionTransLabel :: Label,                -- ^ Returns the Label of the SectionTransition
     sectionTransSectionPreNode :: Section,     -- ^ Returns the Section from which the transition is directing itself
     sectionTransSectionPostNode :: Section,    -- ^ Returns the Section to which the transition is directing itself
     sectionTransPreNode :: Node,               -- ^ Returns the Node from which the transition is directing itself
     sectionTransPostNode :: Node,              -- ^ Returns the Node to which the transition is directing itself
     sectionTransEvents :: [Event]              -- ^ Returns the Event that causes the transition to \"fire\"
    }

-- | A Story describes the overall structure and progression of a storyline
data Story = Story
    {storyLabel :: Label,                       -- ^ Returns the Label of the Story
     storyNodes :: [Section],                   -- ^ Returns the list of Sections that are contained within a Story
     storyHead :: Node,                         -- ^ Returns the \"head\" of the Story, or where the Story begins
     storyTrans :: [SectionTransition],         -- ^ Returns the list of SectionTransitions that are contained within a Story
     storyEvents :: [Event]                     -- ^ Returns the list of Events, which is the ordered input to the StoryManager
    }

------------------
-- Construction --
------------------

-- | Constructs a Story
-- Still needs to check to make sure that there is no node with a non-deterministic flow
-- through it (more than one transition out of a node with same Event)
createStory :: Label -> [Section] -> Node -> [SectionTransition] -> [Event] -> Story
createStory _ [] _ _ _ = error "StoryManager.createStory: empty Section list"
createStory name ns headNode ts es =
    if headNode `elem` (concatMap sectionNodes ns) && ns `containsAll` (map sectionTransSectionPreNode ts ++ map sectionTransSectionPostNode ts)
        then Story name ns headNode ts es
        else error "StoryManager.createStory: head Section is not an element of Section list \
                    \OR all Section contained in SectionTransition list are not elements of Section list"

-- | Constructs a SectionTransition with custom Label
createSectionTransition :: Label -> Section -> Section -> Node -> Node -> [Event] -> SectionTransition
createSectionTransition = SectionTrans

-- | Constructs a SectionTransition with automatically generated Label, based on the StoryNodes provided
createSectionTransitionAutoLabel :: Section -> Section -> Node -> Node -> [Event] -> SectionTransition
createSectionTransitionAutoLabel preStory postStory preSection postSection es =
    createSectionTransition (sectionLabel preStory ++ " ===> " ++ sectionLabel postStory) preStory postStory preSection postSection es

-- | Constructs a Section
-- Still needs to check to make sure that there is no node with a non-deterministic flow
-- through it (more than one transition out of a node with same Event)
createSection :: Label -> [Node] -> [NodeTransition] -> [Subsection] -> Section
createSection _ [] _ _= error "StoryManager.createSection: empty Node list"
createSection name ns ts subs =
    if ns `containsAll` (map nodeTransPreNode ts ++ map nodeTransPostNode ts ++ allSubNodes)
        then if checkUniqueSubsectNodes
                then Section name ns ts subs
                else error "StoryManager.createSection: identical Node in multiple Subsections"
        else error "StoryManager.createSection: head Node is not an element of Node list \
                    \OR all SectionNodes contained in NodeTransition list are not elements of Node list \
                    \OR all Nodes contained in Subsection node lists are not elements of Node list"
        where allSubNodes = concatMap nestedSubsectionNodes subs
              checkUniqueSubsectNodes = length (nub allSubNodes) == length allSubNodes

-- | Constructs a Subsection
createSubsection :: Label -> [Node] -> [Subsection] -> Subsection
createSubsection _ [] _ = error "StoryManager.createSubsection: empty Node list"
createSubsection name ns subs =
    if all (`notElem` allSubNodes) ns
        then Subsection name ns subs
        else error "StoryManager.createSubsection: identical Node at multiple Subsection depths"
        where allSubNodes = concatMap nestedSubsectionNodes subs

-- | Constructs a Node
createNode :: Label -> Node
createNode = Node

-- | Constructs a NodeTransition with custom Label
createNodeTransition :: Label -> Node -> Node -> [Event] -> NodeTransition
createNodeTransition = NodeTrans

-- | Constructs a NodeTransition with automatically generated Label, based on the Nodes provided
createNodeTransitionAutoLabel :: Node -> Node -> [Event] -> NodeTransition
createNodeTransitionAutoLabel pre post es =
    createNodeTransition (nodeLabel pre ++ " ---> " ++ nodeLabel post) pre post es

-- | Constructs an Event
createEvent :: Label -> Event
createEvent = Event
