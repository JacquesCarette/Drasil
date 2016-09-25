-- | Implementation of the Story Manager in Abstract Code
module GOOL.CodeGeneration.AbstractCode.StoryMgrAbstractCode (
    -- * Generated Class Names
    clsNames,

    -- * Constructing 'AbstractCode'
    makeAbstractCode
) where

import Prelude hiding (break,return)

import GOOL.CodeGeneration.AbstractCode
import GOOL.Auxil.DataTypes
import GOOL.Auxil.Helper (makeVarNameValid)

-- | Creates AbstractCode for the StoryManager
makeAbstractCode :: Story -> AbstractCode
makeAbstractCode s =
    let ms = [eventsEnum $ storyEvents s, nodeClass, nodeTransClass, sectionClass, subsectionClass, sectionTransClass, storyClass, storyManagerClass s]
    in AbsCode $ Pack "StoryDSL" ms $ Payload []

-- Class names required globally
eventsName, nodeName, sectName, nodeTransName, sectTransName, storyName, storyManagerName :: String
eventsName = "Event"
nodeName = "Node"
sectName = "Section"
subsectName = "Subsection"
nodeTransName = "NodeTransition"
sectTransName = "SectionTransition"
storyName = "Story"
storyManagerName = "StoryManager"

clsNames :: [String]
clsNames = [eventsName,nodeName,nodeTransName,sectName,subsectName,sectTransName,storyName,storyManagerName]

-- Helper/Convenience
event, node, nodeTransition, section, sectionTransition, story, storyManager :: StateType
event = EnumType eventsName
node = Type nodeName
nodeTransition = Type nodeTransName
section = Type sectName
subsection = Type subsectName
sectionTransition = Type sectTransName
story = Type storyName
storyManager = Type storyManagerName

validEventLabel :: Event -> Label
validEventLabel e = makeVarNameValid $ eventLabel e

validSectionLabel :: Section -> Label
validSectionLabel s = makeVarNameValid $ sectionLabel s

validSubsectLabel :: Subsection -> Label
validSubsectLabel sub = makeVarNameValid $ subsectionLabel sub

validNodeLabel :: Node -> Label
validNodeLabel n = makeVarNameValid $ nodeLabel n

sectionLabelComment :: Section -> Statement
sectionLabelComment s = comment $ "\"" ++ sectionLabel s ++ "\""

subsectionLabelComment :: Int -> Subsection -> Statement
subsectionLabelComment depth s = comment $ dashes ++ "> \"" ++ subsectionLabel s ++ "\""
    where dashes = take depth (repeat '-')

-- Variable names required globally
dstNode,eventData,eventLabels,headVar,label,managerStoryVar,nodeLabelVar,nodeOutTrans,nodes,nodeTrans,nodeTransEventsVar,sections,sectionTransVar,storyState :: String
dstNode = "dstNode"
eventData = "eventData"
eventLabels = "eventLabels"
headVar = "headVar"
label = "label"
managerStoryVar = "story"
nodeLabelVar = "nodeLabel"
nodeOutTrans = "nodeOutTrans"
nodeParent = "nodeParent"
nodes = "nodes"
nodeTrans = "nodeTrans"
nodeTransEventsVar = "nodeTransEvents"
sections = "sections"
sectionTransVar = "sectionTransVar"
storyState = "storyState"

privVarSep, evtsPrefix, nodePrefix, nodesPrefix, transPrefix, sectPrefix, outTransPrefix, toLabel :: String
privVarSep = "__"
evtsPrefix = "events" ++ privVarSep
nodePrefix = "node" ++ privVarSep
nodesPrefix = "nodes" ++ privVarSep
transPrefix = "trans" ++ privVarSep
sectPrefix = "sect" ++ privVarSep
subsectPrefix = "subsect" ++ privVarSep
subsectsPrefix = "subsects" ++ privVarSep
outTransPrefix = "outTrans" ++ privVarSep
sectTransPrefix = "sectTrans" ++ privVarSep
toLabel = privVarSep ++ "To" ++ privVarSep

-- IR (intermediate representation) of the various modules
eventsEnum :: [Event] -> Class
eventsEnum es =
    let eventEnumLabels = map validEventLabel es
    in Enum eventsName Public eventEnumLabels

nodeClass :: Class
nodeClass =
    let modName = "Node"
        modVars = [
            privMVar 2        string                       nodeLabelVar,
            privMVar neverDel (List Static nodeTransition) nodeOutTrans,
            privMVar neverDel section                      nodeParent]
        label = "label"
        trans = "trans"
        parent = "parent"
    in pubClass modName noParent modVars [
        pubMethod (Construct modName) modName [param label string] [
            Block [
                Self$->(Var nodeLabelVar) &=. label,
                Self$->(nodeOutTrans `listOf` nodeTransition) &= litObj (List Static nodeTransition) [litInt 0]]],
        GetMethod nodeLabelVar (typ string),
        GetMethod nodeOutTrans (typ $ List Static nodeTransition),
        GetMethod nodeParent (typ section),
        SetMethod nodeOutTrans $ param trans (List Static nodeTransition),
        SetMethod nodeParent $ param parent section
    ]

nodeTransClass :: Class
nodeTransClass =
    let modName = "NodeTransition"
        modVars = [
            privMVar neverDel node                srcNode,
            privMVar neverDel node                dstNode,
            privMVar 3        (List Static event) nodeTransEventsVar]
        srcNode = "srcNode"
        src = "src"
        dst = "dst"
        evts = "evts"
    in pubClass modName noParent modVars [
        pubMethod (Construct modName) modName [param src node, param dst node, param evts (List Static event)] [
            Block [
                Self$->(Var srcNode) &=. src,
                Self$->(Var dstNode) &=. dst,
                Self$->(Var nodeTransEventsVar) &=. evts
            ]],
        GetMethod srcNode (typ node),
        GetMethod dstNode (typ node),
        GetMethod nodeTransEventsVar (typ $ List Static event)
    ]

sectionClass :: Class
sectionClass =
    let modName = "Section"
        modVars = [
            privMVar 2         string                       sectionLabel,
            privMVar alwaysDel (List Static node)           nodes,
            privMVar alwaysDel (List Static nodeTransition) nodeTrans,
            privMVar alwaysDel (List Static subsection)     subsections]
        sectionLabel = "sectionLabel"
        label = "label"
        ns = "ns"
        trans = "trans"
        subsections = "subsections"
        subsects = "subsects"
    in pubClass modName noParent modVars [
        pubMethod (Construct modName) modName [param label string, param ns (List Static node), param trans (List Static nodeTransition), param subsects (List Static subsection)] [
            Block [
                Self$->(Var sectionLabel) &=. label,
                Self$->(nodes `listOf` node) &=. ns,
                Self$->(nodeTrans `listOf` nodeTransition) &=. trans,
                Self$->(subsections `listOf` subsection) &=. subsects
            ]],
        GetMethod sectionLabel (typ string),
        GetMethod nodes (typ $ List Static node),
        GetMethod nodeTrans (typ $ List Static nodeTransition),
        GetMethod subsections (typ $ List Static subsection)
    ]

subsectionClass :: Class
subsectionClass =
    let modName = "Subsection"
        modVars = [
            privMVar 2         string                   subsectionLabel,
            privMVar 3         (List Static node)       nodes,
            privMVar alwaysDel (List Static subsection) subsections]
        subsectionLabel = "subsectionLabel"
        label = "label"
        nodes = "nodes"
        ns = "ns"
        subsections = "subsections"
        subsects = "subsects"
    in pubClass modName noParent modVars [
        pubMethod (Construct modName) modName [param label string, param ns (List Static node), param subsects (List Static subsection)] [
            Block [
                Self$->(Var subsectionLabel) &=. label,
                Self$->(nodes `listOf` node) &=. ns,
                Self$->(subsections `listOf` subsection) &=. subsects
            ]],
        GetMethod subsectionLabel (typ string),
        GetMethod nodes (typ $ List Static node),
        GetMethod subsections (typ $ List Static subsection)
    ]

sectionTransClass :: Class
sectionTransClass =
    let modName = sectTransName
        modVars = [
            privMVar neverDel section             srcSection,
            privMVar neverDel section             dstSection,
            privMVar neverDel (List Static event) sectionTransEvents]
        srcSection = "srcSection"
        src = "src"
        dstSection = "dstSection"
        dst = "dst"
        sectionTransEvents = "sectionTransEvents"
        evts = "evts"
    in pubClass modName noParent modVars [
        pubMethod (Construct modName) modName [param src section, param dst section, param evts (List Static event)] [
            Block [
                Self$->(Var srcSection) &=. src,
                Self$->(Var dstSection) &=. dst,
                Self$->(sectionTransEvents `listOf` event) &=. evts
            ]],
        GetMethod srcSection (typ section),
        GetMethod dstSection (typ section),
        GetMethod sectionTransEvents (typ $ List Static event)
    ]

storyClass :: Class
storyClass =
    let modName = "Story"
        modVars = [
            privMVar 2         string                          storyLabel,
            privMVar alwaysDel (List Static section)           storySections,
            privMVar neverDel  node                            storyHead,
            privMVar alwaysDel (List Static sectionTransition) storySectionTrans,
            privMVar neverDel  node                            storyState,
            privMVar neverDel  int                             numEvents]
        storyLabel = "storyLabel"
        label = "label"
        storySections = "storySections"
        sects = "sects"
        storyHead = "storyHead"
        h = "h"
        storySectionTrans = "sectionTrans"
        sectTrans = "sectTrans"
        newState = "newState"
        numEvents = "numEvents"
        
        constructor =
            let eventCount = "eventCount"
                i = "i"
                j = "j"
            in pubMethod (Construct modName) modName [param label string, param sects (List Static section), param h node, param sectTrans (List Static sectionTransition)] [
                Block [
                    Self$->(Var storyLabel) &=. label,
                    Self$->(storySections `listOf` section) &=. sects,
                    Self$->(Var storyHead) &=. h,
                    Self$->(storySectionTrans `listOf` sectionTransition) &=. sectTrans,
                    Self$->(Var storyState) &=. h
                ],
                Block [
                    varDecDef eventCount int (litInt 0),
                    --forEach i (storySections `listOf` section) [
                    --    Block [
                    --        forEach j (Var i $. Get nodeTrans) $ oneLiner $
                    --            eventCount &+= (Var j $. Get nodeTransEventsVar $. ListSize)
                    --    ]
                    --]
                    for (varDecDef i int (litInt 0)) (Var i ?< (Self$->(storySections `listOf` section) $. ListSize)) ((&++)i) [
                        Block [
                            for (varDecDef j int (litInt 0)) (Var j ?< (Self$->(storySections `listOf` section) $. at i $. Get nodeTrans $. ListSize)) ((&++)j) $ oneLiner $
                                eventCount &+= (Self$->(storySections `listOf` section) $. at i $. Get nodeTrans $. at j $. Get nodeTransEventsVar $. ListSize)
                        ]
                    ]
                ],
                Block [Self$->(Var numEvents) &=. eventCount]]
    in pubClass modName noParent modVars [
        constructor,
        GetMethod storyLabel (typ string),
        GetMethod storySections (typ $ List Static section),
        GetMethod storyHead (typ node),
        GetMethod storySectionTrans (typ $ List Static sectionTransition),
        GetMethod storyState (typ node),
        GetMethod numEvents (typ int),
        SetMethod storyState $ param newState node
    ]

storyManagerClass :: Story -> Class
storyManagerClass s =
    let modName = "StoryManager"
        modVars = [
            privMVar alwaysDel story                managerStoryVar,
            privMVar neverDel  (List Dynamic bool)  eventData,
            privMVar 3         (List Static string) eventLabels]
        evtData = "evtData"
        processSnapshot = "ProcessSnapshot"
        checkEvents = "CheckEvents"
        
        constructor = pubMethod (Construct modName) modName [] $ concat [
            eventLabelsStatements (storyEvents s),
            labelStatements $ storyLabel s,
            eventsStatements (storyNodes s) (storyTrans s),
            nodesStatements (storyNodes s),
            nodeTransStatements (storyNodes s) (storyTrans s),
            subsectionStatements (storyNodes s),
            sectionStatements (storyNodes s),
            outGoingStatements (storyNodes s) (storyTrans s),
            headStatements (storyHead s),
            sectionTransStatements (storyTrans s),
            storyStatements]
        
        receiveStorySnapshotTransform =
            let newState = "newState"
            in pubMethod (typ node) "ReceiveStorySnapshot" [param evtData (List Dynamic bool)] [
                Block [
                    Self$->(eventData `listOf` bool) &=. evtData,
                    varDecDef newState node (Self $. Func processSnapshot []),
                    returnVar newState
                ]
            ]
        
        eventLabelTransform =
            let evt = "evt"
            in pubMethod (typ string) "EventLabelOf" [param evt event] [
                Block [
                    return $ Self$->(eventLabels `listOf` string) $. ListAccess (EnumVar evt)
                ]
            ]
        
        triggerEventTransform =
            let evt      = "evt"
                newState = "newState"
            in pubMethod (typ node) "TriggerEvent" [param evt event] [
                Block [
                    ValState $ Self$->(eventData `listOf` bool) $. ListSet (EnumVar evt) true,
                    varDecDef newState node (Self $. Func processSnapshot []),
                    returnVar newState
                ]
            ]
        
        processSnapshotTransform =
            let i = "i"
                oldState = "oldState"
                result = "result"
                trans = "trans"
                storyGetState = Self$->(Var managerStoryVar) $. Get storyState
            in privMethod (typ node) processSnapshot [] [
                Block [
                    varDecDef oldState node storyGetState,
                    varDecDef result node (Var oldState),
                    varDecDef trans (List Static nodeTransition) (Var oldState $. Get nodeOutTrans)
                ],
                Block [
                    forEach i (trans `listOf` nodeTransition) [
                        Block [
                            ifCond [(
                                Self $. (Func checkEvents [Var i $. Get nodeTransEventsVar]), [
                                    Block [ValState (Self$->(Var managerStoryVar) $. Set storyState (Var i $. Get dstNode)),
                                    break]]
                            )] noElse
                        ]
                    ]
                    {-for (varDecDef i int (litInt 0)) (Var i ?< (trans `listOf` nodeTransition $. ListSize)) ((&++)i) [
                        Block [
                            ifCond [(
                                Self $. (Func checkEvents [trans `listOf` nodeTransition $. at i $. Get nodeTransEventsVar, eventData `listOf` bool]), [
                                    Block [ValState (Self$->(Var managerStoryVar) $. Set storyState (trans `listOf` nodeTransition $. at i $. Get dstNode)),
                                    break]]
                            )] noElse
                        ]
                    ]-}
                ],
                Block [
                    ifCond [(
                        binExpr (Self$->(Var managerStoryVar) $. Get storyState $. Get nodeLabelVar) NotEqual (Var oldState $. Get nodeLabelVar), [
                            Block [result &.= storyGetState]]
                        )] noElse
                ],
                Block [returnVar result]
            ]
        
        checkEventsTransform =
            let result = "result"
                i = "i"
                evts = "evts"
            in privMethod (typ bool) checkEvents [param evts (List Static event)] [
                Block [varDecDef result bool true],
                Block [
                    {-forEach i (evts `listOf` event) [
                        Block [
                            ifCond [(
                                (?!) $ Self$->(eventData `listOf` bool) $. ListAccess (Var i), [
                                    Block [
                                        Var result &= false,
                                        break
                                    ]
                                ]
                            )] noElse
                        ]
                    ]-}
                    for (varDecDef i int (litInt 0)) (Var i ?< (evts `listOf` event $. ListSize)) ((&++)i) [
                        Block [
                            ifCond [(
                                (?!) $ Self$->(eventData `listOf` bool) $. ListAccess (evts `listOf` event $. at i), [
                                    Block [
                                        result &.= false,
                                        break
                                    ]
                                ]
                            )] noElse
                        ]
                    ]
                ],
                Block [returnVar result]]
        
    in pubClass modName noParent modVars [
        constructor,
        receiveStorySnapshotTransform,
        GetMethod managerStoryVar (typ story),
        GetMethod eventData (typ $ List Dynamic bool),
        GetMethod eventLabels (typ $ List Static string),
        eventLabelTransform,
        triggerEventTransform,
        processSnapshotTransform,
        checkEventsTransform
    ]

eventLabelsStatements :: [Event] -> Body
eventLabelsStatements es =
    let evtLabels = "evtLabels"
        i = "i"
        literals = map (Lit . LitStr . eventLabel) es
        dataSize = Self$->(eventLabels `listOf` string) $. ListSize
    in [
        Block [
            comment "Event labels data",
            listDecValues evtLabels string literals,
            Self$->(eventLabels `listOf` string) &=. evtLabels
        ],
        Block [
            Self$->(eventData `listOf` bool) &= litObj (List Dynamic bool) [dataSize],
            ValState $ Self$->(eventData `listOf` bool) $. ListPopulate dataSize bool
        ]
    ]

labelStatements :: Label -> Body
labelStatements l =
    let commentLabel = "Label of the " ++ storyName
        label = "label"
    in addComments commentLabel $ oneLiner $ varDecDef label string (litString l)

eventsStatements :: [Section] -> [SectionTransition] -> Body
eventsStatements sects ts =
    let commentLabel = "Events"
        sectTransEvts = concatMap (\t -> eventsStatements' (sectionTransPreNode t) (sectionTransPostNode t) (sectionTransEvents t)) ts
    in addComments commentLabel
        (eventsSectionsStatements sects ++ [Block $ (comment $ "Events for " ++ sectTransName ++ "s") : sectTransEvts])

eventsSectionsStatements :: [Section] -> Body
eventsSectionsStatements sects =
    let sectEvents = map (\s -> Block $ sectionLabelComment s : transEvents s) sects
        transEvents s = concatMap (\t -> eventsStatements' (nodeTransPreNode t) (nodeTransPostNode t) (nodeTransEvents t)) (sectionTrans s)
    in sectEvents

eventsStatements' :: Node -> Node -> [Event] -> [Statement]
eventsStatements' preN postN evts =
    let assignedEvts = map (\e -> eventsName $: validEventLabel e) evts
        evtsName = evtsPrefix ++ validNodeLabel preN ++ toLabel ++ validNodeLabel postN
    in [listDecValues evtsName event assignedEvts]

nodesStatements :: [Section] -> Body
nodesStatements ns =
    let commentLabel = nodeName ++ "s"
        sectNodes = concatMap (\n -> prefixFirstBlock (sectionLabelComment n) (nodesStatements' n)) ns
    in addComments commentLabel sectNodes

nodesStatements' :: Section -> Body
nodesStatements' (Section lab ns _ subs) =
    let names = map (\n -> nodePrefix ++ validNodeLabel n) ns
        nodesDec = zipBlockWith (\n nam -> objDecDef nam node (litObj node [litString $ nodeLabel n])) ns names
        subNodes = concatMap (\sub -> prefixFirstBlock (subsectionLabelComment 1 sub) (nodesStatements'' 2 sub)) subs
        assignedNodes = map Var names
        nodesName = nodesPrefix ++ makeVarNameValid lab
    in [nodesDec, Block [listDecValues nodesName node assignedNodes]] ++ subNodes

nodesStatements'' :: Int -> Subsection -> Body
nodesStatements'' depth (Subsection lab ns subs) =
    let assignedNodes = map (\n -> Var $ nodePrefix ++ validNodeLabel n) ns
        subNodes = concatMap (\sub -> prefixFirstBlock (subsectionLabelComment depth sub) (nodesStatements'' (depth+1) sub)) subs
        nodesName = nodesPrefix ++ makeVarNameValid lab
    in Block [listDecValues nodesName node assignedNodes] : subNodes

nodeTransStatements :: [Section] -> [SectionTransition] -> Body
nodeTransStatements ns ts =
    let commentLabel = nodeTransName ++ "s"
        allSects = concatMap (\n -> prefixFirstBlock (sectionLabelComment n) (nodeTransStatements' n (sectNodeTrans n))) ns
        sectNodeTrans n = concatMap (\t -> if sectionTransPreNode t `elem` sectionNodes n then [t] else []) ts
    in addComments commentLabel allSects

nodeTransStatements' :: Section -> [SectionTransition] -> Body
nodeTransStatements' (Section lab _ sectionTs _) storyTs =
    let newTs = map (\t -> NodeTrans (sectionTransLabel t) (sectionTransPreNode t) (sectionTransPostNode t) (sectionTransEvents t)) storyTs
        ts = sectionTs ++ newTs
        preNs = map (\t -> validNodeLabel $ nodeTransPreNode t) ts
        postNs = map (\t -> validNodeLabel $ nodeTransPostNode t) ts
        names = zipWith (\n1 n2 -> n1 ++ toLabel ++ n2) preNs postNs
        transNames = map (\n -> transPrefix ++ n) names
        eventNames = map (\n -> evtsPrefix ++ n) names
        transArgs n1 n2 eName = litObj nodeTransition [Var $ nodePrefix ++ n1, Var $ nodePrefix ++ n2, Var eName]
        nodeTransDec = zipBlockWith4 (\tName n1 n2 eName ->
            objDecDef tName nodeTransition (transArgs n1 n2 eName)) transNames preNs postNs eventNames
        nodeTransLabel = transPrefix ++ makeVarNameValid lab
        assignedTrans = map Var transNames
    in [nodeTransDec, Block [listDecValues nodeTransLabel nodeTransition assignedTrans]]

subsectionStatements :: [Section] -> Body
subsectionStatements ss =
    let commentLabel = subsectName ++ "s"
        sectSubsects = concatMap (\s -> prefixFirstBlock (sectionLabelComment s) (subsectionStatements' s)) ss
    in addComments commentLabel sectSubsects

subsectionStatements' :: Section -> Body
subsectionStatements' (Section lab _ _ subs) =
    let names = map (\sub -> subsectPrefix ++ validSubsectLabel sub) subs
        subsectsDec = zipBlockWith (\sub nam -> objDecDef nam subsection (litObj subsection [litString $ subsectionLabel sub, Var $ nodesPrefix ++ validSubsectLabel sub, Var $ subsectsPrefix ++ validSubsectLabel sub])) subs names
        nextLevelSubs = concatMap (\sub -> prefixFirstBlock (subsectionLabelComment 1 sub) (subsectionStatements'' 2 sub)) subs
        assignedSubsects = map Var names
        subsectsName = subsectsPrefix ++ makeVarNameValid lab
    in nextLevelSubs ++ [subsectsDec, Block [listDecValues subsectsName subsection assignedSubsects]]

subsectionStatements'' :: Int -> Subsection -> Body
subsectionStatements'' depth (Subsection lab _ subs) =
    let names = map (\sub -> subsectPrefix ++ validSubsectLabel sub) subs
        subsectsDec = zipBlockWith (\sub nam -> objDecDef nam subsection (litObj subsection [litString $ subsectionLabel sub, Var $ nodesPrefix ++ validSubsectLabel sub, Var $ subsectsPrefix ++ validSubsectLabel sub])) subs names
        nextLevelSubs = concatMap (\sub -> prefixFirstBlock (subsectionLabelComment depth sub) (subsectionStatements'' (depth+1) sub)) subs
        assignedSubsects = map Var names
        subsectsName = subsectsPrefix ++ makeVarNameValid lab
    in nextLevelSubs ++ [subsectsDec, Block [listDecValues subsectsName subsection assignedSubsects]]

sectionStatements :: [Section] -> Body
sectionStatements ns =
    let commentLabel = sectName ++ "s"
        sectArgs n = litObj section [litString $ sectionLabel n, Var $ nodesPrefix ++ validSectionLabel n, Var $ transPrefix ++ validSectionLabel n, Var $ subsectsPrefix ++ validSectionLabel n]
        sectDecs = map (\n -> objDecDef (sectPrefix ++ validSectionLabel n) section (sectArgs n)) ns
        assignedSects = map (\n -> Var $ sectPrefix ++ validSectionLabel n) ns
    in addComments commentLabel [Block sectDecs, Block [listDecValues sections section assignedSects]]

outGoingStatements :: [Section] -> [SectionTransition] -> Body
outGoingStatements ns ts =
    let commentLabel = "Outgoing " ++ nodeTransName ++ "s and parent " ++ sectName ++ " of " ++ nodeName ++ "s"
        nodeTrans = "nodeTrans"
        nodeTransPrefix = nodeTrans ++ privVarSep
        storyTs s = concatMap (\t ->
            if sectionTransPreNode t `elem` sectionNodes s
                then [NodeTrans (sectionTransLabel t) (sectionTransPreNode t) (sectionTransPostNode t) (sectionTransEvents t)] else []) ts
        allTrans s = sectionTrans s ++ storyTs s
        nodesWithTrans s = concatMap (\n -> [n]) (sectionNodes s)
        
        allSects = concatMap (\n -> [Block (sectionLabelComment n : outGoingStatements' n ts),
            Block [listDecValues (transName n) (List Static nodeTransition) $ assignedTrans n]]) ns
        assignedTrans n = map (\node -> Var $ outTransPrefix ++ validNodeLabel node) $ nodesWithTrans n
        transName n = nodeTransPrefix ++ validSectionLabel n
        assignedNodeTrans = map (Var . transName) ns
        
        allSectTrans = Block $
            [comment ("All " ++ sectName ++ "s " ++ nodeTransName ++ " data"),
            listDecValues nodeTrans (List Static $ List Static nodeTransition) assignedNodeTrans]
        
        outGoingLoop =
            let i = "i"
                j = "j"
            in Block [
                for (varDecDef i int (litInt 0)) (Var i ?< (nodeTrans `listOf` nodeTransition $. ListSize)) ((&++)i) [
                    Block [
                        for (varDecDef j int (litInt 0)) (Var j ?< (nodeTrans `listOf` nodeTransition $. at i $. ListSize)) ((&++)j) [
                            Block [
                                ValState $ Var sections $. at i $. Get nodes $. at j $. Set nodeOutTrans (Var nodeTrans $. at i $. at j)
                            ]
                        ]
                    ]
                ]]
        
        parentLoop =
            let i = "i"
                j = "j"
            in Block [
                -- forEach i (sections `listOf` section) [
                    -- Block [
                        -- forEach j (Var i $. Get nodes) $ oneLiner $
                            -- ValState $ Var j $. Set nodeParent (Var i)
                    -- ]
                -- ]
                for (varDecDef i int (litInt 0)) (Var i ?< (nodeTrans `listOf` nodeTransition $. ListSize)) ((&++)i) [
                    Block [
                        for (varDecDef j int (litInt 0)) (Var j ?< (nodeTrans `listOf` nodeTransition $. at i $. ListSize)) ((&++)j) $ oneLiner $
                            ValState $ Var sections $. at i $. Get nodes $. at j $. Set nodeParent (Var sections $. at i)
                    ]
                ]
            ]
        
    in addComments commentLabel (allSects ++ allSectTrans : outGoingLoop : [parentLoop])

outGoingStatements' :: Section -> [SectionTransition] -> [Statement]
outGoingStatements' (Section _ ns sectionTs _) storyTs =
    let newTs = map (\t -> NodeTrans (sectionTransLabel t) (sectionTransPreNode t) (sectionTransPostNode t) (sectionTransEvents t)) storyTs
        ts = sectionTs ++ newTs
        outTransName n = outTransPrefix ++ validNodeLabel n
        assignedElem n t = Var $ transPrefix ++ validNodeLabel n ++ toLabel ++ (validNodeLabel $ nodeTransPostNode t)
        assignedTrans n = map (assignedElem n) $ outTrans n ts
        nodeOutTrans = concatMap (\n -> [listDecValues (outTransName n) nodeTransition $ assignedTrans n]) ns
    in nodeOutTrans

outTrans :: Node -> [NodeTransition] -> [NodeTransition]
outTrans n = concatMap (\t -> if nodeTransPreNode t == n then [t] else [])

headStatements :: Node -> Body
headStatements n =
    let commentLabel = "Starting " ++ nodeName ++ " for the " ++ storyName
        headNode = oneLiner $
                       objDecDef headVar node $ Var $ nodePrefix ++ validNodeLabel n
    in addComments commentLabel headNode

sectionTransStatements :: [SectionTransition] -> Body
sectionTransStatements ts =
    let commentLabel = sectTransName ++ "s"
        preNs = map (\t -> validNodeLabel $ sectionTransPreNode t) ts
        postNs = map (\t -> validNodeLabel $ sectionTransPostNode t) ts
        preSs = map (\t -> validSectionLabel $ sectionTransSectionPreNode t) ts
        postSs = map (\t -> validSectionLabel $ sectionTransSectionPostNode t) ts
        names = zipWith (\n1 n2 -> n1 ++ toLabel ++ n2) preNs postNs
        transNames = map (\n -> sectTransPrefix ++ n) names
        eventNames = map (\n -> evtsPrefix ++ n) names
        transArgs s1 s2 eName = litObj sectionTransition [Var $ sectPrefix ++ s1, Var $ sectPrefix ++ s2, Var eName]
        sectTransDec = zipBlockWith4 (\tName s1 s2 eName ->
            objDecDef tName sectionTransition (transArgs s1 s2 eName)) transNames preSs postSs eventNames
        assignedTrans = map Var transNames
    in addComments commentLabel [sectTransDec, Block [listDecValues sectionTransVar sectionTransition assignedTrans]]

storyStatements :: Body
storyStatements =
    let commentLabel = storyName
        storyBody = oneLiner $
                        Self$->(Var managerStoryVar) &= litObj story params
        params = [Var $ label, Var $ sections, Var $ headVar, Var $ sectionTransVar]
    in addComments commentLabel storyBody
