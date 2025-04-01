## Specific Examples

Representative of the Population:

`drasil-metadata/lib/Data/Drasil/Domains.hs`:
```haskell
13:compScience  = mkIdea  "compScience"    (cn' "Computer Science")      (Just "CS")
```

In the above, a term is defined along with an abbreviation.

## Constructor Use

<details>

<summary>`rg " (nc|mkIdea|ncUID|mkIdeaUID) " -ths`</summary>

```console
drasil-data/lib/Data/Drasil/Concepts/Education.hs
23:calculus      = nc "calculus"       (cn   "calculus"     )
24:civil         = nc "civil"          (cn'  "civil"        )--FIXME: Adjective
25:degree_       = nc "degree"         (cn'  "degree"       )
26:engineering   = nc "engineering"    (cn'  "engineering"  )
27:mechanics     = nc "mechanics"      (cn   "mechanics"    )
28:structural    = nc "structural"     (cn'  "structural"   )--FIXME: Adjective
29:undergraduate = nc "undergraduate"  (cn'  "undergraduate")--FIXME: Functions as adjective
30:highSchool    = nc "highSchool"     (cn'  "high school"  )--FIXME: Functions as adjective
31:chemistry     = nc "chemistry"      (cn'  "chemistry"    )
32:physical_     = nc "physical"       (cn'  "physical"     )--FIXME: Adjective

drasil-data/lib/Data/Drasil/Concepts/Documentation.hs
110:abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       )
111:acronym         = nc "acronym"        (cn'    "acronym"            )
112:analysis        = nc "analysis"       (cnIS   "analysis"           )
113:appendix        = nc "appendix"       (cnICES "appendix"           )
114:aspect          = nc "aspect"         (cn'    "aspect"             )
115:body            = nc "body"           (cnIES  "body"               )
116:characteristic  = nc "characteristic" (cn'    "characteristic"     )
117:class_          = nc "class"          (cn'''  "class"              )
118:client          = nc "client"         (cn'    "client"             )
119:code            = nc "code"           (cn     "code"               )
120:column          = nc "column"         (cn'    "column"             ) --general enough to be in Documentation?
121:company         = nc "company"        (cnIES  "company"            )
122:component       = nc "component"      (cn'    "component"          )
123:concept         = nc "concept"        (cn'    "concept"            )
124:condition       = nc "condition"      (cn'    "condition"          )
125:connection      = nc "connection"     (cn'    "connection"         )
126:constant        = nc "constant"       (cn'    "constant"           )
127:constraint      = nc "constraint"     (cn'    "constraint"         )
128:consumer        = nc "consumer"       (cn'    "consumer"           )
129:content         = nc "content"        (cn'    "content"            )
130:context         = nc "context"        (cn'    "context"            )
131:coordinate      = nc "coordinate"     (cn'    "coordinate"         )
132:customer        = nc "customer"       (cn'    "customer"           )
133:datum           = nc "datum"          (cnUM   "datum"              )
134:decision        = nc "decision"       (cn'    "decision"           )
135:definition      = nc "definition"     (cn'    "definition"         )
136:dependency      = nc "dependency"     (cnIES  "dependency"         )
137:description     = nc "description"    (cn'    "description"        )
138:design          = nc "design"         (cn'    "design"             )
139:document        = nc "document"       (cn'    "document"           )
140:documentation   = nc "documentation"  (cn'    "documentation"      )
141:effect          = nc "effect"         (cn'    "effect"             )
142:element         = nc "element"        (cn'    "element"            )
143:emphasis        = nc "emphasis"       (cnIS   "emphasis"           )
144:endUser         = nc "end user"       (cn'    "end user"           )
145:environment     = nc "environment"    (cn'    "environment"        ) -- Is this term in the right spot?
146:example         = nc "example"        (cn'    "example"            )
147:failure         = nc "failure"        (cn'    "failure"            )
148:figure          = nc "figure"         (cn'    "figure"             )
149:first           = nc "first"          (cn'    "first"              ) --Does it make sense for this to be here?
150:form            = nc "form"           (cn'    "form"               ) 
151:full            = nc "full"           (cn'    "full"               ) --FIXME: Adjective
152:functional      = nc "functional"     (cn'    "functional"         ) --FIXME: Adjective
153:game            = nc "game"           (cn'    "game"               )
154:general         = nc "general"        (cn'    "general"            ) --FIXME: Adjective
155:goal            = nc "goal"           (cn'    "goal"               )
156:guide           = nc "guide"          (cn'    "guide"              )
157:implementation  = nc "implementation" (cn'    "implementation"     )
158:individual      = nc "individual"     (cn'    "individual"         )
159:information     = nc "information"    (cn     "information"        )
160:interest        = nc "interest"       (cn'    "interest"           )
161:interface       = nc "interface"      (cn'    "interface"          )
162:input_          = nc "input"          (cn'    "input"              )
163:instance_       = nc "instance"       (cn'    "instance"           )
164:intReader       = nc "intReader"      (cn'    "intended reader"    )
165:introduction    = nc "introduction"   (cn'    "introduction"       )
166:issue           = nc "issue"          (cn'    "issue"              )
167:item            = nc "item"           (cn'    "item"               )
168:label           = nc "label"          (cn'    "label"              )
169:library         = nc "library"        (cnIES  "library"            )
170:limitation      = nc "limitation"     (cn'    "limitation"         )
171:literacy        = nc "literacy"       (cnIES  "literacy"           )
172:loss            = nc "loss"           (cn'''  "loss"               )
173:material_       = nc "material"       (cn'    "material"           )
174:mainIdea        = nc "mainIdea"       (cn'    "main idea"          )
175:message         = nc "message"        (cn'    "message"            )
176:method_         = nc "method"         (cn'    "method"             )
177:module_         = nc "module"         (cn'    "module"             )
178:model           = nc "model"          (cn'    "model"              )
179:name_           = nc "name"           (cn'    "name"               )
180:nonfunctional   = nc "non-functional" (cn'    "non-functional"     ) --FIXME: Adjective
181:object          = nc "object"         (cn'    "object"             )
182:offShelf        = nc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      )
183:open            = nc "open"           (cn'    "open"               )
184:organization    = nc "organization"   (cn'    "organization"       )
185:output_         = nc "output"         (cn'    "output"             )
186:physics         = nc "physics"        (cn'    "physics"            )
187:physical        = nc "physical"       (cn'    "physical"           ) --FIXME: Adjective
188:plan            = nc "plan"           (cn'    "plan"               )
189:practice        = nc "practice"       (cn'    "practice"           )
190:priority        = nc "priority"       (cnIES  "priority"           )
191:problem         = nc "problem"        (cn'    "problem"            )
192:procedure       = nc "procedure"      (cn'    "procedure"          )
193:product_        = nc "product"        (cn'    "product"            )
194:project         = nc "project"        (cn'    "project"            )
195:property        = nc "property"       (cnIES  "property"           )
196:purpose         = nc "purpose"        (cn'    "purpose"            )
197:quantity        = nc "quantity"       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
198:realtime        = nc "real-time"      (cn'    "real-time"          )
199:review          = nc "review"         (cn'    "review"             )
200:reference       = nc "reference"      (cn'    "reference"          )
201:requirement_    = nc "requirement"    (cn'    "requirement"        ) --FIXME: Eventually only have one requirement
202:response        = nc "response"       (cn'    "response"           )
203:result          = nc "result"         (cn'    "result"             )
204:reviewer        = nc "reviewer"       (cn'    "reviewer"           )
205:safety          = nc "safety"         (cnIES  "safety"             )
206:scope           = nc "scope"          (cn'    "scope"              )
207:second_         = nc "second"         (cn'    "second"             ) --Does it make sense for this to be here?
208:section_        = nc "section"        (cn'    "section"            )
209:scenario        = nc "scenario"       (cn'    "scenario"           )
210:source          = nc "source"         (cn'    "source"             )
211:simulation      = nc "simulation"     (cn'    "simulation"         )
212:solution        = nc "solution"       (cn'    "solution"           )
213:software        = nc "software"       (cn     "software"           )
214:summary         = nc "summary"        (cnIES  "summary"            )
215:specific        = nc "specific"       (cn'    "specific"           ) --FIXME: Adjective
216:specification   = nc "specification"  (cn'    "specification"      )
217:stakeholder     = nc "stakeholder"    (cn'    "stakeholder"        )
218:standard        = nc "standard"       (cn'    "standard"           )
219:statement       = nc "statement"      (cn'    "statement"          )
220:symbol_         = nc "symbol"         (cn'    "symbol"             )
221:system          = nc "system"         (cn'    "system"             )
222:table_          = nc "table"          (cn'    "table"              )
223:task            = nc "task"           (cn'    "task"               )
224:template        = nc "template"       (cn'    "template"           )
225:term_           = nc "term"           (cn'    "term"               )
226:terminology     = nc "terminology"    (cnIES  "terminology"        )
227:theory          = nc "theory"         (cnIES  "theory"             )
228:traceyGraph     = nc "traceyGraph"    (cn'    "traceability graph" )
229:traceyMatrix    = nc "traceyMatrix"   (cnICES "traceability matrix")
230:type_           = nc "type"           (cn'    "type"               )
231:uncertainty     = nc "uncertainty"    (cnIES  "uncertainty"        )
232:user            = nc "user"           (cn'    "user"               )
233:useCase         = nc "useCase"        (cn'    "use case"           )
234:validation      = nc "validation"     (cn'    "validation"         )
235:value           = nc "value"          (cn'    "value"              )
236:variable        = nc "variable"       (cn'    "variable"           )
237:verification    = nc "verification"   (cn'    "verification"       )
238:video           = nc "video"          (cn'    "video"              )
239:year            = nc "year"           (cn'    "year"               )
240:scpOfTheProjS   = nc "scpOfTheProj"   (cn'    "scope of the project") -- temporary generated for test
247:abbAcc              = nc "TAbbAcc"            (abbreviation `and_PP` acronym)
248:caseProb            = nc "caseProb"           (cn' "case problem")
249:consVals            = nc "consVals"           (cn "values of auxiliary constants")
250:corSol              = nc "corSol"             (cn' "correct solution")
251:charOfIR            = nc "charOfIR"           (characteristic `of_PS` intReader)
252:methAndAnls         = nc "methAndAnls"        (method_ `and_` analysis)
253:orgOfDoc            = nc "orgOfDoc"           (organization `of_` document)
254:procForAnls         = nc "procForAnls"        (procedure `for` analysis)
255:propOfCorSol        = nc "propOfCorSol"       (property `ofAPS` corSol)
256:prpsOfDoc           = nc "prpsOfDoc"          (purpose `of_` document)
257:refMat              = nc "refMat"             (cn' "reference material")
258:reqInput            = nc "ReqInputs"          (cn' "required input")
259:scpOfReq            = nc "scpOfReq"           (scope `of_` requirement)
260:tAuxConsts          = nc "TAuxConsts"         (cn' "auxiliary constant")
261:termAndDef          = nc "termAndDef"         (terminology `and_` definition)
262:tOfCont             = nc "tOfCont"            (table_ `of_` content)
263:tOfSymb             = nc "tOfSymb"            (table_ `of_` symbol_)
264:tOfUnit             = nc "tOfUnit"            (table_ `of_` unit_)
265:inDatumConstraint   = nc "InDataConstraints"  (cn' "input data constraint") -- should be moved below
266:outDatumConstraint  = nc "OutDataConstraints" (cn' "output data constraint")
267:traceyMandG         = nc "traceyMandG"        (and_TGen titleize' titleize' traceyMatrix graph)
268:vav                 = nc "vav"                (verification `and_` validation)
271:scpOfTheProj oper = nc "scpOfTheProj" (scope `of_NINP` theGen oper project) -- reasonable hack?

drasil-data/lib/Data/Drasil/Concepts/Computation.hs
32:application = nc   "application"      (cn' "application") 
33:computer    = nc   "computer"         (cn' "computer") 
34:structure   = nc   "structure"        (cn' "structure")         

drasil-data/lib/Data/Drasil/Software/Products.hs
20:sciCompS   = nc "sciCompS"       (cn' "scientific computing software")

drasil-example/ssp/lib/Drasil/SSP/Defs.hs
37:intrslce = nc "interslice" (cn' "interslice")
38:layer    = nc "layer"      (cn' "layer")
39:material = nc "material"   (cn' "material")
40:slice    = nc "slice"      (cn' "slice")
41:slip     = nc "slip"       (cn  "slip") --FIXME: verb (escape or get loose from (a means of restraint))/noun 
44:slope    = nc "slope"      (cn' "slope")
45:soil     = nc "soil"       (cn  "soil")
46:stability = nc "stability" (cn "stability")
48:morPrice = nc "morPrice"   (pn  "Morgenstern-Price")
103:factor = nc "factor" (cn' "factor") -- possible use this everywhere

drasil-example/swhs/lib/Drasil/SWHS/Concepts.hs
36:full = nc "full" (progName `with` phsChgMtrl)

drasil-lang/lib/Language/Drasil/Chunk/Eq.hs
97:  QD (dqd' (cc' (nw $ ncUID nm desc) def) symb sp (Just $ unitWrapper un)) []
102:  QD (dqd' (cc' (nw $ ncUID nm desc) def) symb sp Nothing) []

drasil-lang/lib/Language/Drasil/Chunk/Quantity.hs
95:implVarUID i des sp sym = QD (nw $ ncUID i des) sp f Nothing
111:vc i des sym space = QD (nw $ nc i des) space (const sym) Nothing
115:vcUnit i des sym space u = QD (nw $ nc i des) space (const sym) (Just u)
119:vcSt i des sym space = QD (nw $ nc i des) space sym Nothing

drasil-example/hghc/lib/Drasil/HGHC/HeatTransfer.hs
72:nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
73:fp = nc "fp" (cn "FP")

drasil-example/projectile/lib/Drasil/Projectile/Concepts.hs
18:durationNC   = nc "duration" (nounPhraseSP "duration")
19:launchNC     = nc "launch"   (nounPhraseSP "launch")
20:offsetNC     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phraseNP (targetPosNC `andThe` landingPosNC))

drasil-example/swhsnopcm/lib/Drasil/SWHSNoPCM/Definitions.hs
7:htTrans = nc "heat transfer" (cn "heat transfer") --Not really a nounphase,

drasil-metadata/lib/Data/Drasil/Domains.hs
13:compScience  = mkIdea  "compScience"    (cn' "Computer Science")      (Just "CS")
15:softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")
17:mathematics  = mkIdea  "mathematics"    (cn' "Mathematics")           Nothing
19:progLanguage = mkIdea  "progLanguage"   (cn' "Programming Language")  Nothing
21:physics      = mkIdea  "physics"        (cn' "Physics")               Nothing
23:civilEng     = mkIdea  "civilEng"       (cn' "Civil Engineering")     Nothing
25:materialEng  = mkIdea  "materialEng"    (cn' "Material Engineering")  Nothing
27:documentc    = mkIdea  "documentc"      (cn' "Document")              (Just "Doc")
29:knowledgemng = mkIdea  "knowledgemng"   (cn' "Knowledge Management")  Nothing

drasil-example/glassbr/lib/Drasil/GlassBR/Concepts.hs
13:idglass      = mkIdea  "glass"          (cn' "Glass")                 Nothing
55:beam         = nc "beam"       (cn' "beam")
56:blastRisk    = nc "blastRisk"  (nounPhraseSP "blast risk")
57:cantilever   = nc "cantilever" (nounPhraseSP "cantilever")
58:edge         = nc "edge"       (cn'          "edge")
59:glass        = nc "glass"      (nounPhraseSP "glass")
60:glaSlab      = nc "glaSlab"    (cn' "glass slab")
61:plane        = nc "plane"      (cn' "plane")
63:ptOfExplsn   = nc "ptOfExplsn" (cn' "point of explosion")

drasil-example/dblpend/lib/Drasil/DblPend/Concepts.hs
18:rod = nc "rod" (cn' "rod")
19:horizontal = nc "horizontal" (cn "horizontal") 
20:vertical = nc "vertical" (cn "vertical") 
```

</details>
