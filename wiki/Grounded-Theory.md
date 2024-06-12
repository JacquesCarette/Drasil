# A reading list

There is a semi-official online definition of Grounded Theory (GT) at [groundedtheory.com](http://www.groundedtheory.com/what-is-gt.aspx). That site is quite horrible. [What is Grounded Theory?](http://www.groundedtheoryonline.com/what-is-grounded-theory/) at [Grounded Theory online](http://www.groundedtheoryonline.com) is nicer. There's a 'How to do it' tab on that site which is a nice intro.  The [Resources page](http://www.groundedtheoryonline.com/bibliography-and-references/) lists several books in the sidebar that seem to be ones we should acquire - though most seem to be available in the library at McMaster, so they should be checked out from there first!  Having said all of that, I must say that the [Wikipedia page](https://en.wikipedia.org/wiki/Grounded_theory) is rather good, and is probably the best place to start.

From an SE perspective, it is worth reading [Grounded Theory in Software Engineering](https://www.researchgate.net/publication/287491381_Grounded_Theory_in_Software_Engineering_Research_A_Critical_Review_and_Guidelines) (also in the private repo).  Note Table 3 in that paper, which gives us a nice list of journals/conferences that might welcome our work. But also carefully note the 'Discussion' section, and the warnings there (of which we 'fall prey' if we merely use it for legitimizing our approach).  While looking around, I found [Grounded Theory for Geeks](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=C3AE411B048E7229DE69A3ECD243EA92?doi=10.1.1.589.4145&rep=rep1&type=pdf) which I enjoyed.

I must admit that the part which is fuzziest to me is [Open Coding](https://en.wikipedia.org/wiki/Open_coding). It basically seems to come down to finding _commonalities_, and then coming up with reasonable words to label them. See [this example](https://prpost.wordpress.com/2013/07/22/an-example-of-how-to-perform-open-coding-axial-coding-and-selective-coding/) and [this slightly longer discussion](http://pages.cpsc.ucalgary.ca/~saul/wiki/uploads/CPSC681/open-coding.pdf).

## Sam's Found References

From the Wikipedia page, [this book](http://www.sxf.uevora.pt/wp-content/uploads/2013/03/Glaser_1967.pdf) by Glaser and Strauss seems like the original description and founding of Grounded Theory, and was suggested by Grounded Theory for Geeks, although it is long. Grounded Theory for Geeks also recommended [Theoretical Sensitivity](https://books.google.ca/books/about/Theoretical_Sensitivity.html?id=wuv4LQEACAAJ&redir_esc=y) by Glaser - I can't find an online PDF. 

I also found [A Critique of Using Grounded Theory as a Research Method](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/blob/git-svn/SciCompAndSoftEngPapers/Allan2003.pdf) (GitLab link to Allan2003) which contains good examples of what good codes should look like and the process of categorization and sorting.
Also, [The Handbook of Information Systems Research](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/blob/git-svn/SciCompAndSoftEngPapers/Whitman2003.pdf) (Whitman2003) isn't the prettiest PDF, but has a lot of good content. [Introduction to Grounded Theory](http://www.analytictech.com/mb870/introtogt.htm) by Steve Borgatti offers some useful clarification on some points.

# Summary of Grounded Theory

### Introduction

Grounded Theory (GT) takes data from observational study and creates a theory from it. The theory originates from the data itself and isn't based on preconceived notions. Data is collected until no new information can be obtained. Completing more case studies allows for a "double check" of the codes, concepts, and categories that are formed, and strengthens your theory. A single case study doesn't allow for generalizability, but a theory based on multiple case studies over a long time period is very generalizable. (Allan2003) Multiple GT studies must be conducted over a long enough time frame to create a generalized formal theory. (Whitman2003)

#### Stages (Whitman2003)

1. **Data Collection** - transcribing data obtained<br>
   Collecting data is usually done in the form of interviewing. The data should be qualitative, as this is what GT excels at pulling theories from.
2. **Open Coding** - classifying recurring themes and labeling them as "codes"<br>
   Coding is the most important part of GT. The repetition of ideas highlight an important concept; this can be used to filter out unimportant "noise". However, the process of coding can sound too ambiguous. A strategy for this is to describe the meaning in phrases, and find similarities. Also, realize that multiple codes can be found in the same text. It is also a good idea to identify which data the codes come from, which allows for traceability and prevents re-coding the same code on future passes of the data. (Allan2003)
3. **Axial Coding** - finding relationships between codes and grouping them into categories<br>
   Causal relationships are found to easier see how data is related. The elements given in Table 1 are focused upon to provide a frame.
4. **Theoretical Models** - writing up ideas about codes and their relationships<br>
   (Also called memo sorting.) Memos are written, in the form of notes, diagrams, sketches, etc., to describe exactly how the codes fit together. These memos are then sorted, by switching between them and the theory that is being developed to work in all the categories. (StolEtAl2016)
5. **Selective Coding** - refining categories and forming a core category<br>
   One category formed is chosen to be the main one around which all the other categories revolve. This allows one, unified, interconnected theory to develop that entails all of the research performed. (Steve Borgatti)
6. **Research Iteration** - constantly comparing new data with old codes and vice versa<br>
   GT is not a linear process; it is important to continually return to the data to find new codes and the codes to find new relationships. This greatly reduces missed information capture.

##### Table 1

|Element|Description|
|---|---|
|Phenomenon|The main concept in the grounded theory; the central focus|
|Casual Conditions|Events or variables that lead to the phenomenon|
|Context|Background conditions that lead to the phenomenon less directly than casual conditions|
|Action Strategies|Intentional actions taken with regard to the phenomenon|
|Consequences|All consequences of the action strategies, including intended ones|

ex. If studying effectiveness of medication, "the phenomenon of interest [could be] pain, the causal conditions ... arthritis, the action strategy ... taking drugs, and the consequence ... pain relief." (Steve Borgatti)

### "Issues" with Grounded Theory

GT at first glance seems too inductive with not enough deduction. However, there needs to be a balance of deduction and induction in science; after all, we learn from induction. Bias and subjectivity are eliminated by constantly comparing data. Another problem is knowing when to stop iterating the process. When new data collected yields very little new information (ie. codes or connections), it is probably best to stop. There is also the question of if analyzing the data should be flexible or structured. In the end, using set and agreed-upon procedures as a guideline is helpful, but you must not filter out data that you deem "unimportant" just because it says something you don't like. A happy medium is best. (Whitman2003)

The objectivist perspective is that truth is out there, while the subjectivist perspective is that truth must be interpreted by us collectively. Once again, a balance is needed; the research must be performed rigourously and be scientifically sound, but the very nature of the data collection (the main form of which is interviews) makes subjective analysis almost unavoidable. (Whitman2003)

The argument has been that qualitative research must be evaluated using trustworthiness, instead of validity (with respect to the positivist/objectivist view). The following summaries are given verbatim in Table 1 on pages 149-150 of Whitman2003, regarding the differences between the positivist view and the view to take when using GT:

1. Objectivity - findings are free from researcher bias vs.  
   Confirmability - conclusions depend on sujects and conditions of the study...
2. Reliability - the study findings can be replicated independently of context, time or researcher vs. 
   Dependability/Auditability - the study process is consistent and reasonably stable over time and between researchers
3. Internal validitiy - a statistically-significant relationship is established, to demonstrate that certain conditions are associated with other conditions... vs.  
   Internal consistency - the research findings are credible and consistent to the people we study and to our readers....
4. External Validity - the researcher establishes a domain in which findings are generalizable vs.  
   Transferability - how far can the findings/conclusions be transferred to other contexts and how do thy help to derive useful theories?

The process of coding has to be carefully monitored. Coding word-by-word and line-by-line takes a lot of time and creates a lot of data to sift through later. It can also cause confusion, as it can be unclear how small "divisions" should be. Looking for codes in phrases and ideas yields results that are much more fruitful. (Allan2003)

The role of a literature search is also ambiguous. A literature search should be completed sufficiently enough to have a literacy in the field of research, but not so thoroughly as to already develop hypotheses. Research that is too intensive exposes the researcher to worldviews and preconceived notions that can affect how they view the data. It is best to let the data speak for itself, which is really the goal of GT, so a cursory look through key terms and definitions is probably sufficient. (Allan2003)

### History 

The GT method was developed by Barney Glaser and Anselm Strauss, while researching dying hospital patients. It was popularized due to criticism towards how deductive the most common research methods were. GT gained credibility because of how systematic it was in creating a theory, and has gained traction in not only health, but other areas of research as well, like manufacturing and education. (Grounded Theory, Wikipedia)

## Reconceptualization

To address Dr. Carette's concern of reconceptualization, I found that pages 151-152 of Whitman2003 talk about how a hierarchical coding scheme should be avoided to counter this very issue. When codes are built from the bottom up, it is easier to see how they relate later on, so codes should be modified as much as necessary in the early stage. However, an aspect of Grounded Theory is iteration, so new codes are applied to old data and vice versa. This way, if you see a code that you made previously isn't really applicable to new data, you can eliminate it. A GT is a process based on the data itself; it is a fluid process and changes with the data available.


# Discussion

What I am puzzled about is that I found no discussion of _reconceptualization_ in the, admittedly cursory, look I did through the grounded theory literature. What if your search through the data was biased (for whatever reason), causing you to do a sub-optimal classification of the data and patterns?  You might have not coded certain aspects of the data which turn out to be crucial. Or you might prematurely find patterns (codes for codes) that fit, but skew things in a bad direction.  How do you recover from that?  I think that a Kolmogorov Complexity influenced approach might really help here.