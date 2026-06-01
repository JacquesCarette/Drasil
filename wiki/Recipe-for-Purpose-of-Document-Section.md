# Overview of Proposal

The design for "Purpose of Document" section could be something like:

This Software Requirements Specification (SRS) document specifies the requirements for software to ...
The stakeholders include ... OR The intended audience includes ...

The purpose of an SRS is ...

To achieve its purpose, this SRS should have the qualities of being ...

The ellipses (...) would be replaced in each instance with the project specific details.  In some cases the ... would really be about the purpose and qualities of all SRS documents.  We could be flexible though and let the author choose those purposes and qualities that are most important to their project.

# Reasoning

From the IEEE (1998) standard, the Purpose subsection should
1. Delineate the purpose of the SRS
2. Specify the intended audience for the SRS

From a generic point of view the purpose of all SRSs is to provide a high quality specification of the requirements to the stakeholders.  In the proposal above, I've provided the option of providing additional details on the value of an SRS and which documentation qualities are important.  The proposal for this section suggests that the stakeholders are the intended audience.  I think we could give the author of the SRS the choice of whether to say stakeholders or intended audience.  For a project like GlassBR, where there is a clearly identified client, stakeholder may make sense.  For generic specifications, like swhs, intended audience may be better.  Of course, for some problems there may be stakeholders that would never read the document, like the public that will be protected by the nuclear safety analysis.

# Example

The GlassBR program provides a good example to start with.  The current version of the purpose section covers most of the information above, but in a meandering way.  It does not identify the stakeholders, leaving that information to a later section.  However, the later section (Section 3 Stakeholders) is not in any of the other SRS examples and it really doesn't provide much more information than could be summarized in a sentence in the Purpose section.  (I think we should delete the Stakeholders section from the GlassBR example.)

For reference, the original Purpose section for GlassBR is:

The main purpose of this document is to predict whether a given glass slab is likely to resist a specified blast. The goals and theoretical models used in the GlassBR code are provided, with an emphasis on explicitly identifying assumptions and unambiguous definitions. This document is intended to be used as a reference to provide all information necessary to understand and verify the analysis. The SRS is abstract because the contents say what problem is being solved, but not how to solve it.

This document will be used as a starting point for subsequent development phases, including writing the design specification and the software verification and validation plan. The design document will show how the requirements are to be realized, including decisions on the numerical algorithms and programming environment. The verification and validation plan will show the steps that will be used to increase confidence in the software documentation and the implementation. Although the SRS fits in a series of documents that follow the so-called waterfall model, the actual development process is not constrained in any way. Even when the waterfall model is not followed, as Parnas and Clements point out, the most logical way to present the documentation is still to "fake" a rational design process.

Following this proposal, the revised Purpose section would be:

This Software Requirements Specification (SRS) document specifies the requirements for software to predict whether a given glass slab is likely to resist a specified blast.  The stakeholders include Dr. Manuel Campidelli, Postdoctoral researcher, Civil Engineering Department, McMaster University; and Entuitive, the company that contracted his expertise.  Other stakeholders include Dr. Campidelli's supervisors: Dr. Michael Tait, Civil Engineering Department, McMaster University and Dr. Wael El-Dakhakhni, Civil Engineering Department, McMaster University. [This could be changed to just list the stakeholders.]

The purpose of an SRS is to provide an official statement of the system requirements for the stakeholders, end-users and software developers.  The SRS serves as a starting point for the software design, a basis for estimating costs and schedules, and a standard against which compliance can be measured during software verification and validation. (IEEE 1998, Sommerville and Sawyer (1997)).

To achieve its purpose, this SRS should have the qualities of being complete, consistent, modifiable, traceable, unambiguous, correct, verifiable and abstract.  These qualities, along with other references on desirable qualities for scientific software can be found in Smith and Koothoor (2016).

# Concluding Remarks

I think this example highlights some knowledge that should be included in Drasil.  The knowledge includes:

- The stakeholders and their biographic information.  To the original example, I added information on Dr. Campidelli's title and affiliation.  It seems like the biographic information on the stakeholders would be similar to biographic information for document authors.  The stakeholder information seems to also add some information relating the different stakeholders.  For instance, Dr. Tait is one of Dr. Campidelli's supervisors.
- In the other examples, our stakeholders (intended audience) might be 
- The purpose of the SRS.  It seems that every document should know its purpose, and probably every section should know its purpose too.
- We could go deeper and define every term, like requirement, quality, unambiguous etc.  For each definition would could include the citation that provided this definition.  This might be going too far, at least for the time being.  In the example above, I just pointed to a reference that had the relevant information.

The current SRS documents include information to "teach" about requirements and software development, such as the reference to Parnas's "fake it" paper.  This seems like valuable information to some audiences.  However, it can interrupt the flow of the presentations, especially for those already familiar with it.  In the revised version, I've tried to just point to external citations instead.  

# Notes

- for some purposes the emphasis of the existing template on solutions doesn't fit.  Some scientists (like Isobel and Alex P from Physics) are more interested in exploration and analysis.  Characterizing the information more in terms of output, rather than solution, might help

Steps

- need to determine what notation is used to represent scientific knowledge in the SRS (analogy with 5 <= x <= 10)
- what this notation means (analogy with P(x) = 5 <= x AND x <= 10, P: R -> B)
- what it REALLY means (analogy with {x: R | x \in [5, 10]})
- how we want to represent the scientific knowledge (analogy with [x: Real, Interval(Closed(5), Closed(10))])