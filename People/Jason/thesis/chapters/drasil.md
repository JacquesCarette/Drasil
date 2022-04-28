# Drasil

Originally known as Literate Scientific Software (LSS), Drasil is an exploration
of this ideology. Drasil's largest domain of knowledge covered originates from
LSS: scientific computing software (SCS). Dr. Jacques Carette and Dr. Spencer
Smith are the principal investigators of Drasil. Embedded in Haskell, Drasil is
currently capable of creating simple HTML websites (such as Drasil's _homepage_)
and scientific software that follows the Scientific Requirements Specifications
(SRS) template as laid out by _Smith and Lai_.

Current case studies include:
* Single Pendulum
* Double Pendulum
* GamePhysics
* HGHC
* PDController
* SWHS
* NoPCM
* SSP

Drasil works by organizing information into _Chunks_ (knowledge fragments) and
collecting them into it's _ChunkDB_. There are various kinds of chunks that
exist in Drasil, for different purposes. The user is tasked with collecting
sufficient knowledge in their ChunkDB instance and then running the transformer
of choice (such as the SRS-based transformer). Each transformer has its own set
of requirements of the knowledge-base that they enforce on their own. For
example, the SRS and code generator requires that the knowledge-base contains
enough information to create a _calculation scheme_ so that the "ins" and "outs"
of the program can be determined and coherently connected together.

Embedded in Haskell, Drasil is a framework for encoding knowledge (currently
known as _chunks_), manipulating knowledge, and creating usable transformers
that convert registered knowledge into usable software artifacts. 

## Questions-guide

1. What is Drasil?
2. Where can I learn more about Drasil?
3. At a design level, how does Drasil work?
4. What can Drasil currently do?
5. What challenges does Drasil currently face?
