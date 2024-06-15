This page will document the results of following issue [#2776](https://github.com/JacquesCarette/Drasil/issues/2776).

When discussing Drasil, we make claims that it is easier and more productive to use compared to lower level programs such as LaTeX or even Word. But proving such a claim is another matter. Results are not as easily measured as something like counting lines of code or time spent typing, as these types of measurements require some context to be meaningful. We cannot simply say that if we write 100 lines of code and generate 10,000 lines, our generative solution is better than someone who wrote 1000 lines and only received 2000 lines. If that were the case, one could simply write a small for-loop that outputs the numbers from 1 to 1,000,000 and claim that to be the ultimate solution. We cannot just rely on lines of code generated or hours sitting at a computer, because we also care about the quality of generated artifacts. Therefore the goal of our tests should not be generating as many lines of code and text as possible, but rather creating a method that can measure our progress of successfully encoding information compared to similar programs while monitoring the user experience. Unfortunately, finding or creating a tool to measure this is not easy. This page will be used to keep track of the progress of quantifying meaningful productivity and ease of use. From there, we should try and determine a method to quantify the use of Drasil. We will begin by examining various aspects of Drasil on a use-case basis, recording feedback on all parts of the process.

# Examples
By looking at some examples of using and working with Drasil to complete various tasks, we might be able to get a better idea of exactly what we want to quantify.

## Creating the Drasil Website
This subsection will detail the process of creating the new Drasil website (as of August 2021) in the style of a journal. Part of the goal of Drasil is to be able to gently introduce new collaborators and developers into the language and processes of Drasil. To gather written data regarding the productivity and ease of using Drasil, this section will record more of the user-experience related aspect of creating a new type of document within Drasil rather than statistics and facts.

### Why Change the Website?
Before the summer of 2021, the Drasil website was made using the [Hakyll](https://jaspervdj.be/hakyll/) library. The original Hakyll-based program took in a barebones HTML template file that would be filled in with information from the program to create the website. The template file would include basic HTML code that could handle simple loops and if-statements to generate the desired HTML file. The script that used Hakyll would pass values (often as `Strings`) to the template, activating if-statements and loops accordingly and using those `Strings` to display program names, links, etc. 

#### Problems Working with Hakyll

Hakyll worked reasonably well for the job we needed it to do. We could link to other webpages, write text cleanly (without much boilerplate), pass values to the template file to activate the desired instructions, and display images from a given file link. However, there were a couple small problems that kept cropping up through our use with it. We realized that under the Example section of the website, the links for the documentation of generated Swift code were broken. Swift is not currently supported by Doxygen, so we could not create the documentation files necessary to get the broken link back in working order.

We then decided that the only way to fix the broken link was to remove it completely from the website. This would require changes in both the Hakyll code and the template file. Unfortunately, neither the setup of the Hakyll scripts nor the HTML template file were flexible enough to create a clean resolution to the issue. We had a few close solutions, but they were much too hacky, painful to work with, and fragile against future changes in the setup of generated code.

### Solution Attempts
Before we continue with the steps taken to generate the new website, I first wanted to detail the different solution attempts and their design. Specifically outlining the problem, we wanted to be able to remove broken links from the Drasil website.

The first solution attempt was modifying the HTML template file. If there was some way that we could check the value of a variable, we could use an if-statement to block any occurrences of `Swift` under the documentation section. However, the HTML [template syntax from Hakyll](https://jaspervdj.be/hakyll/tutorials/04-compilers.html#conditionals) did not support complex if-statements. For example, the `==` and `!=` operators were not available, so there was no way to compare a variable to something else. The language only allows for a single variable to be passed: if it contains information, the if-statement follows through, if not, then it is ignored. Thus, we could not use the idea of comparing variables to change the HTML variable on the fly. Even if this solution worked out, the template file would still be extremely fragile to any future changes in displaying code generation.

Our second attempt at a solution involved finding a way to send an empty variable to the HTML template whenever the term `Swift` appeared. This design had its fair share of troubles, as the ordering and organization of the Hakyll script were a little too static to be changed in such a manner. The variable itself could not be emptied from the way links were coded into the script. Any possible fixes either broke the website or were too hacky to allow through.

After a weeks worth of effort, we decided to look for another solution.

#### A Different Approach
We decided to think about making the website in a more flexible and versatile manner. Drasil has its own HTML generators, so we wanted to try making the website as if it were an SRS. We began to transfer the website from the Hakyll library to Drasil's own language. By imitating the process of creating an SRS using Drasil, we could make a document that acted like the regular website but internally functioned like a Drasil example.

First, we hardcoded all the information from the previous version of the website to new Drasil-based files, including all descriptions, tables, paragraphs, links, etc. At this point, we could only generate the website from low-level functions and variables that contained the paragraphs as `Strings`. This setup was very fragile, but left lots of room for improvement as we now had control over which links and terms we could include. See PR [#2524](https://github.com/JacquesCarette/Drasil/issues/2524) for more details.

Next, we had to change all of the scripts to work with the new implementation of the website, including copying all of the required data files over to the new `drasil-website` folder. At this point, we had changed about 300 lines of code and generated about 500 in return. From this point onwards, we only needed to worry about improving the website.

#### Improving the Website
The first step to cleaning up the website files was to separate all of the sections into different files and then gathering the important functions into one larger `Body.hs` file, following the same setup as the other examples. We then removed the hardcoded links so that the `Makefile` would be able to pass in the correct location. This allowed the website to function both locally and in deploys from the drasil-bot. Along the way, I also learned how to setup my own bot account to perform deploys on my forked repo, which was a nice bonus. Since we were now able to control the output of the website as if it were a Drasil document, we could do some of the following:
- Access all of the datatypes within Drasil. This included the `Sentence` and `Section` types made in `drasil-lang`, along with valuable information types like `ChunkDB` and `SystemInformation`. We could now form tables, images, and links using the Drasil language. ([#2726](https://github.com/JacquesCarette/Drasil/pull/2726), [#2742](https://github.com/JacquesCarette/Drasil/pull/2742))
- Create recipes to generate each of the website's sections, making the website much more flexible. Organizing and moving around the various sections was much simpler as well. Switching the order of the Case Studies and Examples section was as easy as switching their order in the list that held all of the website sections. ([#2726](https://github.com/JacquesCarette/Drasil/pull/2726), [#2742](https://github.com/JacquesCarette/Drasil/pull/2742))
- As another nice result, we could automate both the list of examples and the table of design choices for generated code, allowing the website to become much more flexible and open to new and different examples. Now, adding to the list of examples and the table of choices is as simple as putting the project's `SystemInformation` and `Choices` into a list and sending it through the existing recipes. The recipes will always work, no matter how many examples are put into Drasil. ([#2742](https://github.com/JacquesCarette/Drasil/pull/2742))
- Add Haddock Documentation as a part of the Drasil packages and modules. ([#2744](https://github.com/JacquesCarette/Drasil/pull/2744))
- Hook in Drasil analysis information. More specifically, included all of the generated artifacts from running `make analysis`. ([#2744](https://github.com/JacquesCarette/Drasil/pull/2744))
- Remove many manual references and let the existing Drasil recipes complete the database to reduce user overhead. ([#2746](https://github.com/JacquesCarette/Drasil/pull/2746))
- Easily change folder names for better organization of the repository without breaking the website. ([#2747](https://github.com/JacquesCarette/Drasil/pull/2747))
- Add more detail to the descriptions of the various website sections. This was done as the other changes were being completed, since changing the description functions does not affect the rest of the website's functionality. ([#2744](https://github.com/JacquesCarette/Drasil/pull/2744), [#2746](https://github.com/JacquesCarette/Drasil/pull/2746))

Excluding lines that were generated or counted by changing file names, there was about 1000 lines of code written, and about 860 lines generated. The written lines include [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-website-0.1.0.0/Drasil-Website-Body.html) and comments alongside all written functions. However, I found that creating a new type of document in Drasil was much easier than continuing to use the old website's scripts. A lot of the information on the website is still hardcoded, but that will eventually become more dynamic as Drasil gains more knowledge. Setting up the website for future changes was one of the original objectives of this sub-project, and I think making modifications through the Drasil language will be much easier from here on, as demonstrated in the above section.


## Creating the Chunk Log Generator

The [Chunk](Chunks) Log generator is a method of debugging some common problems found in the examples. Specifically, it aims to aid in solving problems regarding traceability between chunks, conflicting data between chunks with the same [UID](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UID), and checking that all chunks are actually used (see [#2759](https://github.com/JacquesCarette/Drasil/issues/2759) and [#1541](https://github.com/JacquesCarette/Drasil/issues/1541) for more details). Creating a large list of all the available chunks in a given project is very unwieldly and for too time-consuming for a human to do by hand. But because of the generative and [information-gathering](Information-Encoding) nature of Drasil, this idea starts to become plausible. It is only a matter of finding the right information, collecting it, and then giving the program some instructions to output the required information. Thankfully, Drasil's tendency to favour the use of [recipes](Recipes) also contributes to this goal by completing the first two steps for us. The real work involved in implementing this solution comes from writing a printer that can take the gathered information and output it in an easy-to-read format.

The procedure for creating such a printer was as follows:
1. Gather all the information within the examples package. If the example is already generating SRS files, then this step should be done. Specifically, the information is usually gathered in the [`SystemInformation`](https://jacquescarette.github.io/Drasil/docs/full/drasil-database-0.1.1.0/Database-Drasil.html#t:SystemInformation) type (found in each example's `Body.hs` file).
2. Extract the chunk database from that information. We usually want the database under `_sysinfodb`, as opposed to `_usedinfodb` since it is currently used only for the table of symbols in the SRS documents.
3. Extract the necessary fields from that database (of type [`ChunkDB`](https://jacquescarette.github.io/Drasil/docs/full/drasil-database-0.1.1.0/Database-Drasil.html#t:ChunkDB)) using [lenses](Lenses). These are often in the form of [`Maps`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#t:Map) and can be accessed through the [`assocs`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#v:assocs) function. For example, extracting all UIDs of concept-related chunks (chunks with a definition) could be done with the following:
    ```Haskell
    getConceptUIDs :: ChunkDB -> [UID]
    getConceptUIDs db = map fst $ Map.assocs $ defTable db
    ```
    Getting the chunks themselves requires a small augmentation:
    ```Haskell
    getConceptUIDs :: ChunkDB -> [ConceptChunk]
    getConceptUIDs db = map (fst.snd) $ Map.assocs $ defTable db
    ```
4. Decide what information is useful from the chunk types, and extract it from the list of that chunk type given from the `Maps` found in step 3.
5. Now that you have all your information organized, we can start on the actual printer. For this, I used the [HughesPJ PrettyPrint](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html) package to match the style of the other Drasil printers.
6. Here, decide on what values or pieces of information you want your printer functions to take. They should match up with the same values that you decided upon in step 4. Then create a function that converts those pieces of information into what you want. For this specific example, I created a table that outputs the chunk [UID](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UID), [term](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:term), and anything else that was worth printing (like definitions, titles, shortnames, reference addresses, etc.):
    ```Haskell
    -- | General function to make the debugging tables. Takes in printing information, a function
    -- that extracts a certain field from the printing information, a title, three column headers,
    -- and three functions that sort the data from the printing information field into the 
    -- required display formats (often 'UID's, terms, shortnames, definitions, etc.).
    mkTableFromLenses :: PrintingInformation -> (ChunkDB -> UMap a)
      -> String -> String -> String -> String -> (a -> Doc) -> (a -> Doc) -> (a -> Doc) -> Doc
    mkTableFromLenses PI{_ckdb = db} tableLens ttle h1 h2 h3 l1 l2 l3 =
      text ttle <> colon
      $$ header (text h1 $$ nest nestNum (text h2) $$ nest (nestNum*3) (text h3))
      $$ vcat (map chunkLayout chunks)
      where
        chunkLayout x = l1 x $$ nest nestNum (l2 x)
          $$ nest (nestNum*3) (l3 x)
        chunks = map (fst.snd) (Map.assocs $ tableLens db)
        nestNum = 30
    ```
7. Then map the above functions onto the fields you organized from step for. The table of concepts, for example, would be created by calling the above function in this manner:
    ```Haskell
    -- | Makes a table with all concepts in the SRS.
    mkTableConcepts :: PrintingInformation -> Doc
    mkTableConcepts pinfo = mkTableFromLenses pinfo defTable
      "Concepts" "UID" "Term" "Definition"
        (text . view uid)
          (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
            (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)
    ```
8. Once everything is formatted nicely in the [`Doc`](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html#t:Doc) type, just [`render`](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html#v:render) it in the `drasil-gen` package.

This seems to be quite a complicated and in-depth process, but it was only a matter of learning what all the pretty print functions could do, then creating functions to use them. Compared to searching an SRS file for every piece of little information and documenting it in a log file, I would choose the above method every time. In total, I only ended up writing about 270 lines of code and received over 15,000 lines of logs in return. Not only that, but the log files carry enough meaning that two unnoticed issues were found and resolved only days after finishing this chunk log generator (for reference, [#2778](https://github.com/JacquesCarette/Drasil/issues/2778) and [#2779](https://github.com/JacquesCarette/Drasil/issues/2779) were the two solutions in question). And the creation of errors has a much smaller chance of happening with a much higher chance of appearing throughout the generated files, especially compared to doing all of this by hand.

Breaking down the specifics in terms of lines of code written versus lines of code generated is as follows:

Written lines of code:
| `drasil-` package | Lines of code written |
| --- | --- |
| printers | 240 lines |
| generator function | 10 lines |
| examples | 20 lines (10 for importing generator function, 10 for calling generator function) |

Generated lines:
| Example | Log file length (in terms of lines) | Number of unique chunks in each example |
| --- | --- | --- |
| dblpendulum | 1399 | 523 |
| gamephysics | 1796 | 592 |
| glassbr | 1689 | 578 |
| hghc | 922 | 363 |
| nopcm | 1890 | 707 |
| pdcontroller | 1500 | 547 |
| projectile | 1385 | 472 |
| sglpendulum | 1399 | 523 |
| ssp | 2213 | 697 |
| swhs | 2087 | 699 |

The log files can be checked by running `make` and viewing the `code/debug` folder.