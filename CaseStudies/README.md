--------------------------------------------------
### Drasil Case Studies
--------------------------------------------------

This folder holds the "manually" generated case studies that are used as the basis for the development of Drasil.  The case studies are re-implemented using the Drasil DSLs to automate the generation of the artifacts that were previously created manually, which meant a significant amount of "copy and paste" and the associated maintenance problems.

The approach to developing Drasil is to translate the manual created case studies into the Drasil DSLs and then refactor the DSLs to capture the patterns that emerge.  The new versions are then refactored again, since new patterns become apparent after the previous pass.  After sufficient interations, we should arrive at a fixed point where the design of the DSLs has converged.

Each of the case study examples is given in its own folder.

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**gamephys**
  - 2D game physics, based on the Chipmunk library.  (https://chipmunk-physics.net/)
  
**glass**
  - Program to predict the safety of a plate of glass when subjected to a blast load.  Based on work by Manuel Campidelli and Wael El-Dakhakhni from the Civil Engineering Department, McMaster University.
  
**swpcm**
- Solar water heating system incorporating PCM.  Since swpcm (also called swhs) is an on-going project, the full version of the case study is hosted external to the Drasil repo, at: https://github.com/smiths/swhs .  This example is based on the work of Marilyn Lightstone, Mechanical Engineering Department, McMaster University.
  
**ssp**
  - Slope stability analysis program.  This example is based on the work of Brandon Karchewski and Dieter Stolle, Civil Engineering Department, McMaster University.
  
README.md
  - This file
