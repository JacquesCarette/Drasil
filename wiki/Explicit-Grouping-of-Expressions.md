As referenced in issue #14 we have decided that explicitly grouping expressions is something we should include.

Initially all expression grouping was going to be automated (i.e. the generator would handle putting parentheses in the appropriate places to clarify expressions). However, there are certain cases where we would like to be able to explicitly keep an expression separate through the use of grouping because these groupings *mean* something.

For example: when dealing with indices we could write an expression as i+j+1, or we could write it as i+(j+1). The latter having a more nuanced meaning along the lines of "the index i plus one greater than the index j".