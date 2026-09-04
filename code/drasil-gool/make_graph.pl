:- discontiguous base/1.
:- discontiguous parent/2.

:- consult(facts).

:- table node/1.
node(A) :- base(A) ; (parent(A, B) , node(B)).

edge(A, B) :- node(A) , parent(A, B).

write_graphviz(File) :-
    findall([A, B], edge(A, B), Edges),
    open(File, write, Stream),
    format(Stream, 'digraph G {~n', []),
    format(Stream, '    rankdir=BT;~n', []),
    format(Stream, '    ranksep=4;~n', []),
    format(Stream, '    lheight=200;~n', []),
    format(Stream, '    node [shape=box];~n', []),
    forall(
        member([A, B], Edges),
        format(Stream, '    "~w" -> "~w";~n', [A, B])
    ),
    format(Stream, '}~n', []),
    close(Stream).
