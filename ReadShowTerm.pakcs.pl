%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prolog implementation of builtins of module ReadShowTerm:
%

:- installDir(PH), appendAtom(PH,'/src/readShowTerm',RST), use_module(RST).

'ReadShowTerm.prim_showQTerm'(Term,String) :- prim_showQTerm(Term,String).

'ReadShowTerm.prim_showTerm'(Term,String) :- prim_showTerm(Term,String).

'ReadShowTerm.prim_readsQTerm'(String,Term) :- prim_readsQTerm(String,Term).

'ReadShowTerm.prim_readsUnqualifiedTerm'(Prefixes,String,Term) :-
        prim_readsUnqualifiedTerm(Prefixes,String,Term).
