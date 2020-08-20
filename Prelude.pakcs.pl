%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of standard external functions:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of arithmetic functions:
%
'Prelude.prim_plusInt'(Y,X,R) :- R is X+Y.

'Prelude.prim_minusInt'(Y,X,R) :- R is X-Y.

'Prelude.prim_timesInt'(Y,X,R) :- R is X*Y.

'Prelude.prim_divInt'(Y,X,R) :- R is div(X,Y).

'Prelude.prim_modInt'(Y,X,R) :- isMod(R,X,Y).

'Prelude.prim_quotInt'(Y,X,R) :- R is X // Y.

'Prelude.prim_remInt'(Y,X,R) :- isRem(R,X,Y).


'Prelude.prim_plusFloat'(Y,X,R) :- R is X+Y.

'Prelude.prim_minusFloat'(Y,X,R) :- R is X-Y.

'Prelude.prim_timesFloat'(Y,X,R) :- R is X*Y.

'Prelude.prim_divFloat'(Y,X,R) :- R is X/Y.

'Prelude.prim_negateFloat'(X,R) :- R is -X.

% transform an integer into a float:
'Prelude.prim_intToFloat'(X,R) :- R is X*1.0.

% transform a float to an integer:
'Prelude.prim_truncateFloat'(X,R) :- R is integer(X).

% round a float to an integer:
'Prelude.prim_roundFloat'(X,R) :- R is integer(round(X)).

'Prelude.prim_sqrtFloat'(X,R) :- R is sqrt(X).

'Prelude.prim_logFloat'(X,R) :- R is log(X).

'Prelude.prim_expFloat'(X,R) :- R is exp(X).

'Prelude.prim_sinFloat'(X,R) :- R is sin(X).

'Prelude.prim_cosFloat'(X,R) :- R is cos(X).

'Prelude.prim_tanFloat'(X,R) :- R is tan(X).

'Prelude.prim_asinFloat'(X,R) :- R is asin(X).

'Prelude.prim_acosFloat'(X,R) :- R is acos(X).

'Prelude.prim_atanFloat'(X,R) :- R is atan(X).

'Prelude.prim_sinhFloat'(X,R) :- R is sinh(X).

'Prelude.prim_coshFloat'(X,R) :- R is cosh(X).

'Prelude.prim_tanhFloat'(X,R) :- R is tanh(X).

'Prelude.prim_asinhFloat'(X,R) :- R is asinh(X).

'Prelude.prim_acoshFloat'(X,R) :- R is acosh(X).

'Prelude.prim_atanhFloat'(X,R) :- R is atanh(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of comparsion of primitive data:
%
'Prelude.prim_eqInt'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_eqFloat'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_eqChar'(Y,X,R) :- X==Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqInt'(Y,X,R) :- X=<Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqFloat'(Y,X,R) :- X=<Y -> R='Prelude.True' ; R='Prelude.False'.

'Prelude.prim_ltEqChar'(Y,X,R) :-
	char_int(X,VX), char_int(Y,VY),
	VX=<VY -> R='Prelude.True' ; R='Prelude.False'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of conversion functions for characters:
%
'Prelude.prim_ord'(C,N) :- char_int(C,N).

'Prelude.prim_chr'(N,C) :- N>=0, N<1114112, !, char_int(C,N).
'Prelude.prim_chr'(_,_) :- raise_exception('chr: argument out of range').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show and read functions for primitive types:
%
'Prelude.prim_showIntLiteral'(N,S) :- prim_showNumber(N,S).

'Prelude.prim_showFloatLiteral'(N,S) :- prim_showNumber(N,S).

prim_showNumber(N,S) :-
	number_codes(N,SN), map2M(basics:char_int,String,SN),
	(N>=0 -> S = String
	       ; % enclose negative number in parentheses:
	         char_int(Op,40), char_int(Cl,41),
	         append([Op|String],[Cl],S)).

% conversion of string representations of nat literals into Curry terms:
'Prelude.prim_readNatLiteral'([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        natConst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
'Prelude.prim_readNatLiteral'(_,[]). % parse error

% conversion of string representations of float literals into Curry terms:
'Prelude.prim_readFloatLiteral'([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        floatConst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
'Prelude.prim_readFloatLiteral'(_,[]). % parse error

natConst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        natConst(Cs).
natConst([]) --> skipblanks.

floatConst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConst(Cs).
floatConst([46,C|Cs]) --> [PC], { char_int(PC,46) }, [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConstRest(Cs).

floatConstRest([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatConstRest(Cs).
floatConstRest([C|Cs]) --> [CC], {char_int(CC,C), C=69 ; C=101}, !, % exponent
	intConst(Cs).
floatConstRest([]) --> skipblanks.

intConst(Cs) --> ( [CC], {char_int(CC,45)}, natConst(NCs), {Cs=[45|NCs]}
		  ; natConst(Cs)
		  ).


% conversion of string representations of char literals into Curry terms:
% TODO: avoid char_int conversion
'Prelude.prim_readCharLiteral'(String,['Prelude.(,)'(Char,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=39, readChar(PrologString,Tail,Char),
	map2M(basics:char_int,TailString,Tail), !.
'Prelude.prim_readCharLiteral'(_,[]). % parse error

% conversion of string representations of string literals into Curry terms:
% TODO: avoid char_int conversion
'Prelude.prim_readStringLiteral'(String,['Prelude.(,)'(Result,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=34, readString(PrologString,Tail,Result),
	map2M(basics:char_int,TailString,Tail), !.
'Prelude.prim_readStringLiteral'(_,[]). % parse error

'Prelude.prim_showCharLiteral'(C,[Apo|S]) :-
	char_int(Apo,39),  % 39='''
	char_int(C,N),
	(N=39 -> char_int(BS,92), S=[BS,C|SE] % '
	 ; (N=34 -> S=[C|SE] % "
	          ; showTermChar(N,S,SE))),
	SE = [Apo].

'Prelude.prim_showStringLiteral'(Str,[Quot|S]) :-
	char_int(Quot,34), % 34 = '"'
	showTermString(Str,S,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of I/O actions:
%
'Prelude.prim_putChar'(C,'Prelude.()') :-
	char_int(C,N), put_code(N),
	%flush_output. % this is problematic for Sicstus4 (substantial delay)
	(N=10 -> flush_output ; true).

'Prelude.getChar'(C) :- get_code(N), char_int(C,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
