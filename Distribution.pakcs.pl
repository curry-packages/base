%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

'Distribution.curryCompiler'(CS) :- atom2String(pakcs,CS).

'Distribution.curryCompilerMajorVersion'(V) :- compilerMajorVersion(V).

'Distribution.curryCompilerMinorVersion'(V) :- compilerMinorVersion(V).

'Distribution.curryCompilerRevisionVersion'(V) :- compilerRevisionVersion(V).

'Distribution.curryRuntime'(PrologS) :-
        prolog(Prolog), atom2String(Prolog,PrologS).

'Distribution.curryRuntimeMajorVersion'(V) :- prologMajorVersion(V).

'Distribution.curryRuntimeMinorVersion'(V) :- prologMinorVersion(V).

'Distribution.baseVersion'(BVS) :- baseVersion(BVA), atom2String(BVA,BVS).

'Distribution.installDir'(PHS) :-
	installDir(PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Distribution.installDir: cannot determine installation directory!').
