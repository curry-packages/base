%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System:
%

'System.getCPUTime'(MS) :- getRunTime(MS).

'System.getElapsedTime'(MS) :- getElapsedTime(MS).

'System.getArgs'(StringArgs) :-
        (rtArgs(Args) -> true ; getProgramArgs(Args)),
        map2M(basics:atom2String,Args,StringArgs).

'System.prim_getEnviron'(Var,Value) :-
	string2Atom(Var,AtomVar),
	(getEnv(AtomVar,AtomValue) -> atom2String(AtomValue,Value)
	                            ; Value = []). % empty string if undefined

'System.getHostname'(String) :-
        getHostname(Name),
        atom2String(Name,String).

'System.getPID'(Pid) :- currentPID(Pid).

'System.getProgName'(String) :-
        user:currentModuleFile(Name,_),
        atom2String(Name,String).

'System.prim_system'(S,Status) :-
	string2Atom(S,Cmd),
	shellCmd(Cmd,Status).

'System.prim_exitWith'(Code,_) :- halt(Code).

'System.prim_sleep'(S,'Prelude.()') :- sleepSeconds(S).

'System.isWindows'(Flag) :-
	getEnv('COMSPEC', _) ->
	  % Windows systems define this environment variable...
	  Flag = 'Prelude.True'
	; Flag = 'Prelude.False'.
