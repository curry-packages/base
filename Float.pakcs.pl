%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of library Float

'Float.prim_Float_plus'(Y,X,R) :- R is X+Y.

'Float.prim_Float_minus'(Y,X,R) :- R is X-Y.

'Float.prim_Float_times'(Y,X,R) :- R is X*Y.

'Float.prim_Float_div'(Y,X,R) :- R is X/Y.

% transform an integer into a float:
'Float.prim_i2f'(X,R) :- R is X*1.0.

% transform a float to an integer:
'Float.prim_truncate'(X,R) :- R is integer(X).

% round a float to an integer:
'Float.prim_round'(X,R) :- R is integer(round(X)).

'Float.prim_sqrt'(X,R) :- R is sqrt(X).

'Float.prim_log'(X,R) :- R is log(X).

'Float.prim_exp'(X,R) :- R is exp(X).

'Float.prim_sin'(X,R) :- R is sin(X).

'Float.prim_cos'(X,R) :- R is cos(X).

'Float.prim_tan'(X,R) :- R is tan(X).

'Float.prim_asin'(X,R) :- R is asin(X).

'Float.prim_acos'(X,R) :- R is acos(X).

'Float.prim_atan'(X,R) :- R is atan(X).

'Float.prim_sinh'(X,R) :- R is sinh(X).

'Float.prim_cosh'(X,R) :- R is cosh(X).

'Float.prim_tanh'(X,R) :- R is tanh(X).

'Float.prim_asinh'(X,R) :- R is asinh(X).

'Float.prim_acosh'(X,R) :- R is acosh(X).

'Float.prim_atanh'(X,R) :- R is atanh(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
