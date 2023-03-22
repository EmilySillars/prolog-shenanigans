% Example 1.2 from
% https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html

use_module(library(apply)). %to import include

% messing around
likesImprov(jane).

/* Let's typecheck this program: first we will typecheck f, then g, and finally g(7).

def f(x, y):
  isnum(print(x)) && isbool(y)

def g(z):
  f(z, 5)

g(7)

1) Typechecking f 
                    f(x,y)
                      |
                      &&
                    /    \
                   /      \
                  /        \
               isnum      isbool
                 |          |
               print        y
                 |
                 x

Let f_i be a version of f with an instantiated type.
For now, let's focus on finding a type for f_i. 
(Later we can generalize it to a type scheme for f).

We perform DFS on the AST of function f, assigning fresh type variables as needed,
and unifying type variables with types as we learn information.

Start at root.
f_i : T1 -> T2 -> T3                      // f_i takes 2 things and returns something
x:T1                                      // from type of f_i
y:T2                                      // from type of f_2

Recurse on child.
&& : Forall [], [bool -> bool -> bool]    // && has a type scheme, but we need a type!
&&_i : [bool -> bool -> bool]             // let &&_i be a version of && with an instantiated type.

Recurse on left child.
isnum : Forall [X], [X -> Bool]           // isnum has a type scheme, but we need a type!
isnum_i : [X1 -> Bool]                    // let isnum_i be a version of isnum with an instantiated type.
print : Forall [X], [X -> X]              // print has a type scheme, but we need a type!
print_i: [X2 -> X2]                       // let print_i be a version of print with an instantiated type.
x:T1                                      // we know this from earlier

Going back up the tree now!
print(x) : R1                             // give the result of calling print on x the type R1
[T1 -> R1] = [X2 -> X2]                   // unify arg/ret of print with its type
T1 = X2, R1 = X2                          // substitutions yielded from unification
isnum(print(x)) : R2                      // give the result of calling isnum on an arg the type R2
[R1 -> R2] = [X1 -> bool]                 // unify arg/ret of isnum with its type
R1 = X1, R2 = bool                        // substitutions yielded from unification

Recurse on right child.
isbool : Forall [X], [X -> bool]          // isbool has a type scheme, but we need a type!
isbool_i: [X3 -> bool]                    // let isbool_i be a version of isbool with an instantiated type.
y : T2                                    // we know this from earlier

Going back up the tree now!
isbool(y) : R3                            // give the result of calling isbool on y the type R3
[T2 -> R3] = [X3 -> bool]                 // unify arg/ret of isbool with its type
T2 = X3, R3 = bool                        // substitutions yielded from unification
(isnum(print(x)) && isbool(y)) : R4       // give the result of calling && on its 2 args the type R4
[R2 -> R3 -> R4] = [bool -> bool -> bool] // unify args/ret of && with its type
R2 = bool, R3 = bool, R4 = bool           // substitutions yielded from unification

T3 = R4                                   // The return type of f_i is the type of its body.

Collecting all the constraints:
T1 = X2, R1 = X2,
R1 = X1, R2 = bool,
T2 = X3, R3 = bool,
R2 = bool, R3 = bool, R4 = bool
T3 = R4

Then rearranging them:
T1 = X2, R1 = X2, 
T2 = X3,
T3 = R4, R4 = bool
R1 = X1,
R2 = bool
R3 = bool

We can deduce:
f_i : [X1 -> X2 -> bool]

Now we generalize f_i's type to a scheme:
f : Forall [X1, X2], [X1 -> X2 -> bool]


2) Typechecking g
                     g(z)
                      |
                      f
                    /    \
                   /      \
                  /        \
                 z          5 


Let g_i be a version of g with an instantiated type.
For now, let's focus on finding a type for g_i. 
(Later we can generalize it to a type scheme for g).

Start at root.
g_i : T4 -> T5                            // g_i takes one thing and returns something
z:T4                                      // from type of g_i

Recurse on child.
f : Forall [X1, X2], [X1 -> X2 -> bool]   // f has a type scheme, but we need a type!
f_i1 : [X4 -> X5 -> bool]                 // let f_i1 be a version of f with an instantiated type.

Recurse on left child.
z : T4                                    // we know types of literals
Go back up!

Recurse on right child.
5 : int                                   // we know types of literals
Go back up!

f(z,5) : R5                               // give the result of calling f on its args type R5
[T4 -> int -> R5] = [X4 -> X5 -> bool]    // unify args/ret of f_i1 with its type
T4 = X4, X5 = int, R5 = bool              // substitutions yielded from unification

T5 = R5                                   // the return value of function g_i is the type of its body.

Collecting all the constraints:
T4 = X4, X5 = int, R5 = bool
T5 = R5 

Then rearranging them:
T4 = X4,
T5 = R5, R5 = bool,
X5 = int

We can deduce:
g_i : [X4 -> bool]

Now we generalize g_i's type to a scheme:
g : Forall [X4], [X4 -> bool]


3) Typechecking g(7)
                      g
                      |
                      7

Start at root.                            
g : Forall [X4], [X4 -> bool]             // g has a type scheme, but we need a type!
g_i = [X6 -> bool]                        // let g_i be a version of g with an instantiated type

Recurse on child.
7 : int                                   // we know the types of literals
Go back up!

g(7) : R6                                 // give the result of calling g on 7 the type R6
[int -> R6] =  [X6 -> bool]               // unify arg/ret of g_i with its type
X6 = int, R6 = bool                       // substitutions yielded from unification

We can conclude that
g(7) : bool                               // done! (finally >.<")
*/

% built-in type schemes
hasTypeScheme(plus, [], [int,int,int]).
hasTypeScheme(and, [], [bool,bool,bool]).
hasTypeScheme(isnum, [X], [X,bool] ).
hasTypeScheme(print, [X], [X,X]).
hasTypeScheme(isbool, [X], [X,bool]).

% type schemes we wish to resolve thru typechecking (program specific)
hasTypeScheme(f, FORALL, T) :- generalize(f_i, FORALL, T).
hasTypeScheme(g, FORALL, T) :- generalize(g_i, FORALL, T). 

% instantiations of type schemes needed for typechecking (program specific)
instantiates(plus_i, plus).                               
instantiates(f_i, f).                                       

% types
type(int).
type(bool).
type(X) :- arrow(X).
arrow([H1, H2]):- type(H1), type(H2).

% generalize the type of FUNC_I into a type scheme
generalize(FUNC_I, FORALL, T):- hasType(FUNC_I, T), include(var, T, FORALL).

% literals have known types
hasType('false', bool).
hasType('true', bool).
hasType(6, int).                        % program specific
hasType(38, int).                       % program specific


% rest of knowledge base below is program specific...

% we instantiate a type from a type scheme by
% - removing the forall constraint
% - giving the type scheme's type variables unique names
hasType(plus_i, [int,int,int]).
hasType(isnum_i, [X1, bool]).
hasType(print_i, [X2, X2]).
hasType(isbool_i, [X3, bool]).

% type checking f
hasType(f_i, [T1, T2, T3])              :- f_typechecks(T1, T2, T3, _, _, _, _, _, _, _).
hasType(x, T1)                          :- f_typechecks(T1, _, _, _, _, _, _, _, _, _).
hasType(y, T2)                          :- f_typechecks(_, T2, _, _, _, _, _, _, _, _).
hasType(print_x, R1)                    :- f_typechecks(_, _, _, R1, _, _, _, _, _, _).
hasType(isnum_print_x, R2)              :- f_typechecks(_, _, _, _, R2, _, _, _, _, _).
hasType(isbool_y, R3)                   :- f_typechecks(_, _, _, _, _, R3, _, _, _, _).
hasType(isnum_print_x_AND_isbool_y, R4) :- f_typechecks(_, _, _, _, _, _, R4, _, _, _).

f_typechecks(T1, T2, T3, R1, R2, R3, R4, X1, X2, X3) :- 
  T1 = X2, R1 = X2,
  R1 = X1, R2 = bool,
  T2 = X3, R3 = bool,
  R2 = bool, R3 = bool, R4 = bool,
  T3 = R4.

/* interestingly, just keeping unification constraints does not work.
   f yeilds too general of a result:

    ?- hasType(f_i, X).
    X = [_, _, _].

% this does not work:
% f_typechecks(T1, T2, T3, R1, R2, R3, R4, X1, X2, X3) :- 
%   [T1, R1] = [X2, X2],   % unify arg/ret of print with its type
%   [R1, R2] = [X1, bool], % unify arg/ret of isnum with its type
%   [T2, R3] = [X3, bool], % unify arg/ret of isbool with its type
%   T3 = R4,               % return type of f_i is the type of its body.
*/

% this works:
% f_typechecks(T1, T2, T3, R1, R2, R3, R4, X1, X2, X3) :- 
%   [T1, R1] = [X2, X2],   % unify arg/ret of print with its type
%   [R1, R2] = [X1, bool], % unify arg/ret of isnum with its type
%   [T2, R3] = [X3, bool], % unify arg/ret of isbool with its type
%   T3 = R4,               % return type of f_i is the type of its body.
%   T1 = X2, R1 = X2,
%   R1 = X1, R2 = bool,
%   T2 = X3, R3 = bool,
%   R2 = bool, R3 = bool, R4 = bool,
%   T3 = R4.



/*
Hindley Milner Example: https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html#%28part._.Type_inference__guessing_correctly__every_time%29

def f(x, y):
  isnum(print(x)) && isbool(y)

def g(z):
  f(z, 5)

g(7)

Since our functions are not mutually recursive, we can handle them independently.

Start by guessing type variables for f, namely ('T1, 'T2 -> 'T3).
Next, we bind the arguments in our environment,
[x : 'T1, y : 'T2], and recur into the body.

Again we encounter a EPrim2, so we lookup the type scheme for the operator and get 
&& : Forall [], (Bool, Bool -> Bool). We then infer types for both arguments:

  Our first argument is a EPrim1, so we look up the type scheme for the operator and find 
  isnum : Forall ['X], ('X -> Bool). We cannot use this directly, 
  so we instantiate the type by creating new type variables and substituting them: 
  isnum : ('T4 -> Bool). We recur on the argument:

    Our argument is another EPrim1, so we look up the type scheme for the operator and find 
    print : Forall ['X], ('X -> 'X). Again we instantiate the type to get print : ('T5 -> 'T5). 
    Then we recur on the argument, and look it up to find x : 'T1. 
    We create a return type 'prim1result1, and unify 'T1 -> 'prim1result1 with 'T5 -> 'T5, 
    to obtain the substitution ['T1 = 'T5, 'prim1result1 = 'T5]. Our result type is 'prim1result1.

    We create a new result type for this primitive, and unify 'prim1result1 -> 'prim1result2 with 'T4 -> Bool, 
    to obtain the substitution ['prim1result1 = 'T4, 'prim1result2 = Bool]. 
    We combine that with the existing substitution to get 
    ['T1 = 'T5, 'prim1result1 = 'T5, 'T4 = 'T5, 'prim1result2 = Bool], and return a result of 'prim1result2.


  Our second argument is another EPrim1, and the type inference for this is analogous to that for isnum. 
  We look up the type scheme, instantiate it to isbool : 'T6 -> Bool, and infer a type for the nested expression. 
  We look that expression up to obtain y : 'T2, generate a new result type 'prim1result3, 
  and unify 'T2 -> 'prim1result3 with 'T6 -> Bool, to obtain ['T2 = 'T6, 'prim1result3 = Bool].


  We instantiate the type of the operator to (Bool, Bool -> Bool). We create a result type 'prim2result1, 
  and unify the inferred type ('prim1result1, 'prim1result3 -> 'prim2result1) with (Bool, Bool -> Bool), 
  to get ['prim1result1 = Bool, 'prim1result3 = Bool, 'prim2result1 = Bool]. 
  We combine this with all our prior substitutions, and return the result type 'prim2result1.


We unify 'prim2result1 with 'T3, then apply our overall substitution, 
and we obtain ['T1 = 'T5, 'T2 = 'T6, 'T3 = Bool] (along with other substitutions). 
Finally, we generalize this type, to get the final type scheme f : Forall ['T5, 'T6], ('T5, 'T6 -> Bool).


We can repeat the process for g, guessing an initial type of 'T7 -> 'T8. 
When we get to the function call, we instantiate the type for f to f : ('T9, 'T10 -> Bool), and proceed. 
We wind up unifying ['T7 = 'T9, 'T10 = Int, 'T8 = Bool], and generalizing, 
we get a final type scheme of g : Forall ['T9], ('T9 -> Bool).


Finally we proceed to g(7). Again we instantiate the scheme for g, unify the result with Int, 
and obtain a final type of Bool. Our progam is self-consistent.
*/


