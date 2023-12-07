% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, Parc Technologies
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% Print ECLiPSe source in different formats
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Helmut Simonis, Parc Technologies Ltd
% Author/s:	Joachim Schimpf, IC-Parc, Imperial College
% Version:	$Id: eclipsefmt.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(eclipsefmt).

:- lib(source_processor).
:- lib(module_options).

:- export pretty_print / 0.
:- export pretty_print / 1.


%:-pragma(nodebug). % to avoid variable names being used in the compiled code


%----------------------------------------------------------------------
% Option handling
%----------------------------------------------------------------------

:- local struct(options(stream, format, indent)).


% Skeleton option structure with defaults for the user-settable fields
default_options(options{format : txt, indent : 0}).

% User-settable option names and their structure index
valid_option_field(format, format of options).
valid_option_field(indent, indent of options).

% Type checks for user-settable options
valid_option_field(format, txt).
valid_option_value(indent, N) :-
    integer(N).



%----------------------------------------------------------------------
% main entry point
%----------------------------------------------------------------------

pretty_print :-
    pretty_print([]).

:- mode pretty_print(++).
pretty_print(OptionList) :-
    get_options(OptionList, Options),
    !,
    pretty_print_with_options(Options).

pretty_print_with_options(Options) :-
    set_stream_property(output, output_options, [variables(default)]),
    setarg(stream of options, Options, output),
    process_file(stream(input), Options).

:- export pretty_print_term / 5.
:- mode pretty_print_term(+, +, +, +, -).
pretty_print_term(Class, source_term{term : Term}, source_position{module : Module}, State, State) :-
    process_term(Class, Term, State, Module).
		

%----------------------------------------------------------------------


process_file(File, Options) :-
    source_open(File, [keep_comments, no_macro_expansion, no_clause_expansion, ignore_conditionals], SP0),
    (
      fromto(begin, _, Class, end),
      fromto(SP0, SP1, SP2, SPend),
      param(Options)
    do
      source_read(SP1, SP2, Class, SourceTerm),
      pretty_print_term(Class, SourceTerm, SP1, Options, _)
    ),
    source_close(SPend, []).



%----------------------------------------------------------------------
% Actual pretty-printing of source terms
%----------------------------------------------------------------------

process_term(directive, Term, Options, Module) :-
    process_term(query, Term, Options, Module).
process_term(handled_directive, Term, Options, Module) :-
    process_term(query, Term, Options, Module).
process_term(query, Term, Options, Module) :-
    functor(Term, F, _),
    arg(1, Term, Query),
    process_query(Query, F, Options, Module).
process_term(clause, Term, Options, Module) :-
    clause_statement(Term, Options, Module).
process_term(comment, Term, Options, _Module) :-
    colored(Term, comment, Options).
%	setval(last_comment, Term).
process_term(var, Term, Options, _Module) :-
    colored(Term, warning, Options).
process_term(end_include, _Term, _Options, _Module).
process_term(end, _Term, _Options, _Module).



process_query(comment(A, B), _Fct, Options, Module) :-
    !,
    handle_comment(A, B, Options, Module).

process_query(Goal, Fct, Options, Module) :-
    colored_simple(Fct, control, Options),
    ( complex_condition(Goal) ->
      Indent = 1,
      o_nl(Options)
    ;
      Indent = 0,
      o_write(Options, ' ')
    ),
    body(Goal, Indent, none, 1200, Options, Module),
    o_write(Options, '.').



% We assume the following fixed precedences:
%	:- 1200
%	?- 1200
%	1200 :- 1200
%	1200 --> 1200
%	-?-> 1180
%	1100 ; 1101
%	1100 do 1101
%	1050 -> 1051
%	1000 , 1001
% if the subterm's top functor is an operator with Prec >= Number
% then the whole subterm must be parenthesised

clause_statement((Head :- -?-> Body), Options, Module) ?-
    !,
    head(Head, Options, Module),
    blank(Options),
    colored((:- -?->), control, Options),
    o_nl(Options),
    body(Body, 2, none, 1180, Options, Module),
    o_write(Options, '.').
clause_statement((Head :- Body), Options, Module) ?-
    !,
    head(Head, Options, Module),
    blank(Options),
    colored_simple((:-), control, Options),
    o_nl(Options),
    body(Body, 2, none, 1200, Options, Module),
    o_write(Options, '.').
clause_statement((Head ?- Body), Options, Module) ?-
    !,
    head(Head, Options, Module),
    blank(Options),
    colored_simple((?-), control, Options),
    o_nl(Options),
    body(Body, 2, none, 1200, Options, Module),
    o_write(Options, '.').
clause_statement((Head --> Body), Options, Module) ?-
    !,
    head(Head, Options, Module),
    blank(Options),
    colored((-->), control, Options),
    dcg_body(Body, 2, 1200, Options, Module),
    o_write(Options, '.').
clause_statement(Fact, Options, Module) :-
    head(Fact, Options, Module),
    o_write(Options, '.').


/*
print the comment statement in a nice form
*/
handle_comment(A, B, Options, Module) :-
    colored(':- comment(', builtin, Options),
    colored(A, type, Options),
    colored(', ', normal, Options),
    comment_list(B, Options, Module),
    o_write(Options, ').').

comment_list([H|T], Options, Module) :-
    !,
    colored('[', builtin, Options),
    comment_list_pairs([H|T], Options, Module),
    colored(']', builtin, Options).
comment_list(X, Options, Module) :-
    comment_term(X, Options, Module).

comment_list_pairs([], _Options, _Module).
comment_list_pairs([X], Options, Module) :-
    !,
    comment_list_pair(X, Options, Module).
comment_list_pairs([A, B|R], Options, Module) :-
    comment_list_pair(A, Options, Module),
    colored(', ', normal, Options),
    comment_list_pairs([B|R], Options, Module).

comment_list_pair(A : A1, Options, Module) :-
    !,
    o_nl(Options),
    coloredq(A, type, Options),
    colored(:, builtin, Options),
    comment_list(A1, Options, Module).
comment_list_pair(A, Options, Module) :-
    comment_term(A, Options, Module).

comment_term(X, Options, _Module) :-
    string(X),
    !,
    o_printf(Options, "%QDvNw", [X]).
comment_term(X, Options, Module) :-
    term(X, none, 1000, Options, Module).


/*
print the head of a clause or a fact
*/
head(X, Options, Module) :-
    term(X, head, 1200, Options, Module).



/*
Body of definite clause grammar rule
*/
dcg_body((A , B), Indent, _Prec, Options, Module) ?-
    !,
    dcg_body(A, Indent, 1000, Options, Module),
    o_write(Options, (',')),
    dcg_body(B, Indent, 1001, Options, Module).
dcg_body((A -> B ; C), Indent, _Prec, Options, Module) ?-
    !,
    Indent1 is Indent + 1,
    o_nl(Options),
    indent(Options, Indent),
    colored_simple('(', control, Options),
    dcg_body(A, Indent1, 1050, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored((->), control, Options),
    dcg_body(B, Indent1, 1051, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple((;), control, Options),
    dcg_body(C, Indent1, 1101, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple(')', control, Options).
dcg_body((A ; B), Indent, _Prec, Options, Module) ?-
    !,
    Indent1 is Indent + 1,
    o_nl(Options),
    indent(Options, Indent),
    colored_simple('(', control, Options),
    dcg_body(A, Indent1, 1100, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple((;), control, Options),
    dcg_body(B, Indent1, 1101, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple(')', control, Options).
dcg_body({}(Goals), Indent, _Prec, Options, Module) ?-
    !,
    o_nl(Options),
    indent(Options, Indent),
    colored_simple('{', control, Options),
    Indent1 is Indent + 1,
    o_nl(Options),
    body(Goals, Indent1, none, 1200, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple('}', control, Options).
dcg_body(Term, Indent, Prec, Options, Module) :-
    o_nl(Options),
    body(Term, Indent, none, Prec, Options, Module).

	

/*
print the body of a clause
leaves you at the end of a line
*/

body(X, Indent, _, _Prec, Options, Module) :-
    var(X),
    !,
    indent(Options, Indent),
    term(X, none, 1000, Options, Module).

body((X , Y), Indent, _, _Prec, Options, Module) :-
    !,
    body(X, Indent, _, 1000, Options, Module),
    o_writeln(Options, (',')),
    body(Y, Indent, none, 1001, Options, Module).

body((X -> Y ; Z), Indent, _, _Prec, Options, Module) :-
    !,
    (
      fromto(Z, In, Out, []),
      fromto(Bodies, Body2, Body1, []),
      fromto(Conditions, Conditions2, Conditions1, [])
    do
      ( var(In) ->
        Conditions2 = Conditions1,
        Body2 = [In|Body1],
        Out = []
      ; In = (X -> Y ; Z) ->
        Conditions2 = [X|Conditions1],
        Body2 = [Y|Body1],
        Out = Z
      ; In = (X -> Y) ->
        Conditions2 = [X|Conditions1],
        Body2 = [Y|Body1],
        Out = []
      ;
        Out = [],
        Body2 = [In|Body1],
        Conditions2 = Conditions1
      )
    ),
    AllConditions = [X|Conditions],
    AllBodies = [Y|Bodies],
    (
      foreach(Con, AllConditions),
      fromto(no, In, Out, Complex)
    do
      ( complex_condition(Con) ->
        Out = yes
      ;
        Out = In
      )
    ),
    Indent1 is Indent + 1,
    indent(Options, Indent),
    colored_simple('(', control, Options),
    (
      foreach(Condition, AllConditions),
      fromto(AllBodies, [Body|Bodies], Bodies, FinalBody),
      param(Complex, Indent, Indent1, Options, Module)
    do
      ( Complex == no ->
        CondIndent = 0,
        blank(Options),
        body(Condition, CondIndent, none, 1000, Options, Module),
        blank(Options)
      ;
        CondIndent is Indent + 1,
        o_nl(Options),
        body(Condition, CondIndent, none, 1000, Options, Module),
        o_nl(Options),
        indent(Options, Indent)
      ),
      colored((->), control, Options),
      o_nl(Options),
      body(Body, Indent1, none, 1051, Options, Module),
      o_nl(Options),
      ( Bodies == [] ->
        true
      ;
        indent(Options, Indent),
        colored_simple((;), control, Options)
      )
    ),
    ( FinalBody = [Body] ->
      o_nl(Options),
      body(Body, Indent1, none, 1000, Options, Module),
      o_nl(Options),
      indent(Options, Indent),
      colored_simple(')', control, Options)
    ;
      indent(Options, Indent),
      colored_simple(')', control, Options)
    ).
body((X -> Y), Indent, _, _Prec, Options, Module) :-
    !,
    Indent1 is Indent + 1,
    indent(Options, Indent),
    colored_simple('(', control, Options),
    ( complex_condition(X) ->
      CondIndent is Indent + 1,
      o_nl(Options),
      body(X, CondIndent, none, 1000, Options, Module),
      o_nl(Options),
      indent(Options, Indent)
    ;
      CondIndent = 0,
      blank(Options),
      body(X, CondIndent, none, 1000, Options, Module),
      blank(Options)
    ),
    colored((->), control, Options),
    o_nl(Options),
    body(Y, Indent1, none, 1051, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple(')', control, Options).
body((X ; Y), Indent, _, _Prec, Options, Module) :-
    !,
    Indent1 is Indent + 1,
    indent(Options, Indent),
    colored_simple('(', control, Options),
    o_nl(Options),
    body(X, Indent1, none, 1100, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple((;), control, Options),
    o_nl(Options),
    body(Y, Indent1, none, 1101, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple(')', control, Options).
body((X do Y), Indent, _, _Prec, Options, Module) :-
    !,
    Indent1 is Indent + 1,
    indent(Options, Indent),
    colored_simple('(', control, Options),
    o_nl(Options),
    body(X, Indent1, none, 1100, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple((do), control, Options),
    o_nl(Options),
    body(Y, Indent1, none, 1101, Options, Module),
    o_nl(Options),
    indent(Options, Indent),
    colored_simple(')', control, Options).
body(X, Indent, _, Prec, Options, Module) :-
    indent(Options, Indent),
    term(X, +, Prec, Options, Module).


%% more than one line
complex_condition((_ , _)) ?-
    true.
complex_condition((_ ; _)) ?-
    true.
complex_condition((_ -> _)) ?-
    true.


/*
indent N spaces
*/
indent(Options, N) :-
    ( N > 0 ->
      o_write(Options, "  "),
      N1 is N - 1,
      indent(Options, N1)
    ;
      true
    ).

/*

print out a term with the main functor in Color
possibly with a meta calls inside and inside a term of precedence Prec
different cases depending on its type

Meta is '+' when X is a goal
Meta is 'head' when X is a head

*/


term(X, Meta, _Prec, Options, _Module) :-
    var(X),
    !,
    ( Meta == head ->
      colored_simple(X, variable, Options)
    ;
      coloredq(X, variable, Options)
    ).
term(X, _Meta, _Prec, Options, _Module) :-
    string(X),
    !,
    coloredq(X, string, Options). % quoted printout
term(X, _Meta, _Prec, Options, _Module) :-
    number(X),
    !,
    coloredq(X, number, Options).
term(X, _Meta, Prec, Options, Module) :-
    X = subscript(A, I),
    (
      compound(A)
    ;
      var(A)
    ),
    nonvar(I),
    I = [_|_],
    not get_flag(syntax_option, no_array_subscripts) @ Module,
    !,
    term_arg(A, none, 1, Prec, Options, Module),
    term_arg(I, none, 2, Prec, Options, Module).
term(X, _Meta, _Prec, Options, Module) :-
    X = no_macro_expansion(S with Args),
    atom(S),
    proper_list(Args),
    not get_flag(syntax_option, no_curly_arguments) @ Module,
    !,
    coloredq(S, none, Options),
    o_write(Options, '{'),
    ( Args = [] ->
      true
    ;
      term_args(Args, none, 1, Options, Module)
    ),
    o_write(Options, '}').
term(X, Meta, Prec, Options, Module) :-
    X = no_macro_expansion('with attributes'(Var, Attrs)),
    var(Var),
    proper_list(Attrs),
    Attrs = [_|_],
    not get_flag(syntax_option, no_attributes) @ Module,
    !,
    term(Var, Meta, Prec, Options, Module),
    o_write(Options, '{'),
    term_args(Attrs, none, 1, Options, Module),
    o_write(Options, '}').
term(X, Meta, Prec, Options, Module) :-
    X = no_macro_expansion(apply(Var, Args)),
    var(Var),
    proper_list(Args),
    Args = [_|_],
    get_flag(syntax_option, var_functor_is_apply) @ Module,
    !,
    term(Var, Meta, Prec, Options, Module),
    o_write(Options, '('),
    term_args(Args, none, 1, Options, Module),
    o_write(Options, ')').
term(X, _, Prec, Options, Module) :-
    atom(X),
    !,
    atom_is_also_operator(X, Prec1, Module),
    parenthesis_open(Options, Prec, Prec1),
    coloredq(X, _, Options),
    parenthesis_close(Options, Prec, Prec1).
term([H|T], _Meta, _Prec, Options, Module) :-
    !,
    list([H|T], Options, Module).
term(X, _, Prec, Options, Module) :-
    infix_operator(X, F, OpPrec, LPrec, RPrec, Module),
    !,
    arg(1, X, A1),
    arg(2, X, A2),
    parenthesis_open(Options, Prec, OpPrec),
    term_arg(A1, Pattern, 1, LPrec, Options, Module),
    blank(Options),
    colored(F, _, Options),
    blank(Options),
    term_arg(A2, Pattern, 2, RPrec, Options, Module),
    parenthesis_close(Options, Prec, OpPrec).
term(X, _, Prec, Options, Module) :-
    prefix2_operator(X, F, OpPrec, LPrec, RPrec, Module),
    !,
    arg(1, X, A1),
    arg(2, X, A2),
    parenthesis_open(Options, Prec, OpPrec),
    colored(F, _, Options),
    blank(Options),
    term_arg(A1, Pattern, 1, LPrec, Options, Module),
    blank(Options),
    term_arg(A2, Pattern, 2, RPrec, Options, Module),
    parenthesis_close(Options, Prec, OpPrec).
term(X, _, Prec, Options, Module) :-
    prefix_operator(X, F, OpPrec, Prec1, Module),
    !,
    arg(1, X, A1),
    parenthesis_open(Options, Prec, OpPrec),
    colored(F, _, Options),
    blank(Options),
    term_arg(A1, _, 1, Prec1, Options, Module),
    parenthesis_close(Options, Prec, OpPrec).
term(X, _, Prec, Options, Module) :-
    postfix_operator(X, F, OpPrec, Prec1, Module),
    !,
    arg(1, X, A1),
    parenthesis_open(Options, Prec, OpPrec),
    term_arg(A1, _, 1, Prec1, Options, Module),
    blank(Options),
    colored(F, _, Options),
    parenthesis_close(Options, Prec, OpPrec).
term(X, _, _Prec, Options, Module) :-
    functor(X, F, N),
    N > 0,
    !,
    coloredq(F, _, Options),
    o_write(Options, '('),
    X =.. [_|L],
    term_args(L, _, 1, Options, Module),
    o_write(Options, ')').
term(X, _Meta, _Prec, Options, _Module) :-
    o_writeq(Options, X).

    proper_list([]) ?-
    true.
    proper_list([_|T]) ?-
    proper_list(T).


/*
decide if we need parenthesis around a term of precdence Prec inside a term 
of Precedence Prec1, where the operator definition gives either x or y 
preference
*/
parenthesis_open(Options, Prec, Prec1) :-
    ( Prec =< Prec1 ->
      o_write(Options, '(')
    ;
      true
    ).

parenthesis_close(Options, Prec, Prec1) :-
    ( Prec =< Prec1 ->
      o_write(Options, ')')
    ;
      true
    ).


/*
special printout for lists
*/

list([H|T], Options, Module) :-
    o_write(Options, '['),
    list_args([H|T], Options, Module),
    o_write(Options, ']').

list_args([X, Y|R], Options, Module) ?-
    !,
    term(X, none, 1000, Options, Module),
    o_write(Options, ', '),
    list_args([Y|R], Options, Module).
list_args([X], Options, Module) ?-
    !,
    term(X, none, 1000, Options, Module).
list_args([X|R], Options, Module) :-
    term(X, none, 1000, Options, Module),
    o_write(Options, (|)),
    term(R, none, 1000, Options, Module).

/*
print the arguments of a term comma-separated
*/
term_args([X], Meta, N, Options, Module) :-
    !,
    term_arg(X, Meta, N, 1000, Options, Module).
term_args([X, Y|R], Meta, N, Options, Module) :-
    term_arg(X, Meta, N, 1000, Options, Module),
    o_write(Options, ', '),
    N1 is N + 1,
    term_args([Y|R], Meta, N1, Options, Module).

/*
print one argument of a term
find out if it is meta-called
*/
term_arg(X, Meta, N, Prec, Options, Module) :-
    meta_arg(N, Meta, MetaN),
    term(X, MetaN, Prec, Options, Module).

meta_arg(_, head, A) :-
    !,
    A = head.
meta_arg(K, T, A) :-
    functor(T, _F, N),
    K =< N,
    !,
    arg(K, T, A).
meta_arg(_, _, none).


meta_args(head, _, Pattern) :-
    !,
    Pattern = head.
meta_args(+, Goal, Pattern) :-
    functor(Goal, F, N),
    functor(Pattern, F, N),
    metacall_table(Pattern, _, _),
    !.
meta_args(M, Goal, Pattern) :-
    integer(M),
    functor(Goal, F, N),
    Arity is N + M,
    functor(Pattern, F, Arity),
    metacall_table(Pattern, _, _),
    !.
meta_args(_, _NoGoal, none).


/*

utility stuff

*/


blank(options with stream : S) :-
    put(S, 32).

prefix_operator(X, F, Prec, RPrec, Module) :-
    functor(X, F, 1),
    current_op(Prec, Type, F) @ Module,
    ( Type = fx ->
      RPrec = Prec
    ;
      Type = fy,
      RPrec is Prec + 1
    ),
    !.

postfix_operator(X, F, Prec, LPrec, Module) :-
    functor(X, F, 1),
    current_op(Prec, Type, F) @ Module,
    ( Type = xf ->
      LPrec = Prec
    ;
      Type = yf,
      LPrec is Prec + 1
    ),
    !.

infix_operator(X, F, Prec, LPrec, RPrec, Module) :-
    functor(X, F, 2),
    current_op(Prec, Type, F) @ Module,
    ( Type = xfx ->
      LPrec = Prec,
      RPrec = Prec
    ; Type = xfy ->
      LPrec = Prec,
      RPrec is Prec + 1
    ;
      Type = yfx,
      LPrec is Prec + 1,
      RPrec = Prec
    ),
    !.

prefix2_operator(X, F, Prec, LPrec, RPrec, Module) :-
    functor(X, F, 2),
    current_op(Prec, Type, F) @ Module,
    ( Type = fxx ->
      LPrec = Prec,
      RPrec = Prec
    ;
      Type = fxy,
      LPrec = Prec,
      RPrec is Prec + 1
    ),
    !.


atom_is_also_operator(X, Prec, Module) :-
    atom(X),
    current_op(Prec, _Type, X) @ Module,
    !,
    findall(P, current_op(P, _Type, X) @ Module, Ops),
    sort(0, >, Ops, [Prec|_]).	% get highest precedence
atom_is_also_operator(_X, 0, _Module).


/*************************************************************************

the low level output routines, 
print different things according to the options

*************************************************************************/

% print some text X known not to contain tricky characters
colored_simple(X, _Style, options{format : txt, stream : S}) :-
    write(S, X).


% print some text X
colored(X, _Style, options{format : txt, stream : S}) :-
    write(S, X).


% print some text in quoted form
coloredq(X, _Style, options{format : txt, stream : S}) :-
    printf(S, "%Qw", [X]).

o_nl(options with stream : S) :-
    nl(S).

o_write(options with stream : S, Term) :-
    write(S, Term).

o_writeln(options with stream : S, Term) :-
    writeln(S, Term).

o_writeq(options with stream : S, Term) :-
    writeq(S, Term).

o_printf(options with stream : S, Format, Term) :-
    printf(S, Format, Term).



/*
 * description of all meta called predicates
 */

metacall_table((+ , +), T, T).
metacall_table((+ ; +), T, T).
metacall_table((+ -> +), T, T).
metacall_table(once +, T, T).
metacall_table(once +, T, T).
metacall_table(call(+), T, T).
metacall_table(not +, T, T).
metacall_table(fail_if(+), T, T).
metacall_table(\+ +, T, T).
metacall_table(~ +, T, T).
metacall_table(call_priority(+, -), T, T).
metacall_table(call_priority(+, -, -), T, T).
metacall_table(maplist(2, -, -), T, T).
metacall_table(block(+, -, +), _T, block).
metacall_table((- do +), _T, block).
metacall_table(assert(+), _T, assert).
metacall_table(asserta(+), _T, assert).
metacall_table(assertz(+), _T, assert).
metacall_table(retract(+), _T, retract).
metacall_table(retract_all(+), _T, retract).
metacall_table(retractall(+), _T, retract).
metacall_table(make_suspension(+, -, -), _T, suspend).
metacall_table(suspend(+, -, -), _T, suspend).
metacall_table(suspend(+, -, -, -), _T, suspend).
metacall_table(minimize(+, -), _T, optimize).
metacall_table(minimize(+, -, -, -), _T, optimize).
metacall_table(minimize(+, -, -, -, -), _T, optimize).
metacall_table(minimize(+, -, -, -, -, -), _T, optimize).
metacall_table(minimize(+, -, -, -, -, -, -, -), _T, optimize).
metacall_table(min_max(+, -), _T, optimize).
metacall_table(min_max(+, -, -, -), _T, optimize).
metacall_table(min_max(+, -, -, -, -), _T, optimize).
metacall_table(min_max(+, -, -, -, -, -), _T, optimize).
metacall_table(min_max(+, -, -, -, -, -, -, -), _T, optimize).
metacall_table(findall(-, +, -), _T, all).
metacall_table(setof(-, +, -), _T, all).
metacall_table(bagof(-, +, -), _T, all).
metacall_table(coverof(-, +, -), _T, all).
metacall_table((-) ^ (+), _T, all).
metacall_table((-?-> +), _T, all).
metacall_table(+ @ -, _T, all).
metacall_table(set_error_handler(-, p), _T, error).
metacall_table(set_default_error_handler(-, p), _T, error).
metacall_table(set_event_handler(-, p), _T, error).
metacall_table((export p), T, T).
metacall_table((local p), T, T).

% special user-defined metacalls for the retimer source code
metacall_table(annotated_constr(+, -), _T, user).
metacall_table(option_init_trace(+), _T, user).
metacall_table(option_trace(+), _T, user).
metacall_table(local_minimize(+, -, -, -, -, -, -, -), _T, user).
metacall_table(wrap_call(+), _T, user).
metacall_table(wrap_bottom_call(+, -, -, -, -, -), _T, user).
metacall_table(au_dichotomic(+, -, -, -), _T, user).
metacall_table(au_all_solns(+, -, -), _T, user).
metacall_table(timeout(+, -, 1), _T, user).
metacall_table(verbose(+, -), _T, user).
metacall_table(display_call(-, -, -, -, +, -, -, -), _T, user).
metacall_table(display_manage_backtrack(+, +), _T, user).
metacall_table(conditional_call(+, +, -), _T, user).
metacall_table(condition_var(+, -), _T, user).
metacall_table(foldlbasecase(3, -, -, -), _T, user).
metacall_table(abstract_constraint(+), _T, user).
metacall_table(filter(1, -, -), _T, user).
metacall_table(filter(1, -, -, -), _T, user).
