
:- op(1200, xfx, '--->').

%% Non-lexical grammar rules

command : Cmd --->
    opt_will_you, opt_please,
    basic_command : Cmd,
    opt_please.

basic_command : take(Entity) ---> take, entity:Entity.
basic_command : put(Location) ---> move, it, location:Location.
basic_command : move(Entity, Location) ---> move, entity:Entity, location:Location.
%% Improvements
basic_command : where(Entity) ---> where, entity:Entity, opt_interrogation.
basic_command : count(Entity, Location) ---> count, entity:Entity, opt_that_is(Num), location:Location, opt_interrogation.
basic_command : what(Location) ---> what, opt_that_is(Num), location:Location, opt_interrogation.
basic_command : what(Entity, Location) ---> what, opt_that_is(Num), entity:Entity, opt_that_is(Num), location:Location, opt_interrogation.

location : absolute(Relation, Stack) ---> relation:Relation, stack:Stack.
location : relative(Relation, Entity) ---> relation:Relation, entity:Entity.

stack : world ---> the_world.

stack : basic_stack(StackPos) --->
    stack_key,nat:StackPos.

entity : floor ---> the_floor.

entity : basic_entity(Quant, Object) --->
    quantifier(Num):Quant, object(Num):Object.

entity : relative_entity(Quant, Object, Location) ---> 
    quantifier(Num):Quant, object(Num):Object,
    opt_that_is(Num),
    location:Location.

object(Num) : object(Form,Size,Color) ---> size:Size, color:Color, form(Num):Form.
object(Num) : object(Form,Size,Color) ---> color:Color, size:Size, form(Num):Form.
object(Num) : object(Form,'-', Color) ---> color:Color, form(Num):Form.
object(Num) : object(Form,Size,'-')  ---> size:Size, form(Num):Form.
object(Num) : object(Form,'-', '-')  ---> form(Num):Form.

%% Lexical rules

quantifier(sg) : the ---> [the].
quantifier(sg) : any ---> [a] ; [an] ; [any].
quantifier(sg) : all ---> [every].
quantifier(pl) : all ---> [all].

relation : beside ---> [beside].
relation : leftof ---> [left,of] ; [to,the,left,of].
relation : rightof ---> [right,of] ; [to,the,right,of].
relation : above ---> [above].
relation : ontop ---> [on,top,of] ; [on].
relation : under ---> [under] ; [below].
relation : inside ---> [inside] ; [in] ; [into].

size : small ---> [small] ; [tiny].
size : large ---> [large] ; [big].

color : black ---> [black].
color : white ---> [white].
color : blue ---> [blue].
color : green ---> [green].
color : yellow ---> [yellow].
color : red ---> [red].

form(sg) : anyform ---> [object] ; [thing] ; [form].
form(pl) : anyform ---> [objects] ; [things] ; [forms].
form(sg) : brick ---> [brick].
form(pl) : brick ---> [bricks].
form(sg) : plank ---> [plank].
form(pl) : plank ---> [planks].
form(sg) : ball ---> [ball].
form(pl) : ball ---> [balls].
form(sg) : pyramid ---> [pyramid].
form(pl) : pyramid ---> [pyramids].
form(sg) : box ---> [box].
form(pl) : box ---> [boxes].
form(sg) : table ---> [table].
form(pl) : table ---> [tables].

nat : N ---> [N], {integer(N), N > 0}.

%% Lexicon (without semantic content)

the_world ---> [the,world].
the_floor ---> [the,floor].
stack_key ---> [stack].

opt_that_is(_) ---> [].
opt_that_is(sg) ---> [that,is].
opt_that_is(pl) ---> [that,are].

move ---> [move] ; [put] ; [drop].
take ---> [take] ; [grasp] ; [pick,up].
it ---> [it].
%%Improvements
where ---> [find] ; [where,is] ; [where,are].
count ---> [count] ; [how,many].
what ---> [what,is] ; [what,are,the,objects].

opt_will_you ---> [] ; [will,you] ; [can,you] ; [could,you].
opt_please ---> [] ; [please].
opt_interrogation ---> [] ; [?].


/*
  This file is a recursive descent parser of DCG grammars
  stored using the predicate '--->'/2.

  Call like this:
  ?- parse(command, [take, the, white, ball], Tree).
  Tree = take(basic_entity(the, object(ball, -, white))) ;
  no (more) solutions

  ...or like this:
  ?- parse_all(command, [take, the, white, ball], Trees).
  Trees = [take(basic_entity(the, object(ball, -, white)))]
*/

%% parse_all(+Startcat : atom, +Sentence : list(atom), -ParseTrees : list(term))
parse_all(Cat, Tokens, Trees) :-
    findall(T, parse(Cat, Tokens, T), Trees).

%% parse_all(+Startcat : atom, +Sentence : list(atom), ?ParseTree : term)
parse(Cat, Tokens, Tree) :-
    parse_term(Cat:Tree, Tokens, []).

%% parse_term(?ParsingGoal : term, +Sentence : list(atom), ?Remainder : list(atom))
parse_term(LHS, Xs0, Xs) :-
    '--->'(LHS, RHS),
    parse_term(RHS, Xs0, Xs).
parse_term([], Xs, Xs).
parse_term([T|Ts], [T|Xs0], Xs) :-
    parse_term(Ts, Xs0, Xs).
parse_term((A, B), Xs0, Xs) :-
    parse_term(A, Xs0, Xs1),
    parse_term(B, Xs1, Xs).
parse_term((A ; B), Xs0, Xs) :-
    ( parse_term(A, Xs0, Xs)
    ; parse_term(B, Xs0, Xs)
    ).
parse_term({Goal}, Xs, Xs) :-
    call(Goal).

%% Command test
testBase :-
Utterance = [put,the,white,ball,in,a,box,on,the,floor],
parse_all(command, Utterance, Trees),write(Trees).

%test where
test :-
Utterance = [find, the, white, ball],
parse_all(command, Utterance, Trees),write(Trees).

test2 :-
Utterance = [where, is, the, white, ball, ?],
parse_all(command, Utterance, Trees),write(Trees).

test3 :-
Utterance = [where, are, all, white, balls],
parse_all(command, Utterance, Trees),write(Trees).

%test count
test4 :-
Utterance = [count, all, white, balls, in, the, world],
parse_all(command, Utterance, Trees),write(Trees).

test5 :-
Utterance = [count, all, white, balls, on, stack, 1],
parse_all(command, Utterance, Trees),write(Trees).

test6 :-
Utterance = [count, all, white, balls, that, are, left, of, the, blue, box],
parse_all(command, Utterance, Trees),write(Trees).

%test what
test7 :-
Utterance = [what, is, on, stack ,12],
parse_all(command, Utterance, Trees),write(Trees).

test8 :-
Utterance = [what, are, the, objects, that, are, right, of, the, white, ball, on, the, floor, ?],
parse_all(command, Utterance, Trees),write(Trees).