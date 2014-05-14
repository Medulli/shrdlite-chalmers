
:- op(1200, xfx, '--->').

%% Non-lexical grammar rules

%%ask for something to do
command : Cmd --->
    opt_will_you, opt_please,
    basic_command : Cmd,
    opt_please.

%%ask for precision in case of ambiguity
precision : Entity --->
    opt_please,
    entity:Entity,
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
size : medium ---> [medium] ; [middle, sized].
size : large ---> [large] ; [big].

color : black ---> [black].
color : white ---> [white].
color : blue ---> [blue].
color : green ---> [green].
color : yellow ---> [yellow].
color : red ---> [red].

form(sg) : anyform ---> [object] ; [thing] ; [form] ; [one].
form(pl) : anyform ---> [objects] ; [things] ; [forms] ; [ones].
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

nat : N ---> [N], {integer(N), N >= 0}.

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

%Finds object satisfying type size color by checking against a list of possible objects
%if Holding is empty we only have possible objects in world
interpret(object(Type,Size,Color), World, @(null), Objects, SelectedObject) :-
    %get a list of (all) objects
	json(AllPossibleObjects) = Objects,
	%find all objects and that are in world, Col is a list of objects (letters) in world, X is "the letter" of the objects in AllPossibleObjects, which must be a member of the list we're currently checking...
	findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleObjects),
	%get the actual letter of the object we desired from the pool PossibleObjects
	getobj([Type,Size,Color], PossibleObjects, SelectedObject).


%If we're holding something, add that to the possible objects
interpret(object(Type,Size,Color), World, Holding, Objects, SelectedObject) :-
	json(AllPossibleObjects) = Objects,
	findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleWorldObjects),
	member(Holding = json([A1,A2,A3]),AllPossibleObjects),
	append(PossibleWorldObjects,[Holding = json([A1,A2,A3])],PossibleObjects),
	getobj([Type,Size,Color], PossibleObjects, SelectedObject).

%All these are basically "any" or "all" object, I guess we could do something along findall(...,...,[Obj]) for the and findall(...,...,[Obj|_]) for any
interpret(basic_entity(any,X), World, Holding, Objects, [SelectedObject]) :-
    interpret(X, World, Holding, Objects, SelectedObject).

interpret(basic_entity(the,X), World, Holding, Objects, [SelectedObject]) :-
    findall(SelectedObjectAux, interpret(X, World, Holding, Objects, SelectedObjectAux), [SelectedObject]).

interpret(basic_entity(all,X), World, Holding, Objects, SelectedObject) :-
    findall(SelectedObjectAux, interpret(X, World, Holding, Objects, SelectedObjectAux), SelectedObject).


interpret(relative_entity(any,X, Relation), World, Holding, Objects, [SelectedObject]) :-

    %Find all relative objects
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux),
	member(RelativeObjectAux, RelativeObjectListAuxAux)),
	RelativeObjectListAux),
	%Remove duplicates from list
    sort(RelativeObjectListAux,RelativeObjectList),
    %Make one "instance" for every element
	member(SelectedObject, RelativeObjectList),
	%Selected objects must also satisfy description
	interpret(X, World, Holding, Objects, SelectedObject).


interpret(relative_entity(all,X, Relation), World, Holding, Objects, SelectedObject) :-
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux),
								 member(RelativeObjectAux, RelativeObjectListAuxAux)),
								RelativeObjectListAux),
	sort(RelativeObjectListAux,RelativeObjectList),
	%Find all objects which supports the relation
    findall(SelectedObjectAux,( member(SelectedObjectAux, RelativeObjectList),
								interpret(X,        World, Holding, Objects, SelectedObjectAux)),
								SelectedObject).

interpret(relative_entity(the,X, Relation), World, Holding, Objects, [SelectedObject]) :-
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux),
								 member(RelativeObjectAux, RelativeObjectListAuxAux)),
								RelativeObjectListAux),
	sort(RelativeObjectListAux,RelativeObjectList),
	%There should be only one object fitting the description and relation
    findall(SelectedObjectAux,( member(SelectedObjectAux, RelativeObjectList),
								interpret(X,        World, Holding, Objects, SelectedObjectAux)),
								[SelectedObject]).

%find all objects satisfying relations
interpret(relative(beside,X), World, Holding, Objects, SelectedObject) :-
    %find the relative object satisfying type/size/col
    interpret(X, World, Holding, Objects, RelativeObject),
    %find all objects satisfying the relation
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isbeside(SelectedObjectAux,RelativeObjectAux,World)),
	%Can result in an empty list, so add a condition to avoid that
	SelectedObject),SelectedObject \== [].

interpret(relative(leftof,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isleftof(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

interpret(relative(rightof,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isrightof(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

interpret(relative(above,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isabove(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

interpret(relative(ontop,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isontop(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

interpret(relative(ontop,floor), World, _Holding, _Objects, SelectedObject) :-
    member(Col,World),member(SelectedObject,Col),
    nth0(IdxS, Col, SelectedObject),
    (IdxS is 0).

interpret(relative(under,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isunder(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

interpret(relative(inside,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),
	findall(SelectedObjectAux,
	(member(RelativeObjectAux, RelativeObject), isinside(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].

%Find object, and set goal accordingly.
interpret(take(X), World, Holding, Objects,  take(SelectedObject)) :-
    interpret(X, World, Holding, Objects, SelectedObject).

interpret(floor, _World, _Holding, _Objects, floor). %floor is floor... move this somewhere.. meh.

interpret(move(X,relative(beside, Y)), World, Holding, Objects, movebeside(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(leftof, Y)), World, Holding, Objects, moveleft(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(rightof,Y)), World, Holding, Objects, moveright(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(above,  Y)), World, Holding, Objects, moveabove(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(ontop,  Y)), World, Holding, Objects, moveontop(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(under,  Y)), World, Holding, Objects, moveunder(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(move(X,relative(inside, Y)), World, Holding, Objects, moveinside(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
	
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

%%ask for precision in case of ambiguity
test9 :-
Utterance = [the, small, blue, one],
parse_all(precision, Utterance, Trees),write(Trees).