
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
    interpret(X, World, Holding, Objects, SelectedObject).

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
	
%%Stacks

interpret(absolute(beside,basic_stack(N)), World, Holding, Objects, SelectedObject) :-
    %Check if the stack exists
	iswithinbounds(N,World),
    %find all objects satisfying the relation
	findall(SelectedObjectAux,
	isbesidestack(SelectedObjectAux,N,World),
	%Can result in an empty list, so add a condition to avoid that
	SelectedObject),SelectedObject \== [].

interpret(absolute(leftof,basic_stack(N)), World, Holding, Objects, SelectedObject) :-
    iswithinbounds(N,World),
	findall(SelectedObjectAux,
	isleftofstack(SelectedObjectAux,N,World),
	SelectedObject),SelectedObject \== [].

interpret(absolute(rightof,basic_stack(N)), World, Holding, Objects, SelectedObject) :-
    iswithinbounds(N,World),
	findall(SelectedObjectAux,
	isrightofstack(SelectedObjectAux,N,World),
	SelectedObject),SelectedObject \== [].

interpret(absolute(above,basic_stack(N)), World, Holding, Objects, SelectedObject) :-
    iswithinbounds(N,World),
	findall(SelectedObjectAux,
	isabovestack(SelectedObjectAux,N,World),
	SelectedObject),SelectedObject \== [].
	
interpret(absolute(ontop,basic_stack(N)), World, Holding, Objects, SelectedObject) :-
    iswithinbounds(N,World),
	findall(SelectedObjectAux,
	isontopstack(SelectedObjectAux,N,World),
	SelectedObject),SelectedObject \== [].

interpret(absolute(inside,world), World, _Holding, _Objects, SelectedObject) :-
    flatten(World,SelectedObject).
	
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
	
interpret(move(X,absolute(beside, basic_stack(N))), World, Holding, Objects, movebesidestack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(move(X,absolute(leftof, basic_stack(N))), World, Holding, Objects, moveleftstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(move(X,absolute(rightof,basic_stack(N))), World, Holding, Objects, moverightstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(move(X,absolute(above,  basic_stack(N))), World, Holding, Objects, moveabovestack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(move(X,absolute(ontop,  basic_stack(N))), World, Holding, Objects, moveontopstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
	
%%Interpret Where
interpret(where(X), World, Holding, Objects, where(SelectedObjects)) :-
	interpret(X, World, Holding, Objects, SelectedObjects).
	
%%done with
%maplist(whichListInTheWorld(World),SelectedObjects,IdxList).
	
%%Interpret Count
interpret(count(X,relative(beside, Y)), World, Holding, Objects, countbeside(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(leftof, Y)), World, Holding, Objects, countleft(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(rightof,Y)), World, Holding, Objects, countright(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(above,  Y)), World, Holding, Objects, countabove(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(ontop,  Y)), World, Holding, Objects, countontop(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(under,  Y)), World, Holding, Objects, countunder(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(count(X,relative(inside, Y)), World, Holding, Objects, countinside(SelectedObject,RelativeObject)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	interpret(Y, World, Holding, Objects, RelativeObject).
	
interpret(count(X,absolute(beside, basic_stack(N))), World, Holding, Objects, countbesidestack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(count(X,absolute(leftof, basic_stack(N))), World, Holding, Objects, countleftstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(count(X,absolute(rightof,basic_stack(N))), World, Holding, Objects, countrightstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(count(X,absolute(above,  basic_stack(N))), World, Holding, Objects, countabovestack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(count(X,absolute(ontop,  basic_stack(N))), World, Holding, Objects, countontopstack(SelectedObject,[N])) :-
	interpret(X, World, Holding, Objects, SelectedObject).
interpret(count(X,absolute(inside, world)), World, Holding, Objects, countinsidestacks(SelectedObject,N)) :-
	interpret(X, World, Holding, Objects, SelectedObject),
	length(World,LengthWorld),listFirstIndexes(LengthWorld, N).
	
%%Interpret What
interpret(what(relative(beside, Y)), World, Holding, Objects, whatbeside(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(leftof, Y)), World, Holding, Objects, whatleft(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(rightof,Y)), World, Holding, Objects, whatright(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(above,Y)), World, Holding, Objects, whatabove(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(ontop,  Y)), World, Holding, Objects, whatontop(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(under,  Y)), World, Holding, Objects, whatunder(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).
interpret(what(relative(inside, Y)), World, Holding, Objects, whatinside(RelativeObject)) :-
	interpret(Y, World, Holding, Objects, RelativeObject).

interpret(what(absolute(beside, basic_stack(N))), World, Holding, Objects, whatbesidestack([N])).
interpret(what(absolute(leftof, basic_stack(N))), World, Holding, Objects, whatleftstack([N])).
interpret(what(absolute(rightof,basic_stack(N))), World, Holding, Objects, whatrightstack([N])).
interpret(what(absolute(above,  basic_stack(N))), World, Holding, Objects, whatabovestack([N])).
interpret(what(absolute(ontop,  basic_stack(N))), World, Holding, Objects, whatontopstack([N])).
interpret(what(absolute(inside, world)), World, Holding, Objects, whatinsidestacks(N)) :-
	length(World,LengthWorld),listFirstIndexes(LengthWorld, N).
	
%Will return the letter of the type/size/col which satisfies the object in PossibleObjects.
%------------------------------------------------------------------------------------------------------------------------%
getobj([Type,Size,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=Type,size=Size,color=Color]), PossibleObjects).

getobj([Type,Size,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=Type,size=Size,color=_]), PossibleObjects).

getobj([Type,-,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=Type,size=_,color=Color]), PossibleObjects).

getobj([Type,-,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=Type,size=_,color=_]), PossibleObjects).
%------------------------------------------------------------------------------------------------------------------------%
getobj([anyform,Size,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=Size,color=Color]), PossibleObjects).

getobj([anyform,Size,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=Size,color=_]), PossibleObjects).

getobj([anyform,-,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=_,color=Color]), PossibleObjects).

getobj([anyform,-,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=_,color=_]), PossibleObjects).
%------------------------------------------------------------------------------------------------------------------------%	

%---------------------------------------------------------------------------------------------------- Constraints management ----------------------------------------------------------------------------------------------------

%Get the form and the size of an object knowing its name (one letter) and the possible objects. Output : ObjectFormSize=[form,size]
getFormAndSize(ObjectLetter,PossibleObjects,ObjectFormSize) :-
	member(ObjectLetter = ObjectJson,PossibleObjects),ObjectJson=json([form=FormObj,size=SizeObj,color=_]),ObjectFormSize=[FormObj,SizeObj].

/*********************** getFormAndSize ***********************
testFormSize :-
PossibleObjects = [a=json([form=ball,size=small,color=white]), b=json([form=box,size=large,color=black])],
getFormAndSize(a,PossibleObjects,ObjectFormSize),write(ObjectFormSize).
*/

%Defines the constraints : what can be on what ?
%weirds laws for boxes ?
canbeonFormAndSize([Form1,Size1,Form2,Size2]) :-
(Form1 = ball,(
		(Size1 = small,((Form2 = box,Size2 = small);(Form2 = box,Size2 = medium);(Form2 = box,Size2 = large)));
		(Size1 = medium,((Form2 = box,Size2 = medium);(Form2 = box,Size2 = large)));
		(Size1 = large,(Form2 = box,Size2 = large))
	);
Form1 = box,(
		(Size1 = small,((Form2 = table,Size2 = small);(Form2 = table,Size2 = large);(Form2 = plank,Size2 = small)));
		(Size1 = medium,((Form2 = table,Size2 = medium);(Form2 = plank,Size2 = medium)));
		(Size1 = large,((Form2 = table,Size2 = large);(Form2 = plank,Size2 = large);(Form2 = brick,Size2 = large)))
	);
Form1 = brick,(
		(Size1 = small,((Form2 = box,Size2 = medium);(Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = small);(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = small);(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = small);(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = medium,((Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = large,((Form2 = table,Size2 = large);(Form2 = plank,Size2 = large);(Form2 = brick,Size2 = large)))
	);
Form1 = plank,(
		(Size1 = small,((Form2 = box,Size2 = medium);(Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = small);(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = small);(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = small);(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = medium,((Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = large,((Form2 = table,Size2 = large);(Form2 = plank,Size2 = large);(Form2 = brick,Size2 = large)))
	);
Form1 = pyramid,(
		(Size1 = small,((Form2 = box,Size2 = medium);(Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = small);(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = small);(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = small);(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = medium,((Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = large,((Form2 = table,Size2 = large);(Form2 = plank,Size2 = large);(Form2 = brick,Size2 = large)))
	);
Form1 = table,(
		(Size1 = small,((Form2 = box,Size2 = small);(Form2 = box,Size2 = medium);(Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = small);(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = small);(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = small);(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = medium,((Form2 = box,Size2 = medium);(Form2 = box,Size2 = large);
			(Form2 = brick,Size2 = medium);(Form2 = brick,Size2 = large);
			(Form2 = plank,Size2 = medium);(Form2 = plank,Size2 = large);
			(Form2 = table,Size2 = medium);(Form2 = table,Size2 = large)));
		(Size1 = large,((Form2 = box,Size2 = large);(Form2 = table,Size2 = large);(Form2 = plank,Size2 = large);(Form2 = brick,Size2 = large)))
	)
).

%everything can be on the floor
canbeon(O,[],PossibleObjects).
%or it depends on the top of the stack
canbeon(O,[H|L],PossibleObjects) :- getFormAndSize(O,PossibleObjects,OFormSize),getFormAndSize(H,PossibleObjects,HFormSize),append(OFormSize,HFormSize,AllFormSize),canbeonFormAndSize(AllFormSize).


/*********************** canbeon testing ***********************
%false
testFalse :-
PossibleObjects = [a=json([form=ball,size=large,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]),  d=json([form=box,size=large,color=blue]),  e=json([form=box,size=medium,color=red])],
canbeon(a,[b,e,d],PossibleObjects).

%true
testTrue :-
PossibleObjects = [a=json([form=ball,size=small,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]),  d=json([form=box,size=large,color=blue]),  e=json([form=box,size=medium,color=red])],
canbeon(a,[e,d],PossibleObjects).

%true (floor testing)
testFloor :-
PossibleObjects = [a=json([form=ball,size=small,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]),  d=json([form=box,size=large,color=blue]),  e=json([form=box,size=medium,color=red])],
canbeon(a,[],PossibleObjects).
*/

 %-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

isbeside(X,Y,World) :-
    %find list id for relative object
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    %and all other objects
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),
    %however they must satisfy this (to the left;or;to the right)
    (IdxS is IdxR-1;IdxS is IdxR+1).
isleftof(X,Y,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),
    (IdxS < IdxR).
isrightof(X,Y,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),
    (IdxS > IdxR).
isabove(X,Y,World) :-
    %basically same as beside/left/right, however the must belong to the same sub list
    member(Col,World),member(X,Col), member(Y, Col),
    %get idx of objects
    nth0(IdxS, Col, X),
    nth0(IdxR, Col, Y),
    (IdxS > IdxR).
isontop(X,Y,World) :-
	member(Col,World),member(X,Col), member(Y, Col),
    nth0(IdxS, Col, X),
    nth0(IdxR, Col, Y),
    (IdxS is IdxR+1).
isunder(X,Y,World) :-
	member(Col,World),member(X,Col), member(Y, Col),
    nth0(IdxS, Col, X),
    nth0(IdxR, Col, Y),
    (IdxS < IdxR).
isinside(X,Y,World) :-
	member(Col,World),member(X,Col), member(Y, Col),
    nth0(IdxS, Col, X),
    nth0(IdxR, Col, Y),
    (IdxS is IdxR+1).
whatisonstack(Col,World,X) :-
	nth0(Col,World,X).
isabovestack(X,Col,World) :-
	whatisonstack(Col,World,Stack),member(X,Stack).
isontopstack(X,Col,World) :-
	whatisonstack(Col,World,Stack),nth0(0,Stack,X).
isbesidestack(X,N,World) :-
    %find list id for relative object
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    %however they must satisfy this (to the left;or;to the right)
    (IdxS is N-1;IdxS is N+1).
isleftofstack(X,N,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    (IdxS < N).
isrightofstack(X,N,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    (IdxS > N).
iswithinbounds(N,World) :- integer(N), N >= 0, length(World,LengthWorld), N < LengthWorld.

listFirstIndexes(Length, List) :- HighValue is Length - 1, numlist(0,HighValue,List).

%%-----------------------------------------------------

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

test10 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [take, the, white, ball],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test11 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [where, is, the, white, ball, ?],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
/*					
test12 :-
	World = [[e],[l,g],[],[f,m,k],[]],
	whichListInTheWorld(World,f,Idx),write(Idx).	

maplistm(_C, [], [], []).
maplistm( C, [X|Xs], [Y|Ys], [Z|Zs]) :-
   call(C, X, Y, Z),
   maplistm( C, Xs, Ys, Zs).
   
maplistn(_C, [], []).
maplistn( C, [X|Xs], [Y|Ys]) :-
   call(C, X, Y),
   maplistn( C, Xs, Ys).
   
truc(N,R):- R is N*N.
%listWhichListInTheWorld([X|_],LL,N):- whichListInTheWorld(X,LL,N).
%listWhichListInTheWorld([_|L],LL,NewListNum):- call(whichListInTheWorld(L,LL,N))

test13 :-
	World = [[e],[l,g],[],[f,m,k],[]],
	%maplist(maplist(truc),[[1,2],[3,4]],Rss),write(Rss).
	%maplist(append(['soeur']),World,Rss),write(Rss).
	maplist(whichListInTheWorld(World),[e,f],IdxList),write(IdxList).	
*/

test12 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [where, are, all, balls],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test13 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [move, the, white, ball, to, the, left, of, the, red, box, that, is, left, of, the, yellow, box],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test14 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [count, all, balls, that, are, left, of, the, blue, box],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test15 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [what, are, the, objects, that, are, right, of, the, white, ball, on, the, floor, ?],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test16 :-
	World = [[e],[l,g],[],[f,m,k],[]],
	flatten(World,PossibleObjects),write(PossibleObjects).
	
test17 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [move, the, ball, that, is, on, stack, 0, on, the, yellow, box],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test18 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [move, the, white, ball, on, the, floor],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
test19 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [move, the, white, ball, on, stack, 0],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test20 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [count, all, balls, on, stack, 0],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test21 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [count, all, balls, in, the, world],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test22 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [what, is, on, stack ,12],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).
					
test23 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [the, small, blue, one],
parse_all(precision, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).

retrieveGoalElements(Goal, Action, Parameter) :-
	Goal = whatrightstack([Parameter]),Action = whatrightstack.		
retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = take([Parameter]),Action = take.
	
test24 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [what, is, in, the, world],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals),
Goals = [Goal],
retrieveGoalElements(Goal, whatinsidestacks, Parameter),write(Parameter).

plan(_Goal, World, Holding, _Objects, Plan) :-
%%Can be called : retrieveGoalElements(_Goal, take, Element) without the line ActionTake == take
%%To be tested, but should work
      retrieveGoalElements(_Goal, take, Element),
      Holding == @(null),
      whichListInTheWorld(Element,World,K),
      nth0(K,World,LK),
      checkHead(LK,Element),
      pickAt(K,World,NewWorld),
      %nb_setval(world, NewWorld),
      %b_setval(holding, [Element]),
      Plan = [[K,-1]].

%tests if element is the head of the list
checkHead([H|T],Element) :- H = Element.

%tests if element is the tail of the list
checkTail([H|T],Tail) :- T = Tail.

%return the number K if X is in the Kth list of lists LL
%findall(X,whichListInTheWorld(a,[[d,e,f],[a,b,c]],X),R).

whichListInTheWorld([L|_],X,0) :- member(X,L).
whichListInTheWorld([_|LL],X,N) :- whichListInTheWorld(LL,X,M), N is M + 1.

%the third argument is the list of lists corresponding to the one given as second argument in which the head is removed in the list of number: first argument
pickAt(0,[[H|T1]|T2],[T1|T2]).
pickAt(N,[H|T1],[H|T2]) :- pickAt(M,T1,T2), N is M + 1.

%the third argument is the list of lists corresponding to the one given as second argument in which the first argument is added at the head in the list of number: second argument
dropAt(Element,0,[T1|T2],[[Element|T1]|T2]).
dropAt(Element,N,[H|T1],[H|T2]) :- dropAt(Element,M,T1,T2), N is M + 1.

%find a list in which an element can be added given the object, the world, the objects
canbeAt(X,[H|L],Objects,0) :- canbeon(X,H,Objects).
canbeAt(X,[H|L],Objects,N) :- canbeAt(X,L,Objects,M), N is M + 1.

test25 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [take, the, white, ball],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals),
Goals = [Goal],
plan(_Goal, World, Holding, _Objects, Plan),write(Plan).

test26 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [take, the, ball],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals).

%Get the form, the size and the color of an object knowing its name (one letter) and the possible objects. Output : ObjectFormSizeColor=[form,size,color]
getFormSizeColor(PossibleObjects,ObjectLetter,ObjectFormSizeColor) :-
	PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),ObjectFormSizeColor=[SizeObj,ColorObj,FormObj].
getFormSizeColorText(PossibleObjects,ObjectLetter,ObjectFormSizeColor) :-
	PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),
	atom_string(SizeObj,SizeObjStr),atom_string(ColorObj,ColorObjStr),atom_string(FormObj,FormObjStr),
	string_concat('a ',SizeObjStr,FinalStr1),
	string_concat(FinalStr1,' ',FinalStr2),
	string_concat(FinalStr2,ColorObjStr,FinalStr3),
	string_concat(FinalStr3,' ',FinalStr4),
	string_concat(FinalStr4,FormObjStr,FinalStr5),
	string_concat(FinalStr5,'.',FinalStr6),
	ObjectFormSizeColor=FinalStr6.

/*
%Get the form, the size and the color of an object knowing its name (one letter) and the possible objects. Output : ObjectFormSizeColor=[form,size,color]
getFormSizeColor(PossibleObjects,ObjectLetter,ObjectFormSizeColor) :-
	PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),ObjectFormSizeColor=[SizeObj,ColorObj,FormObj].
getFormSizeColorText(PossibleObjects,ObjectLetter,ObjectFormSizeColor) :-
	PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),
	atom_string(SizeObj,SizeObjStr),atom_string(ColorObj,ColorObjStr),atom_string(FormObj,FormObjStr),
	string_concat('\'a ',SizeObjStr,FinalStr1),
	string_concat(FinalStr1,' ',FinalStr2),
	string_concat(FinalStr2,ColorObjStr,FinalStr3),
	string_concat(FinalStr3,' ',FinalStr4),
	string_concat(FinalStr4,FormObjStr,FinalStr5),
	string_concat(FinalStr5,'.\'',FinalStr6),
	ObjectFormSizeColor=FinalStr6.
*/
					
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, whatrightstack, Parameter),
	  length(World, LengthWorld),LengthRest is Parameter + 1,
	  %stack picked is within bounds
	  (LengthRest < LengthWorld ->
		  length(Rest, LengthRest), append(Rest, RightStacks, World),
		  flatten(RightStacks,ListObjLetters),
		  maplist(getFormSizeColorText(_Objects),ListObjLetters,ObjectFormSizeColorList),%,write(ObjectFormSizeColor),
		  Plan = [[ObjectFormSizeColorList,what]]
		  %or not
		; Plan = [[[],what]]
	  ).
	  
getPlan([K,-1,move], Plan) :- Plan = ['I pick up the element at place . . . ', K, [pick, K]],nb_setval(output,'Success!').
getPlan([-1,K,move], Plan) :- Plan = ['I drop it down at place . . . ', K, [drop, K]],nb_setval(output,'Success!').
getPlan([K1,K2,move], Plan) :- Plan = ['I pick up the element at place . . . ', K1, [pick, K1], 'and I drop it down at place . . . ', K2, [drop, K2]],nb_setval(output,'Success!').
getPlan([L,where], Plan) :- Plan=[],list_string(L,LStr),string_concat('On place(s) . . . ',LStr,SuccesStr),nb_setval(output,SuccesStr).
getPlan([L,what], Plan) :- Plan=[],list_string(L,LStr),string_concat('The list of relevant object(s) is . . . ',LStr,SuccesStr),nb_setval(output,SuccesStr).
getPlan([N,count], Plan) :- Plan=[],list_string([N],LStr),string_concat('There is/are . . . ',LStr,SuccesStr1),string_concat(SuccesStr1,' Object(s).',SuccesStr2),nb_setval(output,SuccesStr2).
solve(PlanList, Plan) :- maplist(getPlan, PlanList, PlanAux),append(PlanAux, PlanAppend),
(PlanAppend ==[] ->
	Plan = @(null)
	;Plan=PlanAppend
).

list_codes([], "").

list_codes([Atom], Codes) :- atom_codes(Atom, Codes).

list_codes([Atom|ListTail], Codes) :-
        atom_codes(Atom, AtomCodes),
    append(AtomCodes, ",", AtomCodesWithComma),
    append(AtomCodesWithComma, ListTailCodes, Codes),
    list_codes(ListTail, ListTailCodes).

list_string(List, String) :-
    ground(List),
    list_codes(List, Codes),
    atom_codes(String, Codes).

list_string(List, String) :-
    ground(String),
    atom_codes(String, Codes),
    list_codes(List, Codes).
					

test27 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [what, is, right, of, stack, 1],
parse_all(command, Utterance, Trees),%write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),%write(Goals),
Goals = [Goal],
plan(Goal, World, Holding, Objects, PlanList),%write(PlanList).
solve(PlanList, Plan),write(Plan),nb_getval(output,OutputStr),nl,write(OutputStr).

test28 :-
	A = red,
	B = blue,
	atom_string(A,Astr),
	atom_string(B,Bstr),
	string_concat(Astr,' ',Cstr),
	string_concat(Cstr,Bstr,Dstr),
	write(Dstr).%,write(Bstr).

retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = where(Parameter),Action = where.
	
%% Where : the list of positions (indexes) of the objects
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, where, Parameter),
      maplist(whichListInTheWorld(World),Parameter,IdxList),
      Plan = [[IdxList,where]].	
	  
test29 :-
World = [[e],[l,g],[],[f,m,k],[]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue])
	]),
Utterance = [where, are, all, balls],
parse_all(command, Utterance, Trees),%write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),%write(Goals),
Goals = [Goal],
plan(Goal, World, Holding, Objects, PlanList),write(PlanList),
solve(PlanList, Plan),write(Plan).	

test30 :-
	nb_setval(output,'Success!'),
	%list_string(['a small black ball','a small blue box'],Str),write(Str).
	%atom_string('a small black ball',LStr),write(LStr).
	%etPlan([['a small black ball','a small blue box'],what], Plan),
	%nb_getval(output,OutputStr),nl,write(OutputStr).
	%PlanList=[[['a small black ball','a small blue box'],what]],
	PlanList=[[[0,1],where]],
	%PlanList=[[12,count]],
	solve(PlanList, Plan),nb_getval(output,OutputStr),nl,write(OutputStr).%,Plan==@(null)

%%Precision in case of ambiguity ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = [[Parameter]],Action = precision.
	
test31 :-
World = [[e],[l,g],[n],[f,m,k],[p]],
Holding = @(null),
Objects = json([
	a=json([form=brick,size=large,color=green]),
	b=json([form=brick,size=small,color=white]),
	c=json([form=plank,size=large,color=red]),
	d=json([form=plank,size=small,color=green]),
	e=json([form=ball,size=large,color=white]),
	f=json([form=ball,size=small,color=black]),
	g=json([form=table,size=large,color=blue]),
	h=json([form=table,size=small,color=red]),
	i=json([form=pyramid,size=large,color=yellow]),
	j=json([form=pyramid,size=small,color=red]),
	k=json([form=box,size=large,color=yellow]),
	l=json([form=box,size=large,color=red]),
	m=json([form=box,size=small,color=blue]),
	n=json([form=ball,size=small,color=red]),
	p=json([form=ball,size=large,color=blue])
	]),
Utterance = [take, the, ball],
parse_all(command, Utterance, Trees),write(Trees),
findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),write(Goals),nl,
%[take([e]),take([n]),take([f]),take([p])]
UtterancePrecision = [the, small, red, one],
parse_all(precision, UtterancePrecision, TreesPrecision),write(TreesPrecision),
findall(Goal, (member(Tree, TreesPrecision),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), GoalsPrecision),write(GoalsPrecision),nl.
