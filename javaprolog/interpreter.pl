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

	% Use that for stacks
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

%%Utilities
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