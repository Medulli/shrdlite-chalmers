#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

%% Test from the command line:
%% ./shrdlite.pl < ../examples/medium.json
%:- use_module(library(lists).
:- use_module(library(http/json)).
:- [dcg_parser].
:- [shrdlite_grammar].
:- style_check(-singleton).

main :-
    json_read(user_input, json(Input)),
    member(utterance=Utterance, Input),
    member(world=World, Input),
    member(holding=Holding, Input),
    member(objects=Objects, Input),

    parse_all(command, Utterance, Trees),
    ( Trees == [] ->
      Goals = @(null),
      Plan = @(null),
      Output = 'Parse error!'
    ;
      findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),
      ( Goals == [] ->
        Plan = @(null),
        Output = 'Interpretation error!'
      ; Goals = [_,_|_] ->
        Plan = @(null),
        Output = 'Ambiguity error!'
      ; Goals = [Goal],
        solve(Goal, World, Holding, Objects, Plan),
        Output = 'Success!'
      )
    ),
    findall(JT, (member(T, Trees),
                 write_to_codes(T, Cs),
                 atom_codes(JT, Cs)), JSONTrees),
    findall(GT, (member(G, Goals),
                 write_to_codes(G, Gs),
                 atom_codes(GT, Gs)), JSONGoals),
    Result = [utterance = Utterance,
              trees = JSONTrees,
              goals = JSONGoals,
              plan = Plan,
              output = Output],
    json_write(user_output, json(Result)).

%Take the head of the Kth list if the arm does not hold something
solve(_Goal, World, _Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, ActionTake, Element),
      _Holding == @(null),
      ActionTake == take,
      whichL(Element,World,K),
      nth0(K,World,LK),
      hdtlL(LK,Element),
/*      hdtlLL_at(World,K,NWorld,Element),*/
      Plan = ['I pick it up . . .', [pick, K], '. . . and I drop it down', [drop, K]].

%solve(_Goal, World, _Holding, _Objects, Plan) :-
%    nth1(Col, World, [_|_]),
%    Plan = ['I pick it up . . .', [pick, Col], '. . . and I drop it down', [drop, Col]].

/*	
test :-
Goal = movebeside([e],[g]),
retrieveGoalElements(Goal, Action, Parameter1,Parameter2),write(Action),write(Parameter1),write(Parameter2).

test2 :-
Goal = take([e]),
retrieveGoalElements(Goal, Action, Parameter),write(Action),write(Parameter).
*/

%%--------------------------------------------------------------

%split a list into its head and its tail
hdtlL([H|T],Element) :- H = Element.

%return the number K if X is in the Kth list of lists LL
%findall(X,whichL(a,[[d,e,f],[a,b,c]],X),R).

whichL(X,[L|_],0) :- member(X,L).
whichL(X,[_|LL],N) :- whichL(X,LL,M), N is M + 1.

%the last argument is the list given in second place where X was remove at place K

removeLL_at(X,[Y|Xs],N,[Y|Ys]) :- removeLL_at(X,Xs,M,Ys), N is M + 1.

%R is the list L in which X was added at place K

insertLL_at(X,L,K,R) :- removeLL_at(X,R,K,L).

%R is the list of lists LL in which X is added at the head of the Kth list of LL

consLL_at(X,LL,K,R) :- nth1(K,LL,L), removeLL_at(L,LL,K,Raux), insertLL_at([X|L],Raux,K,R).

%R is the list of lists LL in which the head X of the Kth list of LL was removed

hdtlLL_at(LL,K,R,X) :- nth1(K,LL,L), removeLL_at(L,LL,K,Raux), hdtlL(L,X,T), insertLL_at(T,Raux,K,R).

%We need to check if taking an object which is not the head of a list should be allowed

t2(X,[[X|Rx]|R] ,[Rx|R]).
t2(X,[[Fx|Rx]|R],[[Fx|Sx]|R]) :- t2(X,[Rx|R],[Sx|R]).
t2(X,[F|Rx] ,[F|Sx]) :- t2(X,Rx,Sx).

%%-------------------------- Retrieve Goal info

retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = take([Parameter]),Action = take.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = movebeside(Parameter1,Parameter2),Action = movebeside.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveleft(Parameter1,Parameter2),Action = moveleft.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveright(Parameter1,Parameter2),Action = moveright.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveabove(Parameter1,Parameter2),Action = moveabove.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveontop(Parameter1,Parameter2),Action = moveontop.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveunder(Parameter1,Parameter2),Action = moveunder.
	
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
	Goal = moveinside(Parameter1,Parameter2),Action = moveinside.

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
interpret(object(Type,Size,Color), World, Holding \== @(null), Objects, SelectedObject) :-
	json(AllPossibleObjects) = Objects,
	findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleWorldObjects),
	member(Holding = json([A1,A2,A3]),AllPossibleObjects),
	append(PossibleWorldObjects,Holding = json([A1,A2,A3]),PossibleObjects),
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
interpret(take(X), World, @(null), Objects, take(SelectedObject)) :-
    interpret(X, World, @(null), Objects, SelectedObject).

interpret(take(X), World, Holding \== @(null), Objects,  take(SelectedObject)) :-
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

%interpret(move(_X,Y), World, Holding, Objects, move(SelectedObject)) :-
%    interpret(Y, World, Holding, Objects, SelectedObject).	%for move we find desired object
%	interpret(X, World, Holding, Objects, SelectedObject).
%    t2(SelectedObject,World,SubGoal),						%remove it from world
%    t2(SelectedObject,Goal,SubGoal),						%add it back to all possible places
%    interpret(Y, Goal, Holding, Objects, SelectedObject).	%but it must satisfy the relation

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
/*
%Following LL will be the list of lists representing the world at the moment we want to execute an action, L the list representing the arm (it will be either [] or [o|[]] as we cannot take an object if we already hold one) at the moment we want to execute an action, NLL the list of lists representing the world after the execution of hte action, L the list representing the arm after the execution of the action

%Put the element holded at any place

putanyplace(O1,LL,[O1|L],NLL,L) :- nth1(K,LL,LK), canbeon(O1,LK), consLL_at(O1,LL,K,NLL).

%Put the element holded by the arm above the element O2

putabove(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2,LL,LK2), canbeon(O1,LK2), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm on top of the element O2

putontop(O1,O2,LL,[O1|L],NLL,L) :- canbeon(O1,[O2|-]), whichL(O2,LL,K2), nth1(K2,LL,LK2), hdtlL(LK2,O2,-), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm on top of the element O2 when O2 is not a topmost element

putontop(O1,O2,LL,[O1|L],NLL,L) :- canbeon(O1,[O2|-]), whichL(O2,LL,K2), nth1(K2,LL,LK2), hdtlL(LK2,H,T), moveanyplace(H,LL,[O1|L],LLaux1,Laux1), take(O1,LLaux1,Laux1,LLaux2,Laux2), putontop(O1,O2,LLaux2,Laux2,NLL,L).

%Put the element holded by the arm to the left the element O2

putleft(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2L,LL,LK2), K2L is K2 - 1, canbeon(O1,LK2), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm to the right the element O2

putright(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2R,LL,LK2), K2R is K2 + 1, canbeon(O1,LK2), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm beside the element O2

putbeside(O1,O2,LL,[O1|L],NLL,L) :- putleft(O1,O2,LL,[O1|L],NLL,L).
putbeside(O1,O2,LL,[O1|L],NLL,L) :- putright(O1,O2,LL,[O1|L],NLL,L).

%If the arm holds something and we want to take an object different from the one it holds we put it somewhere

take(O,LL,[H|T],NLL,NL) :- putabove(H,Oaux,LL,Kaux,[H|T],LLaux,T), take(O,LLaux,T,NLL,NL).

%If the arm does not hold something but the head of the list in which there is the element we want to take is not this element we move the head somewhere else

take(O,LL,[],NLL,L) :- whichL(O,LL,K), nth1(K,LL,LK), hdtlL(LK,H,-), move(H,Oaux,LL,[],LLaux,Laux), take(O,LLaux,Laux,NLL,L).

%Move O1 at any place

moveanyplace(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putanyplace(O1,LLaux,Laux,NLL,NL).

%Move O1 above O2

moveabove(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putabove(O1,O2,LLaux,Laux,NLL,NL).

%Move O1 on top of O2

moveontop(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putontop(O1,O2,LLaux,Laux,NLL,NL).

%Move O1 to the left of O2

moveleft(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putleft(O1,O2,LLaux,Laux,NLL,NL).

%Move O1 to the right of O2

moveright(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putright(O1,O2,LLaux,Laux,NLL,NL).

%Move O1 to the beside O2

movebeside(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putbeside(O1,O2,LLaux,Laux,NLL,NL).*/
