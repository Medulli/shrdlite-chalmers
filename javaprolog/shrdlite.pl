#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

%% Test from the command line:
%% ./shrdlite.pl < ../examples/medium.json
%:- use_module(library(lists).
:- use_module(library(http/json)).
:- [dcg_parser].
:- [shrdlite_grammar].


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


solve(_Goal, World, _Holding, _Objects, Plan) :-
    nth0(Col, World, [_|_]),
    Plan = ['I pick it up . . .', [pick, Col], '. . . and I drop it down', [drop, Col]].

%We need to check if taking an object which is not the head of a list should be allowed
t2(X,[[X|Rx]|R] ,[Rx|R]).
t2(X,[[Fx|Rx]|R],[[Fx|Sx]|R]) :- t2(X,[Rx|R],[Sx|R]).
t2(X,[F|Rx] ,[F|Sx]) :- t2(X,Rx,Sx).

%Finds object satisfying type size color by checking against a list of possible objects
%if Holding is empty we only have possible objects in world
interpret(object(Type,Size,Color), World, @(null), Objects, SelectedObject) :-
	json(AllPossibleObjects) = Objects,																							%get a list of (all) objects
	findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleObjects),	%find all objects and that are in world, Col is a list of objects (letters) in world, X is "the letter" of the objects in AllPossibleObjects, which must be a member of the list we're currently checking...
	getobj([Type,Size,Color], PossibleObjects, SelectedObject).																	%get the actual letter of the object we desired from the pool PossibleObjects

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

%find relative objects	
interpret(relative_entity(any,X, Relation), World, Holding, Objects, [SelectedObject]) :-
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux), %Find all relative objects
								 member(RelativeObjectAux, RelativeObjectListAuxAux)),
								 RelativeObjectListAux),
	sort(RelativeObjectListAux,RelativeObjectList),														 %Remove duplicates from list
	member(SelectedObject, RelativeObjectList),															 %Make one "instance" for every element 											
	interpret(X, World, Holding, Objects, SelectedObject).												 %Selected objects must also satisfy description
								
	
interpret(relative_entity(all,X, Relation), World, Holding, Objects, SelectedObject) :-
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux), 
								 member(RelativeObjectAux, RelativeObjectListAuxAux)),
								RelativeObjectListAux),
	sort(RelativeObjectListAux,RelativeObjectList),														 
    findall(SelectedObjectAux,( member(SelectedObjectAux, RelativeObjectList),							 %Find all objects which supports the relation
								interpret(X,        World, Holding, Objects, SelectedObjectAux)),
								SelectedObject).
    
interpret(relative_entity(the,X, Relation), World, Holding, Objects, [SelectedObject]) :-
	findall(RelativeObjectAux, ( interpret(Relation, World, Holding, Objects, RelativeObjectListAuxAux), 
								 member(RelativeObjectAux, RelativeObjectListAuxAux)),
								RelativeObjectListAux),
	sort(RelativeObjectListAux,RelativeObjectList),														 
    findall(SelectedObjectAux,( member(SelectedObjectAux, RelativeObjectList),							 %There should be only one object fitting the description and relation
								interpret(X,        World, Holding, Objects, SelectedObjectAux)),
								[SelectedObject]).

%find all objects satisfying relations
interpret(relative(beside,X), World, Holding, Objects, SelectedObject) :-
    interpret(X, World, Holding, Objects, RelativeObject),												%find the relative object satisfying type/size/col
	findall(SelectedObjectAux, 																			%find all objects satisfying the relation
	(member(RelativeObjectAux, RelativeObject), isbeside(SelectedObjectAux,RelativeObjectAux,World)),
	SelectedObject),SelectedObject \== [].																%Can result in an empty list, so add a condition to avoid that

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
interpret(take(X), World, @(null), Objects, take(SelectedObject/*,World,[],_,_*/)) :-
    interpret(X, World, @(null), Objects, SelectedObject).
	
interpret(take(X), World, Holding \== @(null), Objects,  take(SelectedObject/*,World,Holding,_,_*/)) :-
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
%    interpret(Y, Goal, Holding, Objects, SelectedObject).	%but it must satisfy the realtion

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
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),	%find list id for relative object
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),	%and all other objects
    (IdxS is IdxR-1;IdxS is IdxR+1).							%however they must satisfy this (to the left;or;to the right)
isleftof(X,Y,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),
    (IdxS < IdxR).
isrightof(X,Y,World) :-
    member(ColS,World),member(X,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(Y,ColR), nth0(IdxR,World,ColR),
    (IdxS > IdxR).
isabove(X,Y,World) :-
    member(Col,World),member(X,Col), member(Y, Col),	%basically same as beside/left/right, however the must belong to the same sub list
    nth0(IdxS, Col, X),									%get idx of objects
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

