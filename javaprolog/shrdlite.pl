#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

%% Test from the command line:
%% ./shrdlite.pl < ../examples/Medium.json

:- use_module(library(lists).
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
    Result = [utterance = Utterance,
              trees = JSONTrees,
              goals = Goals,
              plan = Plan,
              output = Output],
    json_write(user_output, json(Result)).


solve(_Goal, World, _Holding, _Objects, Plan) :-
    nth0(Col, World, [_|_]),
    %Plan = [[pick, Col],[drop, Col]].
    (
    Holding == @(null)->
        Plan = [[pick, Col]];
        Plan = [[drop, Col]]
    ).

interpret(_Tree, _World, _Holding, _Objects, @(true)).

%split a list into its head and its tail
hdtlL([H|T],H,T).

%return the number K if X is in the Kth list of lists LL
%findall(X,whichL(a,[[d,e,f],[a,b,c]],X),R).

whichL(X,[L|_],1) :- member(X,L).
whichL(X,[_|LL],N) :- whichL(X,LL,M), N is M + 1.

%the last argument is the list given in second place where X was remove at place K

removeLL_at(X,[Y|Xs],N,[Y|Ys]) :- removeLL_at(X,Xs,M,Ys), N is M + 1.

%R is the list L in which X was added at place K

insertLL_at(X,L,K,R) :- removeLL_at(X,R,K,L).

%R is the list of lists LL in which X is added at the head of the Kth list of LL

consLL_at(X,LL,K,R) :- nth1(K,LL,L). removeLL_at(L,LL,K,Raux), insertLL_at([X|L],Raux,K,R).

%R is the list of lists LL in which the head X of the Kth list of LL was removed

hdtlLL_at(LL,K,R,X) :- nth1(K,LL,L), removeLL_at(L,LL,K,Raux), hdtlL(L,X,T), insertLL_at(T,Raux,K,R).

%We need to check if taking an object which is not the head of a list should be allowed

t2(X,[[X|Rx]|R] ,[Rx|R]).
t2(X,[[Fx|Rx]|R],[[Fx|Sx]|R]) :- t2(X,[Rx|R],[Sx|R]).
t2(X,[F|Rx] ,[F|Sx]) :- t2(X,Rx,Sx).

%Finds object satisfying type size color by checking against a list of possible objects

interpret(object(Type,Size,Color), SelectedObject, World, Holding, Objects, _Goal) :-

%if Holding is empty we only have possible objects in world

Holding == @(null) ->
(

%get a list of (all) objects

json(AllPossibleObjects) = Objects,

%find all objects and that are in world, Col is a list of objects (letters) in world, X is "the letter" of the objects in AllPossibleObjects, which must be a member of the list we're currently checking...

findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleObjects),

%get the actual letter of the object we desired from the pool PossibleObjects

getobj([Type,Size,Color], PossibleObjects, SelectedObject)
)
;
(

%If we're holding something, add that to the possible objects

json(AllPossibleObjects) = Objects,
findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleWorldObjects),
member(Holding = json([A1,A2,A3]),AllPossibleObjects),
append(PossibleWorldObjects,Holding = json([A1,A2,A3]),PossibleObjects),
getobj([Type,Size,Color], PossibleObjects, SelectedObject)
).

%All these are basically "any" or "all" object, I guess we could do something along findall(...,...,[Obj]) for the and findall(...,...,[Obj|_]) for any

interpret(basic_entity(any,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(basic_entity(the,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(basic_entity(all,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

%find relative objects	

interpret(relative_entity(any,X, Relation), SelectedObject, World, Holding, Objects, Goal) :-

%find objects that supports the relation, e.g. besides X is all Objects beside X.

    interpret(Relation, SelectedObject, World, Holding, Objects, Goal),

%the selected object shall satisfy the relation and the type/size/col

    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(relative_entity(the,X, Relation), SelectedObject, World, Holding, Objects, Goal) :-
    findall(SelectedObject,( interpret(Relation, SelectedObject, World, Holding, Objects, Goal),
                             interpret(X, SelectedObject, World, Holding, Objects, Goal)),
                             [SelectedObject]).

%find all objects satisfying relations

interpret(relative(beside,X), SelectedObject, World, Holding, Objects, Goal) :-

%find the relative object satisfying type/size/col

    interpret(X, RelativeObject, World, Holding, Objects, Goal),

%find list id for relative object

    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),

%and all other objects

    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),

%however they must satisfy this (to the left;or;to the right)

    (IdxS is IdxR-1;IdxS is IdxR+1).

interpret(relative(leftof,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),
    (IdxS < IdxR).

interpret(relative(rightof,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),
    (IdxS > IdxR).

interpret(relative(above,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),

%basically same as above, however the must belong to the same sub list

    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),

%get idx of objects

    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),

%must satisfy this

    (IdxS > IdxR).

interpret(relative(ontop,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).

interpret(relative(ontop,floor), SelectedObject, World, _Holding, _Objects, _Goal) :-
    member(Col,World),member(SelectedObject,Col),
    nth0(IdxS, Col, SelectedObject),
    (IdxS is 0).

interpret(relative(under,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS < IdxR).

interpret(relative(inside,X), SelectedObject, World, Holding, Objects, Goal) :-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).

interpret(take(X), World, Holding, Objects, Goal) :-

%take is a remove object from world. This will probably change.

    interpret(X, SelectedObject, World, Holding, Objects, Goal),
    t2(SelectedObject,World,Goal).

interpret(move(X,Y), World, Holding, Objects, Goal) :-

%for move we find desired object

    interpret(X, SelectedObject, World, Holding, Objects, Goal),

%remove it from world

    t2(SelectedObject,World,SubGoal),

%add it back to all possible places

    t2(SelectedObject,Goal,SubGoal),

%but it must satisfy the realtion

    interpret(Y, SelectedObject, Goal, Holding, Objects, _).
	
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
getobj([-,Size,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=Size,color=Color]), PossibleObjects).

getobj([-,Size,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=Size,color=_]), PossibleObjects).

getobj([-,-,Color],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=_,color=Color]), PossibleObjects).

getobj([-,-,-],PossibleObjects,SelectedObject) :-
    member(SelectedObject=json([form=_,size=_,color=_]), PossibleObjects).
%------------------------------------------------------------------------------------------------------------------------%

%Balls

canbeon(O,[H|-]) :- getobj([Ball,Small,-],PossibleObjects,O), getobj([Box,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Ball,Small,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Ball,Small,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Ball,Medium,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Ball,Medium,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Ball,Large,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).

%Boxes

canbeon(O,[H|-]) :- getobj([Box,Small,-],PossibleObjects,O), getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Box,Small,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Box,Medium,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Box,Large,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Box,Small,-],PossibleObjects,O), getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Box,Medium,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Box,Large,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Box,Large,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).

%Bricks

canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Large,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Large,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Small,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Medium,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Brick,Large,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).

%Planks

canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Large,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Large,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Small,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Medium,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Plank,Large,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).

%Pyramids

canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Large,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Large,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Small,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Medium,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Pyramid,Large,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).

%Tables

canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Box,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Large,-],PossibleObjects,O), getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Large,-],PossibleObjects,O), getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Large,-],PossibleObjects,O), getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Small,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Medium,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|-]) :- getobj([Table,Large,-],PossibleObjects,O), getobj([Plank,Large,-],PossibleObjects,H).

%All objects can stand on the floor whatever their size or form

canbeon(O,[]).

%Put the element holded at any place

putanyplace(O1,LL,[O1|L],NLL,L) :- nth1(K,LL,LK), canbeon(O1,LK), consLL_at(O1,LL,K,NLL).

%Put the element holded by the arm above the element O2

putabove(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2,LL,LK2), canbeon(O1,LK2), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm on top of the element O2

putontop(O1,O2,LL,[O1|L],NLL,L) :- canbeon(O1,[O2|-]), whichL(O2,LL,K2), nth1(K2,LL,LK2), hdtlL(LK2,O2,-), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm on top of the element O2 when O2 is not a topmost element

putontop(O1,O2,LL,[O1|L],NLL,L) :- canbeon(O1,[O2|-]), whichL(O2,LL,K2), nth1(K2,LL,LK2), hdtlL(LK2,H,T), moveanyplace(H,LL,[O1|L],LLaux1,Laux1), take(O1,LLaux1,Laux1,LLaux2,Laux2), putontop(O1,O2,LLaux2,Laux2,NLL,L)

%Put the element holded by the arm to the left the element O2

putleft(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2L,LL,LK2), K2L is K2 - 1, canbeon(O1,LK2), consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm to the right the element O2

putright(O1,O2,LL,[O1|L],NLL,L) :- whichL(O2,LL,K2), nth1(K2R,LL,LK2), K2R is K2 + 1, canbeon(O1,LK2, consLL_at(O1,LL,K2,NLL).

%Put the element holded by the arm beside the element O2

putbeside(O1,O2,LL,[O1|L],NLL,L) :- putleft(O1,O2,LL,[O1|L],NLL,L).
putbeside(O1,O2,LL,[O1|L],NLL,L) :- putright(O1,O2,LL,[O1|L],NLL,L).

%Take the head of the Kth list if the arm does not hold something

take(O,LL,[],NLL,[O]) :- whichL(O,LL,K), nth1(K,LL,LK), hdtlL(LK,O,-), hdtlLL_at(LL,K,NLL,O).

%If the arm holds something and we want to take an object different from the one it holds we put it somewhere

take(O,LL,K,[H|T],NLL,NL) :- putabove(H,Oaux,LL,Kaux,[H|T],LLaux,T), take(O,LLaux,T,NLL,NL).

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

movebeside(O1,O2,LL,L,NLL,NL) :- take(O1,LL,L,LLaux,Laux), putbeside(O1,O2,LLaux,Laux,NLL,NL).
