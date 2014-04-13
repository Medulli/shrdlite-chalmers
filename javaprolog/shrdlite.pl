#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

%% Test from the command line:
%% ./shrdlite.pl < ../examples/Medium.json

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

%Put

%All objects can stand on the floor whatever their size or form
canbeon(O,[]).

%If the robot wants to put an object on a stack he needs to hold it AND to be able to put in on the stack with the canbeon predicates

put(O,L,[O|L]) :- canbeon(O,L).

%You can put whatever you want on the floor

put(O,[],[O|T]).

%Take


appendL([],X,X).
appendL([X|Y],Z,[X|W]) :- appendL(Y,Z,W).

hdtlL([H|T],H,T).

%elementL(X,L) means that X is an element of L

elementL(X,[X|-]).
elementL(X,[-|L]) :- elementL(X,L).

%return the element X which is at place K in L

elementLL_at(X,[X|-],1).
elementLL_at(X,[-|L],K+1) :- elementLL_at(X,L,K).

%return the head of the list at place K in L

headLL_at(X,LL,K) :- elementLL_at(L,LL,K).hdtlL(L,X,-).

%the last argument is the list given in second place where X was remove at place K

removeLL_at(X,[Y|Xs],K+1,[Y|Ys]) :- remove_at(X,Xs,K,Ys).

%R is the list L in which X was added at place K

insertLL_at(X,L,K,R) :- removeLL_at(X,R,K,L).

%R is the list of lists LL in which X is added at the head of the Kth list of LL

consLL_at(X,LL,K,R) :- elementLL_at(L,LL,K).removeLL_at(L,LL,K,Raux).insertLL_at([X|L],Raux,R).

%R is the list of lists LL in which the head X of the Kth list of LL was removed

hdtlLL_at(LL,K,R,X) :- elementLL_at(L,LL,K).removeLL_at(L,LL,K,Raux).hdtlL(L,X,T).insertLL_at(T,Raux,K,R).

%We need to check if taking an object which is not the head of a list should be allowed
t2(X,[[X|Rx]|R] ,[Rx|R]).
t2(X,[[Fx|Rx]|R],[[Fx|Sx]|R]) :- t2(X,[Rx|R],[Sx|R]).
t2(X,[F|Rx] ,[F|Sx]) :- t2(X,Rx,Sx).

interpret(object(Type,Size,Color), SelectedObject, World, Holding, Objects, _Goal):-
Holding == @(null) ->
(
json(AllPossibleObjects) = Objects, 
findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleObjects),
getobj([Type,Size,Color], PossibleObjects, SelectedObject)
)
;
(
json(AllPossibleObjects) = Objects, 
findall(X=json([A,B,C]), (member(Col,World),member(X=json([A,B,C]),AllPossibleObjects),member(X,Col)), PossibleWorldObjects),
member(Holding = json([A1,A2,A3]),AllPossibleObjects),
appendL(PossibleWorldObjects,Holding = json([A1,A2,A3]),PossibleObjects),
getobj([Type,Size,Color], PossibleObjects, SelectedObject)
).

interpret(basic_entity(any,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(basic_entity(the,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(basic_entity(all,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(relative_entity(any,X, Relation), SelectedObject, World, Holding, Objects, Goal):-
    interpret(Relation, SelectedObject, World, Holding, Objects, Goal),
    interpret(X, SelectedObject, World, Holding, Objects, Goal).

interpret(relative_entity(the,X, Relation), SelectedObject, World, Holding, Objects, Goal):-
    findall(SelectedObject,( interpret(Relation, SelectedObject, World, Holding, Objects, Goal),
                             interpret(X, SelectedObject, World, Holding, Objects, Goal)),
                             [SelectedObject]).

interpret(relative(beside,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),
    (IdxS is IdxR-1;IdxS is IdxR+1).

interpret(relative(leftof,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),  
    (IdxS < IdxR).

interpret(relative(rightof,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),  
    (IdxS > IdxR).

interpret(relative(above,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS > IdxR).

interpret(relative(ontop,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).

interpret(relative(ontop,floor), SelectedObject, World, _Holding, _Objects, _Goal):-
    member(Col,World),member(SelectedObject,Col),
    nth0(IdxS, Col, SelectedObject),
    (IdxS is 0).

interpret(relative(under,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS < IdxR).

interpret(relative(inside,X), SelectedObject, World, Holding, Objects, Goal):-
    interpret(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).

interpret(take(X), World, Holding, Objects, Goal):-
    interpret(X, SelectedObject, World, Holding, Objects, Goal),
    t2(SelectedObject,World,Goal).

interpret(move(X,Y), World, Holding, Objects, Goal):-
    interpret(X, SelectedObject, World, Holding, Objects, Goal),
    t2(SelectedObject,World,SubGoal),
    t2(SelectedObject,Goal,SubGoal),
    interpret(Y, SelectedObject, Goal, Holding, Objects, _).
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

canbeon(O,[H|T]) :- getobj([Ball,Small,-],PossibleObjects,O). getobj([Box,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Ball,Small,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Ball,Small,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Ball,Medium,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Ball,Medium,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Ball,Large,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).

%Boxes

canbeon(O,[H|T]) :- getobj([Box,Small,-],PossibleObjects,O). getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Box,Small,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Box,Medium,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Box,Large,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Box,Small,-],PossibleObjects,O). getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Box,Medium,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Box,Large,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Box,Large,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).

%Bricks

canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Large,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Large,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Small,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Medium,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Brick,Large,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).

%Planks

canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Large,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Large,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Small,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Medium,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Plank,Large,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).

%Pyramids

canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Large,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Large,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Small,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Medium,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Pyramid,Large,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).

%Tables

canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Box,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Box,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Large,-],PossibleObjects,O). getobj([Box,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Brick,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Brick,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Large,-],PossibleObjects,O). getobj([Brick,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Table,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Table,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Large,-],PossibleObjects,O). getobj([Table,Large,-],PossibleObjects,H).

canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Plank,Small,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Small,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Plank,Medium,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Medium,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
canbeon(O,[H|T]) :- getobj([Table,Large,-],PossibleObjects,O). getobj([Plank,Large,-],PossibleObjects,H).
