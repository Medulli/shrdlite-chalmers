#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

%% Test from the command line:
%% ./shrdlite.pl < ../examples/medium.json

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

bigger(large, medium).
bigger(medium, small).
bigger(z,x) :- bigger (z,y).bigger (y,x).


%Disc into box. Don't know if I can use the bigger rule here so I defined all the possibilities

canbeon(o,[h|t]) :- form(o,small,ball).form(h,small,box).
canbeon(o,[h|t]) :- form(o,small,ball).form(h,medium,box).
canbeon(o,[h|t]) :- form(o,small,ball).form(h,large,box).
canbeon(o,[h|t]) :- form(o,medium,ball).form(h,medium,box).
canbeon(o,[h|t]) :- form(o,medium,ball).form(h,large,box).
canbeon(o,[h|t]) :- form(o,large,ball).form(h,large,box).

%brick

canbeon(o,[h|t]) :- form(o,small,brick).form(h,medium,box).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,large,box).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,large,box).

canbeon(o,[h|t]) :- form(o,small,brick).form(h,small,brick).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,medium,brick).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,large,brick).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,medium,brick).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,large,brick).
canbeon(o,[h|t]) :- form(o,large,brick).form(h,large,brick).

canbeon(o,[h|t]) :- form(o,small,brick).form(h,small,plank).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,medium,plank).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,large,plank).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,medium,plank).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,large,plank).
canbeon(o,[h|t]) :- form(o,large,brick).form(h,large,plank).

canbeon(o,[h|t]) :- form(o,small,brick).form(h,small,table).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,small,brick).form(h,large,table).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,medium,brick).form(h,large,table).
canbeon(o,[h|t]) :- form(o,large,brick).form(h,large,table).



%pyramid

canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,medium,box).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,large,box).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,large,box).

canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,small,brick).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,medium,brick).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,large,brick).

canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,medium,brick).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,large,brick).
canbeon(o,[h|t]) :- form(o,large,pyramid).form(h,large,brick).

canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,small,plank).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,medium,plank).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,large,plank).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,medium,plank).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,large,plank).
canbeon(o,[h|t]) :- form(o,large,pyramid).form(h,large,plank).

canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,small,table).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,small,pyramid).form(h,large,table).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,medium,pyramid).form(h,large,table).
canbeon(o,[h|t]) :- form(o,large,pyramid).form(h,large,table).

%BOXES

canbeon(o,[h|t]) :- form(o,small,box).form(h,medium,box).
canbeon(o,[h|t]) :- form(o,small,box).form(h,large,box).
canbeon(o,[h|t]) :- form(o,medium,box).form(h,large,box).

canbeon(o,[h|t]) :- form(o,small,box).form(h,small,table).
canbeon(o,[h|t]) :- form(o,small,box).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,small,box).form(h,large,table).
canbeon(o,[h|t]) :- form(o,medium,box).form(h,medium,table).
canbeon(o,[h|t]) :- form(o,medium,box).form(h,large,table).
canbeon(o,[h|t]) :- form(o,large,box).form(h,large,table).

canbeon(o,[h|t]) :- form(o,small,box).form(h,small,planck).
canbeon(o,[h|t]) :- form(o,small,box).form(h,medium,planck).
canbeon(o,[h|t]) :- form(o,small,box).form(h,large,planck).
canbeon(o,[h|t]) :- form(o,medium,box).form(h,medium,planck).
canbeon(o,[h|t]) :- form(o,medium,box).form(h,large,planck).
canbeon(o,[h|t]) :- form(o,large,box).form(h,large,planck).


%All objects can stand on the floor whatever their size or form
canbeon(o,[]).

%DAN'S CODE HERE

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).
     
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).
 
t2(X,[[X|Rx]|R] ,[Rx|R]).
t2(X,[[Fx|Rx]|R],[[Fx|Sx]|R]) :- t2(X,[Rx|R],[Sx|R]).
t2(X,[F|Rx] ,[F|Sx])  :- t2(X,Rx,Sx).
 
interpret(Tree, World, Holding, Objects, Goal) :-
foo(Tree, World, Holding, Objects, Goal).
 
 
foo(object(Type,Size,Color), SelectedObject, World, Holding, Objects, _Goal):-
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
append(PossibleWorldObjects,Holding = json([A1,A2,A3]),PossibleObjects),
getobj([Type,Size,Color], PossibleObjects, SelectedObject)
).
 
 
foo(basic_entity(any,X), SelectedObject, World, Holding, Objects, Goal):-
%findall(SelectedObject, foo(X, SelectedObject, World, Objects, Goal), [SelectedObject|_]).
foo(X, SelectedObject, World, Holding, Objects, Goal).
 
foo(basic_entity(the,X), SelectedObject, World, Holding, Objects, Goal):-
%findall(SelectedObject, foo(X, SelectedObject, World, Objects, Goal), [SelectedObject]).
foo(X, SelectedObject, World, Holding, Objects, Goal).
 
foo(basic_entity(all,X), SelectedObject, World, Holding, Objects, Goal):-
foo(X, SelectedObject, World, Holding, Objects, Goal).
 
foo(relative_entity(any,X, Relation), SelectedObject, World, Holding, Objects, Goal):-
    foo(Relation, SelectedObject, World, Holding, Objects, Goal),
    foo(X, SelectedObject, World, Holding, Objects, Goal).
 
foo(relative_entity(the,X, Relation), SelectedObject, World, Holding, Objects, Goal):-
    findall(SelectedObject,( foo(Relation, SelectedObject, World, Holding, Objects, Goal),
                             foo(X, SelectedObject, World, Holding, Objects, Goal)),
                             [SelectedObject]).
     
foo(relative(beside,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),
    (IdxS is IdxR-1;IdxS is IdxR+1).
     
foo(relative(leftof,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),  
    (IdxS < IdxR).
     
foo(relative(rightof,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(ColS,World),member(SelectedObject,ColS), nth0(IdxS,World,ColS),
    member(ColR,World),member(RelativeObject,ColR), nth0(IdxR,World,ColR),  
    (IdxS > IdxR).
     
foo(relative(above,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS > IdxR).
     
foo(relative(ontop,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).
 
foo(relative(ontop,floor), SelectedObject, World, _Holding, _Objects, _Goal):-
    member(Col,World),member(SelectedObject,Col),
    nth0(IdxS, Col, SelectedObject),
    (IdxS is 0).
     
foo(relative(under,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS < IdxR).
     
foo(relative(inside,X), SelectedObject, World, Holding, Objects, Goal):-
    foo(X, RelativeObject, World, Holding, Objects, Goal),
    member(Col,World),member(SelectedObject,Col), member(RelativeObject, Col),
    nth0(IdxS, Col, SelectedObject),
    nth0(IdxR, Col, RelativeObject),
    (IdxS is IdxR+1).
 
foo(take(X), World, Holding, Objects, Goal):-
foo(X, SelectedObject, World, Holding, Objects, Goal),
t2(SelectedObject,World,Goal).
 
foo(move(X,Y), World, Holding, Objects, Goal):-
foo(X, SelectedObject, World, Holding, Objects, Goal),
t2(SelectedObject,World,SubGoal),
t2(SelectedObject,Goal,SubGoal),
foo(Y, SelectedObject, Goal, Holding, Objects, _).
%write([SelectedObject,RelativeObject]).
%write(Goal).
 
 
%------------------------------------------------------------------------------------------------------------------------%
getobj([Type,Size,Color],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=Type,size=Size,color=Color]), PossibleObjects).
 
getobj([Type,Size,-],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=Type,size=Size,color=_]), PossibleObjects).
 
getobj([Type,-,Color],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=Type,size=_,color=Color]), PossibleObjects).
 
getobj([Type,-,-],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=Type,size=_,color=_]), PossibleObjects).
 
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -%
 
getobj([anyform,Size,Color],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=_,size=Size,color=Color]), PossibleObjects).
 
getobj([anyform,Size,-],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=_,size=Size,color=_]), PossibleObjects).
 
getobj([anyform,-,Color],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=_,size=_,color=Color]), PossibleObjects).
 
getobj([anyform,-,-],PossibleObjects,SelectedObject) :-
member(SelectedObject=json([form=_,size=_,color=_]), PossibleObjects).
%------------------------------------------------------------------------------------------------------------------------%
