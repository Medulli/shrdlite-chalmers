#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

:- use_module(library(http/json)).
:- [dcg_parser].
:- [shrdlite_grammar].
:- [interpreter].
:- style_check(-singleton).

main :-
    json_read(user_input, json(Input)),
    member(utterance=Utterance, Input),
    member(world=WorldJson, Input),
    member(holding=HoldingJson, Input),
    member(objects=Objects, Input),

    %reverse each list in the list of lists representing the world
    maplist(reverse,WorldJson,CurrentWorld),

    %set the global variables
    b_setval(world, CurrentWorld),
    b_setval(holding, HoldingJson),

    parse_all(command, Utterance, Trees),
    ( Trees == [] ->
      Goals = @(null),
      Plan = @(null),
      Output = 'Parse error!'
    ;

      b_getval(world,World),
      b_getval(holding,Holding),

      findall(Goal, (member(Tree, Trees),
                     interpret(Tree, World, Holding, Objects, Goal)
                    ), Goals),
      ( %Goals == take(List) -> "Please specify which object from list"
        Goals == [] ->
        Plan = @(null),
        Output = 'Interpretation error!'
      ; Goals = [take([_,_|_])] ->
        Plan = @(null),
Output = 'I can only hold one object!'
      ;
        %Goal is a list of goals i.e. "I can do this and this and this... Please specify what you want"
        Goals = [_,_|_] ->
handleAmbiguity(Goals,World,Holding,Objects,PrecisionGoal),
%we could not get a goal
(FinalGoal = [] ->
Plan = @(null),
Output = 'Ambiguity error, this object does not exist!'
%we have a goal !
;plan(PrecisionGoal, World, Holding, Objects, PlanList),
solve(PlanList, Plan),
nb_getval(output,Output)
)
      ; Goals = [Goal],
        plan(Goal, World, Holding, Objects, PlanList),
        solve(PlanList, Plan),
nb_getval(output,Output)
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

%Solver
getPlan([K,-1,move], Plan) :- Plan = ['I pick up the element at place . . . ', K, [pick, K]],nb_setval(output,'Success!').
getPlan([-1,K,move], Plan) :- Plan = ['I drop it down at place . . . ', K, [drop, K]],nb_setval(output,'Success!').
getPlan([K1,K2,move], Plan) :- Plan = ['I pick up the element at place . . . ', K1, [pick, K1], 'and I drop it down at place . . . ', K2, [drop, K2]],nb_setval(output,'Success!').
getPlan([L,where], Plan) :- Plan=[],list_string(L,LStr),string_concat('On place(s) . . . ',LStr,SuccesStr),nb_setval(output,SuccesStr).
getPlan([L,what], Plan) :- Plan=[],list_string(L,LStr),string_concat('The list of relevant object(s) is . . . ',LStr,SuccesStr),nb_setval(output,SuccesStr).
getPlan([N,count], Plan) :- Plan=[],list_string([N],LStr),string_concat('There is/are . . . ',LStr,SuccesStr1),string_concat(SuccesStr1,' Object(s).',SuccesStr2),nb_setval(output,SuccesStr2).
solve(PlanList, Plan) :- maplist(getPlan, PlanList, PlanAux),append(PlanAux, PlanAppend),
(PlanAppend ==[] ->
%no plan sent, just infos display
Plan = @(null)
%move dem objects nub
;Plan=PlanAppend
).

%Used to get rid of ambiguities
getCorrectGoal(Precision,PossibleGoal,Result) :- retrieveGoalElements(PossibleGoal, _, Parameter),
(Precision = Parameter -> Result = [PossibleGoal]
;Result = []
).
getCorrectGoal(Precision,PossibleGoal,Result) :- retrieveGoalElements(PossibleGoal, _, Parameter1,Parameter2),
((Precision = Parameter1;Precision = Parameter2) -> Result = [PossibleGoal]
;Result = []
).

getCorrectGoalList([X],PossibleGoalsList,FinalGoal) :- X = [Precision],maplist(getCorrectGoal(Precision),PossibleGoalsList,MatchingGoalsList),delete(MatchingGoalsList,[],FinalGoal).
getCorrectGoalList([X|R],PossibleGoalsList,FinalGoal) :- getCorrectGoalList([X],PossibleGoalsList,FinalGoalHead),
(FinalGoalHead = [] -> getCorrectGoalList(R,PossibleGoalsList,FinalGoal)
;FinalGoal = FinalGoalHead
).

handleAmbiguity(Goals,World,Holding,Objects,Plan,Output,FinalGoal) :-
%ask for a new input
%%TO BE CHECKED. Add a prompt message ?
json_read(user_input, json(InputPrecision)),
member(utterance=UtterancePrecision, InputPrecision),
%Parse it and find the corresponding object
parse_all(precision, UtterancePrecision, TreesPrecision),
findall(Goal, (member(Tree, TreesPrecision),
interpret(Tree, World, Holding, Objects, Goal)
), GoalsPrecision),
%if nothing found then raise error
(GoalsPrecision = [] ->
FinalGoal=[]
%else try to match the new object with the ones from the list of goals
;getCorrectGoalList(GoalsPrecision,Goals,FinalGoalList),
%nothing found, raise error
(FinalGoalList = [] ->
FinalGoal=[]
%else we have a goal !
;FinalGoalList = [FinalGoal]
)
).

%Take the selected object if the arm does not hold something
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, Element),
      Holding == @(null),
      whichListInTheWorld(World,Element,K),
      nth0(K,World,LK),
      checkHead(LK,Element),
      pickAt(K,World,NewWorld),
      nb_setval(world, NewWorld),
      b_setval(holding, [Element]),
      Plan = [[K,-1,move]].

%Take the selected object if the arm holds something
/*plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, ActionTake, ElementPick),
      Holding == [ElementDrop],
      ActionTake == take,
      canbeAt(ElementDrop,World,_Objects,KDrop),
      dropAt(ElementDrop,KDrop,World,WorldAux),
      whichListInTheWorld(WorldAux,ElementPick,KPick),
      nth0(KPick,WorldAux,LKPick),
      checkHead(LKPick,ElementPick),
      pickAt(KPick,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      nb_setval(holding, [ElementPick]),
      Plan = ['I drop it down', [drop, KDrop], '. . . and I pick it up . . .', [pick, KPick]].
      Plan = ['I drop it down', [drop, 0], '. . . and I pick it up . . .', [pick, 3]].*/

%Move the selected object beside the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      Holding == @(null),
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux - 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      Holding == @(null),
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux + 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object to the left the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, Element1, Element2),
      Holding == @(null),
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux - 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object to the right the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, Element1, Element2),
      Holding == @(null),
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux + 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object above the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, Element1, Element2),
      Holding == @(null),
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object on top of the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveontop, Element1, Element2),
      Holding == @(null),
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm \== box,
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      checkHead(LK2,Element2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object inside the relative object in one step if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveinside, Element1, Element2),
      Holding == @(null),
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm == box,
      whichListInTheWorld(World,Element1,K1),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      checkHead(LK2,Element2),
      canbeon(Element1,LK2,_Objects),
      pickAt(K1,World,WorldAux),
      dropAt(Element1,K2,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[K1,K2,move]].

%Move the selected object inside the relative object in several steps if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveinside, Element1, Element2),
      Holding == @(null),
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm == box,
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      checkHead(LK2,HdLK2),
      HdLK2 \== Element2,
      canbeAt(HdLK2,World,_Objects,K3),
      pickAt(K2,World,WorldAux),
      dropAt(HdLK2,K3,WorldAux,NewWorld),
      b_setval(world, NewWorld),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[K2,K3,move]|PlanAux].

%% Where : the list of positions (indexes) of the objects
plan(_Goal, World, _, _, Plan) :-
      retrieveGoalElements(_Goal, where, Parameter),
      maplist(whichListInTheWorld(World),Parameter,IdxList),
      Plan = [[IdxList,where]].	

%% Whatrightstack : the list of characteristics (form, size, color) of the objects
plan(_Goal, World, _, _Objects, Plan) :-
      retrieveGoalElements(_Goal, whatrightstack, Parameter),
length(World, LengthWorld),LengthRest is Parameter + 1,
%stack picked is within bounds
(LengthRest < LengthWorld ->
%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
length(Rest, LengthRest), append(Rest, RightStacks, World),
flatten(RightStacks,ListObjLetters),
maplist(getFormSizeColorText(_Objects),ListObjLetters,ObjectFormSizeColorList),
Plan = [[ObjectFormSizeColorList,what]]
%or not
; Plan = [[[],what]]
).

%%--------------------------------------------------------------

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

%%-------------------------- Retrieve Goal info

%% For stacks, Parameter is always the index of the stack

%%Move ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = movebeside([Parameter1],[Parameter2]),Action = movebeside.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveleft([Parameter1],[Parameter2]),Action = moveleft.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveright([Parameter1],[Parameter2]),Action = moveright.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveabove([Parameter1],[Parameter2]),Action = moveabove.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveontop([Parameter1],[Parameter2]),Action = moveontop.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveunder([Parameter1],[Parameter2]),Action = moveunder.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveinside([Parameter1],[Parameter2]),Action = moveinside.

%%%%% NOT IN THE PLANNER BELOW THIS LINE !

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveleftstack([Parameter1],[Parameter2]),Action = moveleftstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moverightstack([Parameter1],[Parameter2]),Action = moverightstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveabovestack([Parameter1],[Parameter2]),Action = moveabovestack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = moveontopstack([Parameter1],[Parameter2]),Action = moveontopstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = movebesidestack([Parameter1],[Parameter2]),Action = movebesidestack.

%%Count ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countbeside([Parameter1],[Parameter2]),Action = countbeside.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countleft([Parameter1],[Parameter2]),Action = countleft.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countright([Parameter1],[Parameter2]),Action = countright.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countabove([Parameter1],[Parameter2]),Action = countabove.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countontop([Parameter1],[Parameter2]),Action = countontop.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countunder([Parameter1],[Parameter2]),Action = countunder.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countinside([Parameter1],[Parameter2]),Action = countinside.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countleftstack([Parameter1],[Parameter2]),Action = countleftstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countrightstack([Parameter1],[Parameter2]),Action = countrightstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countabovestack([Parameter1],[Parameter2]),Action = countabovestack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countontopstack([Parameter1],[Parameter2]),Action = countontopstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countbesidestack([Parameter1],[Parameter2]),Action = countbesidestack.

%% /!\ Parameter is a list of stack numbers !
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = countinsidestacks(Parameter),Action = countinsidestacks.

retrieveGoalElements(Goal, Action, Parameter1) :-
Goal = countontop([Parameter1],floor),Action = countontopfloor.

%%here to get rid of warnings
retrieveGoalElements(Goal, Action, Parameter1) :-
Goal = moveontop([Parameter1],floor),Action = moveontopfloor.

%%Take ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = take([Parameter]),Action = take.

%%Where ---------------------------------------------------------------------------------------------------
%done
retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = where([Parameter]),Action = where.

%%What ---------------------------------------------------------------------------------------------------	
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatbeside([Parameter]),Action = whatbeside.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatleft([Parameter]),Action = whatleft.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatright([Parameter]),Action = whatright.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatabove([Parameter]),Action = whatabove.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatontop([Parameter]),Action = whatontop.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatunder([Parameter]),Action = whatunder.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatinside([Parameter]),Action = whatinside.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatontop([Parameter],floor),Action = whatontopfloor.	

%% /!\ Parameter is a list of stack numbers !
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatinsidestacks(Parameter),Action = whatinsidestacks.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatleftstack([Parameter]),Action = whatleftstack.

%done	
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatrightstack([Parameter]),Action = whatrightstack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatabovestack([Parameter]),Action = whatabovestack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatontopstack([Parameter]),Action = whatontopstack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatbesidestack([Parameter]),Action = whatbesidestack.

%---------------------------------------------------------------------------------------------------------------
/*	
test :-
Goal = movebeside([e],[g]),
retrieveGoalElements(Goal, Action, Parameter1,Parameter2),write(Action),write(Parameter1),write(Parameter2).

test2 :-
Goal = take([e]),
retrieveGoalElements(Goal, Action, Parameter),write(Action),write(Parameter).
*/
%----------------------------------------------------------------- Strings management
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

%The same than getFormSizeColor in text form	
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

%---------------------------------------------------------------------------------------------------- Constraints management ----------------------------------------------------------------------------------------------------
%Get the form, the size and the color of an object knowing its name (one letter) and the possible objects. Output : ObjectFormSizeColor=[form,size,color]
%Not used (yet)
getFormSizeColor(ObjectLetter,PossibleObjects,ObjectFormSizeColor) :-
PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),ObjectFormSizeColor=[FormObj,SizeObj,ColorObj].

%Get the form and the size of an object knowing its name (one letter) and the possible objects. Output : ObjectFormSize=[form,size]
getFormAndSize(ObjectLetter,PossibleObjects,ObjectFormSize) :-
PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=_]),ObjectFormSize=[FormObj,SizeObj].

%Get the form of an object knowing its name (one letter) and the possible objects. Output : ObjectForm=form
getForm(ObjectLetter,PossibleObjects,ObjectForm) :-
PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=ObjectForm,size=_,color=_]).

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
PossibleObjects = [a=json([form=ball,size=large,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]), d=json([form=box,size=large,color=blue]), e=json([form=box,size=medium,color=red])],
canbeon(a,[b,e,d],PossibleObjects).

%true
testTrue :-
PossibleObjects = [a=json([form=ball,size=small,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]), d=json([form=box,size=large,color=blue]), e=json([form=box,size=medium,color=red])],
canbeon(a,[e,d],PossibleObjects).

%true (floor testing)
testFloor :-
PossibleObjects = [a=json([form=ball,size=small,color=white]), b=json([form=box,size=small,color=black]), c=json([form=table,size=large,color=red]), d=json([form=box,size=large,color=blue]), e=json([form=box,size=medium,color=red])],
canbeon(a,[],PossibleObjects).
*/



/*
%Put the element held at any place

putanyplace(O1,LL,[O1|L],NLL,L) :- nth1(K,LL,LK), canbeon(O1,LK), consLL_at(O1,LL,K,NLL).

%Put the element holded by the arm on top of the element O2 when O2 is not a topmost element

putontop(O1,O2,LL,[O1|L],NLL,L) :- canbeon(O1,[O2|-]), whichL(O2,LL,K2), nth1(K2,LL,LK2), hdtlL(LK2,H,T), moveanyplace(H,LL,[O1|L],LLaux1,Laux1), take(O1,LLaux1,Laux1,LLaux2,Laux2), putontop(O1,O2,LLaux2,Laux2,NLL,L).

%If the arm holds something and we want to take an object different from the one it holds we put it somewhere

take(O,LL,[H|T],NLL,NL) :- putabove(H,Oaux,LL,Kaux,[H|T],LLaux,T), take(O,LLaux,T,NLL,NL).

%If the arm does not hold something but the head of the list in which there is the element we want to take is not this element we move the head somewhere else

take(O,LL,[],NLL,L) :- whichL(O,LL,K), nth1(K,LL,LK), hdtlL(LK,H,_), move(H,Oaux,LL,[],LLaux,Laux), take(O,LLaux,Laux,NLL,L).*/
