#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

:- use_module(library(http/json)).
:- [dcg_parser].
:- [shrdlite_grammar].
:- [interpreter].
:- [retrievegoal].
:- [canbeon].
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
/*    checkHead(Utterance,N),
    atom_number(N,X),
    integer(X),
    nl, write('Emeric est une petite salope'), nl,*/
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
		PrecisionMode = 'Activated',
		handleAmbiguity(Goals,World,Holding,Objects,PrecisionGoal),
		%we could not get a goal
		(FinalGoal = [] ->
			Plan = @(null),
			Output = 'Ambiguity error, this object does not exist!'
			%we have a goal !
			;plan(PrecisionGoal, World, Holding, Objects, PlanList),
                        ( PlanList == [-1] ->
                          Plan = @(null),
                          Output = 'Nothing to do!'
                        ; solve(PlanList, Plan),
			  nb_getval(output,Output)
                        )
		)
      ; Goals = [Goal],
        plan(Goal, World, Holding, Objects, PlanList),
        ( PlanList == [-1] ->
          Plan = @(null),
          Output = 'Nothing to do!'
        ; solve(PlanList, Plan),
          nb_getval(output,Output)
        )
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
getPlan([K,-1,move], Plan) :- Plan=[[pick,K]],list_string([K],LStr),string_concat('I pick up the element at place . . . ',LStr,SuccesStr1),string_concat(SuccesStr1,'.',SuccesStr2),nb_setval(output,SuccesStr2).
getPlan([-1,K,move], Plan) :- Plan=[[drop,K]],list_string([K],LStr),string_concat('I drop it down at place . . . ',LStr,SuccesStr1),string_concat(SuccesStr1,'.',SuccesStr2),nb_setval(output,SuccesStr2).
getPlan([K1,K2,move], Plan) :- Plan=[[pick,K1],[drop,K2]],list_string([K1],LStr1),string_concat('I pick up the element at place . . . ',LStr1,SuccesStr1),list_string([K2],LStr2),string_concat(SuccesStr1,' . . . and I drop it down at place . . . ',SuccesStr2),string_concat(SuccesStr2,LStr2,SuccesStr3),string_concat(SuccesStr3,'.',SuccesStr4),nb_setval(output,SuccesStr4).
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

handleAmbiguity(Goals,World,Holding,Objects,FinalGoal) :-
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

%Does nothing when asked to take the selected object if the arm already holds it
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding \== @(null),
      Holding = ElementPick,
      Plan = [-1].

%Take the selected object if the arm holds something
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding \== @(null),
      Holding = ElementHold,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

%Does nothing when asked to move the selected object beside the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      K3 is K1 + 1,
      K2 == K3,
      Plan = [-1].

%Does nothing when asked to move the selected object beside the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      K3 is K1 - 1,
      K2 == K3,
      Plan = [-1].

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

%Does nothing when asked to move the selected object to the left of the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, Element1, Element2),
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      K3 is K1 + 1,
      K2 == K3,
      Plan = [-1].

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

%Does nothing when asked to move the selected object to the right of the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, Element1, Element2),
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      K3 is K1 - 1,
      K2 == K3,
      Plan = [-1].

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

%Does nothing when asked to move the selected object above the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, Element1, Element2),
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      K1 == K2,
      Plan = [-1].

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

%Does nothing when asked to move the selected object on top of the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveontop, Element1, Element2),
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm \== box,
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      pickAt(K1,World,WorldAux),
      nth0(K2,WorldAux,LK2),
      checkHead(LK2,Element2),
      K1 == K2,
      Plan = [-1].

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

%Does nothing when asked to move the selected object inside the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveinside, Element1, Element2),
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm == box,
      whichListInTheWorld(World,Element1,K1),
      whichListInTheWorld(World,Element2,K2),
      nth0(K1,World,LK1),
      checkHead(LK1,Element1),
      pickAt(K1,World,WorldAux),
      nth0(K2,WorldAux,LK2),
      checkHead(LK2,Element2),
      K1 == K2,
      Plan = [-1].

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
