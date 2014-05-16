:- style_check(-singleton).

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

%Move the selected object beside the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux - 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object beside the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux + 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object to the left the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux - 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object to the right the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      whichListInTheWorld(World,Element2,K2Aux),
      K2 is K2Aux + 1,
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object above the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object on top of the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveontop, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm \== box,
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      checkHead(LK2,Element2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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

%Move the selected object inside the relative object in one step if the arm holds it and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveinside, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold = Element1,
      getForm(Element2,_Objects,ObjectForm),
      ObjectForm == box,
      whichListInTheWorld(World,Element2,K2),
      nth0(K2,World,LK2),
      checkHead(LK2,Element2),
      canbeon(Element1,LK2,_Objects),
      dropAt(Element1,K2,World,NewWorld),
      b_setval(world, NewWorld),
      Plan = [[-1,K2,move]].

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
