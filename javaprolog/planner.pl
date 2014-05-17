:- [canbeon].
:- [retrievegoal].
:- [planner_tools].
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
      b_setval(holding, [ElementPick]),
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

%Move the selected object beside the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
      b_setval(holding, @(null)),
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
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

%Move the selected object to the left the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

%Move the selected object to the right the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

%Move the selected object above the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

%Move the selected object on top of the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveontop, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

%Move the selected object inside the relative object in one step if the arm holds something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveinside, Element1, Element2),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== Element1,
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

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
%% What ----------------------------------------------------------------------

%What right (every stacks on the right)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatright, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	length(World, LengthWorld),LengthRest is Position + 1,
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

%What left (every stacks on the left)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatleft, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	length(World, LengthWorld),
	%stack picked is within bounds
	((Position >= 0,Position < LengthWorld )->
		%World is split into 2 parts : Left with the left until the stack, RightStacks with everything we want to examine.
		length(Left, Position), append(Left, RightStacks, World),
		flatten(Left,ListObjLetters),
		maplist(getFormSizeColorText(_Objects),ListObjLetters,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
		%or not
		; Plan = [[[],what]]
	).

plan(_Goal, World, _, _Objects, Plan) :-
	retrieveGoalElements(_Goal, whatbeside, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	length(World, LengthWorld),LeftPos is Position - 1,RightPos is Position + 1,
	%stack picked is within bounds
	%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
	(LeftPos >= 0 ->
		nth0(LeftPos,World,LeftStack),
		((LeftStack = [],ObjectFormSizeColorListLeft = [])
			;flatten(LeftStack,ListObjLettersLeft),
			maplist(getFormSizeColorText(_Objects),ListObjLettersLeft,ObjectFormSizeColorListLeft)
		)
		;ObjectFormSizeColorListLeft = []
	),
	(RightPos < LengthWorld ->
		nth0(RightPos,World,RightStack),
		((RightStack = [],ObjectFormSizeColorListRight = [])
			;flatten(RightStack,ListObjLettersRight),
			maplist(getFormSizeColorText(_Objects),ListObjLettersRight,ObjectFormSizeColorListRight)
		)
		;ObjectFormSizeColorListRight = []
	),
	(ObjectFormSizeColorListLeft =[] ->
		Str1 = ['nothing on the left']
		;append(ObjectFormSizeColorListLeft,[' on the left'],Str1)
	),
	(ObjectFormSizeColorListRight =[] ->
		append(Str1,[' nothing on the right.'],ObjectFormSizeColorList)
		;append(Str1,ObjectFormSizeColorListRight,Str2),append(Str2,[' on the right.'],ObjectFormSizeColorList)
	),
	Plan = [[ObjectFormSizeColorList,what]].

%What above (everything above)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatabove, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, Parameter),
	%get the list of objects
	(StackPosition >= 0 ->
		length(LeftStack, StackPosition), append(LeftStack, RightStack, Stack),
		maplist(getFormSizeColorText(_Objects),LeftStack,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
		;Plan = [[[],what]]
	).
	
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
