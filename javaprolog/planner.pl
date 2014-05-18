:- [canbeon].
:- [retrievegoal].
:- [planner_tools].
:- style_check(-singleton).

%% Take ----------------------------------------------------------------------
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== ElementPick,
      whichListInTheWorld(World,ElementPick,KPick),
      ( canbeMoved(ElementHold,KPick,World,_Objects,K) ->

        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        b_setval(holding, @(null)),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]

      ; canbeAt(ElementHold,World,_Objects,K),
        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        b_setval(holding, @(null)),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      nth0(KPick,World,LK),
      checkHead(LK,ElementTop),
      ElementTop == ElementPick,
      pickAt(KPick,World,NewWorld),
      nb_setval(world, NewWorld),
      b_setval(holding, [ElementPick]),
      Plan = [[KPick,-1,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      nth0(KPick,World,LK),
      checkHead(LK,ElementTop),
      ElementTop \== ElementPick,
      canbeMoved(ElementTop,KPick,World,_Objects,KDrop),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementTop,KDrop,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDrop,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, take, ElementPick),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      nth0(KPick,World,LK),
      checkHead(LK,ElementTop),
      ElementTop \== ElementPick,
      not(canbeMoved(ElementTop,KPick,World,_Objects,KDrop)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      canbeMoved(ElementPickAux,KPickAux,World,_Objects,KDropAux),
      pickAt(KPickAux,World,WorldAux),
      dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPickAux,KDropAux,move]|PlanAux].

%% Moveleft ----------------------------------------------------------------------
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux - 1,
      nth0(K2M,World,LK2M),
      canbeon(ElementHold,LK2M,_Objects),
      dropAt(ElementHold,K2M,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      Plan = [[-1,K2M,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux - 1,
      nth0(K2M,World,LK2M),
      not(canbeon(ElementHold,LK2M,_Objects)),
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== ElementPick,
      whichListInTheWorld(World,ElementPick,KPick),
      ( canbeMoved(ElementHold,KPick,World,_Objects,K) ->

        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]

      ; canbeAt(ElementHold,World,_Objects,K),
        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick == KDropM,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      canbeAt(ElementPick,World,_Objects,KDropM),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDropM,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      Plan = [[KPick,KDropM,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux),
      pickAt(KDropM,World,WorldAux),
      canbeMoved(ElementTopAux,KDropM,WorldAux,_Objects,KDropAux),
      dropAt(ElementTopAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KDropM,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux),
      not(canbeMoved(ElementTopAux,KDropM,World,_Objects,K)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      ( canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropM) ->

        dropAt(ElementPickAux,KDropM,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropM,move]|PlanAux]

      ; canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropAux),
        dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropAux,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      pickAt(KPick,World,WorldAux),
      canbeMoved(ElementTop,KPick,WorldAux,_Objects,KDropAux),
      dropAt(ElementTop,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveleft, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      not(canbeMoved(ElementTop,KPick,World,_Objects,KDropAux)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      canbeAt(ElementPickAux,WorldAux,_Objects,KDropAux),
      KPickAux \== KDropAux,
      dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPickAux,KDropAux,move]|PlanAux].

%% Moveright ----------------------------------------------------------------------
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux + 1,
      nth0(K2M,World,LK2M),
      canbeon(ElementHold,LK2M,_Objects),
      dropAt(ElementHold,K2M,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      Plan = [[-1,K2M,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux + 1,
      nth0(K2M,World,LK2M),
      not(canbeon(ElementHold,LK2M,_Objects)),
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== ElementPick,
      whichListInTheWorld(World,ElementPick,KPick),
      ( canbeMoved(ElementHold,KPick,World,_Objects,K) ->

        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]

      ; canbeAt(ElementHold,World,_Objects,K),
        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick == KDropM,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      canbeAt(ElementPick,World,_Objects,KDropM),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDropM,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      Plan = [[KPick,KDropM,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux),
      pickAt(KDropM,World,WorldAux),
      canbeMoved(ElementTopAux,KDropM,WorldAux,_Objects,KDropAux),
      dropAt(ElementTopAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KDropM,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux),
      not(canbeMoved(ElementTopAux,KDropM,World,_Objects,K)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      ( canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropM) ->

        dropAt(ElementPickAux,KDropM,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropM,move]|PlanAux]

      ; canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropAux),
        dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropAux,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      pickAt(KPick,World,WorldAux),
      canbeMoved(ElementTop,KPick,WorldAux,_Objects,KDropAux),
      dropAt(ElementTop,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveright, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick \== KDropM,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      not(canbeMoved(ElementTop,KPick,World,_Objects,KDropAux)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      canbeAt(ElementPickAux,WorldAux,_Objects,KDropAux),
      KPickAux \== KDropAux,
      dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPickAux,KDropAux,move]|PlanAux].

%% Movebeside ----------------------------------------------------------------------
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux - 1,
      nth0(K2M,World,LK2M),
      canbeon(ElementHold,LK2M,_Objects),
      dropAt(ElementHold,K2M,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      Plan = [[-1,K2M,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux + 1,
      nth0(K2M,World,LK2M),
      canbeon(ElementHold,LK2M,_Objects),
      dropAt(ElementHold,K2M,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      Plan = [[-1,K2M,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2Aux),
      K2M is K2Aux - 1,
      nth0(K2M,World,LK2M),
      not(canbeon(ElementHold,LK2M,_Objects)),
      K2P is K2Aux + 1,
      nth0(K2P,World,LK2P),
      not(canbeon(ElementHold,LK2P,_Objects)),
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== ElementPick,
      whichListInTheWorld(World,ElementPick,KPick),
      ( canbeMoved(ElementHold,KPick,World,_Objects,K) ->

        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]

      ; canbeAt(ElementHold,World,_Objects,K),
        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick == KDropM,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop + 1,
      KPick == KDropM,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      canbeAt(ElementPick,World,_Objects,KDropM),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDropM,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      Plan = [[KPick,KDropM,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      canbeAt(ElementPick,World,_Objects,KDropP),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDropP,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      Plan = [[KPick,KDropP,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      not(canbeAt(ElementPick,World,_Objects,KDropP)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux),
      pickAt(KDropM,World,WorldAux),
      canbeMoved(ElementTopAux,KDropM,WorldAux,_Objects,KDropAux),
      dropAt(ElementTopAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KDropM,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      not(canbeAt(ElementPick,World,_Objects,KDropP)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux1),
      not(canbeMoved(ElementTopAux1,KDropM,World,_Objects,KDropAux1)),
      nth0(KDropP,World,LKDropP),
      checkHead(LKDropP,ElementTopAux2),
      pickAt(KDropP,World,WorldAux),
      canbeMoved(ElementTopAux2,KDropM,WorldAux,_Objects,KDropAux2),
      dropAt(ElementTopAux2,KDropAux2,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KDropP,KDropAux2,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDropM)),
      not(canbeAt(ElementPick,World,_Objects,KDropP)),
      nth0(KDropM,World,LKDropM),
      checkHead(LKDropM,ElementTopAux1),
      nth0(KDropP,World,LKDropP),
      checkHead(LKDropP,ElementTopAux2),
      not(canbeMoved(ElementTopAux1,KDropM,World,_Objects,K1)),
      not(canbeMoved(ElementTopAux2,KDropP,World,_Objects,K2)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      ( canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropM) ->

        dropAt(ElementPickAux,KDropM,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropM,move]|PlanAux]

      ; ( canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropP) ->

          dropAt(ElementPickAux,KDropP,WorldAux,NewWorld),
          nb_getval(listOfVisitedWorlds,Tail),
          not(member(NewWorld,Tail)),
          b_setval(world, NewWorld),
          nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
          plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
          Plan = [[KPickAux,KDropP,move]|PlanAux]

        ; canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropAux),
          dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
          nb_getval(listOfVisitedWorlds,Tail),
          not(member(NewWorld,Tail)),
          b_setval(world, NewWorld),
          nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
          plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
          Plan = [[KPickAux,KDropAux,move]|PlanAux]
        )
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      pickAt(KPick,World,WorldAux),
      canbeMoved(ElementTop,KPick,WorldAux,_Objects,KDropAux),
      dropAt(ElementTop,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, movebeside, ElementPick, ElementDrop),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KDropM is KDrop - 1,
      KPick \== KDropM,
      KDropP is KDrop + 1,
      KPick \== KDropP,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      not(canbeMoved(ElementTop,KPick,World,_Objects,KDropAux)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      canbeAt(ElementPickAux,WorldAux,_Objects,KDropAux),
      KPickAux \== KDropAux,
      dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPickAux,KDropAux,move]|PlanAux].

%% Moveabove ----------------------------------------------------------------------
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2),
      nth0(K2,World,LK2),
      canbeon(ElementHold,LK2,_Objects),
      dropAt(ElementHold,K2,World,NewWorld),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      Plan = [[-1,K2,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold == ElementPick,
      whichListInTheWorld(World,ElementDrop,K2),
      nth0(K2,World,LK2),
      not(canbeon(ElementHold,LK2,_Objects)),
      canbeAt(ElementHold,World,_Objects,K),
      dropAt(ElementHold,K,World,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      b_setval(holding, @(null)),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
      Plan = [[-1,K,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding \== @(null),
      Holding = ElementHold,
      ElementHold \== ElementPick,
      whichListInTheWorld(World,ElementPick,KPick),
      ( canbeMoved(ElementHold,KPick,World,_Objects,K) ->

        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]

      ; canbeAt(ElementHold,World,_Objects,K),
        dropAt(ElementHold,K,World,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        b_setval(holding, @(null)),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, @(null), _Objects, PlanAux),
        Plan = [[-1,K,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick == KDrop,
      Plan = [-1].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick \== KDrop,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      canbeAt(ElementPick,World,_Objects,KDrop),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDrop,WorldAux,NewWorld),
      nb_setval(world, NewWorld),
      Plan = [[KPick,KDrop,move]].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick \== KDrop,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDrop)),
      nth0(KDropM,World,LKDrop),
      checkHead(LKDrop,ElementTopAux),
      pickAt(KDrop,World,WorldAux),
      canbeMoved(ElementTopAux,KDrop,WorldAux,_Objects,KDropAux),
      dropAt(ElementTopAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KDrop,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick \== KDrop,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop == ElementPick,
      not(canbeAt(ElementPick,World,_Objects,KDrop)),
      nth0(KDrop,World,LKDrop),
      checkHead(LKDrop,ElementTopAux),
      not(canbeMoved(ElementTopAux,KDrop,World,_Objects,K)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      ( canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDrop) ->

        dropAt(ElementPickAux,KDrop,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDrop,move]|PlanAux]

      ; canbeMoved(ElementPickAux,KPickAux,WorldAux,_Objects,KDropAux),
        dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
        nb_getval(listOfVisitedWorlds,Tail),
        not(member(NewWorld,Tail)),
        b_setval(world, NewWorld),
        nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
        plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
        Plan = [[KPickAux,KDropAux,move]|PlanAux]
      ).

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick \== KDrop,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      pickAt(KPick,World,WorldAux),
      canbeMoved(ElementTop,KPick,WorldAux,_Objects,KDropAux),
      dropAt(ElementTop,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDropAux,move]|PlanAux].

plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveabove, ElementPick, ElementDrop, World),
      Holding == @(null),
      whichListInTheWorld(World,ElementPick,KPick),
      whichListInTheWorld(World,ElementDrop,KDrop),
      KPick \== KDrop,
      nth0(KPick,World,LKPick),
      checkHead(LKPick,ElementTop),
      ElementTop \== ElementPick,
      not(canbeMoved(ElementTop,KPick,World,_Objects,KDropAux)),
      member(Stack,World),
      checkHead(Stack,ElementPickAux),
      whichListInTheWorld(World,ElementPickAux,KPickAux),
      pickAt(KPickAux,World,WorldAux),
      canbeAt(ElementPickAux,WorldAux,_Objects,KDropAux),
      KPickAux \== KDropAux,
      dropAt(ElementPickAux,KDropAux,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPickAux,KDropAux,move]|PlanAux].

%Does nothing when asked to move the selected object on top of the relative object in one step if it is already the case
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, moveontop, Element1, Element2, World),
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
      retrieveGoalElements(_Goal, moveontop, Element1, Element2, World),
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
      retrieveGoalElements(_Goal, moveontop, Element1, Element2, World),
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
      retrieveGoalElements(_Goal, moveontop, Element1, Element2, World),
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
      getForm(Element2,_Objects,box),
      ( Holding == @(null) ->

        whichListInTheWorld(World,Element1,K1),
        whichListInTheWorld(World,Element2,K2),

        ( K1 == K2 ->

          nth0(K1,World,LK1),
          checkHead(LK1,Element1),
          pickAt(K1,World,WorldAux),
          nth0(K2,WorldAux,LK2),
          checkHead(LK2,Element2),
          Plan = [-1]
          %We probably need to handle the possibility of having the selected object above the relative object but not inside

        ; nth0(K1,World,LK1),
          nth0(K2,World,LK2),

          ( checkHead(LK1,Element1) ->

            ( checkHead(LK2,Element2) ->

              ( canbeon(Element1,LK2,_Objects) ->
                pickAt(K1,World,WorldAux),
                dropAt(Element1,K2,WorldAux,NewWorld),
                b_setval(world, NewWorld),
                Plan = [[K1,K2,move]]

              ; member(Stack,World),
                checkHead(Stack,ElementPick),
                whichListInTheWorld(World,ElementPick,KPick),
                canbeAt(ElementPick,World,_Objects,KDrop),
                pickAt(KPick,World,WorldAux),
                dropAt(ElementPick,KDrop,WorldAux,NewWorld),
                nb_getval(listOfVisitedWorlds,Tail),
                not(member(NewWorld,Tail)),
                b_setval(world, NewWorld),
                nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
                plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
                Plan = [[KPick,KDrop,move]|PlanAux]
              )
            ; member(Stack,World),
              checkHead(Stack,ElementPick),
              whichListInTheWorld(World,ElementPick,KPick),
              canbeAt(ElementPick,World,_Objects,KDrop),
              pickAt(KPick,World,WorldAux),
              dropAt(ElementPick,KDrop,WorldAux,NewWorld),
              nb_getval(listOfVisitedWorlds,Tail),
              not(member(NewWorld,Tail)),
              b_setval(world, NewWorld),
              nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
              plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
              Plan = [[KPick,KDrop,move]|PlanAux]
            )
          ; member(Stack,World),
            checkHead(Stack,ElementPick),
            whichListInTheWorld(World,ElementPick,KPick),
            canbeAt(ElementPick,World,_Objects,KDrop),
            pickAt(KPick,World,WorldAux),
            dropAt(ElementPick,KDrop,WorldAux,NewWorld),
            nb_getval(listOfVisitedWorlds,Tail),
            not(member(NewWorld,Tail)),
            b_setval(world, NewWorld),
            nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
            plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
            Plan = [[KPick,KDrop,move]|PlanAux]
          )
        )
      ).
/*
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
*/
/*
%Move the selected object inside the relative object in several steps if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, AnyMove, Element1, Element2),
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

%Move the selected object inside the relative object in several steps if the arm does not hold something and the selected object can be on the relative object
plan(_Goal, World, Holding, _Objects, Plan) :-
      retrieveGoalElements(_Goal, AnyMove, Element1, Element2),
      Holding == @(null),
      member(Stack,World),
      checkHead(Stack,ElementPick),
      whichListInTheWorld(World,ElementPick,KPick),
      canbeAt(ElementPick,World,_Objects,KDrop),
      pickAt(KPick,World,WorldAux),
      dropAt(ElementPick,KDrop,WorldAux,NewWorld),
      nb_getval(listOfVisitedWorlds,Tail),
      not(member(NewWorld,Tail)),
      b_setval(world, NewWorld),
      nb_setval(listOfVisitedWorlds,[NewWorld|Tail]),
      plan(_Goal, NewWorld, Holding, _Objects, PlanAux),
      Plan = [[KPick,KDrop,move]|PlanAux].
*/
%% Where : the list of positions (indexes) of the objects
plan(_Goal, World, _, _, Plan) :-
	retrieveGoalElements(_Goal, where, Parameter),
	maplist(whichListInTheWorld(World),Parameter,IdxList),
	Plan = [[IdxList,where]].
	
%% What ----------------------------------------------------------------------
/* can delete if it works
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
	
%What above (everything above)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatabove, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, Parameter),
	%get the list of objects
	(StackPosition > 0 ->
		length(LeftStack, StackPosition), append(LeftStack, RightStack, Stack),
		maplist(getFormSizeColorText(_Objects),LeftStack,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
		;Plan = [[[],what]]
	).

%What on top (one object)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatontop, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, Parameter),
	%get the list of objects
	(StackPosition > 0 ->
		ObjectTopPosition is StackPosition - 1,
		nth0(ObjectTopPosition, Stack, ObjectTop),
		getFormSizeColorText(_Objects,ObjectTop,ObjectFormSizeColor),
		Plan = [[[ObjectFormSizeColor],what]]
		;Plan = [[[],what]]
	).
	
%What under (all objects)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatunder, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, Parameter),
	%get index
	length(Stack,LengthStack),
	LeftStackLength is StackPosition + 1,
	%get the list of objects
	(LeftStackLength < LengthStack ->
		length(LeftStack, LeftStackLength), append(LeftStack, RightStack, Stack),
		maplist(getFormSizeColorText(_Objects),RightStack,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
		;Plan = [[[],what]]
	).
	
%What inside (one object)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatinside, Parameter),
	whichListInTheWorld(World,Parameter,Position),
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, Parameter),
	%get the list of objects
	(StackPosition > 0 ->
		ObjectTopPosition is StackPosition - 1,
		nth0(ObjectTopPosition, Stack, ObjectTop),
		getFormSizeColorText(_Objects,ObjectTop,ObjectFormSizeColor),
		Plan = [[[ObjectFormSizeColor],what]]
		;Plan = [[[],what]]
	).
*/
%What right (every stacks on the right)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatright, Parameter),
	% list everything right of all param
	maplist(rightOfObjectLetter(World),Parameter,ListObjRight),
	%intersect to get the rightmost
	intersectLL(ListObjRight,ListObjRightInter),
	(ListObjRightInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjRightInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).

%What left (every stacks on the left)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatleft, Parameter),	
	% list everything left of all param
	maplist(leftOfObjectLetter(World),Parameter,ListObjLeft),
	%intersect to get the leftmost
	intersectLL(ListObjLeft,ListObjLeftInter),
	(ListObjLeftInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjLeftInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).
	
%what beside
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
	% list everything above all param
	maplist(aboveOfObjectLetter(World),Parameter,ListObjAbove),
	%intersect to get the rightmost
	intersectLL(ListObjAbove,ListObjAboveInter),
	(ListObjAboveInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjAboveInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).
	
%What on top (one object)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatontop, Parameter),
	% list everything ontop all param2
	maplist(ontopOfObjectLetter(World),Parameter,ListObjOntop),
	%intersect to get the rightmost
	intersectLL(ListObjOntop,ListObjOntopInter),
	(ListObjOntopInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjOntopInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).
	
%What under (all objects)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatunder, Parameter),
	% list everything under all param
	maplist(underOfObjectLetter(World),Parameter,ListObjUnder),
	%intersect to get the rightmost
	intersectLL(ListObjUnder,ListObjUnderInter),
	(ListObjUnderInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjUnderInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).

%What inside (one object)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatinside, Parameter),
	% list everything inside all param
	maplist(insideOfObjectLetter(World),Parameter,ListObjInside),
	%intersect to get the rightmost
	intersectLL(ListObjInside,ListObjInsideInter),
	(ListObjInsideInter = [] -> Plan = [[[],what]]
		;maplist(getFormSizeColorText(_Objects),ListObjInsideInter,ObjectFormSizeColorList),
		Plan = [[ObjectFormSizeColorList,what]]
	).
	
%What on the floor (all objects)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatontopfloor),
	getObjectsOnFloor(World,ObjectsList),
	maplist(getFormSizeColorText(_Objects),ObjectsList,ObjectFormSizeColorList),
	Plan = [[ObjectFormSizeColorList,what]].
	
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
	
%% whatleftstack : the list of characteristics (form, size, color) of the objects
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatleftstack, Position),
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
	retrieveGoalElements(_Goal, whatbesidestack, Position),
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
    retrieveGoalElements(_Goal, whatabovestack, Position),
	%get the whole stack
	nth0(Position,World,Stack),
	maplist(getFormSizeColorText(_Objects),Stack,ObjectFormSizeColorList),
	Plan = [[ObjectFormSizeColorList,what]].
	
%What on top (one object)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatontopstack, Position),
	%get the whole stack
	nth0(Position,World,Stack),
	nth0(0, Stack, ObjectTop),
	getFormSizeColorText(_Objects,ObjectTop,ObjectFormSizeColor),
	Plan = [[[ObjectFormSizeColor],what]].
	
%What inside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, whatinsidestacks, Position),
	%get the whole stack
	nth0(Position,World,Stack),
	maplist(getFormSizeColorText(_Objects),Stack,ObjectFormSizeColorList),
	Plan = [[ObjectFormSizeColorList,what]].
	
%% Count ----------------------------------------------------------------------

%Count right (every stacks on the right)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countright, Parameter1,Parameter2),
	% list everything left of all param2
	maplist(rightOfObjectLetter(World),Parameter2,ListObjRight),
	%intersect to get the rightmost
	intersectLL(ListObjRight,ListObjRightInter),
	%match with param1
	intersection(ListObjRightInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%Count left (every stacks on the left)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countleft, Parameter1,Parameter2),
	% list everything left of all param2
	maplist(leftOfObjectLetter(World),Parameter2,ListObjLeft),
	%intersect to get the leftmost
	intersectLL(ListObjLeft,ListObjLeftInter),
	%match with param1
	intersection(ListObjLeftInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%Count beside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countbeside, Parameter1,Parameter2),	
	whichListInTheWorld(World,Parameter2,Position),
	length(World, LengthWorld),LeftPos is Position - 1,RightPos is Position + 1,
	%stack picked is within bounds
	%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
	(LeftPos >= 0 ->
		nth0(LeftPos,World,LeftStack),
		((LeftStack = [],MatchingObjLeft = [])
			;flatten(LeftStack,ListObjLettersLeft),
			intersection(Parameter1,ListObjLettersLeft,MatchingObjLeft)
		)
		;MatchingObjLeft = []
	),
	(RightPos < LengthWorld ->
		nth0(RightPos,World,RightStack),
		((RightStack = [],MatchingObjRight = [])
			;flatten(RightStack,ListObjLettersRight),
			intersection(Parameter1,ListObjLettersRight,MatchingObjRight)
		)
		;MatchingObjRight = []
	),
	length(MatchingObjLeft,CountLeft),length(MatchingObjRight,CountRight),
	append([CountLeft],['object(s) on the left'],Str1),append(Str1,[CountRight],Str2),append(Str2,['object(s) on the right.'],Count),
	Plan = [[Count,countbeside]].
	
%count above
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countabove, Parameter1,Parameter2),
	% list everything above all param2
	maplist(aboveOfObjectLetter(World),Parameter2,ListObjAbove),
	%intersect to get the rightmost
	intersectLL(ListObjAbove,ListObjAboveInter),
	%match with param1
	intersection(ListObjAboveInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%count under
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countunder, Parameter1,Parameter2),
	% list everything under all param2
	maplist(underOfObjectLetter(World),Parameter2,ListObjUnder),
	%intersect to get the rightmost
	intersectLL(ListObjUnder,ListObjUnderInter),
	%match with param1
	intersection(ListObjUnderInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%count ontop
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countontop, Parameter1,Parameter2),
	% list everything ontop all param2
	maplist(ontopOfObjectLetter(World),Parameter2,ListObjOntop),
	%intersect to get the rightmost
	intersectLL(ListObjOntop,ListObjOntopInter),
	%match with param1
	intersection(ListObjOntopInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%count inside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countinside, Parameter1,Parameter2),
	% list everything inside all param2
	maplist(insideOfObjectLetter(World),Parameter2,ListObjInside),
	%match with param1
	intersection(ListObjInside,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].

%Count right (every stacks on the right)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countrightstack, Parameter1,Parameter2),	
	length(World, LengthWorld),LengthRest is Parameter2 + 1,
	%stack picked is within bounds
	(LengthRest < LengthWorld ->
		%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
		length(Rest, LengthRest), append(Rest, RightStacks, World),
		flatten(RightStacks,ListObjLetters),
		intersection(ListObjLetters,Parameter1,Intersection),
		length(Intersection,Count)
	; Count = 0
	),
	Plan = [[Count,count]].
	
%Count left (every stacks on the right)
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countleftstack, Parameter1,Parameter2),
	length(World, LengthWorld),
	%stack picked is within bounds
	((Parameter2 >= 0,Parameter2 < LengthWorld )->
		%World is split into 2 parts : Left with the left until the stack, RightStacks with everything we want to examine.
		length(Left, Parameter2), append(Left, RightStacks, World),
		flatten(Left,ListObjLetters),
		intersection(ListObjLetters,Parameter1,Intersection),
		length(Intersection,Count)
		%or not
		;Count = 0
	),
	Plan = [[Count,count]].
	
%Count beside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countbesidestack, Parameter1,Position),
	length(World, LengthWorld),LeftPos is Position - 1,RightPos is Position + 1,
	%stack picked is within bounds
	%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
	(LeftPos >= 0 ->
		nth0(LeftPos,World,LeftStack),
		((LeftStack = [],MatchingObjLeft = [])
			;flatten(LeftStack,ListObjLettersLeft),
			intersection(Parameter1,ListObjLettersLeft,MatchingObjLeft)
		)
		;MatchingObjLeft = []
	),
	(RightPos < LengthWorld ->
		nth0(RightPos,World,RightStack),
		((RightStack = [],MatchingObjRight = [])
			;flatten(RightStack,ListObjLettersRight),
			intersection(Parameter1,ListObjLettersRight,MatchingObjRight)
		)
		;MatchingObjRight = []
	),
	length(MatchingObjLeft,CountLeft),length(MatchingObjRight,CountRight),
	append([CountLeft],['object(s) on the left'],Str1),append(Str1,[CountRight],Str2),append(Str2,['object(s) on the right.'],Count),
	Plan = [[Count,countbeside]].
	
%count above
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countabovestack, Parameter1,Position),
	% list everything above all param2
	nth0(Position,World,Stack),
	%match with param1
	intersection(Stack,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%count ontop
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countontopstack, Parameter1,Position),
	% list everything ontop all param2	
	nth0(Position,World,Stack),
	nth0(0, Stack, ObjectTop),
	%match with param1
	intersection([ObjectTop],Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].

%%%%%%%%--------------------------------- FOR TIME OUT
%%%% PUT THIS VERSION THAT WORKS WHEN TESTING IS DONE
/*
%count inside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countinsidestacks, Parameter1,IdxList),
	% list everything inside all IdxList
	makeStackList(World,IdxList,StackList),
	flatten(StackList,ListObjInside),
	%match with param1
	intersection(ListObjInside,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
*/

%count inside
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countinsidestacks, Parameter1,IdxList),
	% list everything inside all IdxList
	makeStackList(World,IdxList,StackList),
	flatten(StackList,ListObjInside),
	%intersect to get the rightmost
	intersectLL(ListObjInside,ListObjInsideInter),
	%match with param1
	intersection(ListObjInsideInter,Parameter1,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].
	
%count ontop floor
plan(_Goal, World, _, _Objects, Plan) :-
    retrieveGoalElements(_Goal, countontopfloor, Parameter),
	% list everything ontop floor
	getObjectsOnFloor(World,ObjectsList),	
	%match with param1
	intersection(ObjectsList,Parameter,Intersection),
	length(Intersection,Count),
	Plan = [[Count,count]].

%----------------------------------------------------------------- Strings management

%The same than getFormSizeColor in text form
getFormSizeColorText(PossibleObjects,ObjectLetter,ObjectFormSizeColor) :-
	PossibleObjects = json(PossibleObjectsJson),member(ObjectLetter = ObjectJson,PossibleObjectsJson),ObjectJson=json([form=FormObj,size=SizeObj,color=ColorObj]),
	atom_string(SizeObj,SizeObjStr),atom_string(ColorObj,ColorObjStr),atom_string(FormObj,FormObjStr),
	string_concat('a ',SizeObjStr,FinalStr1),
	string_concat(FinalStr1,' ',FinalStr2),
	string_concat(FinalStr2,ColorObjStr,FinalStr3),
	string_concat(FinalStr3,' ',FinalStr4),
	string_concat(FinalStr4,FormObjStr,ObjectFormSizeColor).
	
%-------------------------------------------------------------------- Aux
getObjectsOnFloorAux(L,Object) :- is_list(L),(L = [] ->
	Objects = []
	;length(L,Length),
	Pos is Length - 1,
	nth0(Pos,L,Object)
).
getObjectsOnFloor(World,ObjectsList) :- maplist(getObjectsOnFloorAux,World,MatchingObjectsList),flatten(MatchingObjectsList,ObjectsList).

intersectLL([X],X).	
intersectLL([L1,L2],Intersection) :- is_list(L1),is_list(L2),intersection(L1,L2,Intersection).
intersectLL([L1,L2|R],Intersection) :- intersectLL([L1,L2],L3),intersectLL([L3|R],Intersection).

makeStackListAux(World,Idx,Stack) :- nth0(Idx,World,Stack);Stack = [].
makeStackList(World,IdxList,StackList) :- maplist(makeStackListAux(World),IdxList,StackList).

matchObjectOnStack(Stack,Object,MatchObject) :- (member(Object,Stack),MatchObject = [Object]);MatchObject = [].
matchObjectsListOnStack(ObjectsList,Stack,MatchingObjects) :- maplist(matchObjectOnStack(Stack),ObjectsList,MatchingObjectsList),flatten(MatchingObjectsList,MatchingObjects).
matchObjectsListOnStackList(ObjectsList,StackList,MatchingObjects) :- maplist(matchObjectsListOnStack(ObjectsList),StackList,MatchingObjectsList),flatten(MatchingObjectsList,MatchingObjects).

rightOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),
	length(World, LengthWorld),LengthRest is Position + 1,
	%stack picked is within bounds
	(LengthRest < LengthWorld ->
		%World is split into 2 parts : Rest with the left until the stack, RightStacks with everything we want to examine.
		length(Rest, LengthRest), append(Rest, RightStacks, World),
		flatten(RightStacks,ListObjLetters)
		%or not
		;ListObjLetters = []
	).
	
leftOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),
	length(World, LengthWorld),
	%stack picked is within bounds
	((Position >= 0,Position < LengthWorld )->
		%World is split into 2 parts : Left with the left until the stack, RightStacks with everything we want to examine.
		length(Left, Position), append(Left, RightStacks, World),
		flatten(Left,ListObjLetters)
		%or not
		;ListObjLetters = []
	).

aboveOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),	
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, ObjectLetter),
	%get the list of objects
	(StackPosition > 0 ->
		length(ListObjLetters, StackPosition), append(ListObjLetters, RightStack, Stack)
		;ListObjLetters = []
	).
	
ontopOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),	
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, ObjectLetter),
	%get the list of objects
	(StackPosition > 0 ->
		ObjectTopPosition is StackPosition - 1,
		nth0(ObjectTopPosition, Stack, ObjectTop),
		ListObjLetters = [ObjectTop]
		;ListObjLetters = []
	).
	
underOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),	
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, ObjectLetter),
	%get index
	length(Stack,LengthStack),
	LeftStackLength is StackPosition + 1,
	%get the list of objects
	(LeftStackLength < LengthStack ->
		length(LeftStack, LeftStackLength), append(LeftStack, ListObjLetters, Stack)
		;ListObjLetters = []
	).
	
insideOfObjectLetter(World,ObjectLetter,ListObjLetters) :-
	whichListInTheWorld(World,ObjectLetter,Position),	
	%get the whole stack
	nth0(Position,World,Stack),
	%get position of the object in the stack
	nth0(StackPosition, Stack, ObjectLetter),
	%get the list of objects
	(StackPosition > 0 ->
		ObjectTopPosition is StackPosition - 1,
		nth0(ObjectTopPosition, Stack, ObjectTop),
		ListObjLetters = [ObjectTop]
		;ListObjLetters = []
	).
