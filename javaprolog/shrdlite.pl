#!/usr/bin/env swipl -q -g main,halt -t halt(1) -s

:- use_module(library(http/json)).
:- [dcg_parser].
:- [shrdlite_grammar].
:- [interpreter].
:- [solver].
:- [planner].
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

%precision
parse_all(precision, Utterance, TreesForPrecision),
(TreesForPrecision \== [] ->	  
        b_getval(world,World),
        b_getval(holding,Holding),
	readUnpreciseGoal(UnpreciseGoals),
	handleAmbiguity(UnpreciseGoals,World,Holding,Objects,TreesForPrecision,PrecisionGoalList),
	%we could not get a goal
	(PrecisionGoalList = [] ->
		Plan = @(null),
		Output = 'Ambiguity error, this object does not exist!'
		%we have a goal !
		;PrecisionGoalList = [PrecisionGoal],
		nb_setval(listOfVisitedWorlds,[World]),
		plan(PrecisionGoal, World, Holding, Objects, PlanList),
			( PlanList == [-1] ->
			  Plan = @(null),
			  Output = 'Nothing to do!'
			; solve(PlanList, Plan),
			nb_getval(output,Output)
			)
	),
	findall(JT, (member(T, TreesForPrecision),
				 write_to_codes(T, Cs),
				 atom_codes(JT, Cs)), JSONTrees),
	findall(GT, (member(G, PrecisionGoal),
				 write_to_codes(G, Gs),
				 atom_codes(GT, Gs)), JSONGoals),
	Result = [utterance = Utterance,
			  trees = JSONTrees,
			  goals = JSONGoals,
			  plan = Plan,
			  output = Output],
	json_write(user_output, json(Result))	
	
	%command
	;parse_all(command, Utterance, Trees),
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
			Output = 'Which object do you mean?',
			writeUnpreciseGoal(Goals),
			Plan= @(null)
		  ; Goals = [Goal],
			nb_setval(listOfVisitedWorlds,[World]),
			plan(Goal, World, Holding, Objects, PlanList),
			( PlanList == [-1] ->
			  Plan = @(null),
			  Output = 'Nothing to do!'
			; ( PlanList == [-2] ->
				Plan = @(null),
				Output = 'This move is impossible!'
			  ; solve(PlanList, Plan),
				nb_getval(output,Output)
			  )
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
	json_write(user_output, json(Result))
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

handleAmbiguity(Goals,World,Holding,Objects,TreesPrecision,FinalGoal) :-
	%Parse the new input and find the corresponding object
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

readUnpreciseGoal(UnpreciseGoal) :-
    open('unpreciseGoal.txt',read,In),
    read_line_to_codes(In,X),
    close(In),
	atom_codes(Y,X),
	term_to_atom(UnpreciseGoal,Y).

writeUnpreciseGoal(UnpreciseGoal) :-
    open('unpreciseGoal.txt',write,Out),
    write(Out,UnpreciseGoal),
    close(Out).  
