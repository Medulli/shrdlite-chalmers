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
Goal = countbeside(Parameter1,[Parameter2]),Action = countbeside.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countleft(Parameter1,Parameter2),Action = countleft.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countright(Parameter1,Parameter2),Action = countright.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countabove(Parameter1,Parameter2),Action = countabove.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countontop(Parameter1,Parameter2),Action = countontop.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countunder(Parameter1,Parameter2),Action = countunder.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countinside(Parameter1,Parameter2),Action = countinside.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countleftstack(Parameter1,[Parameter2]),Action = countleftstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countrightstack(Parameter1,[Parameter2]),Action = countrightstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countabovestack(Parameter1,[Parameter2]),Action = countabovestack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countontopstack(Parameter1,[Parameter2]),Action = countontopstack.

retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countbesidestack(Parameter1,[Parameter2]),Action = countbesidestack.

%% /!\ Parameter is a list of stack numbers !
retrieveGoalElements(Goal, Action, Parameter1,Parameter2) :-
Goal = countinsidestacks(Parameter1, Parameter2),Action = countinsidestacks.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = countontop(Parameter,floor),Action = countontopfloor.

%%here to get rid of warnings
retrieveGoalElements(Goal, Action, Parameter1) :-
Goal = moveontop([Parameter1],floor),Action = moveontopfloor.

%%Take ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = take([Parameter]),Action = take.

%%Where ---------------------------------------------------------------------------------------------------

retrieveGoalElements(Goal, Action, Parameter) :-
        Goal = where(Parameter),Action = where.

%%What ---------------------------------------------------------------------------------------------------
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatbeside([Parameter]),Action = whatbeside.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatleft(Parameter),Action = whatleft.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatright(Parameter),Action = whatright.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatabove(Parameter),Action = whatabove.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatontop(Parameter),Action = whatontop.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatunder(Parameter),Action = whatunder.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatinside(Parameter),Action = whatinside.

%% /!\ Parameter is a list of stack numbers !
retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatinsidestacks(Parameter),Action = whatinsidestacks.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatleftstack([Parameter]),Action = whatleftstack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatrightstack([Parameter]),Action = whatrightstack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatabovestack([Parameter]),Action = whatabovestack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatontopstack([Parameter]),Action = whatontopstack.

retrieveGoalElements(Goal, Action, Parameter) :-
Goal = whatbesidestack([Parameter]),Action = whatbesidestack.

retrieveGoalElements(Goal, Action) :-
Goal = whatontop(floor),Action = whatontopfloor.

%---------------------------------------------------------------------------------------------------------------
/*
test :-
Goal = movebeside([e],[g]),
retrieveGoalElements(Goal, Action, Parameter1,Parameter2),write(Action),write(Parameter1),write(Parameter2).

test2 :-
Goal = take([e]),
retrieveGoalElements(Goal, Action, Parameter),write(Action),write(Parameter).
*/
