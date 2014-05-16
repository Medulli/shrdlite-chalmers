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
