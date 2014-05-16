:- style_check(-singleton).

%---------------------------------------------------------------------------------------------------- Constraints management ----------------------------------------------------------------------------------------------------
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
