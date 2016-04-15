/*NAIVE SORT*/
/*Generates every permutation then checks*/
naive_sort(List,Sorted) :- perm(List,Sorted),is_sorted(Sorted).
   
is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|Rest]) :- X=<Y,is_sorted([Y|Rest]).

/*INSERT SORT*/
insert_sort(List,Sorted) :- i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|Rest],Acc,Sorted) :- insert(H,Acc,NAcc),i_sort(Rest,NAcc,Sorted).
   
insert(X,[Y|Rest],[Y|NRest]) :- X>Y,insert(X,Rest,NRest).
insert(X,[Y|Rest],[X,Y|Rest]) :- X=<Y.
insert(X,[],[X]).

/*BUBBLE SORT*/
bubble_sort(List,Sorted) :- b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|Rest],Acc,Sorted) :- bubble(H,Rest,NRest,Max),b_sort(NRest,[Max|Acc],Sorted).
   
bubble(X,[],[],X).
bubble(X,[Y|Rest],[Y|NRest],Max) :- X>Y,bubble(X,Rest,NRest,Max).
bubble(X,[Y|Rest],[X|NRest],Max) :- X=<Y,bubble(Y,Rest,NRest,Max).

/*MERGE SORT*/
merge_sort([],[]).    
merge_sort([X],[X]).   
merge_sort(List,Sorted) :- 
    List=[_,_|_],divide(List,L1,L2),     % list with at least two elements is divided into two parts
	merge_sort(L1,Sorted1),merge_sort(L2,Sorted2),  
	merge(Sorted1,Sorted2,Sorted).                  
merge([],L,L).
merge(L,[],L) :- L\=[].
merge([X|Rest1],[Y|Rest2],[X|Rest]) :- X=<Y,merge(Rest1,[Y|Rest2],Rest).
merge([X|Rest1],[Y|Rest2],[Y|Rest]) :- X>Y,merge([X|Rest1],Rest2,Rest).

divide(L,L1,L2) :- append(L1, L2, L),
    length(L1, N),
    length(L2, M),
    (M=N ; M is N-1).

/*QUICK SORT*/
pivoting(H,[],[],[]).
pivoting(H,[X|Rest],[X|L],G) :- X>H,pivoting(H,Rest,L,G).
pivoting(H,[X|Rest],L,[X|G]) :- X=<H,pivoting(H,Rest,L,G).

quick_sort(List,Sorted) :- q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|Rest],Acc,Sorted) :- 
	pivoting(H,Rest,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).