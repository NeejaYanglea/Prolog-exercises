/*genealogical tree*/
genitore(salvatore, veronica).
genitore(salvatore, fabio).
genitore(irene, veronica).
genitore(irene, fabio).
genitore(nicola, salvatore).
genitore(nicola, luigi).
genitore(giuseppina, salvatore).
genitore(giuseppina, luigi).
genitore(alfredo, irene).
genitore(anna, irene).
genitore(alfredo, angela).
genitore(anna, angela).
genitore(angela, vincenzo).
genitore(pippo, anna).

fratello(X,Y) :- genitore(Z, X), genitore(Z, Y), X\=Y.
nonno(X,Y) :- genitore(X, Z), genitore(Z, Y).
zio(X,Y) :- fratello(X, Z), genitore(Z, Y).
cugino(X,Y) :- genitore(A,X), genitore(B,Y), fratello(A,B), \+ fratello(X,Y).
discendente(X,Y) :- genitore(X, Y).
discendente(X,Y) :- genitore(X, Z), discendente(Z,Y).

/*fact(+X,+Y)*/
/*verify*/
fact(0,1).
fact(1,1).
fact(X,Y) :- X>0, N is X-1, M is Y/X, fact(N, M).

/*fact2(+X,?Y)*/
/*calculate*/
fact2(0,1).
fact2(X,Y) :- X>0,
			 N is X-1,
			 fact2(N, M),
			 Y is M*X, !.

/*palindroma(X)*/
palindroma([_]).
palindroma([X|Rest]) :- append(L, [X], Rest), palindroma(L). 


/*maxlist(+L,?N)*/
/*L list of numbers, N is max in L, fails if empty*/
maxlist([],N) :- fail.
maxlist([X],X) :- !.
maxlist([X|Rest],N) :- maxlist(Rest,M), X>M, N is X, !.
maxlist([X|Rest],N) :- maxlist(Rest,M), N is M.


pari(X) :- 0 is X mod 2.
/*split(+L,?P,?D)*/
/*L list of numbers, P contains evens and D contanis odds, in order of appearence in L*/
split([],[],[]).
split([X|Rest], [X|P], D) :- pari(X), split(Rest, P, D), !.
split([X|Rest], P, [X|D]) :- split(Rest, P, D).

/*prefisso(Pre,L)*/
prefisso([], L).
prefisso([X|Rest], [X|Coda]) :- prefisso(Rest, Coda).

/*suffisso(Suf,L)*/
suffisso(Suf,L) :- append(A, Suf, L).

/*sublist(S,L)*/
sublist(S,L) :- append(_,S,X), append(X,_,L).

/*subset(+Sub,?Set)*/
subset([], Set).
subset([X|Rest], Set) :- member(X, Set), subset(Rest, Set).

/*rev(+X,?Y)*/
rev([],[]) :- !.
rev([X|Rest], Y) :- append(Z,[X],Y), rev(Rest,Z), !.

/*del_first(+X,+L,?Resto)*/
del_first(X,[X|Resto], Resto) :- !.
del_first(X,[Y|Rest], Resto) :- del_first(X,Rest,L), append([Y],L,Resto).

/*del(+X,+L,?Resto)*/
del(X,L,L) :- \+ member(X, L), !. 
del(X,L,Resto) :- del_first(X,L,PartialClean), del(X,PartialClean,Resto).

/*subst(+X,+Y,+L,-Nuova)*/
subst(X,Y,L,L) :- \+ member(X, L), !. 
subst(X,Y,[X|Rest],[Y|Nuova]) :- subst(X,Y,Rest,Nuova), !. 
subst(X,Y,[Z|Rest],[Z|Nuova]) :- subst(X,Y,Rest,Nuova).

/*mkset(+L,-Set)*/
mkset([],[]).
mkset([X|Rest],[X|Set]):- \+ member(X,Rest), !,mkset(Rest,Set).
mkset([_|Rest],Set) :- mkset(Rest,Set). 

/*union(+A,+B,-Union)*/
union(A,B,Union) :- append(A,B,Append), mkset(Append,Union).

/*occurs_in(?X,+Y)*/
occurs_in(X,X).
occurs_in(X,[Y|Rest]) :- occurs_in(X, Y); occurs_in(X,Rest), X\=Rest.

/*flat(+X,?Y)*/
flat([], []) :- !.
flat([L|Ls], FlatL) :-
    !,
    flat(L, NewL),
    flat(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flat(L, [L]).

/*trees*/

bin_height(empty,0).
bin_height(t(_,Left,Right),H) :- bin_height(Left,N), bin_height(Right,M), (N>M, H is N+1 ; N=<M, H is M+1).

bin_size(empty,0).
bin_size(t(_,Left,Right),H) :- bin_size(Left,N), bin_size(Right,M), H is N+M+1.

reflect(empty,empty).
reflect(t(X,empty,empty),t(X,empty,empty)).
reflect(t(X,Left,Right),t(X,Right,Left)).

bin_labels(empty,[]).
bin_labels(t(X,empty,empty),[X]).
bin_labels(t(X,Left,Right),[X|Rest]) :- 
		bin_labels(Left,L),
		bin_labels(Right,R),
		append(L,R,Rest).
		
balanced(empty).
balanced(t(_,Left, Right)) :- 
		bin_height(Left, LeftH), 
		bin_height(Right, RightH), 
		X is abs(RightH-LeftH), 
		X=<1, 
		balanced(Left), 
		balanced(Right).
		
branch(t(Leaf,empty, empty),Leaf,[Leaf]).
branch(t(X,Left,_), Leaf, [X|Tail]) :- branch(Left, Leaf, Tail).
branch(t(X,_,Right), Leaf, [X|Tail]) :- branch(Right, Leaf, Tail).

%% Root => Subtrees

op(600,xfx,=>).

height(empty, 0).
height(_ => SubTree,N) :- !, height(SubTree,X), N is X+1.
height([], 0) :- !.
height([X|SubTrees],N) :- !, height(X,This), height(SubTrees,Partial), max(This,Partial,N).
height(_, 1).

size(empty, 0).
size(_ => SubTree,N) :- !, size(SubTree, X), N is X+1.
size([], 0) :- !.
size([X|SubTrees],N) :- !, size(X,This), size(SubTrees,Other), N is This+Other.
size(_, 1).

label(empty, []).
label(Root => SubTree,[Root|Tail]) :- !, label(SubTree, Tail). 
label([], []) :- !.
label([X|SubTrees],Labels) :- !, label(X,LabelX), label(SubTrees,OtherLabels), append(LabelX,OtherLabels,Labels).
label(X, [X]).


/*graph path*/ 

arc(a,b).
arc(a,e).
arc(b,a).
arc(b,c).
arc(c,c).
arc(c,d).
arc(d,c).
arc(d,b).
arc(e,c).

path(Start, Goal, Path) :- path(Start, Goal, Path, []).

path(Start, Start, [Start], Visited) :- \+ member(Start, Visited).
path(Start, Goal, [Start|Path], Visited) :-
		\+ member(Start, Visited), 
		arc(Start, N), 
		path(N, Goal, Path, [Start|Visited]).

/*Exam Exercises Solutions*/
del_leaf(X,t(X,empty,empty),empty).
del_leaf(X,t(Y,Left,Right),t(Y,Newleft,Right)) :- del_leaf(X,Left,Newleft).
del_leaf(X,t(Y,Left,Right),t(Y,Left,Newright)) :- del_leaf(X,Right,Newright).

subst_node(X,Y,t(X,Left,Right),t(Y,Left,Right)).
subst_node(X,Y,t(Z,Left,Right),t(Z,Newleft,Right)) :- subst_node(X,Y,Left,Newleft).
subst_node(X,Y,t(Z,Left,Right),t(Z,Left,Newright)) :- subst_node(X,Y,Right,Newright).