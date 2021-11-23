% Path = "E:/Depot_Git/inf6120/inf6120-tp2/".

:-dynamic sommet/2, arc/3, compteur/1, cible/1.

/**
 * Calcul la distance entre deux phrases.
 * Cette valeur est un point flottant.
 * distance( +PhraseA, +PhraseB, -Distance ).
 * les arguments PhraseA et PhraseB sont des listes de mots (atome).
 * Si les phrases sont trop différente, alors la différence est de 1000.0.
 */
distance( PhraseA, PhraseB, Distance ) :-
	intersection( PhraseA, PhraseB, MotsCommuns ),
	length( PhraseA, NbrMotA ),
	length( PhraseB, NbrMotB ),
	MaxCommun is min( NbrMotA, NbrMotB ),
	length( MotsCommuns, NbrMotsCommuns ),
	( ( \+ NbrMotsCommuns =:= 0.0,
	    Distance is MaxCommun / NbrMotsCommuns );
	  ( NbrMotsCommuns =:= 0.0,
	    Distance is 1000.0 )
	).

/**
 * Prédicat principal.
 * Ce prédicat charge la base de connaissance contenant le texte à résumer.
 * Ensuite il construit le résumé et l'affiche.
 */
resoudre( Nom, K ) :-
	consult( Nom ),
	init_teardown(K).

affiche :-
	cible(S), write(S),nl,
	getSommets(Sommets),
	forall(member(Sommet, Sommets), (write(Sommet), nl )).

init_teardown(Cible) :-
	setup_call_cleanup(
		initAll(Cible), 
		affiche,
		teardownAll
	).

initAll(Cible) :-
	asserta(cible(Cible)),
	initSommets.


initSommets :-
	findall(sommet(Id,[Phrase]), p(Id, Phrase), Sommets),
	forall(member(Sommet, Sommets), assertz(Sommet)). 

getSommets(Sommets) :-
	findall(sommet(C, D), sommet(C, D), Sommets).
	

teardownAll :-
	retract(cible(A)),
	abolish(sommet/2),
	abolish(arc/3).






	
    
