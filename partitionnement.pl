% "E:/Depot_Git/inf6120/inf6120-tp2/".

:-dynamic sommet/4, arc/3, compteur/1, cible/1, addition/1.

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
	compteur(C), write(C), nl,
	getSommets(Sommets),
	afficher(Sommets),
	getArcs(Arcs),
	afficher(Arcs).

init_teardown(Cible) :-
	setup_call_cleanup(
		initAll(Cible), 
		affiche,
		teardownAll
	).

afficher(Liste) :-
	forall(member(Elem, Liste), (write(Elem), nl )).

initAll(Cible) :-
	asserta(cible(Cible)),
	asserta(compteur(0)),
	initSommets,
	initArcs.

getSommets(Sommets) :-
	findall(sommet(A, B, C, D), sommet(A, B, C, D), Sommets).

getArcs(Arcs) :-
	findall(arc(A,B,C), arc(A,B,C), Arcs).

initSommets :-
	findall(sommet(Id,[Phrase], Phrase, 0.0), p(Id, Phrase), Sommets),
	forall(member(Sommet, Sommets), assertz(Sommet)). 

initArcs :-
	getSommets(Sommets),
	forall(
		member(Sommet, Sommets),
		(
			select(Sommet,Sommets, Reste),
			ajouterArcs(Sommet, Reste)
		)
	).

ajouterArcs(Sommet, ListeSommets) :-
	forall(
		member(Bout, ListeSommets),
		(
			sommet(IdS,_, RepA, _) = Sommet,
			sommet(IdB,_, RepB, _) = Bout,
			distance(RepA, RepB, Distance),
			assertz(arc(IdS, IdB, Distance))
		)
	).

phraseType(ListePhrases, PhraseType) :-
	length(ListePhrases, X),
	NbrPhrases is X - 1,
	forall(
		member(Phrase, ListePhrases),
		(
			select(Phrase, ListePhrases, Reste),
			moyennePhrase(Phrase, Reste, NbrPhrases, Moy),
			asserta(representante())
		)
	).

moyennePhrase(Phrase, Reste, NbrPhrases, Moy) :-
	asserta(addition(0)),
	forall(
		member(Autre, Reste),
		addition(Phrase, Autre)
	),
	retract(addition(A)),
	Moy is A / NbrPhrases.

addition(Phrase, Autre) :-
	distance(Phrase, Autre, Distance),
	retract(addition(A)),
	C is A + Distance,
	asserta(addition(C)).




teardownAll :-
	abolish(cible/1),
	abolish(compteur/1),
	abolish(sommet/4),
	abolish(arc/3).






	
    
