
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
	creerGraphe().

creerGraphe() :-
	p(Id, Mots),
	assertz(graphe(p(Id, Mots), 0, 0)).

:-dynamic graphe/3.



	
    
