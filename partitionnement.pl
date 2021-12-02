% "E:/Depot_Git/inf6120/inf6120-tp2/".

:-dynamic sommet/3, arc/3, compteur/1, cible/1, addition/1, rep/2.

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
/**
 * Affiche les compteurs, les sommet et les arcs.
 */
affiche :-
	cible(S), write(S),nl,
	compteur(C), write(C), nl,
	getSommets(Sommets),
	afficher(Sommets),
	getArcs(Arcs),
	afficher(Arcs).

/**
 * init_teardown(+Cible)
 * Prédicat principal. Initialise les prédicat, lance les opérations et supprime les prédicats pour exécution future.
 *
 * @Cible - Le nombre de phrases voulue.
 */
init_teardown(Cible) :-
	setup_call_cleanup(
		initAll(Cible), 
		affiche,
		teardownAll
	).

/**
 * afficher(+Liste)
 * Affiche chaque élément d'une liste suivit d'un saut de ligne.
 *
 * @Liste - La liste à afficher.
 */
afficher(Liste) :-
	forall(member(Elem, Liste), (write(Elem), nl )).


/**
 * initAll(+Cible)
 * Partie de l'initialisation des prédicats dynamiques. Initialise les compteurs.
 *
 * @Cible - Le nombre de phrases voulue.
 */
initAll(Cible) :-
	asserta(cible(Cible)),
	asserta(compteur(0)),
	initSommets,
	initArcs.

/**
 * getSommets(-Sommets)
 * Retourne une liste de toutes les structures sommet/3.
 *
 * @Sommets - Une liste contenant des éléments sommet/3.
 */
getSommets(Sommets) :-
	findall(sommet(A, B, C), sommet(A, B, C), Sommets).

/**
 * getArcs(-Arcs)
 * Retourne une liste de toutes les structures arc/3.
 *
 * @Arcs - Une liste contenant des éléments arc/3.
 */
getArcs(Arcs) :-
	findall(arc(A,B,C), arc(A,B,C), Arcs).

/**
 * Initialise tous les sommets de départ à partir des structures p/2. 
 */
initSommets :-
	findall(sommet(Id,[Phrase], rep(Phrase, 0.0)), p(Id, Phrase), Sommets),
	forall(member(Sommet, Sommets), assertz(Sommet)). 

/**
 * Initialise tous les arcs de départ à partir des sommets de départ. Fait un produit cartésien des sommets en arcs.
 */
initArcs :-
	getSommets(Sommets),
	forall(
		member(Sommet, Sommets),
		(
			select(Sommet,Sommets, Reste),
			ajouterArcs(Sommet, Reste)
		)
	).

/**
 * ajouterArcs(+Sommet, +ListeSommets)
 * Ajoute des arc/3 entre @Sommet et Chaque membre de @ListeSommets.
 * 
 * @Sommet - Une structure sommet/3
 * @ListeSommets - Une liste de structures sommet/3
 */
ajouterArcs(Sommet, ListeSommets) :-
	forall(
		member(Bout, ListeSommets),
		(
			sommet(IdA,_, rep(RepA, _)) = Sommet,
			sommet(IdB,_, rep(RepB, _)) = Bout,
			distance(RepA, RepB, Distance),
			assertz(arc(IdA, IdB, Distance))
		)
	).


/**
 * phraseType(+ListePhrase, -PhraseType)
 * Retourne une structure rep/2, phrase type d'un sommet.
 *
 * @ListePhrase - Une liste de phrases.
 * @PhraseType - La phrase type de @ListePhrase, rep(Phrase, Moyenne)
 */
phraseType(ListePhrases, PhraseType) :-
	length(ListePhrases, X),
	NbrPhrases is X - 1,
	asserta(rep("", 1001.0)),
	forall(
		member(Phrase, ListePhrases),
		(
			select(Phrase, ListePhrases, Reste),
			moyennePhrase(Phrase, Reste, NbrPhrases, Moy),
			rep(PhraseTmp, MinTmp),
			determineMin(rep(PhraseTmp, MinTmp), rep(Phrase, Moy), RepMin),
			retract(rep(PhraseTmp, MaxTmp)),
			asserta(RepMin)
		)
	),
	retract(rep(PhraseMin, MoyMin)),
	PhraseType = rep(PhraseMin, MoyMin).

/**
 * determineMin(+rep(P1,M1), +rep(P2,M2), -RepMin)
 * Retourne la représentante ayant la moyenne la plus petite.
 *
 * @rep(P1, M1) - La première représentante.
 * @rep(P2, M2) - La deuxième représentante.
 * @RepMin - La plus petites des deux représentantes.
 */
determineMin(rep(P1, M1), rep(P2, M2), RepMin) :-
	M1 =< M2,
	RepMin = rep(P1, M1).
determineMin(rep(P1, M1), rep(P2, M2), RepMin) :-
	M2 < M1,
	RepMin = rep(P2, M2).

/**
 * moyennePhrase(+Phrase, +Reste, +NbrPhrases, -Moy)
 * Retourne la moyenne de @Phrase par rapport aux phrase de @Reste
 *
 * @Phrase - La phrase pour laquelle on veut définir la moyenne.
 * @Reste - Une liste de phrase.
 * @NbrPhrases - Le nombre de phrases dans @Reste
 * @Moy - La moyenne de @Phrase
 */
moyennePhrase(Phrase, Reste, NbrPhrases, Moy) :-
	asserta(addition(0)),
	forall(
		member(Autre, Reste),
		addition(Phrase, Autre)
	),
	retract(addition(A)),
	Moy is A / NbrPhrases.

/**
 * Prédicat intermédiaire
 * addition(+Phrase, +Autre)
 * Met à jour l'addition des distances entre deux phrases
 * dans une structure addition/1 dans la base de connaissance.
 * 
 * @Phrase - Une première phrase.
 * @Autre - Une deuxième phrase. 
 */
addition(Phrase, Autre) :-
	distance(Phrase, Autre, Distance),
	retract(addition(A)),
	C is A + Distance,
	asserta(addition(C)).



/**
 * Efface tous les prédicats de la base de connaissance.
 */
teardownAll :-
	abolish(cible/1),
	abolish(compteur/1),
	abolish(sommet/3),
	abolish(arc/3).





	
    
