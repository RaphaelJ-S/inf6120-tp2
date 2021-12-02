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
 * Prédicat d'élimination de sommets.
 */
fusionnerArcs(NvId, NvPhrases) :-
	getArcs(Arcs),
	trouverMin(Arcs, Min),
	arc(P1,P2,D1) = Min,
	arc(P2, P1, D1) = Bout,
	retract(sommet(P1, Phrases1, _)),
	retract(sommet(P2, Phrases2, _)),
	append(Phrases1, Phtases2, NvPhrases),
	enleverArcs(Min).
	




enleverArcs(Arc) :-
	arc(P1, P2, D1) = Arc,
	retract(arc(P1, Fin, _)),
	retract(arc(P2, Fin, _)),
	retract(arc(Debut, P1, _)),
	retract(arc(Debut, P2, _)),
	fail.
	
/**
 * trouverMin(+Liste, -Min)
 * Retourne le plus petit arc d'une liste d'arcs.
 * 
 * @Liste - Une liste d'arc/3.
 * @Min - L'arc/3 ayant la distance la plus petite.
 */
trouverMin([X,Y|XS], Min):-
	X = arc(P1,P2,D1),
	Y = arc(P3, P4, D2),
	D1 =< D2,
	trouverMin([X|XS], Min).
trouverMin([X,Y|XS], Min):-
	X = arc(P1,P2,D1),
	Y = arc(P3, P4, D2),
	D1 > D2,
	trouverMin([Y|XS], Min).
trouverMin([X], X).

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

% -------------- Affichages et getter --------------


/**
 * Affiche les compteurs, les sommet et les arcs.
 */
affiche :-
	cible(S), write(S),nl,
	compteur(C), write(C), nl,
	getSommets(Sommets),
	afficher(Sommets),
	getArcs(Arcs),
	afficher(Arcs),nl,nl,
	fusionnerArcs(NvId, NvPhrases); % Il faut maintenant créer le nouveau sommet et les nouveau arcs
	getArcs(NvArcs),
	getSommets(NvSommets),
	afficher(NvArcs),
	afficher(NvSommets).


/**
 * afficher(+Liste)
 * Affiche chaque élément d'une liste suivit d'un saut de ligne.
 *
 * @Liste - La liste à afficher.
 */
afficher(Liste) :-
	forall(member(Elem, Liste), (write(Elem), nl )).


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


% ----------------- Initialisation et teardown ------------


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
 * Efface tous les prédicats de la base de connaissance.
 */
teardownAll :-
	abolish(cible/1),
	abolish(compteur/1),
	abolish(sommet/3),
	abolish(arc/3).





	
    
