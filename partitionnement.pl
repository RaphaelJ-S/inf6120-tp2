% "E:/Depot_Git/inf6120/inf6120-tp2/".
/**
 * Nom : Jacob-Simard
 * Prénom : Raphaël
 * Code permanent : JACR26038907
 * Cours : INF6120
 * Groupe : 20
 */

/**
 * sommet/3 : sommet(Identifiant, Liste de phrases, rep/2)
 * arc/3 : arc(Identifiant du premier sommet, Identifiant du deuxième sommet, Distance entre les deux sommets)
 * cible/1 : cible(Le nombre de sommets à atteindre)
 * addition/1 : addition(valeur temporaire)
 * rep/2 : rep(Phrase type du sommet, Moyenne)
 */
:-dynamic sommet/3, arc/3, cible/1, addition/1, rep/2.


/**
 * Prédicat principal.
 * Ce prédicat charge la base de connaissance contenant le texte à résumer.
 * Ensuite il construit le résumé et l'affiche.
 */
resoudre( Nom, K ) :-
	consult( Nom ),
	init_teardown(K).

/**
 * Prédicat d'élimination de sommets.
 * fusionnerSommets(-Sommet)
 *
 * Supprime le plus petit arc et tous les arcs connectés aux sommets du plus petit arc.
 * Modifie la base de donnée avec un nouveau sommet/3 qui est la fusion des deux sommets 
 * et le retourne.
 *
 * @Sommet - le nouveau sommet/3
 */
fusionnerSommets :-
	aAtteintCible,
	getArcs(Arcs),
	trouverMin(Arcs, Min),
	arc(P1,P2,_) = Min,
	arc(P2, P1, _) = Bout,
	retract(sommet(P1, Phrases1, _)),
	retract(sommet(P2, Phrases2, _)),
	append(Phrases1, Phrases2, NvPhrases),
	phraseType(NvPhrases, PhraseType),
	Sommet = sommet(P1, NvPhrases, PhraseType),
	!,
	\+ enleverArcs(Min),
	assertz(Sommet),
	actualiserArcs(Sommet),
	fusionnerSommets.
	


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
 * actualiserArcs(+Sommet)
 *
 * Mets à jours la base de connaissances pour inclure les arcs du nouveau @Sommet
 * vers tous les autres sommets et vice-versa.
 *
 * @Sommet - Le nouveau sommet/3 à connecter aux autres sommets/3
 */
actualiserArcs(Sommet) :-
	getSommets(Sommets),
	select(Sommet, Sommets, Reste),
	!,
	ajouterArcs(Sommet, Reste),
	Liste = [Sommet],
	forall(
		member(Unit, Reste),
		ajouterArcs(Unit, Liste)
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
 * enleverArcs(+Arc)
 *
 * Supprime de la base de connaissance tous les arc/3 qui partagent un sommet
 * avec @Arc.
 *
 * @Arc - L'arc/3 pour lequel on désire supprimer toutes les connexions des sommets.
 */
enleverArcs(Arc) :-
	arc(P1, P2, D1) = Arc,
	retract(arc(P1, Fin, _)),
	retract(arc(P2, Fin, _)),
	retract(arc(Debut, P1, _)),
	retract(arc(Debut, P2, _)),
	fail.


/**
 * calculerNombreSommets(-NbrSommets)
 *
 * Retourne le nombre de sommets dans la base de connaissances.
 * 
 * @NbrSommets - Le nombre de sommets dans la base de connaissances.
 */
calculerNombreSommets(NbrSommets) :-
	getSommets(ListeSommets),
	length(ListeSommets, NbrSommets).

/**
 * Vérifie si le nombre de sommets cible a été atteint.
 */
aAtteintCible :-
	cible(A),
	calculerNombreSommets(B),
	B > A.

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
 * distanceEntreSommets(+ListePhrase1, +ListePhrase2, -Distance)
 */
distanceEntreSommets([X|XS], ListePhrase2, Distance) :-
	distanceEntreSommets(XS, ListePhrases2, Inter),
	distanceEntrePhraseEtListe(X, ListePhrase, Sub),
	Distance is Sub + Inter.
distanceEntreSommets([X], ListePhrase2, Distance) :-
	distanceEntrePhraseEtListe(X, ListePhrase2, Distance).

distanceEntrePhraseEtListe(Phrase, Liste, Distance) :-
	asserta(addition(0)),
	forall(
		member(Membre, Liste),
		addition(Phrase, Membre)
	),
	retract(addition(Distance)).

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
 * Calcule la distance entre deux phrases.
 * Cette valeur est un point flottant.
 * distance( +PhraseA, +PhraseB, -Distance ).
 * les arguments PhraseA et PhraseB sont des listes de mots (atome).
 * Si les phrases sont trop différente, alors la différence est de 1000.0.
 */
distance( PhraseA, PhraseB, Distance ) :-
    distanceD( PhraseA, PhraseB, DistanceAB ),
    distanceD( PhraseB, PhraseA, DistanceBA ),
    Distance is ( DistanceAB + DistanceBA ) / 2.0.

distanceD( PhraseA, PhraseB, Distance ) :-
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

% -------------- Affichages et getter --------------

/**
 * Affiche les compteurs, les sommet et les arcs.
 */
affiche :-
	getSommets(NvSommets),
	forall(
		member(Sommet, NvSommets),
		(
			sommet(_, _, rep(A,B))=Sommet,
			concat(A, Phrase),
			write(B), write(" : "), write(Phrase),write("."),nl
		)
	).

/**
 * concat(+ListeString, -String)
 *
 * Concatene une liste de string avec chaque mot séparé d'un espace.
 *
 * @ListeString - La liste de string à concatèner.
 * @String - La chaîne résultante.
 */
concat([X,Y|XS], String) :-
	string_concat(X," ",TmpString),
	string_concat(TmpString, Y, TmpString2),
	concat([TmpString2|XS], String).
concat([X], X).

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
		\+ fusionnerSommets,
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
	initSommets,
	calculerNombreSommets(NbrSommets),
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
	affiche,
	abolish(cible/1),
	abolish(sommet/3),
	abolish(arc/3).





	
    
