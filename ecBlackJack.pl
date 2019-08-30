%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simple example for Prolog game, including Tau-Prolog and Pengine
%
% Based on a example for an article for the German magazine Heise Online
% Code uses asserta to store the card deck. 
% 
% Author: Hans N. Beck (c)
% Last Change: 28.08.2019
%
% License: MIT 
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(ecBlacJack, [playGame/2, do/6, showDeck/1]).
:- use_module(library(pengines)).

:- dynamic card/3.


% Faktenbasis - das Kartendeck.
% card(Kartenfarbe, Kartenname, Punktwert) sei im folgenden benutze Kartenstruktur
card(herz, 10, 10).
card(herz, bube, 11).
card(herz, dame, 12).
card(herz, koenig, 13).
card(herz, ass, 14).

% Wer sticht wen ?
win(dame, bube).
win(koenig, dame).
win(ass, koenig).

% Beschreibung einer transitive Relation
% winAlso(+Kartenname, -Kartenname)
winAlso(X,Y) :-
	win(X,dame),
	win(dame, Y).

%%%%%%%%% Datenbank programmatisch erweitern %%%%%%%%%%%%%%%%

% Abbruchbedingung
addFact(_, End, End).
% addFact(+Kartenfarbe, Endnummer, -Laufender Index)
addFact(Farbe, End, I) :-
	asserta(card(Farbe, I, I)),
	I2 is I + 1,
	addFact(Farbe, End, I2).

% Die beiden müssen in dieser Reihenfolge sein, sonst endet es nicht !


% Fülle alle Karten zwischen Start Nummer und EndNummer auf
% fillDB(+Kartenfarbe, +StartNummer, +EndNummer)
fillDB(Farbe, Start, End) :-
	End2 is End + 1, 
	addFact(Farbe, End2, Start).

% initialisiere das Deck
% initDeck(-Liste aller nun bekannten Karten)
initDeck(List) :-
	fillDB(herz, 1, 9),	
	showDeck(List).

% showDeck(-Liste aller nun bekannten Karten)
showDeck(List) :- 
	findall(X, card(_, X, _), List).


% Beispiel für eine Datenstruktur
% newPlayer(+Spielernummer, -Player Struktur)
% player(Spielernummer, Ausspielfeld als Liste von Kartenstrukturen)
newPlayer(Num, player(Num, [])).

%%%%%%%%%%% Alternative für eine Kartenbasis %%%%%%%%%%%%%%%

% Kartenbasis mit Array - Vorteil, wenn mehrer Instanzen eines Typs nötig sind
% oder für sortieren, und häufige Manipulationen

% so gehts nicht !
addCard(Deck, Farbe, Name, Deck2) :-
	A = card(Farbe, Name, _),
	append(Deck, [A], Deck2).
% so gehts
addCard2(Deck, Farbe, Name, Deck2) :-
	card(Farbe, Name, X),
	append(Deck, [card(Farbe, Name, X)], Deck2).
% so gehts mit wiederverwendbarer Variable
addCard3(Deck, Farbe, Name, Deck2) :-
	cardBuilder(Farbe, Name, C),
	append(Deck, [C], Deck2).
% ein Builder als Hilfskonstrukt
cardBuilder(Farbe, Name, card(Farbe, Name, X)) :-
	card(Farbe, Name, X).


%%%%%%%%%%%%%%%%%%% Spielaktionen %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Karten auspielen oder vom Deck "ziehen"

% Eine Möglichkeit
% drawCard(+Kartenfarbe, +Kartenname, -Kartenstruktur)
drawCard(Farbe, Name, Card) :-
	Card = card(Farbe, Name, _),
	retract(Card). % retract instanziert auch intern

% Zufallsziehen
% drawCard(-Kartenstruktur)
drawCard(Card) :-
	random_between(1,14, Num), 
	Card = card(_, _, Num),
	retract(Card). 

% Greife auf die Kartenpunkte der Struktur zu
% cardPoints(+Farbe, +Name, -Points)
cardPoints(Farbe, Name, Points) :-
	card(Farbe, Name, Points).


%%%% Karte auspielen

%% Hier ist Player eine Zahl
% playCard(+Spielernummer, +Kartenfarbe, +Kartenname, +Spielfeld vorher, -Spielfeld nachher)
playCard(_, Farbe, Name, Field, Field2) :-
	drawCard(Farbe, Name, Card), 
	append(Field, [Card], Field2).

%% Besser strukturiert
% playCard(+Spielerstruktur, vorher, -Spielerstruktur nacher)
playCard(player(Num, Field), player(Num, Field2)) :-
	drawCard(Card), 
	append(Field, [Card], Field2), 
	format("your draw ~w\n",  [Card]).

%% Variante für den Interpreter
playCard(P1, P2, go) :-
	playCard(P1, P2).


%%%%%%%%%%%%%%%%%%%%% Gewinnregeln %%%%%%%%%%%%%%%%%%

%% Kartenauswertung

% sumPoints(+Kartenstruktur, +Punktesumme vorher, -Punktesumme plus Kartenpunkte)
sumPoints(card(_,_,Point), Sum, Sum2) :-
	Sum2 is Sum + Point.

% Abbruchbedingung
sumCards([], Sum, Sum).
% sumCards(+Liste von Karten als Kartenstrukturen, +Punkte vorher, -Punkte summiert)
sumCards([C|Cards], Sum, Sum3) :-
	sumPoints(C, Sum, Sum2),
	sumCards(Cards, Sum2, Sum3).
% sumCards(s(+Liste von Karten als Kartenstrukturen, -Summe der Punkte aller Karten)
sumCards(Cards, Sum) :-
	sumCards(Cards, 0, Sum).


%%% Gewinnbedingungen

% Teste ob 21 überschritten ist
% Distance ist im folgenden ein Abstand eines Punktwertes zu 21

% cardsTest(+Kartenliste, -Abstand zu 21 (ggf negativ))
cardsTest(Cards, Distance) :-
	sumCards(Cards, Sum), 
	Distance is 21 - Sum.
% Test auf unter 21
cardsOk(Distance, ok) :-
	Distance >= 0.
% Test über 21
cardsOk(Distance, loser) :-
	Distance < 0.

%%% Gewinnreglen

winner( [], [], 0). 

winner([C|Cs1], [C2|Cs2], Winner) :-
	cardsTest([C|Cs1], Distance1), 
	cardsTest([C2|Cs2], Distance2),
	winner(Distance1, Distance2, Winner).

% Auswertung der Abstände zu 21 und daraus festzulegender Gewinner
% Spieler 1 gewinnt, wenn er unter 21 ist und Spiele 2 nicht
winner(Distance1, Distance2, 1) :-
	cardsOk(Distance1, ok),
	cardsOk(Distance2, loser). 
% Spieler 2 gewinnt, wenn er unter 21 ist und Spieler 1 nicht	
winner(Distance1, Distance2, 2) :- 
	cardsOk(Distance2, ok),
	cardsOk(Distance1, loser). 
% Spieler 1 gewinnt, wenn er näher an 21 ist
winner(Distance1, Distance2, 1) :-
	Distance1 < Distance2.
% Spieler 2 gewinnt, wenn er näher an 21 ist
winner(Distance1, Distance2, 2) :-
	Distance1 > Distance2.

% Kein Gewinner in allen anderen Fällen
winner(_, _, 0).


%%%%%%%%%%%%% simpler Interpreter für das Spiel %%%%%%%%%%%%%
% dieses Prädikat startet das sehr simple Spiel mit der Eingabe 
% "playGame()." in der Kommandozeile eines neu gestarteten SWI-Prolog.
% Abwechselnd geben die Spieler ihre Kommandos ein (immer mit Punkt dahinter!)
% mögliche Weiterentwicklungen:
% in dieser simplen Variante werden Karten nur gelöscht.
% es muss aber sichergestellt sein, dass das Ziehen einer Karte
% immer eine Karte liefert, solange noch welche im Deck sind.
% Dann könnte man Karten zugedeckt ziehen und aufdecken wie
% im richtigen 17+4 usw.
% Ausserdem sollte der Cut Operator benutzt werden, es sind 
% Endlosschleifen möglich etc ;)
% Was man auch verbessern kann: da die Wissensbasis direkt manipuliert wird, 
% muss das Beispiel in einem neu gestarteten SWI-Prolog ausgeführt werden 
%
playGame(P1, P2) :-
	initDeck(_), 
	newPlayer(1, P1), 
	newPlayer(2, P2).
	%play(P1, P2, go).

% A, P sind player strukturen, für aktiven und passiven Spieler
play(A, P, go) :-
	A = player(Num, _),
	format("Player ~d <playCard> or <stop> ", [Num] ), 
	read(Command), 
	do(Command, A, P, A2, P2, Finish),
	nextPlayer(A2, P2, A3, P3),
	play(A3, P3, Finish).

play(_, _, stop).

% Kommando: Spielende und Gewinnermittlung
stop(player(1, Feld1), player(2, Feld2), stop) :-
	stateWinner(Feld1, Feld2).
% falls der aktive Spieler Nr. 2 war
stop(player(2, Feld2), player(1, Feld1), stop) :-
	stateWinner(Feld1, Feld2).

% +Feld1 Karten des Spieler 1 
% +Feld2 Karten des Spieler 2
% -Winner wird instanziert mit gewinnender Spielernummer
stateWinner(Feld1, Feld2) :-
	winner(Feld1, Feld2, Winner), 
	format("The Winner is ~d\n", [Winner]).


% nur player 1 
do(stop, A, P, A, P, Finish) :- 
	call(stop, A, P, Finish).

% ziehe eine Karte und spiele sie aus:
do(playCard, A, P, A2, P, Finish) :-
	call(playCard, A, A2, Finish).

do(showDeck, A, P, A, P, go) :-
	showDeck(L), 
	format("Current Deck: ~p\n", [L]).

% Achtung: man mache sich klar: 
% dieses Prädikat wird nicht nur aufgerufen, wenn ein Kommando nicht erkannt wird
% sondern auch, wenn etwas dazu führt, dass die Regel falsch ist - 
% z.B. wenn eine playCard versucht eine schon gespielte Karte zu spielen und "ins leere greift"
% siehe obiger Kommentar für Verbesserungen.

do(_, A, P, A, P, _) :-
	format("Rubbisch, commands are <playCard> or <stop> \n").

% Spielerwechsel: aktiver wird passiver und umgekehrt
nextPlayer(player(1, F1), player(2, F2), player(2, F2), player(1, F1)).
nextPlayer(player(2, F2), player(1, F1), player(1, F1), player(2, F2)).

% Call