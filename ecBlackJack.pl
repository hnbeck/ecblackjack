%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simple example for a Black Jack Prolog game, including Tau-Prolog and Pengine
%
% Based on an example for an article for the German magazine Heise Online
% written by me.
% Code uses asserta to store the card deck. 
% 
% Author: Hans N. Beck (c)
% Last Change: 05.10.2019
%
% License: MIT 
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(ecBlackJack, [playGame/4, stop/4, listDeck/2, playCard/5]).

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- dynamic card/3.
:- multifile sandbox:safe_primitive/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Note: this is the server side Prolog level of the application. It implements
% the game rules, the objects of the games (the cards), their logical structure
% It declares which structural changes are associated with the playing actions. 
% But it says nothing about where the cards laying, if we have a desk or 
% using only our hands. It says nothing about the order of the cards or time. 
% That all is not a natural part of the game itself.
% Display, geometry and such things are responsibility of the browser, which is
% the Javascript or Tau-Prolog level
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% picture deck cards
% card structure is: card(card color, card name, points)
% card color is one of herz caro kreuz schippe (German identifier)
card(herz, 10, 10).
card(herz, bube, 11).
card(herz, dame, 12).
card(herz, koenig, 13).
card(herz, ass, 14).
card(karo, 10, 10).
card(karo, bube, 11).
card(karo, dame, 12).
card(karo, koenig, 13).
card(karo, ass, 14).
card(kreuz, 10, 10).
card(kreuz, bube, 11).
card(kreuz, dame, 12).
card(kreuz, koenig, 13).
card(kreuz, ass, 14).
card(schippe, 10, 10).
card(schippe, bube, 10).
card(schippe, dame, 10).
card(schippe, koenig, 10).
card(schippe, ass, 11).

% Provide access to card structure
% cardPoints(+ Card Color, +Name, -Points)
cardPoints(Farbe, Name, Points) :-
	card(Farbe, Name, Points).

%%%%%%%%%%%%%%%%%%%%%%% Fill up card data base %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initialize deck. Picture cards were added by hand -> see above
% now fill up the number cards
% initDeck(-list of all known cards)
initDeck() :-
	fillDeck(herz, 2, 9),	
	fillDeck(karo, 2, 9),	
	fillDeck(schippe, 2, 9),
	fillDeck(kreuz, 2, 9).

% add a card to Prolog db = card deck here
% stop
addFact(_, End, End).
% addFact(+color, end number, -index)
addFact(Farbe, End, I) :-
	asserta(card(Farbe, I, I)),
	I2 is I + 1,
	addFact(Farbe, End, I2).

% fill all cards between start and end number
% fillDeck(+color, +start number, +end Number)
fillDeck(Farbe, Start, End) :-
	End2 is End + 1, 
	addFact(Farbe, End2, Start).

% listDeck(-List: list of all known cards, -N: number of cards )
listDeck(List, N) :- 
	findall(card(A, B, C), card(A, B, C), List),
	length(List, N).

% needed for pengine - declare safe predicate
sandbox:safe_primitive(ecBlackJack:listDeck(_,_)).

% newPlayer(+player number, -Player structure)
% player structure is player(Playernumber, List of his cards)
% List of cards is also called "Playfield" or Field for short
newPlayer(Num, player(Num, [])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% play actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% idea: there is player 1 and player 2. In every turn, one is the active player
% after a play action, the active and passive player changes
% Init the game
% playGame(	-P1: first player, 
% 			-P2: scnd player, 
%			-Flag: go or turn or stop indicating if game contiune and how
%			-Msg: a message string)
playGame(P1, P2, go, Msg) :-
	initDeck(), 
	newPlayer(1, P1), 
	newPlayer(2, P2),
	format(atom(Msg), '~s', ["Game can start now"]).

% Kommando: game over, can be triggerd by player or system
% stop(+P1: a player, -P2: changed player, -Flag: stop or continue, -Msg: Message)
% if player 1 was active player
stop(player(1, Feld1), player(2, Feld2), stop, Msg) :-
	stateWinner(Feld1, Feld2, Msg).
% if active player was player 2
stop(player(2, Feld2), player(1, Feld1), stop, Msg) :-
	stateWinner(Feld1, Feld2, Msg).

% playCard(+player, -updated player)
playCard(player(Num, Field), player(Num, Field2), Card, Flag, Msg) :-
	drawCard(Card),
	append(Field, [Card], Field2),
	stopCondition(Field2, Flag), 
	format(atom(Msg), "your draw ~w\n",  [Card]).
% drawCard was false - no Cards anymore in Deck - stop Game
playCard(P1, P2, _, stop, Msg) :-
	stop(P1, P2, _, Msg ).

%%%%%%%%%%%%%%%%%%%% transformations - not visible outside %%%%%%%%%%%%%%%%%%%%%

% Draw a Card from deck. The card is removed from deck, which means here
% is is removed from the Prolog database
% drawCard(-card structure)
% can be false if deck is empty
drawCard(card(A, B, C)) :-
	listDeck(CardList, N),
	random_between(1, N, Num), 
	nth1(Num, CardList, card(A, B, C)),
	retractall(card(A,B,C)). 

sandbox:safe_primitive(ecBlackJack:drawCard(_)).

% +Feld1 cards of player 1 
% +Feld2 cards of player 2
% -Winner hold the number of winning player
stateWinner(Feld1, Feld2, Msg) :-
	winner(Feld1, Feld2, Winner), 
	format(atom(Msg), "The Winner is ~d\n", [Winner]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% game and winning rules %%%%%%%%%%%%%%%%%%%%%%%%%%%

% which card  wins over which other?
win(dame, bube).
win(koenig, dame).
win(ass, koenig).
blackjack(ass, dame).
blackjack(ass, koenig).
blsckjack(ass, bube).

% transitiv wining relation
winAlso(X,Y) :-
	win(X,dame),
	win(dame, Y).

% test if 21 is crossed
% distance is the distance to 21 from both directions

% cardsTest(+card list, -distance)
cardsTest(Cards, Distance) :-
	cardsSum(Cards, Sum), 
	Distance is 21 - Sum.
% test of < 21
cardsOk(Distance, turn) :-
	Distance >= 0.
% Test of > 21
cardsOk(Distance, stop) :-
	Distance < 0.

%%% sum the points of all cards of given list
% cardsSum(s(+card list, -sum of all card points)
cardsSum(Cards, Sum) :-
	cardsSum(Cards, 0, Sum).
% stop condition
cardsSum([], Sum, Sum).
% cardsSum(+card list, +points before, -summized points)
cardsSum([C|Cards], Sum, Sum3) :-
	sumPoints(C, Sum, Sum2),
	cardsSum(Cards, Sum2, Sum3).

% sum a card to the current points
% sumPoints(+card, +sum before, -sum plus card points)
sumPoints(card(_,_,Point), Sum, Sum2) :-
	Sum2 is Sum + Point.

%%%%%%%%%%%%%%%%% winning rules %%%%%%%%%%%%%%%%%%%

% check if game is over - this is when a player is over 21 points
stopCondition( [], go).
% stopCondition(+L:list of cards, -Flag: go or stop)
stopCondition([C|Cs1], Flag) :-
	cardsTest([C|Cs1], Distance1), 
	cardsOk(Distance1, Flag).

% Evaluation of distance to 21 for every player and determing winner
% player 1 wins if <21 and player 2 not
winner( [], [], 0). 
%
winner([C|Cs1], [C2|Cs2], Winner) :-
	cardsTest([C|Cs1], Distance1), 
	cardsTest([C2|Cs2], Distance2),
	judgement(Distance1, Distance2, Winner).

judgement(Distance1, Distance2, 1) :-
	cardsOk(Distance1, turn),
	cardsOk(Distance2, stop). 
% player 2 wins if <21 and player1 not	
judgement(Distance1, Distance2, 2) :- 
	cardsOk(Distance2, turn),
	cardsOk(Distance1, stop). 
% player 1 wins if closer to 21
judgement(Distance1, Distance2, 1) :-
	Distance1 < Distance2.
% player 2 wins if closer to 21
judgement(Distance1, Distance2, 2) :-
	Distance1 > Distance2.

% no winner in all ather cases
judgement(_, _, 0).
