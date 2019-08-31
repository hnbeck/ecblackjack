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
:- module(ecBlackJack, [playGame/3, stop/4, showDeck/1]).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- dynamic card/3.

:- multifile sandbox:safe_primitive/1.

% card deck
card(herz, 10, 10).
card(herz, bube, 11).
card(herz, dame, 12).
card(herz, koenig, 13).
card(herz, ass, 14).

% who wins ?
win(dame, bube).
win(koenig, dame).
win(ass, koenig).

% transitiv wining relation
winAlso(X,Y) :-
	win(X,dame),
	win(dame, Y).

%%%%%%%%% Expand data base %%%%%%%%%%%%%%%%

% stop
addFact(_, End, End).
% addFact(+color, end number, -index)
addFact(Farbe, End, I) :-
	asserta(card(Farbe, I, I)),
	I2 is I + 1,
	addFact(Farbe, End, I2).



% fill all cards between start and end
% fillDeck(+color, +start number, +EndNummer)
fillDeck(Farbe, Start, End) :-
	End2 is End + 1, 
	addFact(Farbe, End2, Start).

% initialize deck
% initDeck(-list of all known cards)
initDeck(List) :-
	fillDeck(herz, 1, 9),	
	showDeck(List).

% showDeck(-list of all known cards)
showDeck(List) :- 
	findall(X, card(_, X, _), List).

sandbox:safe_primitive(ecBlackJack:showDeck(_)).

% Example for a data structure
% newPlayer(+player number, -Player structure)
newPlayer(Num, player(Num, [])).

%%%%%%%%%%% Alternative for deck - using arrays and not the database %%%%%%%%%%%%%%%

addCard2(Deck, Farbe, Name, Deck2) :-
	card(Farbe, Name, X),
	append(Deck, [card(Farbe, Name, X)], Deck2).
% variant
addCard3(Deck, Farbe, Name, Deck2) :-
	cardBuilder(Farbe, Name, C),
	append(Deck, [C], Deck2).
% generator
cardBuilder(Farbe, Name, card(Farbe, Name, X)) :-
	card(Farbe, Name, X).


%%%%%%%%%%%%%%%%%%% play actions %%%%%%%%%%%%%%%%%%%%%%%%%%%


% random draw
% drawCard(-structure)
drawCard(Card) :-
	random_between(1,14, Num), 
	Card = card(_, _, Num),
	retractall(Card). 

sandbox:safe_primitive(ecBlackJack:drawCard(_)).

% cardPoints(+color, +Name, -Points)
cardPoints(Farbe, Name, Points) :-
	card(Farbe, Name, Points).


%%%% play card

% playCard(+player, -updated player)
playCard(player(Num, Field), player(Num, Field2), Msg) :-
	drawCard(Card), 
	append(Field, [Card], Field2), 
	format(atom(Msg), "your draw ~w\n",  [Card]).

%% command for Interpreter
playCard(P1, P2, go, Msg) :-
	playCard(P1, P2, Msg).


%%%%%%%%%%%%%%%%%%%%% winning rules %%%%%%%%%%%%%%%%%%

%% Evaluation of cards

% sumPoints(+card, +sum before, -sum plus card points)
sumPoints(card(_,_,Point), Sum, Sum2) :-
	Sum2 is Sum + Point.

% Abbruchbedingung
sumCards([], Sum, Sum).
% sumCards(+card list, +points before, -summized points)
sumCards([C|Cards], Sum, Sum3) :-
	sumPoints(C, Sum, Sum2),
	sumCards(Cards, Sum2, Sum3).
% sumCards(s(+card list, -sum of all card points)
sumCards(Cards, Sum) :-
	sumCards(Cards, 0, Sum).


%%% winning rules

% test if 21 is crossed
% distance is the distance to 21 from both directions

% cardsTest(+card list, -distance)
cardsTest(Cards, Distance) :-
	sumCards(Cards, Sum), 
	Distance is 21 - Sum.
% test < 21
cardsOk(Distance, ok) :-
	Distance >= 0.
% Test over 21
cardsOk(Distance, loser) :-
	Distance < 0.

%%% Rules

winner( [], [], 0). 

winner([C|Cs1], [C2|Cs2], Winner) :-
	cardsTest([C|Cs1], Distance1), 
	cardsTest([C2|Cs2], Distance2),
	winner(Distance1, Distance2, Winner).

% Evaluation of distance to 21 for every player and determing winner
% player 1 wins if <21 and player 2 not
winner(Distance1, Distance2, 1) :-
	cardsOk(Distance1, ok),
	cardsOk(Distance2, loser). 
% player 2 wins if <21 and player1 not	
winner(Distance1, Distance2, 2) :- 
	cardsOk(Distance2, ok),
	cardsOk(Distance1, loser). 
% player 1 wins if closer to 21
winner(Distance1, Distance2, 1) :-
	Distance1 < Distance2.
% player 2 wins if closer to 21
winner(Distance1, Distance2, 2) :-
	Distance1 > Distance2.

% no winner in all ather cases
winner(_, _, 0).


%%%%%%%%%%%%% simpler Interpreter for the fame %%%%%%%%%%%%%
%
% Init the game
playGame(P1, P2, Msg) :-
	initDeck(_), 
	newPlayer(1, P1), 
	newPlayer(2, P2),
	format(atom(Msg), '~s', ["Game can start now"]).


% Kommando: game over
stop(player(1, Feld1), player(2, Feld2), stop, Msg) :-
	stateWinner(Feld1, Feld2, Msg).
% if active player was player 2
stop(player(2, Feld2), player(1, Feld1), stop, Msg) :-
	stateWinner(Feld1, Feld2, Msg).

% +Feld1 cards of player 1 
% +Feld2 cards of player 2
% -Winner hold the number of winning player
stateWinner(Feld1, Feld2, Msg) :-
	winner(Feld1, Feld2, Winner), 
	format(atom(Msg), "The Winner is ~d\n", [Winner]).


% nur player 1 
% +A active player +P passive player -Finish finish flag
%do(stop, A, P, A, P, Finish, Msg) :- 
%	call(stop, A, P, Finish, Msg).

% draw card and play
%do(playCard, A, P, A2, P, Finish, Msg) :-
%	call(playCard, A, A2, Finish, Msg).

%do(showDeck, A, P, A, P, go, Msg) :-
%	showDeck(L), 
%	format(atom(Msg), 'Current Deck: ~p\n', [L]).

% wrong command
%do(_, A, P, A, P, _, Msg) :-
%	format(atom(Msg), 'Rubbisch, commands are <playCard> or <stop> \n').

% the do command is redunant by using with pengines
% showDeck, playCard and stop will be called directly

%% this is also done in Tau Prolog now
%nextPlayer(player(1, F1), player(2, F2), player(2, F2), player(1, F1)).
%nextPlayer(player(2, F2), player(1, F1), player(1, F1), player(2, F2)).

% Call