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
:- module(ecBlackJack, [createGame/4, stop/5, listDeck/2, playCard/5, banksTurn/7]).

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- dynamic card/4.
:- dynamic current_process/4, current_location/3.

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
card(herz, 10, 10, up).
card(herz, bube, 10, up).
card(herz, dame, 10, up).
card(herz, koenig, 10, up).
card(herz, ass, 11, up).
card(karo, 10, 10, up).
card(karo, bube, 10, up).
card(karo, dame, 10, up).
card(karo, koenig, 10, up).
card(karo, ass, 11, up).
card(kreuz, 10, 10, up).
card(kreuz, bube, 10, up).
card(kreuz, dame, 10, up).
card(kreuz, koenig, 10, up).
card(kreuz, ass, 11, up).
card(schippe, 10, 10, up).
card(schippe, bube, 10, up).
card(schippe, dame, 10, up).
card(schippe, koenig, 10, up).
card(schippe, ass, 11, up).

% Provide access to some data structures
% cardPoints(+ Card Color, +Name, -Points)
cardPoints(Farbe, Name, Points) :-
	card(Farbe, Name, Points, _).

playerCardsCount(player(_, List), Len) :-
	length(List, Len).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Server Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code by Anne Ogborn from Ludum Dara44 Game

createGame(_,_, go, 'EC Black Jack - Enjoy') :-
	pengine_self(PengineID),
	current_process(PengineID, _, _, _),
	!,
	debug(ld(redundant), 'game already created', []).

createGame(P1, P2, State, Msg) :-
	pengine_self(PengineID),
	playGame(P1, P2, State, Msg),
	thread_at_exit(killGame(PengineID)).

sandbox:safe_primitive(ecBlackJack:createGame(_,_,_,_)).

killGame(PengineID) :-
	current_process(PengineID, PID, _, _),
	process_kill(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% play actions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% idea: there is player 1 and player 2. In every turn, one is the active player
% after a play action, the active and passive player changes
% Init the game
% playGame(	-P1: first player, 
% 			-P2: scnd player, 
%			-Flag: go or turn or stop indicating if game contiune and how
%			-Msg: a message string)
playGame(P12, P22, start, Msg3) :-
	genDeck, 
	genPlayer(1, P1), 
	genPlayer(2, P2),
	initialDraw(P1, P12, _, '', Msg2),
	dealerDraw(P2, P22, Msg2, Msg3).

% initial Draw according black jack rules: 
% every player 2 cards, dealer 1 open one fliped)
% stop condition will not be applied
% the first draw
initialDraw(P1, P13, _, Msg, Msg5) :-
	playCard(P1, P12, _, _, Msg2),
	format(atom(Msg3), '~s~n~s~n',  [Msg, Msg2]),
	playCard(P12, P13, _,  _, Msg4),
	format(atom(Msg5), '~s~n~s~n',  [Msg3, Msg4]).

dealerDraw(P, P4, Msg, Msg4) :-
	playCard(P, P2, _, _, Msg2),
	format(atom(Msg3), '~s~n~s~n',  [Msg, Msg2]),
	playCard(P2, P3, _,  _, _),
	flipLastCard(P3, P4),
	format(atom(Msg4), '~s~n~s~n',  [Msg3, 'Dealer draw done']).

% bank is active player, passive player remains unchainged
banksTurn(PA, PP, PA3, PP, Winner, Flag, Msg3) :-
	flipAllCards(PA, PA2),
	bankStrategy(PA2, PA3, Msg),
	stop(PA3, PP, Winner, Flag, Msg2),
	format(atom(Msg3), '~s~n~s~n',  [Msg, Msg2]).

% playCard(+player, -updated player)
playCard(player(Num, Field), player(Num, Field2), Card, Flag, Msg) :-
	drawCard(Card),
	append(Field, [Card], Field2),
	stopCondition(Field2, Flag),
	format(atom(Msg), "You draw ~w~n",  [Card]).
% drawCard was false - no Cards anymore in Deck - stop Game
% only theoretical case
playCard(P1, P2, _, over, Msg) :-
	stop(P1, P2, _, _, Msg ).

bankStrategy(player(N, Field), player(N, Field2), Msg) :-
	stopCondition(Field, Flag),
	(Flag = go ->
		lastDrawStrategy(player(N, Field), player(N, Field2), Msg);
		Field2 = Field,
		Msg = ''
	).

lastDrawStrategy(player(N, Field), player(N, Field2), Msg) :-
	cardsSum(Field, Points),
	(Points < 17 ->
		playCard(player(N, Field), player(N, Field2),_, _, Msg);
		Field2 = Field,
		Msg = ''
	).
% Kommando: game over, can be triggerd by player or system
% stop(+P1: a player, -P2: changed player, -Flag: stop or continue, -Msg: Message)
% if player 1 was active player
stop(player(1, Feld1), player(2, Feld2), Winner, over, Msg) :-
	stateWinner(Feld1, Feld2, Winner, Msg).
% if active player was player 2
stop(player(2, Feld2), player(1, Feld1), Winner, over, Msg) :-
	stateWinner(Feld1, Feld2, Winner, Msg).

%%%%%%%%%%%%%%%%%%%% transformations - not visible outside %%%%%%%%%%%%%%%%%%%%%

% Draw a Card from deck. The card is removed from deck, which means here
% is is removed from the Prolog database
% drawCard(-card structure)
% can be false if deck is empty
drawCard(card(A, B, C, _)) :-
	listDeck(CardList, N),
	random_between(1, N, Num), 
	nth1(Num, CardList, card(A, B, C, _)),
	retractall(card(A, B, C, _)). 

sandbox:safe_primitive(ecBlackJack:drawCard(_)).

flipLastCard(player(N,  Field1), player(N, Field2)) :-
	flipLast(Field1, Field2).

flipLast([], []).
flipLast(card(A, B, C, up), card(A, B, C, down)).
flipLast([C], [C2]) :-
	flipLast(C, C2).
flipLast([C|CS], [C|CS2]) :-
	flipLast(CS, CS2).

flipAll([], []).
flipAll([card(A, B, C, _)| CS], [card(A, B, C, up) | CS2]) :-
	flipAll(CS, CS2).
	
flipAllCards(player(N,  Field1), player(N,  Field2)) :-
	flipAll(Field1, Field2).

% +Feld1 cards of player 1 
% +Feld2 cards of player 2
% -Winner hold the number of winning player
stateWinner(Feld1, Feld2, Winner, Msg) :-
	winner(Feld1, Feld2, Winner), 
	format(atom(Msg), 'The Winner is ~d~n', [Winner]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% game and winning rules %%%%%%%%%%%%%%%%%%%%%%%%%%%

blackjackDef(ass, dame).
blackjackDef(ass, koenig).
blackjackDef(ass, bube).
blackjackDef(ass, 10).


% check if game is over - this is when a player is over 21 points
stopCondition([], go).
% stopCondition(+L:list of cards, -Flag: go or stop)
% 
stopCondition(Cards, Flag2) :-
	blackjack(Cards, Flag),
	(Flag = go ->
		bust(Cards, Flag2);
		Flag2 = Flag
	).
		
% is blackjack
blackjack([card(_, Name, _, up), card(_, Name2, _, up)], win) :- 
	blackjackDef(Name, Name2),!.

blackjack([_|_], go).

% test if 21 is crossed
% distance is the distance to 21 from both directions
bust(Cards, Flag) :-
	cardsTest(Cards, Distance1), 
	cardsOk(Distance1, Flag).

% cardsTest(+card list, -distance)
cardsTest(Cards, Distance) :-
	cardsSum(Cards, Sum), 
	Distance is 21 - Sum.
% test of < 21
cardsOk(Distance, go) :-
	Distance >= 0.
% Test of > 21
cardsOk(Distance, bust) :-
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
sumPoints(card(_,_,Point, up), Sum, Sum2) :-
	Sum2 is Sum + Point.
sumPoints(card(_,_,_, down), Sum, Sum).

%%%%%%%%%%%%%%%%% winner rules %%%%%%%%%%%%%%%%%%%

% Evaluation of distance to 21 for every player and determing winner
% player 1 wins if <21 and player 2 not
winner( [], [], 0). 
%
winner(Cards1, Cards2, Winner) :-
	cardsTest(Cards1, Distance1), 
	cardsTest(Cards2, Distance2),
	judgement(Distance1, Distance2, Winner).

judgement(Distance1, Distance2, 1) :-
	cardsOk(Distance1, go),
	cardsOk(Distance2, bust). 
% player 2 wins if <21 and player1 not	
judgement(Distance1, Distance2, 2) :- 
	cardsOk(Distance2, go),
	cardsOk(Distance1, bust). 
% player 1 wins if closer to 21
judgement(Distance1, Distance2, 1) :-
	Distance1 < Distance2.
% player 2 wins if closer to 21
judgement(Distance1, Distance2, 2) :-
	Distance1 > Distance2.

% no winner in all ather cases
judgement(_, _, 0).


%%%%%%%%%%%%%%%%%%%%%%%% generators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% genPlayer(+player number, -Player structure)
% player structure is player(Playernumber, List of his cards)
% List of cards is also called "Playfield" or Field for short
genPlayer(Num, player(Num, [])).


%%%%%%%%%%%%%%%%%%%%%% Fill up card data base %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initialize deck. Picture cards were added by hand -> see above
% now fill up the number cards
% genDeck(-list of all known cards)
genDeck :-
	fillDeck(herz, 2, 9),	
	fillDeck(karo, 2, 9),	
	fillDeck(schippe, 2, 9),
	fillDeck(kreuz, 2, 9).

% fill all cards between start and end number
% fillDeck(+color, +start number, +end Number)
fillDeck(Farbe, Start, End) :-
	End2 is End + 1, 
	addFact(Farbe, End2, Start).

% add a card to Prolog db = card deck here
% stop
addFact(_, End, End).
% addFact(+color, end number, -index)
addFact(Farbe, End, I) :-
	asserta(card(Farbe, I, I, up)),
	I2 is I + 1,
	addFact(Farbe, End, I2).

% listDeck(-List: list of all known cards, -N: number of cards )
listDeck(List, N) :- 
	findall(card(A, B, C, D), card(A, B, C, D), List),
	length(List, N).

% needed for pengine - declare safe predicate
sandbox:safe_primitive(ecBlackJack:listDeck(_,_)).

