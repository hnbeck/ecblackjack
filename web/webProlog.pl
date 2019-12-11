
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tau Prolog code for the Prolog part in the browser
% This is the bridge between the graphics in Processing (P5js) and user and the
% the game itself provided by the Prolog server (SWI Prolog via Pengines)
% It takes information from Pengine and triggers querys to Pengine
% as response to user actions
% 
% Author: Hans N. Beck (c)
% Last Change: 30.10.2019
%
% License: MIT 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dom)).
:- use_module(library(js)).
:- use_module(library(lists)).  

% setup the initial next player and the deck costume
init :-
	get_by_id('btStand', Stand),
	bind(Stand, click, _, standAction),
	holdTerm(nextPlayer(p1, p2), next),
	write('Tau Prolog: done').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The action interface - every action triggers a query over pengine to
% SWI Prolog in order to do a game move
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% principle: the query contains bounded variables (bounded via Tau)
% Strings indicating unbounded variable names and of course - as first element -
% the name of the functor in SWI Prolog.
% the functor will be queried via Pengine. The bounded variables are Tau Prolog
% terms which are stringified. The variable names will be bound by the 
% SWI Prolog query. These are returned as JSON Objects and will addad to the Tau
% Prolog knowledge base. 

% Example "Current" will be bound by SWI Prolog with the card(...) drawn.
% The terhm card(...) will be added to the Tau prolog knowledge base as 
% state(current, card(...)) via the takeResult predicate here. 

% trigger drawing a card by the player
playAction :-
	activePlayerStr(P, PResultStr),
	Term =.. [playCard, P, PResultStr, 'DrawCard', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).
	
% user wants to stand (give control to dealer)
standAction :-
	get_by_id('btStand', Stand),
	set_attr(Stand, disabled, true), 
	nextTurn,
	activePlayer(PA),
	passivePlayer(PP),
	Term =.. [banksTurn, PA, PP, 'P1', 'P2', 'Winner', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).

% last thing in the game: show the winner. Ask SWI Prolog for the winner
stopAction :-
	get_by_id('btStand', Stand),
	set_attr(Stand, disabled, true), 
	state(p1, PA),
	state(p2, PP),
	Term =.. [stop, PA, PP, 'Winner', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Structure Elements of visual representation = costumes
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% list of cards on table, first card is the player and dealer costume
% both are not cards in strict sense, but a graphic element
% there are  in fact two lists: a list of names, a list of corresponding orientation
playerCostume(p1, ['player'], ['up']).
playerCostume(p2, ['dealer'], ['up']).

% determine the costume by calculate the file name of the graphic file
% +Farbe card color, given by SWI Prolog +Name: Bube, Dame, 10, 9 etc...
genCostume(Farbe, Name, O, [File], [O]) :-
	atomic_list_concat([Farbe, Name], File). 

% costume of a card 
% + Farbe card Color, +Name card name, +O Orientation (up or down)
cardCostume(Farbe, Name, O) :-
	activePlayer(P),
	playerNo(P, PNo),
	genCostume(Farbe, Name, O, NList, OList),
	appendCostume(PNo, NList, OList).

% add costume to the list of cards on desk
% +PNo: player number, +NNew / +ONew Name and orienation of new costume 
appendCostume(PNo, NNew, ONew) :-
	costumes(PNo, NList, OList), 
	append(NList, NNew, NList2), 
	append(OList, ONew, OList2),
	updateCostumes(PNo, NList2, OList2).

% build all costumes again from updated player card list provided by SWI Prolog
refreshCostumes(Pid) :-
	state(Pid, P),
	playerNo(P, PNo),
	playerCostume(Pid, NList, OList),
	playerCards(P, List), 
	refreshCards(List, PNo, NList, OList, NList2, OList2),
	updateCostumes(PNo, NList2, OList2).

refreshCards([], _, A, B, A, B).
refreshCards([card(Farbe, Name, _, O) | T], PNo, NList, OList, NList3, OList3) :-
	genCostume(Farbe, Name, O, NNew, ONew),
	append(NList, NNew, NList2), 
	append(OList, ONew, OList2),
	refreshCards(T, PNo, NList2, OList2, NList3, OList3).


% make costume list persistent
updateCostumes(PNo, NewListName, NewListO) :-
	retractall(costumes(PNo, _, _)),
	asserta(costumes(PNo, NewListName, NewListO)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Communication Tau Prolog back to JS level
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write the term to be send via Pengine into a DOM node to get it as string
msg2JS(FktID, Term) :-
	writeHTML('Tauhtml',Term, _),
	prop(FktID, JSFkt),
	apply(JSFkt, [], _).

% write a message into a DOM element to show so that user can see it
msg2JS(DOMId) :-
	state(msg, Msg),
	writeHTML(DOMId, Msg, _).


% write the query term in a non visible DOM element where it can be accessed
% at JS Level
% +Term : a Tau Prolog term
% -HMTLString : the term as String
writeHTML(ID, Term, HTMLString) :-
	get_by_id(ID, HTML),
	open(HTML, write, Stream), 
	write(Stream, Term), 
	close(Stream),
	get_html(HTML, HTMLString).

% read in from a DOM element
% -Term : a Tau Prolog Term
readHTML(ID, Term) :-
	get_by_id(ID, HTML),
	open(HTML, read, Stream), 
	read(Stream, Term),
	close(Stream).

% initate Process for drawing the costumes, call a JS function for drawing
% +PNo Player no
visCostume(PNo) :-
	costumes(PNo, CtList1, CtList2),
	% write('Tau Prolog cardCostume File'), write(File), write(PNo),  %for debug
	prop('visCostumeJS', JSFkt), 
	apply(JSFkt, [PNo, CtList1, CtList2], _).

%%%%% state processing - how to continue the came after a draw or stand?
% after set up the game
gameContinue :-
	state(flag, start), !,
	refreshCostumes(p1),
	refreshCostumes(p2),
	visCostume(1),
	visCostume(2),
	writeHTML('Taumsg', 'Player: draw or stand', _).

% draw to complete updated costume list
visGame :-
	refreshCostumes(p1),
	refreshCostumes(p2),
	visCostume(1),
	visCostume(2).

% if game is over (someone wins)
gameContinue :-
	state(flag, over),
	state(winner, Winner),
	visGame, 
	stateWinner(Winner, Text),
	writeHTML('Taumsg',Text, _),
	prop('stopGame', JSFkt), 
	apply(JSFkt, [Winner], _),!.

% after drawing a card
gameContinue :-
	msg2JS('pout'),
	state(drawcard, card(Farbe, Name, _, O)),
	activePlayerNo(PNo),
	cardCostume(Farbe, Name, O),
	visCostume(PNo),
	gameContinue2.

% player can continue, all is fine
gameContinue2 :-
	state(flag, go), 
	writeHTML('Taumsg', 'Player: draw or stand', _).

% player is bust
gameContinue2 :-
	state(flag, bust),
	writeHTML('Taumsg', 'You bust', _),
	stopAction.

% text for the winner message
stateWinner(1, 'Winner is Player').
stateWinner(2, 'Winner is Dealer').
stateWinner(0, 'No Winner').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Player control: at start the SWI Prolog application provides to playerStr
% it is the task of the UI level to determine which player is the active player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

playerStr(1, 'P1').
playerStr(2, 'P2').	

% next turn means exchange player
nextTurn :-
	state(next, nextPlayer(A, B)), 
	holdTerm(nextPlayer(B, A), next).

% transform a player term to a player string for query building
% +P a player 
activePlayerStr(P, PStr) :-
	activePlayer(P),
	playerNo(P, PNum),
	playerStr(PNum, PStr).
	
% determine the active player
% -P active player (as term)
activePlayer(P) :-
	state(next, nextPlayer(Pid,_)),
	state(Pid, P).

% determine the number of active player
activePlayerNo :-
	activePlayer(P), 
	playerNo(P, PNum),
	activePlayerText(PNum, Text),
	writeHTML('Taumsg', Text, String).

activePlayerNo(PNo) :-
	activePlayer(P), 
	playerNo(P, PNo).

% text for the player to display if it is active
activePlayerText(1, 'Draw a card or stop').	
activePlayerText(2, 'Bank').

% get active playerCards
passivePlayer(P) :-
	state(next, nextPlayer(_,Pid)),
	state(Pid, P).

% get player number out of player term
playerNo(player(PNo, _), PNo).

% number of player cards
playerCardsCount(player(_, List), Len) :-
	length(List, Len).

% access player card list
playerCards(player(_, List), List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Pengine  - Tau Prolog interface
%
%  The following predicates take the answer from a Pengine Query which is 
% given as JS Object and transform it back to a Prolog statement.
% This will be persistent via asserta
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% store the fact under reference of property
% which means if pengine query contains variable P1
% the answer will be included in Tau database as
% state(p1, AnswerTerm)
holdTerm(TauTerm, H) :-
	% write('Taustate '), write(state(H, TauTerm)), % for debug
	retractall(state(H, _)),
	asserta(state(H, TauTerm)).

% Pengine answer is coded as a JS object which is a list of properties
% go through all properties given by the list and parse them
% if all is parsed message is available and can put out
% here, the SWI Prolog applicaton is designed to bind the Variable MSG
% with a message to be displayed in use interface

% property list done, last action is to give back the message in msg
% no drawn card available only send the message
takeResult([], _, _).
	
% takeResult(+Propertylist, +id of the variable containing the js object, -Tau Term)
% H is one property which is identical to the name of a bound variable in Pengine answer!
takeResult([H|T], JSObjectID, Term) :-
	prop(JSObjectID, JSObject),
	prop(JSObject, H, SubJSObject),
	parseTerm(SubJSObject, TauTerm),
	holdTerm(TauTerm, H),
	takeResult(T, JSObjectID, Term).
	

% +JSObjectID: an reference to a JS object containing the answer of a Pengine query
% every variable is a property containing an JS object for its bining
% Example if Penge answer binds P to player(1,name) then there is something lile
% {..."P":{functor:player, args:[1,name]}....}
% -TauTerm: the Pengine answer as Tau Prolog Term

% if element is not defined
parseTerm(Elem, _) :- var(Elem).
% if element is atomic
parseTerm(Elem, Elem) :- atomic(Elem).
% if element is a json object
parseTerm(JSObject, TauTerm) :-
	prop(JSObject, args, ArgList),
	prop(JSObject, functor, Functor),
	parseList(ArgList, TermList),
	append([Functor], TermList, TermList2),
	TauTerm =.. TermList2.
% if elem is a list
parseList([], []).
parseList([Head | Tail ], [Head2 | Tail2]) :-
	(is_list(Head) -> 
		parseList(Head, Head2);
		(atomic(Head) -> 
			Head2 = Head; 
			parseTerm(Head, Head2)
		)
	),
	parseList(Tail, Tail2).

