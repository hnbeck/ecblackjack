
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tau Prolog code for the Prolog part in the browser
% This is the bridge between the graphics in Processing and user and the
% the game itself provided by the Prolog server (SWI Prolog via Pengines)
% It takes player information from Pengine and triggers querys to Pengine
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

% user want to play a card - build the Prolog query to be sent over Pengine
% The identifier between '' are unbound variables bind by Prolog
% and the terms binded will be stored in Tau Prolog associated with this identifier
% Example "Current" will be bound with the card(...) drawn and stored in 
% Tau Prolog as state(current, card(...)
playAction :-
	activePlayerStr(P, PResultStr),
	Term =.. [playCard, P, PResultStr, 'DrawCard', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).
	
% user wants to stop the game
standAction :-
	get_by_id('btStand', Stand),
	set_attr(Stand, disabled, true), 
	nextTurn,
	activePlayer(PA),
	passivePlayer(PP),
	Term =.. [banksTurn, PA, PP, 'P1', 'P2', 'Winner', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).

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

% costume of the Player itself
% costume(player No, Name, Orientation, Age)
% list of cards on table, first card is the player and dealer costume
% both are not cards, but a graphic element

playerCostume(p1, ['player'], ['up']).
playerCostume(p2, ['dealer'], ['up']).

%Costumes is a set of 2 lists, one for name, one for orientation
%costumes(1, [], []).
%costumes(2, [], []).

% genCostume(Spieleno, Farbe, Name Orientation, -costumeList1, costumeLIst2)
genCostume(Farbe, Name, O, [File], [O]) :-
	atomic_list_concat([Farbe, Name], File). 

cardCostume(Farbe, Name, O) :-
	activePlayer(P),
	playerNo(P, PNo),
	genCostume(Farbe, Name, O, NList, OList),
	appendCostume(PNo, NList, OList).

appendCostume(PNo, NNew, ONew) :-
	costumes(PNo, NList, OList), 
	append(NList, NNew, NList2), 
	append(OList, ONew, OList2),
	updateCostumes(PNo, NList2, OList2).

% build all costumes again from updated player card list
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

% initate Process for drawing the costumes NEu DEFINIEREN
visCostume(PNo) :-
	costumes(PNo, CtList1, CtList2),
	% write('Tau Prolog cardCostume File'), write(File), write(PNo),  %for debug
	prop('visCostumeJS', JSFkt), 
	apply(JSFkt, [PNo, CtList1, CtList2], _).

% nach ausgabe aller karten
gameContinue :-
	state(flag, start), !,
	refreshCostumes(p1),
	refreshCostumes(p2),
	visCostume(1),
	visCostume(2),
	writeHTML('Taumsg', 'Player: draw or stand', _).

visGame :-
	refreshCostumes(p1),
	refreshCostumes(p2),
	visCostume(1),
	visCostume(2).

% ende des Spiels
gameContinue :-
	state(flag, over),
	state(winner, Winner),
	visGame, 
	stateWinner(Winner, Text),
	writeHTML('Taumsg',Text, _),
	prop('stopGame', JSFkt), 
	apply(JSFkt, [Winner], _),!.
% eine Karte gezogen
gameContinue :-
	msg2JS('pout'),
	state(drawcard, card(Farbe, Name, _, O)),
	activePlayerNo(PNo),
	cardCostume(Farbe, Name, O),
	visCostume(PNo),
	gameContinue2.

gameContinue2 :-
	state(flag, go), 
	writeHTML('Taumsg', 'Player: draw or stand', _).
	
gameContinue2 :-
	state(flag, bust),
	writeHTML('Taumsg', 'You bust', _),
	stopAction.

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

% exchange P1, P2
activePlayerStr(P, PStr) :-
	activePlayer(P),
	playerNo(P, PNum),
	playerStr(PNum, PStr).
	
% determine the active player
activePlayer(P) :-
	state(next, nextPlayer(Pid,_)),
	state(Pid, P).

% determine the active player
activePlayerNo :-
	activePlayer(P), 
	playerNo(P, PNum),
	activePlayerText(PNum, Text),
	writeHTML('Taumsg', Text, String).

activePlayerNo(PNo) :-
	activePlayer(P), 
	playerNo(P, PNo).

% Player 2 is the bank
activePlayerText(1, 'Draw a card or stop').	
activePlayerText(2, 'Bank').

passivePlayer(P) :-
	state(next, nextPlayer(_,Pid)),
	state(Pid, P).

playerNo(player(PNo, _), PNo).

playerCardsCount(player(_, List), Len) :-
	length(List, Len).

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

