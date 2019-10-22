
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tau Prolog code for the Prolog part in the browser
% This is the bridge between the graphics in Processing and user and the
% the game itself provided by the Prolog server (SWI Prolog via Pengines)
% It takes player information from Pengine and triggers querys to Pengine
% as response to user actions
% 
% Author: Hans N. Beck (c)
% Last Change: 05.10.2019
%
% License: MIT 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dom)).
:- use_module(library(js)).
:- use_module(library(lists)).  

% setup DOM event bindings and other inits
init :-
	%get_by_id('btplay', Play),
	get_by_id('btstop', Stop),
	%bind(Play, click, _, playAction),
	bind(Stop, click, _, stopAction),
	holdTerm(nextPlayer(p2, p1), next),
	write('Tau Prolog: Binding done').

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
	nextPlayerArg(P, PResultStr),
	prop('deckCostume', DeckID),
	prop(DeckID, 'mytext', Fkt),
	apply(Fkt, ['WUMMER'], _),
	Term =.. [playCard, P, PResultStr, 'Current', 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).
	
% user wants to stop the game
stopAction :-
	get_by_id('btplay', Play),
	set_attr(Play, disabled, true), 
	set_style(Play, 'background-color', 'white'),
	state(p1, P1),
	state(p2, P2),
	Term =.. [stop, P1, P2, 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).

% write the term to be send via Pengine into a DOM node to get it as string
msg2JS(FktID, Term) :-
	writeHTML('Tauhtml',Term, String),
	prop(FktID, JSFkt),
	apply(JSFkt, [], _).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Player control: at start the SWI Prolog application provides to playerStr
% it is the task of the UI level to determine which player is the active player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% exchange P1, P2
nextPlayerArg(P, PStr) :-
	state(next, nextPlayer(A, B)), 
	holdTerm(nextPlayer(B, A), next),
	state(A, P),
	playerStr(A, PStr).
	
playerStr(p1, 'P1').
playerStr(p2, 'P2').	

% determine the active player
activePlayer(P) :-
	state(next, nextPlayer(Pid, _)),
	state(Pid, P).

% determine the active player
activePlayerNo :-
	activePlayer(P), 
	playerNo(P, PNum),
	atomic_list_concat( ['Player ', PNum], Text),
	writeHTML('Tauhtml', Text, String).

activePlayerNo :-
	writeHTML('Tauhtml', '', String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Bridge to the visual representation of things
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

costume(Farbe, Name) :-
	activePlayer(P),
	playerNo(P, PNo),
	atomic_list_concat([Farbe, Name], File), 
	% write('Prolog costume File'), write(File), write(PNo),  %for debug
	prop('newCostume', JSFkt), 
	apply(JSFkt, [File, PNo], _).


playerNo(player(PNo, _), PNo).
	
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
takeResult([], _, _) :- 
	state(current, card(Farbe, Name, _)),
	state(msg, Msg),
	writeHTML('pout', Msg, String),
	costume(Farbe, Name),!.
% property list done, last action is to give back the message in msg
takeResult([], _, _) :- 
	state(msg, Msg),
	writeHTML('pout', Msg, String).
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

