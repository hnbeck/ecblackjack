
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tau Prolog code for the Prolog part in the browser
% 
%  Autor: Hans N. Beck (c)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dom)).
:- use_module(library(js)).
:- use_module(library(lists)). 

init :-
	get_by_id('btplay', Play),
	get_by_id('btstop', Stop),
	bind(Play, click, _, playAction),
	bind(Stop, click, _, stopAction),
	%holdTerm(card(herz, ass, 14), current),
	write('Binding done').

playAction :-
	nextQueryArgs(PA, PP),
	Term =.. [playCard, PA, PP, 'PA2', 'PP2', 'Flag', 'Current', 'Msg'],
	msg2JS('sendPengine', Term).
	
stopAction :-
	get_by_id('btplay', Play),
	set_attr(Play, disabled, true), 
	set_style(Play, 'background-color', 'white'),
	nextQueryArgs(PA, PP),
	Term =.. [stop, PA, PP, 'Flag', 'Msg'],
	msg2JS('sendPengine', Term).

msg2JS(FktID, Term) :-
	writeHTML('Tauhtml',Term, String),
	prop(FktID, JSFkt),
	apply(JSFkt, [], _).

nextQueryArgs(PA2, PP2) :-
	state(pa2, PA),
	state(pp2, PP),
	nextPlayer(PA, PP, PA2, PP2).

% +Term : a Tau Prolog term
% -HMTLString : the term as String
writeHTML(ID, Term, HTMLString) :-
	get_by_id(ID, HTML),
	open(HTML, write, Stream), 
	write(Stream, Term), 
	close(Stream),
	get_html(HTML, HTMLString).

% -Term : a Tau Prolog Term
readHTML(ID, Term) :-
	get_by_id(ID, HTML),
	open(HTML, read, Stream), 
	read(Stream, Term),
	close(Stream).

costume(Farbe, Name) :-
	state(pp2, P),
	playerNo(P, PNo),
	atomic_list_concat([Farbe, Name], File), 
	% write('Prolog costume File'), write(File), write(PNo),  %for debug
	prop('newCostume', JSFkt), 
	apply(JSFkt, [File, PNo], _).

nextPlayer(PA, PP, PP, PA).

playerNo(player(PNo, _), PNo).
	
% +JSObjectID: an reference to a JS object containing the answer of a Pengine query
% -TauTerm: the Pengine answer as Tau Prolog Term
%analyse(JSObject, TauTerm) :-
%	parseTerm(JSObject, TauTerm),
%	state(msg, Msg),
%	writeHTML('pout', Msg, String).

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

% store the fact under reference of property
% which means if pengine query contains variable P1
% the answer will be included in Tau database as
% state(p1, AnswerTerm)
holdTerm(TauTerm, H) :-
	% write('Taustate '), write(state(H, TauTerm)), % for debug
	retractall(state(H, _)),
	asserta(state(H, TauTerm)).

% go through all properties given by the list and parse them
% if all is parsed message is available and can put out
takeResult([], _, _) :- 
	state(current, card(Farbe, Name, _)),
	state(msg, Msg),
	writeHTML('pout', Msg, String),
	costume(Farbe, Name),!.
	
takeResult([], _, _) :- 
	state(msg, Msg),
	writeHTML('pout', Msg, String).
	
takeResult([H|T], JSObjectID, Term) :-
	prop(JSObjectID, JSObject),
	prop(JSObject, H, SubJSObject),
	parseTerm(SubJSObject, TauTerm),
	holdTerm(TauTerm, H),
	takeResult(T, JSObjectID, Term).
	



