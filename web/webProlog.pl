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


	% +Term : a Tau Prolog term
	% -HMTLString : the term as String
	writeHTML(ID, Term, HTMLString) :-
		get_by_id(ID, HTML),
		open(HTML, write, Stream), 
		write(Stream, Term),
		close(Stream),
		get_html(HTML, HTMLString).

	% -Term : a Tau Prolog Term
	readHTML(Term) :-
		get_by_id('TauInput', HTML),
		open(HTML, read, Stream), 
		read(Stream, Term),
		clode(Stream).

	nextPlayer(player(1, F1), player(2, F2), player(2, F2), player(1, F1)).
	% +JSObjectID: an reference to a JS object containing the answer of a Pengine query
	% -TauTerm: the Pengine answer as Tau Prolog Term
	analyse(JSObject, TauTerm) :-
		parseTerm(JSObject, TauTerm),
		fact(msg, Msg),
		writeHTML('pout', Msg, String).

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
	% fact(p1, AnswerTerm)
	holdTerm(TauTerm, H) :-
		retractall(fact(H, _)),
		asserta(fact(H, TauTerm)).

	% go through all properties given by the list and parse them
	% if all is parsed message is available and can put out
	takeResult([], _, _) :- 
		fact(msg, Msg),
		writeHTML('pout', Msg, String).
	takeResult([H|T], JSObjectID, Term) :-
		prop(JSObjectID, JSObject),
		prop(JSObject, H, SubJSObject),
		parseTerm(SubJSObject, TauTerm),
		holdTerm(TauTerm, H),
		takeResult(T, JSObjectID, Term).
		