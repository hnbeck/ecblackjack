%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% A simple web server for the ecBlackJack demo game
% 
% Main purpose is to serve the index.html file containing all JS code
% for the Pengine JS client and UI
% Based on the SWI Prolog documentation
%
% Autor: Hans N. Beck (c)
% Last Change: 39.08.2019
%
% License: MIT 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- use_module(pengine_sandbox:ecBlackJack).


:- http_handler(files(.), serve_files,  [prefix]).
:- http_handler(root(.), main,  [prefix]).

:- multifile http_json/1.

http_json:json_type('application/x-javascript').
http_json:json_type('text/javascript').
http_json:json_type('text/x-javascript').
http_json:json_type('text/x-json').
http_json:json_type('text/x-prolog').

http:location(files, '/web', []).


server(Port) :- 
		http_server(http_dispatch, [port(Port)]).


main(Request) :-
		http_reply_from_files('.', [], Request).

serve_files(Request) :-
		http_reply_from_files(web, [], Request).

%serve_files(Request) :-
%		http_404([\p('Sorry could not find')], Request).