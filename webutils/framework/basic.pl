:- module(basic, []).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

user:body(basic,Body) --> 
   html_requires(weblib('styles/home.css')),
	html(body([ div(id(content),Body) ])).
