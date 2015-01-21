:- module(base, []).

/** <module> Enables use of Base CSS framework
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(htmlutils)).

:- set_prolog_flag(double_quote,string).

user:file_search_path(base,weblib(base)).

http:location( base, weblib(base), []).

user:head(base(_),Head) --> !,
   html(head([ meta([name(viewport), content("width-device-width,initial-scale=1")]) 
             | Head ])).

user:body(base(Nav),Body) --> !,
   { setting(htmlutils:appname, AppName) },
   use_font("Open Sans",opensans),
   html_requires(base('style.css')),
   html_requires(base('js/modernizr.js')),

	html(body([ div(class([row,clear,container]),
                   [ div(class("col col-2"),[])
                   , div(class("col col-10"),Body)
                   ]) 
              ])).


button_link(Ref,Content) --> 
   html(a([class(['pure-button','pure-button-primary']),href(Ref)],Content)).
