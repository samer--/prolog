:- module(kube, []).

/** <module> Enables use of Kube CSS framework
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(htmlutils)).

:- set_prolog_flag(double_quote,string).

http:location( kube, weblib(kube310), []).

user:head(kube(_),Head) --> !,
   html(head([ meta([name(viewport), content("width-device-width,initial-scale=1")]) 
             | Head ])).

requirements -->
   html_requires("//imperavi.com/css/combined.min.css"),
   html_requires("//imperavi.com/js/combined.min.js").

user:body(kube(Nav),Body) --> !,
   { setting(htmlutils:appname, AppName) },
   use_font("Source Sans Pro",sourcepro),
   % html_requires(kube('kube.min.css')),
   html_requires("//imperavi.com/css/combined.min.css"),
   html_requires("//imperavi.com/js/combined.min.js"),

	html(body(class=kubepage,
             [ div( class=wrap, 
                   [ nav([id=nav,class=[navbar,fullwidth]], 
                     \navmenu(AppName,Nav))
                   ])
             , div(class=wrap,
                   div(id=main, 
                       [   aside(id=side,nav([class=nav,id="side-nav"],
                                             \navmenu(AppName,Nav)))
                       ,   div(id=area,Body)
                       ]))
             , div(class=wrap,
                   footer(id=footer, 
                          [a(href='.',AppName), nav(class=[navbar,'navbar-right'], ul(\Nav))]))
             ])).

navmenu(AppName,Nav) --> html(ul([li(\link(root,b(AppName))),\Nav])).
navmenu(Nav) --> html(nav(class=[navbar,"navbar-left"],ul(\Nav))).
sidebar(Contents) --> html(aside(id=side,Contents)).

button_link(Ref,Content) --> 
   html(a([class('btn btn-small'),href(Ref)],Content)).

% menu(Classes,Content) --> 
%    html(div(class(['pure-menu', 'pure-menu-open'|Classes]), Content)).
% % Add pure-menu-selected to active item
% hmenu(Heading,Items) --> 
%    menu(['pure-menu-horizontal'], [a([href='.',class="pure-menu-heading"],Heading), ul(\Items)]).

% vmenu(Heading,Items) --> 
%    menu([], [a([href='.',class="pure-menu-heading"],Heading), ul(\Items)]).
