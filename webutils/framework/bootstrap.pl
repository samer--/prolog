:- module(bootstrap, 
      [  navbar//3
      ,  navbar_menu//2
      ]).

/** <module> Enables use of Bootstrap CSS framework
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(htmlutils)).
:- use_module(library(dcg_core)).

:- set_prolog_flag(double_quote,string).

http:location(bootstrap,"//maxcdn.bootstrapcdn.com/bootstrap/3.2.0",[]).
http:location(bootstrap_examples,"//getbootstrap.com/examples",[]).

:- html_resource( bootstrap("css/bootstrap.min.css"), []).
:- html_resource( bootstrap("css/bootstrap-theme.min.css"), 
                  [requires(bootstrap("css/bootstrap.min.css"))]).
:- html_resource( bootstrap_examples("starter-template/starter-template.css"),
                  [requires(bootstrap("css/bootstrap.min.css"))]).
:- html_resource( bootstrap_examples("navbar/navbar.css"),
                  [requires(bootstrap("css/bootstrap.min.css"))]).
:- html_resource( bootstrap_examples("navbar-static-top/navbar-static-top.css"),
                  [requires(bootstrap("css/bootstrap.min.css"))]).
:- html_resource( bootstrap_examples("navbar-fixed-top/navbar-fixed-top.css"),
                  [requires(bootstrap("css/bootstrap.min.css"))]).
:- html_resource( bootstrap_examples("theme/theme.css"), 
                  [requires(bootstrap("css/bootstrap-theme.min.css"))]).

navbar_class_option('navbar-inverse').
navbar_class_option('navbar-fixed-top').
navbar_class_option('navbar-static-top').

user:head(bootstrap(_,_),Head) --> !,
   html(head([ meta([name(viewport), content("width-device-width,initial-scale=1")]) 
             | Head ])).

% use abstract paths here?ads
user:body(bootstrap(Var,Nav),Body) --> !,
	html_requires(bootstrap("css/bootstrap.min.css")),
   bootstrap_body(Var,Nav,Body).

bootstrap_body(1,Nav,Body) -->
   { setting(htmlutils:appname, AppName) },
   html_requires(bootstrap_examples("starter-template/starter-template.css")),
	html( body( div( class=container, 
                    \bootstrap_body(AppName,['navbar-static-top'],Nav,
                                    div(class="starter-template", Body))))).

bootstrap_body(2,Nav,Body) -->
   { setting(htmlutils:appname, AppName) },
   html_requires(bootstrap_examples("navbar-fixed-top/navbar-fixed-top.css")),
   html(body( 
   %   div( class=container,
           \bootstrap_body(AppName,['navbar-default','navbar-fixed-top'],Nav,Body)
           %     )
      )).
                   % \bootstrap_body("swiPlayer",['navbar-inverse','navbar-fixed-top'],Nav,Body)))).

bootstrap_body(3,Nav,Body) -->
   { setting(htmlutils:appname, AppName) },
   html_requires(bootstrap_examples("navbar-static-top/navbar-static-top.css")),
   % html_requires(bootstrap_examples("theme/theme.css")),
   html( body(role(document),
         [  \navbar(AppName,['navbar-inverse','navbar-static-top'],navbar_menu(left,Nav))
         ,  div([class=container,role=main], Body)
         ,  \script("//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js")
         ,  \script("//getbootstrap.com/dist/js/bootstrap.min.js")
         ])).

bootstrap_body(AppName,NavbarClasses,Nav,Body) -->
   html( [  \navbar(AppName,NavbarClasses, navbar_menu(left,Nav))
         ,  div(class=container,Body)
         ,  \script("//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js")
         ,  \script("//getbootstrap.com/dist/js/bootstrap.min.js")
         ]).

divider --> html(li(class=divider,[])).
dropown_header(Text) --> html(li(class='dropdown-header',Text)).

dropdown(Name,Items) -->
   html(li(class(dropdown), 
           [ a( [ href='#', class='dropdown-toggle', 'data-toggle'=dropdown],
                [ Name, span(class=caret,[])])
           , ul( [ class='dropdown-menu', role=menu],
                 Items)
           ])).
        

button(Content,Classes) --> 
   html(button([type=button,class=[btn|Classes]], Content)).

button_link(Ref,Content) --> 
   html(a([role=button,class=[btn,'btn-default'],href=Ref],Content)).

%% navbar( +Header:html_item, +Classes:list(html_class), +Menus:phrase(html))// is det.
navbar(AppName,NavbarClasses,Menus) --> 
   html( div( [ class=[navbar | NavbarClasses], role=navigation],
              div( class=container,
                   [ div( class='navbar-header',
                          [ \navbar_toggle
                          , a([class='navbar-brand', href='.'], AppName)
                          ])
                    , div( class=[collapse,'navbar-collapse'], \Menus)
                    ]))).

%% navbar_menu(Just:oneof([left,right]), +Items:phrase(html))// is det.
navbar_menu(left,Items) --> html(ul(class=[nav,'navbar-nav'], \Items)).
navbar_menu(right,Items) --> html(ul(class=[nav,'navbar-nav','navbar-right'], \Items)).

navbar_toggle -->
   html( button(
     [ type=button
     , class='navbar-toggle'
     , 'data-toggle'=collapse
     , 'data-target'='.navbar-collapse'
     ], 
     [ span(class='sr-only',"Toggle navigation")
     , \rep(3,icon_bar)
     ])).

icon_bar --> html(span(class='icon-bar',[])).

user:style(bootstrap(1),'Bootstrap, variation 1').
user:style(bootstrap(2),'Bootstrap, variation 2').
user:style(bootstrap(3),'Bootstrap, variation 3').

side_nav(Items) --> 
   html(div(class='col-md-3', 
            div([ class=['bs-docs-sidebar','hidden-print','hidden-xs','hidden-sm'],
                  role=complementary ],
                [ ul(class='nav bs-docs-sidenav', \Items)
                , a([class='back-to-top',href='#top'],"Back to top")
                ]))).
