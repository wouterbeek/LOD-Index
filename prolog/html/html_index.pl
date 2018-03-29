:- module(
  html_index,
  [
    html_url/2 % +Root, -Uri
  ]
).

/** <module> LOD-Index tools for HTML
*/

:- use_module(library(lists)).
:- use_module(library(nb_set)).
:- use_module(library(sgml)).

:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).
:- use_module(library(xpath)).





%! html_url(+Root:atom, -Uri:atom) is nondet.

html_url(Root, Uri) :-
  empty_nb_set(Visited),
  add_nb_set(Root, Visited),
  html_url_(Root, Visited, Uri).

html_url_(Root, Visited, Uri) :-
  http_open2(Root, In, [accept(html),failure(404),metadata(Metas)]),
  http_metadata_content_type(Metas, media(text/html,_)),
  call_cleanup(
    (
      load_html(In, Dom, []),
      xpath(Dom, //a(@href), Rel), %NONDET
      % Don't move up.
      Rel \== '../',
      uri_resolve(Rel, Root, Abs),
      % Don't leave the site.
      uri_components(Abs, Comps),
      uri_data(authority, Comps, 'downloads.dbpedia.org'),
      add_nb_set(Abs, Visited, true)
    ),
    close(In)
  ),
  (   sub_atom(Abs, _, 1, 0, /)
  ->  html_url_(Abs, Visited, Uri)
  ;   uri_comps(Abs, uri(_,_,Segments,_,_)),
      memberchk(tmp, Segments)
  ->  fail
  ;   uri_file_extensions(Abs, Exts),
      member(Ext, Exts),
      memberchk(Ext, [csv,json,md5,txt])
  ->  fail
  ;   Uri = Abs,
      writeln(Abs)
  ).
