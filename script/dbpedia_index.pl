:- module(dbpedia_index, [run/0]).

/** <module> DBpedia → LOD Index

@author Wouter Beek
@version 2018
*/

:- use_module(library(http/json)).

:- use_module(library(dict)).
:- use_module(library(html/html_index)).
:- use_module(library(uri_ext)).

run :-
  findall(
    Pair,
    (
      dbpedia_version(Version),
      dbpedia_version_pair(Version, Pair)
    ),
    Pairs
  ),
  dict_pairs(Dict, Pairs),
  setup_call_cleanup(
    open('dbpedia.json', write, Out),
    json_write_dict(Out, _{set: Dict}),
    close(Out)
  ).

dbpedia_version_pair(Version, Pair) :-
  atomic_list_concat([dbpedia,Version], -, DName),
  uri_comps(Root, uri(http,'downloads.dbpedia.org',[Version,''],_,_)),
  findall(Doc, html_url(Root, Doc), Docs),
  Pair = DName-_{'void:dataDump': Docs}.

dbpedia_version('1.0').
dbpedia_version('2.0').
dbpedia_version('3.0').
dbpedia_version('3.0rc').
dbpedia_version('3.1').
dbpedia_version('3.2').
dbpedia_version('3.3').
dbpedia_version('3.4').
dbpedia_version('3.5').
dbpedia_version('3.5.1').
dbpedia_version('3.6').
dbpedia_version('3.7').
dbpedia_version('3.8').
dbpedia_version('3.9').
dbpedia_version('2014').
dbpedia_version('2015-04').
dbpedia_version('2015-10').
dbpedia_version('2016-04').
dbpedia_version('2016-10').
