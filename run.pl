:- use_module(library(http/json)).

:- use_module(library(dict)).
:- use_module(library(html/html_index)).
:- use_module(library(ll/ll_index)).
:- use_module(library(uri_ext)).

create_sitemap :-
  findall(
    Pair,
    (
      dbpedia_version(Version),
      create_dbpedia_sitemap(Version, Pair)
    ),
    Pairs
  ),
  dict_pairs(Dict, Pairs),
  setup_call_cleanup(
    open('dbpedia.json', write, Out),
    json_write_dict(Out, _{set: Dict}),
    close(Out)
  ).

create_dbpedia_sitemap(Version, Pair) :-
  atomic_list_concat([dbpedia,Version], -, DName),
  dbpedia_uri([Version,''], Root),
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

dbpedia_uri(Segments, Uri) :-
  uri_comps(Uri, uri(http,'downloads.dbpedia.org',Segments,_,_)).



upload_sitemap :-
  absolute_file_name(json('index.json'), File),
  process_index(File).
