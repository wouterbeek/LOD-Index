:- use_module(library(http/json)).

:- use_module(library(dict)).
:- use_module(library(html/html_index)).
:- use_module(library(ll/ll_index)).
:- use_module(library(uri_ext)).

create_dbpedia_sitemap :-
  findall(
    Pair,
    (
      dbpedia_version_(Version),
      create_dbpedia_sitemap_(Version, Pair)
    ),
    Pairs
  ),
  dict_pairs(Dict, Pairs),
  setup_call_cleanup(
    open('dbpedia.json', write, Out),
    json_write_dict(Out, _{set: Dict}),
    close(Out)
  ).

create_dbpedia_sitemap_(Version, Pair) :-
  atomic_list_concat([dbpedia,Version], -, DName),
  uri_comps(Root, uri(http,'downloads.dbpedia.org',[Version,''],_,_)),
  findall(Doc, html_url(Root, Doc), Docs),
  Pair = DName-_{'void:dataDump': Docs}.

dbpedia_version_('1.0').
dbpedia_version_('2.0').
dbpedia_version_('3.0').
dbpedia_version_('3.0rc').
dbpedia_version_('3.1').
dbpedia_version_('3.2').
dbpedia_version_('3.3').
dbpedia_version_('3.4').
dbpedia_version_('3.5').
dbpedia_version_('3.5.1').
dbpedia_version_('3.6').
dbpedia_version_('3.7').
dbpedia_version_('3.8').
dbpedia_version_('3.9').
dbpedia_version_('2014').
dbpedia_version_('2015-04').
dbpedia_version_('2015-10').
dbpedia_version_('2016-04').
dbpedia_version_('2016-10').



upload_sitemap :-
  absolute_file_name(data('index.json'), File),
  process_index(File).
