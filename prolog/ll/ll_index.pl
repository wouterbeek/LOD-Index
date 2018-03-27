:- module(
  ll_index,
  [
    process_index/1 % +File
  ]
).

/** <module> LOD Index processor

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(dict)).
:- use_module(library(http/json)).





%! process_index(+File:atom) is det.

process_index(File) :-
  class_stream_file(File, process_index_).

process_index_(In) :-
  json_read_dict(In, Dict1),
  _{index: Dict2} :< Dict1,
  dict_pairs(Dict2, Pairs),
  maplist(process_org, Pairs).

process_org(OName-Dict1) :-
  _{loc: File} :< Dict1,
  json_read_dict(In, Dict2),
  _{set: Dict3} :< Dict2,
  dict_pairs(Dict3, Pairs),
  maplist(process_dataset(OName), Pairs).

process_dataset(OName, DName-Dict) :-
  _{'void:dataDump': Docs} :< Dict,
  add_seed(_{
