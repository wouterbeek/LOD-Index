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
:- use_module(library(http/json)).

:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(ll/ll_seeder)).





%! process_index(+File:atom) is det.

process_index(File) :-
  call_stream_file(File, process_index_).

process_index_(In) :-
  json_read_dict(In, Dict1),
  _{index: Dict2} :< Dict1,
  dict_pairs(Dict2, Pairs),
  maplist(process_org, Pairs).

process_org(OName-Dict) :-
  _{loc: File} :< Dict,
  call_stream_file(File, process_org_(OName)).

process_org_(OName, In) :-
  json_read_dict(In, Dict),
  _{set: Set} :< Dict,
  dict_pairs(Set, Pairs),
  maplist(process_dataset(OName), Pairs).

process_dataset(OName, DName-Dict) :-
  _{'void:dataDump': Docs} :< Dict,
  add_seed(
    _{
      approach: dummy,
      name: DName,
      documents: Docs,
      organization: _{name: OName}
    }
  ).
