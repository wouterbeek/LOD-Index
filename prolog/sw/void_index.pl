:- module(
  void_index,
  [
    void_url/1 % -Uri
  ]
).

/** <module> LOD-Index tools for VoID
*/

:- use_module(library(aggregate)).

:- use_module(library(sw/rdf_prefix)).
:- use_module(library(tapir)).

:- rdf_assert_prefix(void, 'http://rdfs.org/ns/void#').





%! void_url(-Uri:atom) is nondet.

void_url(Uri) :-
  dataset(OName, DName, Dataset, void:dataDump, Uri),
  % ASSUMPTION: A dataset description is not distributed over multiple
  % datasets.
  aggregate_all(set(Triple), cbd(OName, DName, Dataset, Triple), Triples).
