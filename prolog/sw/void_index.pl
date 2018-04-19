:- module(
  void_index,
  [
    void_dataset/1 % -Triples
  ]
).

/** <module> LOD-Index tools for VoID

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(http/tapir)).
:- use_module(library(sw/rdf_prefix)).

:- maplist(rdf_assert_prefix, [
     dct-'http://purl.org/dc/terms/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
   ]).





%! void_dataset(-Triples:ordset(rdf_triple)) is nondet.

void_dataset(Triples) :-
  distinct(Dataset, statement(wouter, index, Dataset, rdf:type, dcat:'Dataset')),
  % documents
  findall(Triple), cbd(wouter, index, Dataset, Triple), Triples).
