:- module(
  index_api,
  [
    index_dataset/1,           % -Dataset
    index_download_location/1, % -Uri
    index_statement/3          % ?S, ?P, ?O
  ]
).

/** <module> LOD Index API

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- maplist(rdf_register_prefix, [
     ldm-'https://ldm.cc/',
     rdf
   ]).

:- rdf_meta
   index_statement(r, r, o).





%! index_dataset(-Dataset:iri) is nondet.

index_dataset(Dataset) :-
  index_statement(Dataset, rdf:type, ldm:'Dataset').



%! index_download_location(-Uri:atom) is nondet.

index_download_location(Uri) :-
  index_statement(_, ldm:downloadLocation, Uri0),
  rdf_literal_value(Uri0, Uri).



%! index_statement(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.

index_statement(S, P, O) :-
  current_user(User),
  statement(_, User, index, S, P, O).
