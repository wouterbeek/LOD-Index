:- module(
  index_api,
  [
    dataset/1,          % -Dataset
    download_location/1 % -Uri
  ]
).

/** <module> LOD Index API

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- maplist(rdf_assert_prefix, [
     ldm-'https://ldm.cc/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
   ]).

:- rdf_meta
   statement_(r, r, o).





%! dataset(-Dataset:iri) is nondet.

dataset(Dataset) :-
  statement_(Dataset, rdf:type, ldm:'Dataset').



%! download_location(-Uri:atom) is nondet.

download_location(Uri) :-
  statement_(_, ldm:downloadLocation, Uri0),
  rdf_literal_value(Uri0, Uri).





% HELPERS %

statement_(S, P, O) :-
  current_user(User),
  statement(User, index, S, P, O).
