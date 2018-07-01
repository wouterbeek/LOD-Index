:- module(
  lod_index_api,
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

:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- maplist(rdf_assert_prefix, [
     ldm-'https://ldm.cc/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
   ]).





%! dataset(-Dataset:iri) is nondet.

dataset(Dataset) :-
  statement(_, index, Dataset, rdf:type, ldm:'Dataset').



%! download_location(-Uri:atom) is nondet.

download_location(Uri) :-
  statement(_, index, _, ldm:downloadLocation, Uri0, _),
  rdf_literal_value(Uri0, Uri).
