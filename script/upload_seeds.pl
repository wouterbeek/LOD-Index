:- module(
  upload_seeds,
  [
    assertall_seeds/0,
    retractall_seeds/0
  ]
).

/** <module> Upload seeds from LOD Index to LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(http/tapir)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- use_module(seedlist_client).

:- rdf_assert_prefix(dcat, 'http://www.w3.org/ns/dcat#').





%! assertall_seeds is det.

assertall_seeds :-
  forall(
    findnsols(1 000, Uri, url_(Uri), Uris),
    assert_seeds(Uris)
  ).

url_(Uri) :-
  statement(wouter, index, _, dcat:downloadURL, Uri0),
  rdf_literal_value(Uri0, Uri).



%! retractall_seeds is det.

retractall_seeds :-
  forall(
    seed(Seed),
    retract_seed(Seed)
  ).
