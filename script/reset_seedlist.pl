/* Reset LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(lists)).
:- use_module(library(ll/seedlist_api)).



run :-
  forall(
    (
      member(Status, [idle,processing]),
      seed_by_type(Status, Seed)
    ),
    (
      retract_seed(Seed),
      assert_seed(Seed.url)
    )
  ).
