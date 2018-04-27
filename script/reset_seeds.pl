:- module(
  reset_seeds,
  [
    remove_seeds/0,
    reset_seeds_by_status/1 % +Status
  ]
).

/** <module> Reset seeds in LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).

:- use_module(seedlist_client).





%! remove_seeds is det.

remove_seeds :-
  forall(
    seed_by_type(stale, Seed),
    retract_seed(Seed)
  ).



%! reset_seeds_by_status(+Status:oneof([idle,processing,stale])) is det.

reset_seeds_by_status(Status) :-
  must_be(oneof([idle,processing,stale]), Status),
  forall(
    seed_by_type(Status, Seed),
    (
      retract_seed(Seed),
      assert_seed(Seed.url)
    )
  ).
