:- module(reset_seeds, [clear_all/0,reset_processing/0]).

/** <module> Reset seeds in LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(seedlist_client).





clear_all :-
  forall(
    seed_by_type(stale, Seed),
    retract_seed(Seed)
  ).



reset_processing :-
  forall(
    seed_by_type(processing, Seed),
    (
      retract_seed(Seed),
      assert_seed(Seed)
    )
  ).
