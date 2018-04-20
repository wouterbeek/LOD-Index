:- module(reset_seeds, [run/0]).

/** <module> Reset seeds in LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(seedlist_client).





run :-
  forall(
    seed_by_type(processing, Seed),
    (
      retract_seed(Seed),
      assert_seed(Seed)
    )
  ).
