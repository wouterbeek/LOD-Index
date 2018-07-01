%! reset_seeds is det.
%
% Reset all seeds in LOD Seedlist.

reset_seeds :-
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
