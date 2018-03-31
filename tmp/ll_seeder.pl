%! end_seed(+Seed:dict) is det.

end_seed(Seed) :-
  seedlist_request_(
    [seed,processing],
    [hash(Seed.hash)],
    close,
    [failure(404),method(patch)]
  ).



%! seed_by_hash(+Hash:atom, -Seed:dict) is det.

seed_by_hash(Hash, Seed) :-
  seedlist_request_([seed], [hash(Hash)], seed_(Seed), []).



%! processing_seed(-Seed:dict) is nondet.

processing_seed(Seed) :-
  seedlist_request_([seed,processing], [], seed_(Seed), []).



%! reset_seed(+Seed:dict) is det.

reset_seed(Seed) :-
  seedlist_request_(
    [seed,idle],
    [hash(Seed.hash)],
    close,
    [failure(404),method(patch)]
  ).



%! start_seed(-Seed:dict) is semidet.

start_seed(Seed) :-
  seedlist_request_(
    [seed,stale],
    [],
    seed_(Seed),
    [failure(404),method(patch)]
  ).
