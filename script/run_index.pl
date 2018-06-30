run_index :-
  forall(
    statement(nightly, wouter, index, Org0, rdf:type, foaf:'Org', _),
    run_organization(Org0)
  ).

run_organization(Org0) :-
  rdf_prefix_iri(org, Org, Org0),
  call_pp(organization_create(nightly, wouter, Org, _{})),
  forall(
    statement(nightly, wouter, index, Dataset0, dct:creator, Org0, _),
    run_dataset(Org, Dataset0)
  ).

run_dataset(Org, Dataset0) :-
  rdf_prefix_iri(data, Dataset, Dataset0),
  Options1 = _{accessLevel: public},
  (   statement(nightly, wouter, index, Dataset0, dct:description, Description0, _)
  ->  rdf_literal_lexical_form(Description0, Description),
      dict_put(description, Options1, Description, Options2)
  ;   Options2 = Options1
  ),
  call_pp(dataset_create(nightly, Org, Dataset, Options2)),
  forall(
    statement(nightly, wouter, index, Dataset0, ldm:distribution, Distribution0, _),
    run_distribution(Distribution0)
  ).

run_distribution(Distribution0) :-
  forall(
    statement(nightly, wouter, index, Distribution0, ldm:downloadLocation, Uri0, _),
    run_uri(Uri0)
  ).

run_uri(Uri0) :-
  rdf_literal_lexical_form(Uri0, Uri),
  ll_download(Uri).
