:- module(
  void_index,
  [
    void_dataset/3 % ?OName, ?DName, -Dataset
  ]
).

/** <module> LOD-Index tools for VoID
*/

:- use_module(library(aggregate)).
:- use_module(library(solution_sequences)).

:- use_module(library(sw/rdf_prefix)).
:- use_module(library(tapir)).

:- maplist(rdf_assert_prefix, [
     dct-'http://purl.org/dc/terms/',
     foaf-'http://xmlns.com/foaf/0.1/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#',
     void-'http://rdfs.org/ns/void#'
   ]).





%! void_dataset(?OName:atom, ?DName:atom, -Dataset:dict) is nondet.

void_dataset(OName, DName, Dict) :-
  distinct(
    [OName,DName,Dataset],
    dataset(OName, DName, Dataset, void:dataDump, _)
  ),
  % ASSUMPTION: A dataset description is not distributed over multiple
  aggregate_all(set(Triple), cbd(OName, DName, Dataset, Triple), Triples),
  % documents
  aggregate_all(set(Doc), rdf_prefix_member(rdf(_,void:dataDump,Doc), Triples), Docs),
  % name
  (   rdf_prefix_member(P, [dct:title,rdfs:label]),
      rdf_prefix_member(rdf(Dataset,P,Name0), Triples)
  ->  rdf_term_atom(Name0, Name)
  ;   rdf_term_atom(Dataset, Name)
  ),
  % source
  format(string(Source), "VoID ~a/~a", [OName,DName]),
  Dict1 = _{documents: Docs, name: Name, source: Source, url: Dataset},
  % description (optional)
  description_(Dict1, Triples, Dict2),
  (   rdf_prefix_member(rdf(Dataset,dct:description,Desc0), Triples)
  ->  rdf_term_atom(Desc0, Desc),
      Dict2 = Dict1.put(_{description: Desc})
  ;   Dict2 = Dict1
  ),
  % license (optional)
  (   rdf_prefix_member(P, [dct:license,dct:rights,wv:norms,wv:waiver]),
      rdf_prefix_member(rdf(_,P,License0), Triples)
  ->  rdf_term_atom(License0, License),
      Dict3 = Dict2.put(_{license: License})
  ;   Dict3 = Dict2
  ),
  % organization
  (   rdf_prefix_member(P, [dct:publisher,dct:creator,dct:contributor]),
      rdf_prefix_member(rdf(Dataset,P,Org), Triples),
      rdf_term_name(Org, Name)
  ->  Dict1 = _{id: Org, name: Name},
      (   rdf_prefix_member(Q, [foaf:homepage,foaf:page,dct:source,foaf:mbox]),
          statement(OName, DName, Org, Q, Url)
      ->  Dict2 = Dict1.put(_{url: Url})
      ;   Dict2 = Dict1
      )
  ;   true
  ).

  (   rdf_is_literal(Org0)
  ->  rdf_literal_lexical_form(Org0, Name),
      Dict2 = _{name: Name}
  ;   statement(OName, DName, Org0, rdfs:label, Name),
      Dict1 = _{name: Name},
  ), !.

% foaf:homepage, foaf:page, dct:source, foaf:mbox
url_(Dict1, Triples, Dict2) :-
  rdf_prefix_member(P, [foaf:homepage,foaf:page,dct:source,foaf:mbox]),
  rdf_prefix_member(rdf(_,P,Url), Triples), !,
  Dict2 = Dict1.put(_{url: Url}).
url_(Dict, _, Dict).
