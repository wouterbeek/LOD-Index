/* LOD Cloud Index

Generated LOD Index descriptions for LOD Cloud files.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(file_ext)).
:- use_module(library(json_ext)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- maplist(rdf_register_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dcterm,
     dist-'https://index.lodlaundromat.org/distribution/',
     foaf,
     graph-'https://lodlaundromat.org/graph/',
     ldm-'https://ldm.cc/',
     org-'https://index.lodlaundromat.org/organization/',
     rdf,
     rdfs,
     topic-'https://index.lodlaundromat.org/topic/',
     xsd
   ]).

:- initialization
   set_setting(rdf_term:bnode_prefix_scheme, https),
   set_setting(rdf_term:bnode_prefix_authority, 'lodlaundromat.org').

run :-
  run('/home/wbeek/data/LOD-Cloud/2018-07-08.json.gz').

run(File) :-
  file_base_name(File, Local1),
  file_name_extensions(Local1, Name, [json,gz]),
  file_name_extensions(Local2, Name, [ttl]),
  json_load(File, Struct),
  forall(
    get_dict(Key, Struct, Dict),
    assert_dataset(Key, Dict)
  ),
  rdf_save(Local2).

assert_dataset(Key, Dict) :-
  rdf_prefix_iri(data:Key, Dataset),
  rdf_assert_triple(Dataset, rdf:type, ldm:'Dataset'),
  forall(
    get_dict(LTag, Dict.description, Lex),
    rdf_assert_triple(Dataset, dcterm:description, Lex-LTag)
  ),
  rdf_prefix_iri(topic:Dict.domain, Topic),
  rdf_assert_triple(Dataset, dcterm:subject, Topic),
  rdf_bnode_iri(Org),
  rdf_assert_triple(Dataset, dcterm:creator, Org),
  rdf_assert_triple(Org, rdf:type, foaf:'Organization'),
  rdf_assert_triple(Org, foaf:homepage, uri(Dict.contact_point.email)),
  rdf_assert_triple(Org, rdfs:label, str(Dict.contact_point.name)),
  rdf_assert_triple(Dataset, foaf:homepage, uri(Dict.website)),
  rdf_assert_triple(Dataset, rdfs:label, str(Dict.title)),
  get_dict(triples, Dict, Triples),
  (atom(Triples) -> Lex = Triples ; atom_number(Lex, Triples)),
  rdf_assert_triple(Dataset, ldm:triples, str(Lex)),
  %(   get_dict(namespace, Dict, Namespace)
  %->  rdf_assert_triple(Dataset, ldm:namespace, uri(Namespace))
  %;   true
  %),
  maplist(assert_distribution(Dataset, access_url), Dict.other_download),
  maplist(assert_distribution(Dataset, download_url), Dict.full_download).
  %maplist(assert_endpoint(Dataset), Dict.sparql).

assert_distribution(Dataset, Key, Dict) :-
  rdf_bnode_iri(Distribution),
  rdf_assert_triple(Dataset, ldm:distribution, Distribution),
  rdf_assert_triple(Distribution, rdf:type, ldm:'Distribution'),
  (   get_dict(description, Dict, Lex1)
  ->  rdf_assert_triple(Distribution, dcterm:description, str(Lex1))
  ;   true
  ),
  get_dict(Key, Dict, Url),
  % Skip syntactically malformed download URIs.
  (   catch(is_iri(Url), E, print_message(error,E)),
      var(E)
  ->  rdf_assert_triple(Distribution, ldm:downloadLocation, uri(Url))
  ;   true
  ),
  (   get_dict(media_type, Dict, Lex2)
  ->  rdf_assert_triple(Distribution, ldm:mediaType, str(Lex2))
  ;   true
  ),
  (   get_dict(title, Dict, Lex3)
  ->  rdf_assert_triple(Distribution, rdfs:label, str(Lex3))
  ;   true
  ).

%assert_endpoint(Dataset, Dict) :-
%  get_dict(access_url, Dict, Url), !,
%  rdf_assert_triple(Dataset, ldm:sparqlEndpoint, uri(Url)).
%assert_endpoint(_, _).
