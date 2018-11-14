:- module(lod_cloud_index, [run/0]).

/** <module> LOD Cloud Index script

Generates LOD Index descriptions based on LOD Cloud source files.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(file_ext)).
:- use_module(library(json_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- maplist(rdf_register_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dct,
     dist-'https://index.lodlaundromat.org/distribution/',
     foaf,
     graph-'https://lodlaundromat.org/graph/',
     ldm,
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
  rdf_equal(G, graph:tmp),
  forall(
    get_dict(Key, Struct, Dict),
    assert_dataset(mem(G), Key, Dict)
  ),
  rdf_save_file(Local2, [graph(G)]).

assert_dataset(B, Key, Dict) :-
  rdf_prefix_iri(data:Key, Dataset),
  assert_instance(B, Dataset, ldm:'Dataset'),
  forall(
    get_dict(LTag, Dict.description, Lex),
    assert_triple(B, Dataset, dct:description, Lex-LTag)
  ),
  rdf_prefix_iri(topic:Dict.domain, Topic),
  assert_triple(B, Dataset, dct:subject, Topic),
  rdf_bnode_iri(Org),
  assert_triple(B, Dataset, dct:creator, Org),
  assert_instance(B, Org, foaf:'Organization'),
  assert_triple(B, Org, foaf:homepage, uri(Dict.contact_point.email)),
  assert_triple(B, Org, rdfs:label, string(Dict.contact_point.name)),
  assert_triple(B, Dataset, foaf:homepage, uri(Dict.website)),
  assert_triple(B, Dataset, rdfs:label, string(Dict.title)),
  get_dict(triples, Dict, Triples),
  (atom(Triples) -> Lex = Triples ; atom_number(Lex, Triples)),
  assert_triple(B, Dataset, ldm:triples, string(Lex)),
  maplist(assert_distribution(Dataset, access_url), Dict.other_download),
  maplist(assert_distribution(Dataset, download_url), Dict.full_download).

assert_distribution(Dataset, Key, Dict) :-
  rdf_bnode_iri(Distribution),
  assert_triple(B, Dataset, ldm:distribution, Distribution),
  assert_instance(B, Distribution, ldm:'Distribution'),
  (   get_dict(description, Dict, Lex1)
  ->  assert_triple(B, Distribution, dct:description, string(Lex1))
  ;   true
  ),
  % When we look at the source file, these are not URLs/URIs but IRIs!
  get_dict(Key, Dict, Iri),
  % Skip syntactically malformed download URIs.
  (   uri_iri(Uri, Iri),
      is_uri(Uri)
  ->  assert_triple(B, Distribution, ldm:downloadLocation, uri(Uri))
  ;   true
  ),
  (   get_dict(media_type, Dict, Lex2)
  ->  assert_triple(B, Distribution, ldm:mediaType, string(Lex2))
  ;   true
  ),
  (   get_dict(title, Dict, Lex3)
  ->  assert_triple(B, Distribution, rdfs:label, string(Lex3))
  ;   true
  ).
