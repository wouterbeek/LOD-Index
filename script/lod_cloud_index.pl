:- module(lod_cloud_index, [run/0]).

/** <module> LOD Cloud → LOD Index

Uploads the index for the LOD Cloud picture.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(settings)).
:- use_module(library(zlib)).

:- use_module(library(sw/rdf_export)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- maplist(rdf_assert_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dc-'http://purl.org/dc/elements/1.1/',
     dcat-'http://www.w3.org/ns/dcat#',
     dct-'http://purl.org/dc/terms/',
     dist-'https://index.lodlaundromat.org/distribution/',
     foaf-'http://xmlns.com/foaf/0.1/',
     graph-'https://lodlaundromat.org/graph/',
     org-'https://index.lodlaundromat.org/organization/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#',
     topic-'https://index.lodlaundromat.org/topic/',
     void-'http://rdfs.org/ns/void#',
     xsd-'http://www.w3.org/2001/XMLSchema#'
   ]).

:- initialization
   set_setting(rdf_term:bnode_prefix_scheme, https),
   set_setting(rdf_term:bnode_prefix_authority, 'lodlaundromat.org').

run :-
  run('../data/2018-lod-cloud.json.gz').

run(File) :-
  setup_call_cleanup(
    gzopen(File, read, In),
    json_read_dict(In, Dict, [value_string_as(atom)]),
    close(In)
  ),
  forall(
    get_dict(Local, Dict, Dict0),
    assert_dataset(Local, Dict0)
  ),
  TmpFile = 'index.nq.gz',
  setup_call_cleanup(
    gzopen(TmpFile, write, Out),
    forall(
      rdf_triple(S, P, O, G),
      rdf_write_quad(Out, S, P, O, G)
    ),
    close(Out)
  ),
  dataset_upload(ll, 'lod-cloud', index, _{accessLevel: public, files: [TmpFile]}),
  delete_file(TmpFile).

assert_dataset(Local, Dict) :-
  writeln(Local),
  %(Local == 'bio2rdf-lsr' -> gtrace ; true),
  rdf_equal(graph:index, G),
  rdf_global_id(data:Local, S),
  rdf_global_id(topic:Dict.domain, Topic),
  rdf_assert_triple(S, rdf:type, dcat:'Dataset', G),
  forall(
    get_dict(LTag, Dict.description, Lex),
    rdf_assert_triple(S, dc:description, Lex-LTag, G)
  ),
  rdf_assert_triple(S, dcat:theme, Topic, G),
  rdf_bnode_iri(Publisher),
  rdf_assert_triple(S, dct:publisher, Publisher, G),
  rdf_assert_triple(Publisher, rdf:type, foaf:'Organization', G),
  rdf_assert_triple(Publisher, foaf:homepage, uri(Dict.contact_point.email), G),
  rdf_assert_triple(Publisher, rdfs:label, str(Dict.contact_point.name), G),
  rdf_assert_triple(S, foaf:homepage, uri(Dict.website), G),
  rdf_assert_triple(S, rdfs:label, str(Dict.title), G),
  get_dict(triples, Dict, Triples),
  (atom(Triples) -> Lex = Triples ; atom_number(Lex, Triples)),
  rdf_assert_triple(S, void:triples, str(Lex), G),
  rdf_assert_triple(S, void:uriSpace, uri(Dict.namespace), G),
  maplist(assert_distribution(S, G, access_url), Dict.other_download),
  maplist(assert_distribution(S, G, download_url), Dict.full_download),
  maplist(assert_endpoint(S, G), Dict.sparql).

assert_distribution(S, G, Key, Dict) :-
  rdf_bnode_iri(O),
  rdf_assert_triple(S, dcat:distribution, O, G),
  rdf_assert_triple(O, rdf:type, dcat:'Distribution', G),
  (get_dict(description, Dict, Lex1) -> rdf_assert_triple(O, dc:description, str(Lex1), G) ; true),
  get_dict(Key, Dict, Url),
  rdf_assert_triple(O, dcat:downloadURL, uri(Url), G),
  (get_dict(media_type, Dict, Lex2) -> rdf_assert_triple(O, dcat:mediaType, str(Lex2), G) ; true),
  (get_dict(title, Dict, Lex3) -> rdf_assert_triple(O, rdfs:label, str(Lex3), G) ; true).

assert_endpoint(S, G, Dict) :-
  (get_dict(access_url, Dict, Url) -> rdf_assert_triple(S, void:sparqlEndpoint, uri(Url), G) ; true).
