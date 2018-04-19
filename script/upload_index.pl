:- module(upload_index, [run/0]).

/** <module> LOD Index uploader

@author Wouter Beek
@version 2018
*/

:- use_module(library(tapir)).

run :-
  expand_file_name('../data/*.ttl', Files),
  Prefixes = [
    data-'https://index.lodlaundromat.org/dataset/',
    dcat-'http://www.w3.org/ns/dcat#',
    dct-'http://purl.org/dc/terms/',
    dist-'https://index.lodlaundromat.org/distribution/',
    foaf-'http://xmlns.com/foaf/0.1/',
    graph-'https://data.lodlaundromat.org/wouter/index/graphs/',
    org-'https://index.lodlaundromat.org/organization/',
    rdfs-'http://www.w3.org/2000/01/rdf-schema#',
    skos-'http://www.w3.org/2004/02/skos/core#',
    topic-'https://index.lodlaundromat.org/topic/',
    void-'http://rdfs.org/ns/void#',
    xsd-'http://www.w3.org/2001/XMLSchema#'
  ],
  dataset_upload(wouter, index, _{files: Files, prefixes: Prefixes}).
