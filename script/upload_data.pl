/* Upload to LOD Index

Uploads the data files in `/data' and the asset files in `/img' to the
LOD Index.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(image)).
:- use_module(library(tapir/tapir_api)).

run :-
  expand_file_name('../img/*', ImageFiles0),
  include(is_image, ImageFiles0, ImageFiles),
  expand_file_name('../data/*.ttl', DataFiles),
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
  dataset_upload(
    index,
    _{assets: ImageFiles, files: DataFiles, prefixes: Prefixes}
  ).
