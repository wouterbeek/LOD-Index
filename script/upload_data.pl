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
  current_user(User),
  %expand_file_name('../img/*', ImageFiles0),
  %include(is_image, ImageFiles0, ImageFiles),
  expand_file_name('../data/*.{nt,ttl}', DataFiles),
  Prefixes = [
    bnode,
    data-'https://index.lodlaundromat.org/dataset/',
    dcterm,
    dist-'https://index.lodlaundromat.org/distribution/',
    foaf,
    graph-'https://data.lodlaundromat.org/wouter/index/graphs/',
    ldm,
    org-'https://index.lodlaundromat.org/organization/',
    rdfs,
    skos,
    topic-'https://index.lodlaundromat.org/topic/',
    xsd
  ],
  dataset_upload(_, User, index, _{files: DataFiles, prefixes: Prefixes}).
