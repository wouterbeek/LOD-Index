:- module(upload_datasets, [run/0]).

/** <module> Upload datasets

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).

:- debug(ll).

:- initialization
   init_upload_datasets.

:- maplist(rdf_assert_prefix, [
     ldm-'https://ldm.cc/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
   ]).

:- setting(data_directory, any, _, "").



run :-
  forall(
    statement(wouter, index, Dataset, rdf:type, ldm:'Dataset'),
    upload_dataset_(Dataset)
  ).

upload_dataset_(Dataset) :-
  % (re)create the user
  rdf_triple(Dataset, dct:publisher, Publisher),
  user_name(Publisher, User),
  (   user(Site, User, _)
  ->  true
  ;   user_create(Site, _, User, _{}, _)
  ),
  setting(ll:data_directory, Dir0),
  findall(
    File,
    (
      rdf_triple(Dataset, ldm:distribution, Distribution),
      rdf_triple(Distribution, ldm:file, File0),
      rdf_triple(File0, ldm:downloadLocation, Uri0),
      rdf_literal_value(Uri0, Uri),
      md5(Uri, Hash),
      directory_file_path(Dir0, Hash, Dir),
      directory_file_path(Dir, 'clean.nq.gz', File)
    ),
    Files
  ),
  Properties1 = _{accessLevel: public, files: Files},
  (   rdf_triple(Dataset, dct:description, Desc0)
  ->  rdf_literal_value(Desc0, Desc),
      Properties2 = Properties1.put(_{description: Desc})
  ;   Properties2 = Properties1
  ),
  (   rdf_triple(Dataset, foaf:depiction, Img0)
  ->  rdf_literal_value(Img0, Img),
      Properties3 = Properties2.put(_{avatar: Img})
  ;   Properties3 = Properties2
  ),
  (   rdf_triple(Dataset, dct:license, License0)
  ->  rdf_literal_value(License0, License),
      Properties4 = Properties3.put(_{license: License})
  ;   Properties4 = Properties3
  ),
  dataset_upload(User, Dataset, Properties4),
  debug(ll, "DONE ~a ~a", [User,Dataset]).



% INITIALIZATION %

init_upload_dataset :-
  conf_json(Conf),
  set_setting(data_directory, Conf.'data-directory').
