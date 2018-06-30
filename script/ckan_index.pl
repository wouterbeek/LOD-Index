:- module(ckan_index, [run/0,run/1]).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(settings)).
:- use_module(library(zlib)).

:- use_module(library(conf_ext)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(http/ckan_api)).
:- use_module(library(http/ckan_export)).
:- use_module(library(http/http_client2)).
:- use_module(library(pp)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).
:- use_module(library(uri_ext)).

:- initialization
   init_ckan_index.

:- maplist(rdf_assert_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dct-'http://purl.org/dc/terms/',
     dist-'https://index.lodlaundromat.org/distribution/',
     file-'https://index.lodlaundromat.org/file/',
     foaf-'http://xmlns.com/foaf/0.1/',
     ldm-'https://ldm.cc/',
     org-'https://index.lodlaundromat.org/organization/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#',
     topic-'https://index.lodlaundromat.org/topic/',
     xsd-'http://www.w3.org/2001/XMLSchema#'
   ]).

:- setting(data_directory, any, _, "").



run :-
  run('https://opendata.oorlogsbronnen.nl/').

run(Uri) :-
  setting(data_directory, Dir1),
  ckan_uri_directory(Dir1, Uri, Dir2),
  create_directory(Dir2),
  %ckan_export(Uri, Dir2),
  directory_file_path(Dir2, img, Dir3),
  create_directory(Dir3),
  open_json(Dir2, organization, OrgDicts),
  maplist(assert_organization(Dir3), OrgDicts),
  open_json(Dir2, package, DatasetDicts),
  maplist(assert_dataset, DatasetDicts),
  %aggregate_all(set(Image), directory_path(Dir3, Image), Images),
  directory_file_path(Dir2, 'index.nt.gz', File),
  rdf_save(File),
  %uri_hash(Uri, Hash),
  %dataset_upload(Hash, _{accessLevel: public, assets: Images, files: ['index.nt.gz']}),
  true.

assert_dataset(DatasetDict) :-
  _{
    'license_id': LicenseId,
    name: DatasetLocal,
    notes: Description,
    organization: OrgDict,
    resources: ResourceDicts,
    title: Title,
    url: Uri
  } :< DatasetDict,
  _{name: OrgLocal} :< OrgDict,
  rdf_global_id(data:DatasetLocal, Dataset),
  rdf_assert_triple('https://index.lodlaundromat.org', ldm:dataset, Dataset),
  rdf_assert_triple(Dataset, rdf:type, ldm:'Dataset'),
  (   Description \== ''
  ->  rdf_assert_triple(Dataset, dct:description, str(Description))
  ;   true
  ),
  license_uri(LicenseId, License),
  rdf_global_id(org:OrgLocal, Org),
  rdf_assert_triple(Dataset, dct:creator, Org),
  rdf_assert_triple(Dataset, dct:license, License),
  rdf_assert_triple(Dataset, dct:title, str(Title)),
  (   Uri \== ''
  ->  rdf_assert_triple(Dataset, foaf:homepage, uri(Uri))
  ;   true
  ),
  rdf_assert_triple(Dataset, rdfs:label, str(Title)),
  rdf_global_id(dist:DatasetLocal, Distribution),
  rdf_assert_triple(Distribution, rdf:type, ldm:'Distribution'),
  rdf_assert_triple(Dataset, ldm:distribution, Distribution),
  maplist(assert_distribution(Distribution), ResourceDicts).

assert_distribution(Distribution, ResourceDict) :-
  _{description: Description, id: Local, name: Name, url: Uri} :< ResourceDict,
  (   Uri \== ''
  ->  rdf_global_id(file:Local, File),
      rdf_assert_triple(Distribution, ldm:file, File),
      rdf_assert_triple(File, ldm:downloadLocation, uri(Uri)),
      (   Description \== ''
      ->  rdf_assert_triple(File, dct:description, str(Description))
      ;   true
      ),
      rdf_assert_triple(File, dct:title, str(Name)),
      rdf_assert_triple(File, rdfs:label, str(Name))
  ;   true
  ).

assert_organization(Dir, OrgDict) :-
  _{description: Description, image_url: ImageUri1, name: Local, title: Name} :< OrgDict,
  rdf_global_id(org:Local, Org),
  rdf_assert_triple(Org, rdf:type, foaf:'Org'),
  (   Description \== ''
  ->  rdf_assert_triple(Org, dct:description, str(Description))
  ;   true
  ),
  rdf_assert_triple(Org, dct:title, str(Name)),
  image_uri_file(Local, ImageUri1, ImageFile),
  directory_file_path(Dir, ImageFile, ImagePath),
  (   is_uri(ImageUri1)
  ->  http_download(ImageUri1, ImagePath),
      triply_image_uri(wouter, index, ImageFile, ImageUri2),
      rdf_assert_triple(Org, foaf:depiction, uri(ImageUri2))
  ;   true
  ),
  rdf_assert_triple(Org, rdfs:label, str(Name)).

image_uri_file(Local, Uri, File) :-
  member(Ext, [gif,jpeg,jpg,png]),
  sub_atom_icasechk(Uri, _, Ext), !,
  file_name_extension(Local, Ext, File).
image_uri_file(Local, _, Local).

license_uri('cc-by', 'https://creativecommons.org/licenses/by/4.0/').
license_uri('cc-by-sa', 'https://creativecommons.org/licenses/by-sa/4.0/').
license_uri('cc-nc', 'https://creativecommons.org/licenses/by-nc/4.0/').
license_uri('cc-zero', 'https://creativecommons.org/publicdomain/zero/1.0/').

triply_image_uri(User, Dataset, File, Uri) :-
  uri_comps(
    Uri,
    uri(
      https,
      'data.lodlaundromat.org',
      ['_api',datasets,User,Dataset,assets,download],
      [fileName(File)],
      _
    )
  ).



% INITIALIZATION %

init_ckan_index :-
  conf_json(Conf),
  directory_file_path(Conf.'data-directory', 'LOD-Index', Dir),
  create_directory(Dir),
  set_setting(data_directory, Dir).



% HELPERS %

open_json(Dir, Base, Dicts) :-
  file_name_extension(Base, 'json.gz', Local),
  directory_file_path(Dir, Local, File),
  setup_call_cleanup(
    gzopen(File, read, In),
    json_read_dict(In, Dicts, [value_string_as(atom)]),
    close(In)
  ).
