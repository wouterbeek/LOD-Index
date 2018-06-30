:- module(ckan_index, [site_index/0,site_index/1]).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).
:- use_module(library(zlib)).

:- use_module(library(dict)).
:- use_module(library(http/ckan_export)).
:- use_module(library(http/http_client2)).
:- use_module(library(pp)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir/tapir_api)).
:- use_module(library(uri_ext)).

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



site_index :-
  site_index(
    _{
      dir: 'opendata.oorlogsbronnen.nl-2018-6-17',
      lang: 'nl-nl',
      url: 'https://opendata.oorlogsbronnen.nl/'
    }
  ).

site_index(Conf) :-
  _{dir: Dir, lang: LTag, url: _Uri} :< Conf,
  %ckan_export(Uri, Dir),
  site_index_datasets(Dir, LTag).

site_index_datasets(Dir, LTag) :-
  open_json(Dir, organization, OrgDicts),
  maplist(assert_organization(LTag), OrgDicts),
  open_json(Dir, package, DatasetDicts),
  maplist(assert_dataset(LTag), DatasetDicts).

assert_dataset(LTag, DatasetDict) :-
  _{
    'license_id': LicenseId,
    name: DatasetLocal,
    notes: Description,
    organization: OrgDict,
    resources: ResourceDicts,
    title: Title,
    url: Uri
  } :< DatasetDict,
  _{
    name: OrgLocal
  } :< OrgDict,
  rdf_global_id(data:DatasetLocal, Dataset),
  rdf_assert_triple('https://index.lodlaundromat.org', ldm:dataset, Dataset),
  rdf_assert_triple(Dataset, rdf:type, ldm:'Dataset'),
  (   Description \== ''
  ->  rdf_assert_triple(Dataset, dct:description, literal(lang(LTag,Description)))
  ;   true
  ),
  license_uri(LicenseId, License),
  rdf_global_id(org:OrgLocal, Org),
  rdf_assert_triple(Dataset, dct:creator, Org),
  rdf_assert_triple(Dataset, dct:license, License),
  rdf_assert_triple(Dataset, dct:title, literal(lang(LTag,Title))),
  (   Uri \== ''
  ->  rdf_assert_triple(Dataset, foaf:homepage, literal(type(xsd:anyURI,Uri)))
  ;   true
  ),
  rdf_assert_triple(Dataset, rdfs:label, literal(lang(LTag,Title))),
  rdf_global_id(dist:DatasetLocal, Distribution),
  rdf_assert_triple(Distribution, rdf:type, ldm:'Distribution'),
  rdf_assert_triple(Dataset, ldm:distribution, Distribution),
  maplist(assert_distribution(LTag, Distribution), ResourceDicts).

assert_distribution(LTag, Distribution, ResourceDict) :-
  _{description: Description, id: Local, name: Name, url: Uri} :< ResourceDict,
  (   Uri \== ''
  ->  rdf_global_id(file:Local, File),
      rdf_assert_triple(Distribution, ldm:file, File),
      rdf_assert_triple(File, ldm:downloadLocation, literal(type(xsd:anyURI,Uri))),
      (   Description \== ''
      ->  rdf_assert_triple(File, dct:description, literal(lang(LTag,Description)))
      ;   true
      ),
      rdf_assert_triple(File, dct:title, literal(lang(LTag,Name))),
      rdf_assert_triple(File, rdfs:label, literal(lang(LTag,Name)))
  ;   true
  ).

assert_organization(LTag, OrgDict) :-
  _{description: Description, image_url: ImageUri1, name: Local, title: Name} :< OrgDict,
  rdf_global_id(org:Local, Org),
  rdf_assert_triple(Org, rdf:type, foaf:'Org'),
  (   Description \== ''
  ->  rdf_assert_triple(Org, dct:description, literal(lang(LTag,Description)))
  ;   true
  ),
  rdf_assert_triple(Org, dct:title, literal(lang(LTag,Name))),
  image_uri_file(Local, ImageUri1, ImageFile),
  (   is_uri(ImageUri1)
  ->  http_download(ImageUri1, ImageFile),
      triply_image_uri(wouter, index, ImageFile, ImageUri2),
      rdf_assert_triple(Org, foaf:depiction, literal(type(xsd:anyURI,ImageUri2)))
  ;   true
  ),
  rdf_assert_triple(Org, rdfs:label, literal(lang(LTag,Name))).

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
      'nightly.triply.cc',
      ['_api',datasets,User,Dataset,assets,download],
      [fileName(File)],
      _
    )
  ).



% HELPERS %

open_json(Dir, Base, Dicts) :-
  file_name_extension(Base, 'json.gz', Local),
  directory_file_path(Dir, Local, File),
  setup_call_cleanup(
    gzopen(File, read, In),
    json_read_dict(In, Dicts, [value_string_as(atom)]),
    close(In)
  ).
