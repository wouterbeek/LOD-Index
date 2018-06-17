:- module(ckan_index, [site_index/1]).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).
:- use_module(library(zlib)).

:- use_module(library(http/ckan_export)).
:- use_module(library(http/http_client2)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- maplist(rdf_assert_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dcat-'http://www.w3.org/ns/dcat#',
     dct-'http://purl.org/dc/terms/',
     dist-'https://index.lodlaundromat.org/distribution/',
     foaf-'http://xmlns.com/foaf/0.1/',
     org-'https://index.lodlaundromat.org/organization/',
     rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#',
     topic-'https://index.lodlaundromat.org/topic/',
     xsd-'http://www.w3.org/2001/XMLSchema#'
   ]).

site_index(Uri) :-
  %ckan_export(Uri, Dir),
  site_index_datasets('opendata.oorlogsbronnen.nl-2018-6-17').

site_index_datasets(Dir) :-
  open_json(Dir, organization, OrganizationDicts),
  maplist(assert_organization, OrganizationDicts),
  open_json(Dir, package, DatasetDicts),
  maplist(assert_dataset, DatasetDicts).

assert_dataset(DatasetDict) :-
  _{
    'license_id': LicenseId,
    name: DatasetLocal,
    notes: Description,
    organization: OrganizationDict,
    resources: ResourceDicts,
    title: Title,
    url: Uri
  } :< DatasetDict,
  _{
    name: OrganizationLocal
  } :< OrganizationDict,
  rdf_global_id(data:DatasetLocal, Dataset),
  rdf_assert_triple('https://index.lodlaundromat.org', dcat:dataset, Dataset),
  rdf_assert_triple(Dataset, rdf:type, dcat:'Dataset'),
  (   Description \== ''
  ->  rdf_assert_triple(Dataset, dct:description, literal(lang('nl-nl',Description)))
  ;   true
  ),
  license_uri(LicenseId, License),
  rdf_global_id(org:OrganizationLocal, Organization),
  rdf_assert_triple(Dataset, dct:creator, Organization),
  rdf_assert_triple(Dataset, dct:license, License),
  rdf_assert_triple(Dataset, dct:title, literal(lang('nl-nl',Title))),
  (   Uri \== ''
  ->  rdf_assert_triple(Dataset, foaf:homepage, literal(type(xsd:anyURI,Uri)))
  ;   true
  ),
  rdf_assert_triple(Dataset, rdfs:label, literal(lang('nl-nl',Title))),
  maplist(assert_distribution(Dataset), ResourceDicts).

assert_distribution(Dataset, ResourceDict) :-
  _{
    description: Description,
    id: Local,
    name: Name,
    url: Uri
  } :< ResourceDict,
  (   Uri \== ''
  ->  rdf_global_id(dist:Local, Distribution),
      rdf_assert_triple(Distribution, rdf:type, dcat:'Distribution'),
      rdf_assert_triple(Dataset, dcat:distribution, Distribution),
      rdf_assert_triple(Distribution, dcat:downloadURL, literal(type(xsd:anyURI,Uri))),
      (   Description \== ''
      ->  rdf_assert_triple(Distribution, dct:description, literal(lang('nl-nl',Description)))
      ;   true
      ),
      rdf_assert_triple(Distribution, dct:title, literal(lang('nl-nl',Name))),
      rdf_assert_triple(Distribution, rdfs:label, literal(lang('nl-nl',Name)))
  ;   true
  ).

assert_organization(OrganizationDict) :-
  _{
    description: Description,
    image_url: ImageUri1,
    name: Local,
    title: Name
  } :< OrganizationDict,
  rdf_global_id(org:Local, Organization),
  rdf_assert_triple(Organization, rdf:type, foaf:'Organization'),
  (   Description \== ''
  ->  rdf_assert_triple(Organization, dct:description, literal(lang('nl-nl',Description)))
  ;   true
  ),
  rdf_assert_triple(Organization, dct:title, literal(lang('nl-nl',Name))),
  image_uri_file(Local, ImageUri1, ImageFile),
  (   is_uri(ImageUri1)
  ->  http_download(ImageUri1, ImageFile),
      triply_image_uri(wouter, index, ImageFile, ImageUri2),
      rdf_assert_triple(Organization, foaf:depiction, literal(type(xsd:anyURI,ImageUri2)))
  ;   true
  ),
  rdf_assert_triple(Organization, rdfs:label, literal(lang('nl-nl',Name))).

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
