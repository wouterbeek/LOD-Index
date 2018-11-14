/* CKAN Index

Generates LOD Index descriptions for CKAN sites.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(conf_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/ckan_api)).
:- use_module(library(http/http_client2)).
:- use_module(library(image)). % Incorrectly shown in red in PCEmacs.
:- use_module(library(media_type)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(tapir/tapir_api)).
:- use_module(library(uri_ext)).

:- maplist(rdf_register_prefix, [
     data-'https://index.lodlaundromat.org/dataset/',
     dct,
     dist-'https://index.lodlaundromat.org/distribution/',
     file-'https://index.lodlaundromat.org/file/',
     foaf,
     ldm,
     organization-'https://index.lodlaundromat.org/organization/',
     rdf,
     rdfs,
     topic-'https://index.lodlaundromat.org/topic/',
     xsd
   ]).

run :-
  run('https://opendata.oorlogsbronnen.nl/').

run(Uri) :-
  % Create the various directories.
  conf_json(Conf),
  ckan_uri_name(Uri, Name),
  directory_file_path(Conf.'data-directory', Name, Dir),
  maplist(directory_file_path(Dir), [data,img,tmp], [DataDir,ImgDir,TmpDir]),
  maplist(make_directory_path, [DataDir,ImgDir,TmpDir]),
  % Store CKAN content in RDF.
  rdf_equal(G, graph:tmp),
  forall(
    ckan_organization(Uri, Dict),
    assert_organization(mem(G), ImgDir, Dict)
  ),
  forall(
    ckan_package(Uri, Dict),
    assert_dataset(mem(G), Dict)
  ),
  % Save RDF to file.
  absolute_file_name(
    Name,
    File,
    [access(write),extensions([nt]),relative_to(DataDir)]
  ),
  rdf_save_file(File, [graph(G)]).

assert_dataset(B, DatasetDict) :-
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
  rdf_prefix_iri(data:DatasetLocal, Dataset),
  assert_triple(B, 'https://index.lodlaundromat.org', ldm:dataset, Dataset),
  assert_instance(B, Dataset, ldm:'Dataset'),
  (   Description \== ''
  ->  assert_triple(B, Dataset, dct:description, string(Description))
  ;   true
  ),
  license_uri(LicenseId, License),
  rdf_prefix_iri(organization:OrgLocal, Org),
  assert_triple(B, Dataset, dct:creator, Org),
  assert_triple(B, Dataset, dct:license, License),
  assert_triple(B, Dataset, dct:title, string(Title)),
  (   Uri \== ''
  ->  assert_triple(B, Dataset, foaf:homepage, uri(Uri))
  ;   true
  ),
  assert_triple(B, Dataset, rdfs:label, string(Title)),
  rdf_prefix_iri(dist:DatasetLocal, Distribution),
  assert_instance(B, Distribution, ldm:'Distribution'),
  assert_triple(B, Dataset, ldm:distribution, Distribution),
  maplist(assert_distribution(B, Distribution), ResourceDicts).

assert_distribution(B, Distribution, ResourceDict) :-
  _{description: Description, id: Local, name: Name, url: Uri} :< ResourceDict,
  (   is_uri(Uri)
  ->  rdf_prefix_iri(file:Local, File),
      assert_triple(B, Distribution, ldm:file, File),
      assert_triple(B, File, ldm:downloadLocation, uri(Uri)),
      (   Description \== ''
      ->  assert_triple(B, File, dct:description, string(Description))
      ;   true
      ),
      assert_triple(B, File, dct:title, string(Name)),
      assert_triple(B, File, rdfs:label, string(Name))
  ;   true
  ).

assert_organization(B, ImgDir, OrgDict) :-
  _{description: Description, image_url: ImageUri, name: Local, title: Name} :< OrgDict,
  rdf_prefix_iri(organization:Local, Org),
  assert_instance(B, Org, foaf:'Org'),
  (   Description \== ''
  ->  assert_triple(B, Org, dct:description, string(Description))
  ;   true
  ),
  assert_triple(B, Org, dct:title, string(Name)),
  assert_organization_image(B, ImgDir, Org, Local, ImageUri),
  assert_triple(B, Org, rdfs:label, string(Name)).

assert_organization_image(B, ImgDir, Org, Local, Uri) :-
  directory_file_path(ImgDir, Local, Path),
  (   is_http_uri(Uri)
  ->  http_download(Uri, Path),
      catch(image_format(Path, Format), E, true),
      (   var(E)
      ->  downcase_atom(Format, Extension),
          (   media_type_extension(_MediaType, Extension)
          ->  update_image_file_name(Path, Extension),
              file_name_extension(Local, Extension, AssetName),
              asset_uri(_, _, index, AssetName, AssetUri),
              assert_triple(B, Org, foaf:depiction, uri(AssetUri))
          ;   print_message(warning, unrecognized_image_format(Format)),
              delete_file(Path)
          )
      ;   print_message(warning, not_an_image(Path)),
          delete_file(Path)
      )
  ;   true
  ).

update_image_file_name(Path1, Extension) :-
  file_extensions(Path1, Extensions),
  (   Extensions == [Extension]
  ->  true
  ;   (   Extensions == [jpg],
          Extension == jpeg
      ->  change_file_name_extension(Path1, jpg, jpeg, Path2)
      ;   file_name_extension(Path1, Extension, Path2)
      ),
      rename_file(Path1, Path2)
  ).

license_uri('cc-by', 'https://creativecommons.org/licenses/by/4.0/').
license_uri('cc-by-sa', 'https://creativecommons.org/licenses/by-sa/4.0/').
license_uri('cc-nc', 'https://creativecommons.org/licenses/by-nc/4.0/').
license_uri('cc-zero', 'https://creativecommons.org/publicdomain/zero/1.0/').
