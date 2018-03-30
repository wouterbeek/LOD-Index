:- module(upload_index, [upload_index/0]).

/** <module> LOD Index uploader

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- use_module(library(conf_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- initialization
   init_index.

:- setting(authority, any, _,
           "URI scheme of the seedlist server location.").
:- setting(data_directory, any, _,
           "The directory where seeder reports are stored.").
:- setting(password, any, _, "").
:- setting(scheme, oneof([http,https]), https,
           "URI scheme of the seedlist server location.").
:- setting(user, any, _, "").





%! upload_index is det.

upload_index :-
  maplist(
    setting,
    [authority,password,scheme,user],
    [Auth,Password,Scheme,User]
  ),
  uri_comps(Uri, uri(Scheme,Auth,[seed],_,_)),
  maplist(
    {Password,Uri,User}/[File0]>>(
      absolute_file_name(data/File0, File),
      http_open2(Uri, In, [accept(json),
                           authorization(basic(User,Password)),
                           post(file(File)),
                           success(201)]),
      close(In)
    ),
    ['bio2rdf.ttl','dbpedia.ttl','kadaster.ttl','other.ttl','topics.ttl']
  ).





% INITIALIZATION %

%! init_index is det.

init_index :-
  conf_json(Conf),
  % data directory
  create_directory(Conf.'data-directory'),
  set_setting(data_directory, Conf.'data-directory'),
  % seedlist
  _{
    authority: Auth,
    password: Password,
    scheme: Scheme,
    user: User
  } :< Conf.seedlist,
  maplist(
    set_setting,
    [authority,password,scheme,user],
    [Auth,Password,Scheme,User]
  ).
