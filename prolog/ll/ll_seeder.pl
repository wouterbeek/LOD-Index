:- module(
  ll_seeder,
  [
    add_seed/1,        % +Seed
    delete_seed/1,     % +Seed
    end_seed/1,        % +Seed
    processing_seed/1, % -Seed
    reset_seed/1,      % +Seed
    seed/1,            % -Seed
    seed_by_hash/2,    % +Hash, -Seed
    start_seed/1       % -Seed
  ]
).

/** <module> LOD Laundromat seeder

  * approach(atom) REQUIRED
  * dataset(dict)
    * description(string)
    * image(uri)
    * 'last-modified'(float) REQUIRED
    * license(uri)
    * name(atom) REQUIRED
    * url(uri)
  * documents(list(uri)) REQUIRED
  * organization(dict)
    * name(atom)
    * image(uri)
    * url(uri)

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(uri_ext)).

:- initialization
   init_seeder.

:- meta_predicate
    seedlist_request_(+, +, 1, +).

:- setting(authority, any, _,
           "URI scheme of the seedlist server location.").
:- setting(data_directory, any, _,
           "The directory where seeder reports are stored.").
:- setting(password, any, _, "").
:- setting(scheme, oneof([http,https]), https,
           "URI scheme of the seedlist server location.").
:- setting(user, any, _, "").





%! add_seed(+Seed:dict) is det.

add_seed(Seed) :-
  catch(
    seedlist_request_([seed], _, close, [post(json(Seed)),success(201)]),
    E,
    true
  ),
  (   var(E)
  ->  true
  ;   E = error(http_status(200,_),_)
  ->  print_message(informational, seed_already_exists)
  ;   throw(E)
  ).



%! delete_seed(+Seed:dict) is det.

delete_seed(Seed) :-
  seedlist_request_(
    [seed],
    [hash(Seed.hash)],
    close,
    [failure(404),method(delete)]
  ).



%! end_seed(+Seed:dict) is det.

end_seed(Seed) :-
  seedlist_request_(
    [seed,processing],
    [hash(Seed.hash)],
    close,
    [failure(404),method(patch)]
  ).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  seedlist_request_([seed], _, seed_(Seed), []).



%! seed_by_hash(+Hash:atom, -Seed:dict) is det.

seed_by_hash(Hash, Seed) :-
  seedlist_request_([seed], [hash(Hash)], seed_(Seed), []).



%! processing_seed(-Seed:dict) is nondet.

processing_seed(Seed) :-
  seedlist_request_([seed,processing], [], seed_(Seed), []).



%! reset_seed(+Seed:dict) is det.

reset_seed(Seed) :-
  seedlist_request_(
    [seed,idle],
    [hash(Seed.hash)],
    close,
    [failure(404),method(patch)]
  ).



%! start_seed(-Seed:dict) is semidet.

start_seed(Seed) :-
  seedlist_request_(
    [seed,stale],
    [],
    seed_(Seed),
    [failure(404),method(patch)]
  ).





% GENERICS %

%! seed_(-Seed:dict, +In:stream) is det.

seed_(Seed, In) :-
  call_cleanup(
    (
      json_read_dict(In, Seeds, [value_string_as(atom)]),
      (is_list(Seeds) -> member(Seed, Seeds) ; Seed = Seeds)
    ),
    close(In)
  ).



%! seedlist_request_(+Segments:list(atom), ?Query:list(compound), :Goal_1,
%!                   +Options:list(compound)) is semidet.

seedlist_request_(Segments, Query, Goal_1, Options) :-
  maplist(
    setting,
    [authority,password,scheme,user],
    [Auth,Password,Scheme,User]
  ),
  uri_comps(Uri, uri(Scheme,Auth,Segments,Query,_)),
  http_call(
    Uri,
    Goal_1,
    [accept(json),authorization(basic(User,Password))|Options]
  ).





% INITIALIZATION %

%! init_seeder is det.

init_seeder :-
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
