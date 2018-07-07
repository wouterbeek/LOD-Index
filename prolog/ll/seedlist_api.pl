:- module(
  seedlist_api,
  [
    assert_seed/1,  % +Uri
    assert_seeds/1, % +Uris
    retract_seed/1, % +Seed
    seed/1,         % -Seed
    seed_by_type/2  % +Type, -Seed
  ]
).

/** <module> LOD Seedlist API

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- initialization
   init_seedlist_api.

:- meta_predicate
    seedlist_request_(+, +, 1, +).

:- setting(seedlist:authority, any, _,
           "URI scheme of the LOD Seedlist server location.").
:- setting(seedlist:password, any, _, "").
:- setting(seedlist:scheme, oneof([http,https]), https,
           "URI scheme of the LOD Seedlist server location.").
:- setting(seedlist:user, any, _, "").





%! assert_seed(+Uri:atom) is det.

assert_seed(Uri) :-
  must_be(atom, Uri),
  assert_seeds([Uri]).



%! assert_seeds(+Uris:list(atom)) is det.

assert_seeds(Uris) :-
  seedlist_request_([seed], [], close, [post(json(_{urls: Uris}))]).



%! retract_seed(+Seed:dict) is det.

retract_seed(Seed) :-
  seedlist_request_(
    [seed],
    [hash(Seed.hash)],
    close,
    [failure(404),method(delete)]
  ).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  seedlist_request_([seed], [], seed_(Seed), []).



%! seed_by_type(+Type:oneof([idle,processing,stale]), -Seed:dict) is nondet.

seed_by_type(Type, Seed) :-
  must_be(oneof([idle,processing,stale]), Type),
  seedlist_request_([seed,Type], [], seed_(Seed), []).





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



%! seedlist_request_(+Segments:list(atom), +Query:list(compound), :Goal_1,
%!                   +Options:list(compound)) is semidet.

seedlist_request_(Segments, Query, Goal_1, Options) :-
  maplist(
    setting,
    [seedlist:authority,seedlist:password,seedlist:scheme,seedlist:user],
    [Auth,Password,Scheme,User]
  ),
  uri_comps(Uri, uri(Scheme,Auth,Segments,Query,_)),
  http_call(
    Uri,
    Goal_1,
    [accept(json),authorization(basic(User,Password))|Options]
  ).





% INITIALIZATION %

%! init_seedlist_api is det.

init_seedlist_api :-
  conf_json(Conf),
  maplist(
    set_setting,
    [seedlist:authority,seedlist:password,seedlist:scheme,seedlist:user],
    [
      Conf.seedlist.authority,
      Conf.seedlist.password,
      Conf.seedlist.scheme,
      Conf.seedlist.user
    ]
  ).
