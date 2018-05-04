:- module(
  lod_index,
  [
    reset_seeds/0,
    upload/1       % +User
  ]
).

/** <module> LOD Index script

Simple scripts for interacting with the LOD Index.

---

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
:- use_module(library(http/tapir)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- initialization
   init_seedlist_client.

:- meta_predicate
    seedlist_request_(+, +, 1, +).

:- rdf_assert_prefix(dcat, 'http://www.w3.org/ns/dcat#').

:- setting(authority, any, _,
           "URI scheme of the LOD Seedlist server location.").
:- setting(password, any, _, "").
:- setting(scheme, oneof([http,https]), https,
           "URI scheme of the LOD Seedlist server location.").
:- setting(user, any, _, "").





% SCRIPT %

%! reset_seeds is det.
%
% Reset all seeds in LOD Seedlist.

reset_seeds :-
  forall(
    (
      member(Status, [idle,processing]),
      seed_by_type(Status, Seed)
    ),
    (
      retract_seed(Seed),
      assert_seed(Seed.url)
    )
  ).



%! upload(+User:oneof(['lod-cloud','lod-laundromat'])) is det.
%
% Upload the LOD Index of User to LOD Seedlist.

upload(User) :-
  forall(
    findnsols(1 000, Uri, uri_(User, Uri), Uris),
    assert_seeds(Uris)
  ).

uri_(User, Uri) :-
  statement(ll, User, index, _, dcat:downloadURL, Uri0, _),
  rdf_literal_value(Uri0, Uri).





% API %

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

%! init_seedlist_client is det.

init_seedlist_client :-
  conf_json(Conf),
  maplist(
    set_setting,
    [authority,password,scheme,user],
    [
      Conf.seedlist.authority,
      Conf.seedlist.password,
      Conf.seedlist.scheme,
      Conf.seedlist.user
    ]
  ).
