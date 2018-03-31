:- module(
  upload_seeds,
  [
    assertall_seeds/0,
    retractall_seeds/0
  ]
).

/** <module> LOD Index upload seeds

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir)).
:- use_module(library(uri_ext)).

:- initialization
   upload_seeds_init.

:- meta_predicate
    seedlist_request_(+, +, 1, +).

:- rdf_assert_prefix(dcat, 'http://www.w3.org/ns/dcat#').

:- setting(authority, any, _,
           "URI scheme of the seedlist server location.").
:- setting(password, any, _, "").
:- setting(scheme, oneof([http,https]), https,
           "URI scheme of the seedlist server location.").
:- setting(user, any, _, "").





%! assertall_seeds is det.

assertall_seeds :-
  forall(
    findnsols(1 000, Uri, url_(Uri), Uris),
    seedlist_request_([seed], [], close, [post(json(_{urls: Uris}))])
  ).

url_(Uri) :-
  statement(wouter, index, _, dcat:downloadURL, Uri0),
  rdf_literal_value(Uri0, Uri).



%! retract_seed(+Seed:dict) is det.

retract_seed(Seed) :-
  seedlist_request_(
    [seed],
    [hash(Seed.hash)],
    close,
    [failure(404),method(delete)]
  ).



%! retractall_seeds is det.

retractall_seeds :-
  forall(
    seed(Seed),
    retract_seed(Seed)
  ).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  seedlist_request_([seed], _, seed_(Seed), []).





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

%! upload_seeds_init is det.

upload_seeds_init :-
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
