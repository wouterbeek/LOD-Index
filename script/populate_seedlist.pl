/* LOD Index → LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).

:- use_module(lod_index_api).
:- use_module(lod_seedlist_api).

:- initialization
   init_populate_seedlist.



run :-
  aggregate_all(set(Uri), download_location(Uri), Uris),
  assert_seeds(Uris).



% INITIALIZATION %

%! init_populate_seedlist is det.

init_populate_seedlist :-
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
