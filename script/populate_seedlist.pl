/* LOD Index → LOD Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).

:- use_module(library(ll/index_api)).
:- use_module(library(ll/seedlist_api)).



run :-
  aggregate_all(set(Uri), index_download_location(Uri), Uris),
  assert_seeds(Uris).
