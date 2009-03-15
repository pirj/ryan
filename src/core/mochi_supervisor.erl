-module(mochi_supervisor).
-export([init/1, start_link/0]).
-behaviour(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Web = {webapp_web, {webapp_web, start, [{port, 8000}]}, permanent, 5000, worker, dynamic},
	Processes = [Web],
	{ok, {{one_for_one, 10, 10}, Processes}}.
