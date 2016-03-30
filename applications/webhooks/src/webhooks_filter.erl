-module(webhooks_filter).

-export([eval/3]).
-export([test/0]).

-include("webhooks.hrl").

-define(AND,   <<"and">>).
-define(OR,    <<"or">>).
-define(NOT,   <<"not">>).
-define(EQ,    <<"==">>).
-define(NEQ,   <<"!=">>).

eval([?AND | Exps], Props, Fun) ->
    and_(Exps, Props, Fun);
eval([?OR | Exps], Props, Fun) ->
    or_(Exps, Props, Fun);
eval([?NOT, Exp], Props, Fun) ->
    not_(Exp, Props, Fun);
eval([?EQ, K, V], Props, Fun) ->
    Fun(K, Props) =:= V;
eval([?NEQ, K, V], Props, Fun) ->
    Fun(K, Props) =/= V.

and_([], _Props, _Fun) -> true;
and_([Exp | Exps], Props, Fun) ->
    case eval(Exp, Props, Fun) of
        true -> and_(Exps, Props, Fun);
        _ -> false
    end.

or_([], _Props, _Fun) -> false;
or_([Exp | Exps], Props, Fun) ->
    case eval(Exp, Props, Fun) of
        false -> or_(Exps, Props, Fun);
        _ -> true
    end.

not_(Exp, Props, Fun) ->
    not eval(Exp, Props, Fun).

test() ->
    case couch_mgr:get_results(?KZ_WEBHOOKS_DB
                               ,<<"webhooks/webhooks_listing">> %% excludes disabled
                               ,['include_docs']
                              ) of
        {ok, []} -> error;
        {ok, Hooks} ->
            [begin
                 JObj = wh_json:get_value(<<"doc">>, Hook),
                 io:format("~p~n", [JObj]),
                 Exp = wh_json:get_value([<<"custom_data">>, <<"filter">>, <<"condition">>], JObj),
                 io:format("~p~n", [Exp]),
                 eval(Exp, JObj,
                      fun (K, O) ->
                              V = wh_json:get_value(K, O, <<"undefined">>),
                              io:format("~p~n", [V]),
                              V
                      end)
             end || Hook <- Hooks];
        _ -> error
    end.
