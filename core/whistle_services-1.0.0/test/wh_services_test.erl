%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services_test).

-include_lib("eunit/include/eunit.hrl").

-define(CAT, <<"phone_numbers">>).
-define(ITEM, <<"did_us">>).

-record(state, {service_plan_jobj :: kzd_service_plan:plan()
                ,services :: wh_services:services()
                ,services_jobj :: wh_json:object()
                ,account_plan :: kzd_service_plan:plan()
               }).

services_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun service_plan_schema_check/1
       ,fun services_json_to_record/1
       ,fun services_record_to_json/1
       ,fun service_plan_json_to_plans/1
       ,fun increase_quantities/1
      ]
    }.

init() ->
    lists:foldl(fun init_fold/2
                ,#state{}
                ,[fun read_service_plan/1
                  ,fun read_services/1
                 ]).

init_fold(F, State) ->
    F(State).

stop(#state{}=_State) ->
    'ok'.

read_service_plan(State) ->
    ServicePlan1 = filename:join([priv_dir(), "example_service_plan_1.json"]),
    JObj = read_json(ServicePlan1),

    State#state{service_plan_jobj=JObj}.

read_services(#state{service_plan_jobj=ServicePlan}=State) ->
    ServicesPath = filename:join([priv_dir(), "example_account_services.json"]),

    JObj = read_json(ServicesPath),

    Services = wh_services:from_service_json(JObj, 'false'),

    Overrides = kzd_services:plan_overrides(JObj, wh_doc:id(ServicePlan)),
    AccountPlan = kzd_service_plan:merge_overrides(ServicePlan, Overrides),

    State#state{services_jobj=JObj
                ,services=Services
                ,account_plan=AccountPlan
               }.

priv_dir() ->
    case code:priv_dir('whistle_services') of
        Path when is_list(Path) -> Path;
        _ ->
            {'ok', AppDir} = file:get_cwd(),
            filename:join([AppDir, "priv"])
    end.

read_json(Path) ->
    {'ok', JSON} = file:read_file(Path),
    wh_json:decode(JSON).

services_json_to_record(#state{services=Services
                               ,services_jobj=JObj
                              }) ->
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

services_record_to_json(#state{services=Services}) ->
    JObj = wh_services:to_json(Services),
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

quantity_checks(Services, JObj) ->
    {Tests, _} = wh_json:foldl(fun category_checks/3
                               ,{[], Services}
                               ,kzd_services:quantities(JObj)
                              ),
    Tests.

category_checks(Category, CategoryJObj, Acc) ->
    wh_json:foldl(fun(K, V, Acc1) ->
                          item_checks(Category, K, V, Acc1)
                  end
                  ,Acc
                  ,CategoryJObj
                 ).

item_checks(Category, Item, Quantity, {Tests, Services}) ->
    {[item_check(Category, Item, Quantity, Services) | Tests]
     ,Services
    }.

item_check(Category, Item, Quantity, Services) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, Quantity]))
     ,?_assertEqual(Quantity, wh_services:quantity(Category, Item, Services))
    }.

service_plan_schema_check(#state{service_plan_jobj=ServicePlan}) ->


    {"checking service plan fixture against schema"
     ,case wh_json_schema:validate(<<"service_plans">>, ServicePlan) of
          {'ok', _} -> ?_assert('true');
          {'error', _E} -> ?_assert('false')
      end
    }.

service_plan_json_to_plans(#state{service_plan_jobj=ServicePlan
                                  ,account_plan=AccountPlan
                                 }) ->

    [{"Verify plan from file matches services plan"
      ,?_assertEqual(wh_doc:account_id(ServicePlan)
                     ,kzd_service_plan:account_id(AccountPlan)
                    )
     }
     ,{"Verify cumulative discount rate from service plan"
       ,?_assertEqual(0.5, rate(cumulative_discount(did_us_item(ServicePlan))))
      }
     ,{"Verify cumulative discount rate was overridden"
       ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(AccountPlan))))
      }
    ].

did_us_item(Plan) ->
    kzd_service_plan:item(Plan, ?CAT, ?ITEM).

cumulative_discount(Item) ->
    kzd_item_plan:cumulative_discount(Item).

rate(JObj) ->
    wh_json:get_float_value(<<"rate">>, JObj).

increase_quantities(#state{account_plan=_AccountPlan
                           ,services=Services
                          }) ->
    ItemQuantity = wh_services:quantity(?CAT, ?ITEM, Services),

    random:seed(os:timestamp()),
    Increment = random:uniform(10),

    UpdatedServices = wh_services:update(?CAT, ?ITEM, ItemQuantity+Increment, Services),

    UpdatedItemQuantity = wh_services:quantity(?CAT, ?ITEM, UpdatedServices),

    DiffItemQuantity = wh_services:diff_quantity(?CAT, ?ITEM, UpdatedServices),

    [{"Verify base quantity on the services doc"
      ,?_assertEqual(9, ItemQuantity)
     }
     ,{"Verify incrementing the quantity"
       ,?_assertEqual(ItemQuantity+Increment, UpdatedItemQuantity)
      }
     ,{"Verify the diff of the quantity"
       ,?_assertEqual(Increment, DiffItemQuantity)
      }
     | category_quantities(Services, UpdatedServices, Increment)
     ++ dry_run_charges(UpdatedServices, Increment)
    ].

category_quantities(CurrentServices, UpdatedServices, Increment) ->
    CategoryQuantity = wh_services:category_quantity(?CAT, CurrentServices),
    UpdatedCategoryQuantity = wh_services:category_quantity(?CAT, UpdatedServices),

    TollFreeQuantity = wh_services:quantity(?CAT, <<"toll_free">>, UpdatedServices),
    DIDUSQuantity = wh_services:quantity(?CAT, ?ITEM, UpdatedServices),

    MinusTollFree = wh_services:category_quantity(?CAT, [<<"toll_free">>], UpdatedServices),
    MinusDIDUS = wh_services:category_quantity(?CAT, [?ITEM], UpdatedServices),

    [{"Verify base category quantities"
      ,?_assertEqual(10, CategoryQuantity)
     }
     ,{"Verify updated category quantities"
       ,?_assertEqual(CategoryQuantity + Increment, UpdatedCategoryQuantity)
      }
     ,{"Verify updated category quantities minus toll_free numbers"
       ,?_assertEqual(MinusTollFree, UpdatedCategoryQuantity-TollFreeQuantity)
      }
     ,{"Verify updated category quantities minus did_us numbers"
       ,?_assertEqual(MinusDIDUS, UpdatedCategoryQuantity-DIDUSQuantity)
      }
    ].

dry_run_charges(Services, _Increment) ->
    DryRunCharges = wh_services:dry_run(Services),
    ?debugFmt("dry run: ~p~n", [DryRunCharges]),
    ?debugFmt("services: ~p~n", [wh_services:updated_quantities(Services)]),
    [].
