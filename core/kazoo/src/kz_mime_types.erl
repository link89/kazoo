%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_mime_types).

-export([from_extension/1]).
-export([to_extension/1]).

-define(CONFIG_CAT, <<"mime_types">>).

from_extension(<<".", Extension/binary>>) ->
    from_extension(Extension);
from_extension(<<>>) ->
    kapps_config:get(?CONFIG_CAT, <<"default_content_type">>, <<"application/octet-stream">>);
from_extension(<<"wav">>) -> <<"audio/x-wav">>;
from_extension(<<"mp3">>) -> <<"audio/mp3">>;
from_extension(<<Extension/binary>>) ->
    case props:get_value(kz_util:to_list(Extension), mime_types()) of
        undefined ->
            kapps_config:get(?CONFIG_CAT, <<"default_content_type">>, <<"application/octet-stream">>);
        ContentType -> kz_util:to_binary(ContentType)
    end;
from_extension(Extension) ->
    from_extension(kz_util:to_binary(Extension)).

to_extension(<<>>) ->
    kapps_config:get(?CONFIG_CAT, <<"default_extension">>, <<"mp3">>);
to_extension(<<"audio/x-wav">>) -> <<"wav">>;
to_extension(<<"audio/wav">>) -> <<"wav">>;
to_extension(<<"audio/mpeg">>) -> <<"mp3">>;
to_extension(<<"audio/mp3">>) -> <<"mp3">>;
to_extension(<<CT/binary>>) ->
    case lists:keysearch(kz_util:to_list(CT), 2, mime_types()) of
        {value, {_, Extension}} -> kz_util:to_binary(Extension);
        false ->
            kapps_config:get(?CONFIG_CAT, <<"default_extension">>, <<"mp3">>)
    end;
to_extension(CT) ->
    to_extension(kz_util:to_binary(CT)).

mime_types() ->
    Default = filename:join(code:lib_dir(inets), "examples/server_root/conf/mime.types"),
    MimeTypesFile = kapps_config:get(?CONFIG_CAT, <<"mime_types_file">>, kz_util:to_binary(Default)),
    {ok, MimeTypes} = httpd_conf:load_mime_types(MimeTypesFile),
    MimeTypes.
