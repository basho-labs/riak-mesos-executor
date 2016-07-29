-module(rme_config).

-export([get_value/2, get_value/3,
         convert_value/2]).

-spec get_value(atom(), term()) -> term().
get_value(Key, Default) ->
    case get_env_value(Key) of
        false ->
            application:get_env(rms, Key, Default);
        Value ->
            Value
    end.

-spec get_value(atom(), term(), atom()) -> term().
get_value(Key, Default, Type) ->
    case get_value(Key, Default) of
        Default ->
            Default;
        Value ->
            convert_value(Value, Type)
    end.

%% Internal functions.

-spec convert_value(term(), atom()) -> term().
convert_value(Value, number) when is_list(Value) ->
    try
        list_to_float(Value)
    catch 
        error:badarg -> list_to_integer(Value)
    end;
convert_value(Value, integer) when is_list(Value) ->
    list_to_integer(Value);
convert_value(Value, float) when is_list(Value) ->
    list_to_float(Value);
convert_value(Value, boolean) when is_list(Value) ->
    list_to_atom(Value);
convert_value(Value, atom) when is_list(Value) ->
    list_to_atom(Value);
convert_value(Value, binary) when is_list(Value) ->
    list_to_binary(Value);
convert_value(Value, html_string) when is_binary(Value) ->
    unescape_html(binary_to_list(Value));
convert_value(Value, html_string) when is_list(Value) ->
    unescape_html(Value);
convert_value(Value, _Type) ->
    Value.

-spec unescape_html(string()) -> string().
unescape_html([]) -> [];
unescape_html("&quot;"++Rest) -> "\"" ++ unescape_html(Rest);
unescape_html("&lt;" ++ Rest) -> "<" ++ unescape_html(Rest);
unescape_html("&gt;" ++ Rest) -> ">" ++ unescape_html(Rest);
unescape_html("&amp;"++ Rest) -> "&" ++ unescape_html(Rest);
unescape_html([C | Rest]) -> [ C | unescape_html(Rest) ].

-spec get_env_value(atom()) -> string() | false.
get_env_value(Key) ->
    Key1 = "RIAK_MESOS_" ++ string:to_upper(atom_to_list(Key)),
    os:getenv(Key1).
