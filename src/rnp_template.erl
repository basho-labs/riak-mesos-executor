-module(rnp_template).
-export([match/1]).
-export([mustachify/1]).

%% TODO This. Is. TYRANNY.
%% mustache template variables have to:
%%  - start lowercase (i.e. not with a '.')
%%  - be passed as atom keys
%%  - be in a dict
mustachify(Tmpl) ->
    mustachify(match(Tmpl), Tmpl).

match(Tmpl) ->
    re:run(Tmpl, << "{{+\\.[^}]+}}+" >>).

%% TODO This might be removable if we change the templates in the golang scheduler
mustachify(nomatch, Tmpl) -> Tmpl;
mustachify({match, [{Start, Len}]}, Tmpl) ->
    Key = binary:part(Tmpl, Start, Len),
    %% TODO Mustache has a bug: if the template variable is immediately followed by a }
    %% that } gets eaten.
    %% i.e. { foobar, {{foobarbaz}}}
    %% becomes { foobar, baz
    %% not { foobar, baz}
    NoDot = binary:replace(Key, <<"{{\.">>, <<"{{">>),
    Space0 = binary:replace(NoDot, <<"{{{">>, <<"{ {{">>),
    Space1 = binary:replace(Space0, <<"}}}">>, <<"}} }">>),
    Lower = list_to_binary(string:to_lower(binary_to_list(Space1))),
    mustachify(binary:replace(Tmpl, Key, Lower)).
