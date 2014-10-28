%%% @doc JSON decoding module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(jsone_decode).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([decode/1, decode/2]).

%%--------------------------------------------------------------------------------
%% Macros & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).

-type next() :: {array_next, [jsone:json_value()]}
              | {object_value, jsone:json_object_members()}
              | {object_next, jsone:json_string(), jsone:json_object_members()}.

-type whitespace_next() :: value
                         | array
                         | object
                         | {array_next, [jsone:json_value()]}
                         | {object_key, jsone:json_object_members()}
                         | {object_value, jsone:json_string(), jsone:json_object_members()}
                         | {object_next, jsone:json_object_members()}.

-type decode_result() :: {ok, jsone:json_value(), Rest::binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.

-record(decode_opt_v1, { format = eep18 :: eep18 | proplist }).
-define(DECODE_OPT, #decode_opt_v1).
-type decode_opt() :: #decode_opt_v1{}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc Decodes an erlang term from json text (a utf8 encoded binary)
-spec decode(binary()) -> decode_result().
decode(<<Json/binary>>) ->
    jsone:try_decode(Json).

-spec decode(binary(), [jsone:decode_option()]) -> decode_result().
decode(<<Json/binary>>, Options) ->
    Opts = parse_options(Options),
    whitespace(Json, value, [], <<"">>, Opts).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next(binary(), jsone:json_value(), [next()], binary(), decode_opt()) -> decode_result().
next(<<Bin/binary>>, Value, [], _Buf, _Opts) ->
    {ok, Value, Bin};
next(<<Bin/binary>>, Value, [Next | Nexts], Buf, Opts) ->
    case Next of
        {array_next, Values}        -> whitespace(Bin, {array_next, [Value | Values]}, Nexts, Buf, Opts);
        {object_value, Members}     -> whitespace(Bin, {object_value, Value, Members}, Nexts, Buf, Opts);
        {object_next, Key, Members} -> whitespace(Bin, {object_next, [{Key, Value} | Members]}, Nexts, Buf, Opts)
    end.

-spec whitespace(binary(), whitespace_next(), [next()], binary(), decode_opt()) -> decode_result().
whitespace(<<$  , Bin/binary>>, Next, Nexts, Buf, Opts) -> whitespace(Bin, Next, Nexts, Buf, Opts);
whitespace(<<$\t, Bin/binary>>, Next, Nexts, Buf, Opts) -> whitespace(Bin, Next, Nexts, Buf, Opts);
whitespace(<<$\r, Bin/binary>>, Next, Nexts, Buf, Opts) -> whitespace(Bin, Next, Nexts, Buf, Opts);
whitespace(<<$\n, Bin/binary>>, Next, Nexts, Buf, Opts) -> whitespace(Bin, Next, Nexts, Buf, Opts);
whitespace(<<Bin/binary>>,      Next, Nexts, Buf, Opts) ->
    case Next of
        value  -> value(Bin, Nexts, Buf, Opts);
        array  -> array(Bin, Nexts, Buf, Opts);
        object -> object(Bin, Nexts, Buf, Opts);
        {object_key, Members}        -> object_key(Bin, Members, Nexts, Buf, Opts);
        {array_next, Values}         -> array_next(Bin, Values, Nexts, Buf, Opts);
        {object_value, Key, Members} -> object_value(Bin, Key, Members, Nexts, Buf, Opts);
        {object_next, Members}       -> object_next(Bin, Members, Nexts, Buf, Opts)
    end.

-spec value(binary(), [next()], binary(), decode_opt()) -> decode_result().
value(<<"false", Bin/binary>>, Nexts, Buf, Opts) -> next(Bin, false, Nexts, Buf, Opts);
value(<<"true", Bin/binary>>, Nexts, Buf, Opts)  -> next(Bin, true, Nexts, Buf, Opts);
value(<<"null", Bin/binary>>, Nexts, Buf, Opts)  -> next(Bin, null, Nexts, Buf, Opts);
value(<<$[, Bin/binary>>, Nexts, Buf, Opts)      -> whitespace(Bin, array, Nexts, Buf, Opts);
value(<<${, Bin/binary>>, Nexts, Buf, Opts)      -> whitespace(Bin, object, Nexts, Buf, Opts);
value(<<$", Bin/binary>>, Nexts, Buf, Opts)      -> string(Bin, byte_size(Buf), Nexts, Buf, Opts);
value(<<Bin/binary>>, Nexts, Buf, Opts)          -> number(Bin, Nexts, Buf, Opts).

-spec array(binary(), [next()], binary(), decode_opt()) -> decode_result().
array(<<$], Bin/binary>>, Nexts, Buf, Opts) -> next(Bin, [], Nexts, Buf, Opts);
array(<<Bin/binary>>, Nexts, Buf, Opts)     -> value(Bin, [{array_next, []} | Nexts], Buf, Opts).

-spec array_next(binary(), [jsone:json_value()], [next()], binary(), decode_opt()) -> decode_result().
array_next(<<$], Bin/binary>>, Values, Nexts, Buf, Opts) -> next(Bin, lists:reverse(Values), Nexts, Buf, Opts);
array_next(<<$,, Bin/binary>>, Values, Nexts, Buf, Opts) -> whitespace(Bin, value, [{array_next, Values} | Nexts], Buf, Opts);
array_next(Bin,                Values, Nexts, Buf, Opts) -> ?ERROR(array_next, [Bin, Values, Nexts, Buf, Opts]).

-spec object(binary(), [next()], binary(), decode_opt()) -> decode_result().
object(<<$}, Bin/binary>>, Nexts, Buf, ?DECODE_OPT{format=eep18}=Opts)    -> next(Bin, {[]}, Nexts, Buf, Opts);
object(<<$}, Bin/binary>>, Nexts, Buf, ?DECODE_OPT{format=proplist}=Opts) -> next(Bin, [{}], Nexts, Buf, Opts);
object(<<Bin/binary>>, Nexts, Buf, Opts)                                  -> object_key(Bin, [], Nexts, Buf, Opts).

-spec object_key(binary(), jsone:json_object_members(), [next()], binary(), decode_opt()) -> decode_result().
object_key(<<$", Bin/binary>>, Members, Nexts, Buf, Opts) -> string(Bin, byte_size(Buf), [{object_value, Members} | Nexts], Buf, Opts);
object_key(<<Bin/binary>>, Members, Nexts, Buf, Opts)     -> ?ERROR(object_key, [Bin, Members, Nexts, Buf, Opts]).

-spec object_value(binary(), jsone:json_string(), jsone:json_object_members(), [next()], binary(), decode_opt()) -> decode_result().
object_value(<<$:, Bin/binary>>, Key, Members, Nexts, Buf, Opts) -> whitespace(Bin, value, [{object_next, Key, Members} | Nexts], Buf, Opts);
object_value(Bin,                Key, Members, Nexts, Buf, Opts) -> ?ERROR(object_value, [Bin, Key, Members, Nexts, Buf, Opts]).

-spec object_next(binary(), jsone:json_object_members(), [next()], binary(), decode_opt()) -> decode_result().
object_next(<<$}, Bin/binary>>, Members, Nexts, Buf, ?DECODE_OPT{format=eep18}=Opts)    -> next(Bin, {Members}, Nexts, Buf, Opts);
object_next(<<$}, Bin/binary>>, Members, Nexts, Buf, ?DECODE_OPT{format=proplist}=Opts) -> next(Bin, Members, Nexts, Buf, Opts);
object_next(<<$,, Bin/binary>>, Members, Nexts, Buf, Opts)                              -> whitespace(Bin, {object_key, Members}, Nexts, Buf, Opts);
object_next(Bin,                Members, Nexts, Buf, Opts)                              -> ?ERROR(object_next, [Bin, Members, Nexts, Buf, Opts]).

-spec string(binary(), non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
string(<<Bin/binary>>, Start, Nexts, Buf, Opts) ->
    string(Bin, Bin, Start, Nexts, Buf, Opts).

-spec string(binary(), binary(), non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
string(<<$", Bin/binary>>, Base, Start, Nexts, Buf, Opts) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(Bin) - 1),
    case Start =:= byte_size(Buf) of
        true  -> next(Bin, Prefix, Nexts, Buf, Opts);
        false ->
            Buf2 = <<Buf/binary, Prefix/binary>>,
            next(Bin, binary:part(Buf2, Start, byte_size(Buf2) - Start), Nexts, Buf2, Opts)
    end;
string(<<$\\, B/binary>>, Base, Start, Nexts, Buf, Opts) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(B) - 1),
    case B of
        <<$", Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $">>, Opts);
        <<$/, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $/>>, Opts);
        <<$\\,Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\\>>, Opts);
        <<$b, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\b>>, Opts);
        <<$f, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\f>>, Opts);
        <<$n, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\n>>, Opts);
        <<$r, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\r>>, Opts);
        <<$t, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\t>>, Opts);
        <<$u, Bin/binary>> -> unicode_string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary>>, Opts);
        _                  -> ?ERROR(string, [<<$\\, B/binary>>, Base, Start, Nexts, Buf, Opts])
    end;
string(<<C, Bin/binary>>, Base, Start, Nexts, Buf, Opts) when 16#20 =< C ->
    string(Bin, Base, Start, Nexts, Buf, Opts).

-spec unicode_string(binary(), non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
unicode_string(<<N:4/binary, Bin/binary>>, Start, Nexts, Buf, Opts) ->
    case binary_to_integer(N, 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            %% surrogate pair
            case Bin of
                <<$\\, $u, N2:4/binary, Bin2/binary>> ->
                    case binary_to_integer(N2, 16) of
                        Low when 16#DC00 =< Low, Low =< 16#DFFF ->
                            Unicode = 16#10000 + (High - 16#D800) * 16#400 + (Low - 16#DC00),
                            string(Bin2, Start, Nexts, unicode_to_utf8(Unicode, Buf), Opts);
                        _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opts])
                    end;
                _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opts])
            end;
        Unicode when 16#DC00 =< Unicode, Unicode =< 16#DFFF ->  % second part of surrogate pair (without first part)
            ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opts]);
        Unicode -> 
            string(Bin, Start, Nexts, unicode_to_utf8(Unicode, Buf), Opts)
    end;
unicode_string(Bin, Start, Nexts, Buf, Opts) ->
    ?ERROR(unicode_string, [Bin, Start, Nexts, Buf, Opts]).

-spec unicode_to_utf8(0..1114111, binary()) -> binary().
unicode_to_utf8(Code, Buf) when Code < 16#80 ->
    <<Buf/binary, Code>>;
unicode_to_utf8(Code, Buf) when Code < 16#800 ->
    A = 2#11000000 bor (Code bsr 6),
    B = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B>>;
unicode_to_utf8(Code, Buf) when Code < 16#10000 ->
    A = 2#11100000 bor (Code bsr 12),
    B = 2#10000000 bor ((Code bsr 6) band 2#111111),
    C = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B, C>>;
unicode_to_utf8(Code, Buf) ->
    A = 2#11110000 bor (Code bsr 18),
    B = 2#10000000 bor ((Code bsr 12) band 2#111111),
    C = 2#10000000 bor ((Code bsr  6) band 2#111111),
    D = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B, C, D>>.

-spec number(binary(), [next()], binary(), decode_opt()) -> decode_result().
number(<<$-, Bin/binary>>, Nexts, Buf, Opts) -> number_integer_part(Bin, -1, Nexts, Buf, Opts);
number(<<Bin/binary>>,     Nexts, Buf, Opts) -> number_integer_part(Bin,  1, Nexts, Buf, Opts).

-spec number_integer_part(binary(), 1|-1, [next()], binary(), decode_opt()) -> decode_result().
number_integer_part(<<$0, Bin/binary>>, Sign, Nexts, Buf, Opts) ->
    number_fraction_part(Bin, Sign, 0, Nexts, Buf, Opts);
number_integer_part(<<C, Bin/binary>>, Sign, Nexts, Buf, Opts) when $1 =< C, C =< $9 ->
    number_integer_part_rest(Bin, C - $0, Sign, Nexts, Buf, Opts);
number_integer_part(Bin, Sign, Nexts, Buf, Opts) ->
    ?ERROR(number_integer_part, [Bin, Sign, Nexts, Buf, Opts]).

-spec number_integer_part_rest(binary(), non_neg_integer(), 1|-1, [next()], binary(), decode_opt()) -> decode_result().
number_integer_part_rest(<<C, Bin/binary>>, N, Sign, Nexts, Buf, Opts) when $0 =< C, C =< $9 ->
    number_integer_part_rest(Bin, N * 10 + C - $0, Sign, Nexts, Buf, Opts);
number_integer_part_rest(<<Bin/binary>>, N, Sign, Nexts, Buf, Opts) ->
    number_fraction_part(Bin, Sign, N, Nexts, Buf, Opts).

-spec number_fraction_part(binary(), 1|-1, non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
number_fraction_part(<<$., Bin/binary>>, Sign, Int, Nexts, Buf, Opts) ->
    number_fraction_part_rest(Bin, Sign, Int, 0, Nexts, Buf, Opts);
number_fraction_part(<<Bin/binary>>, Sign, Int, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, Sign * Int, 0, Nexts, Buf, Opts).

-spec number_fraction_part_rest(binary(), 1|-1, non_neg_integer(), non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
number_fraction_part_rest(<<C, Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf, Opts) when $0 =< C, C =< $9 ->
    number_fraction_part_rest(Bin, Sign, N * 10 + C - $0, DecimalOffset + 1, Nexts, Buf, Opts);
number_fraction_part_rest(<<Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf, Opts) when DecimalOffset > 0 ->
    number_exponation_part(Bin, Sign * N, DecimalOffset, Nexts, Buf, Opts);
number_fraction_part_rest(Bin, Sign, N, DecimalOffset, Nexts, Buf, Opts) ->
    ?ERROR(number_fraction_part_rest, [Bin, Sign, N, DecimalOffset, Nexts, Buf, Opts]).

-spec number_exponation_part(binary(), integer(), non_neg_integer(), [next()], binary(), decode_opt()) -> decode_result().
number_exponation_part(<<$e, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<$E, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<$e, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<$E, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<$e, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<$E, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opts);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opts) ->
    case DecimalOffset of
        0 -> next(Bin, N, Nexts, Buf, Opts);
        _ -> next(Bin, N / math:pow(10, DecimalOffset), Nexts, Buf, Opts)
    end.

-spec number_exponation_part(binary(), integer(), non_neg_integer(), 1|-1, non_neg_integer(), boolean(), [next()], binary(), decode_opt()) -> decode_result().
number_exponation_part(<<C, Bin/binary>>, N, DecimalOffset, ExpSign, Exp, _, Nexts, Buf, Opts) when $0 =< C, C =< $9 ->
    number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp * 10 + C - $0, false, Nexts, Buf, Opts);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, ExpSign, Exp, false, Nexts, Buf, Opts) ->
    Pos = ExpSign * Exp - DecimalOffset,
    next(Bin, N * math:pow(10, Pos), Nexts, Buf, Opts);
number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf, Opts) ->
    ?ERROR(number_exponation_part, [Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf, Opts]).

-spec parse_options([jsone:decode_option()]) -> decode_opt().
parse_options(Options) ->
    parse_option(Options, ?DECODE_OPT{}).

parse_option([], Opt) -> Opt;
parse_option([{format, eep18}|T], Opt) ->
    parse_option(T, Opt?DECODE_OPT{format=eep18});
parse_option([{format, proplist}|T], Opt) ->
    parse_option(T, Opt?DECODE_OPT{format=proplist}).
