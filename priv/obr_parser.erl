-file("priv/obr_parser.yrl", 0).
-module(obr_parser).
-file("priv/obr_parser.erl", 3).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("priv/obr_parser.yrl", 420).

-define(MType, '__struct__').

% list_tail({_, List}) -> List.
str_of(Obj) when is_tuple(Obj) -> tstr_of(Obj);
str_of(Obj) when is_map(Obj) -> mstr_of(Obj).

value_of(Obj) when is_tuple(Obj) -> tvalue_of(Obj);
value_of(Obj) when is_map(Obj) -> mvalue_of(Obj).

tstr_of(Token) ->
    % io:format('str_of ~w~n', [Token]),
    element(2, Token).

mstr_of(Map) ->
    % io:format('mstr_of ~w~n', [Map]),
    maps:get(str, Map, nil).

tvalue_of(Token) ->
    element(3, Token).

mvalue_of(Map) ->
    % io:format('value_of ~w~n', [Map]),
    maps:get(value, Map, nil).

-file("/home/sea/.asdf/installs/erlang/27.2.4/lib/parsetools-2.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef (YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef (YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef (YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [],
              {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Location}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location}).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer,_,N}) -> io_lib:write(N);
yecctoken2string1({float,_,F}) -> io_lib:write(F);
yecctoken2string1({char,_,C}) -> io_lib:write_char(C);
yecctoken2string1({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string,_,S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("priv/obr_parser.erl", 219).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function,  yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function,  yeccpars2_0/7}).
yeccpars2_0(S, 't_module', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function,  yeccpars2_3/7}).
yeccpars2_3(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(S, 't_import', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2_5(5, Cat, [4 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function,  yeccpars2_5/7}).
yeccpars2_5(S, 't_const', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccpars2_16(16, Cat, [5 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_module_importlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function,  yeccpars2_7/7}).
yeccpars2_7(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function,  yeccpars2_8/7}).
yeccpars2_8(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_implist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function,  yeccpars2_10/7}).
yeccpars2_10(S, 't_assign', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_import(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function,  yeccpars2_11/7}).
yeccpars2_11(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function,  yeccpars2_12/7}).
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_import(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_importlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_14: see yeccpars2_7

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function,  yeccpars2_15/7}).
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_implist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function,  yeccpars2_16/7}).
yeccpars2_16(S, 't_type', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2_203(203, Cat, [16 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function,  yeccpars2_17/7}).
yeccpars2_17(S, 't_begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccpars2_111(111, Cat, [17 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccpars2_21(21, Cat, [18 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, 't_equ', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function,  yeccpars2_20/7}).
yeccpars2_20(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function,  yeccpars2_21/7}).
yeccpars2_21(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_declarationsequence_const(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, 't_mul', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_identdef(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_identdef(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_const_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_const_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_27(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_27(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function,  yeccpars2_27/7}).
yeccpars2_cont_27(S, 'character', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 'real', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_false', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_nil', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_tilda', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(S, 't_true', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_28/7}).
-compile({nowarn_unused_function,  yeccpars2_28/7}).
yeccpars2_28(S, 't_and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 't_div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 't_divide', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 't_mod', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 't_mul', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_term(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function,  yeccpars2_29/7}).
yeccpars2_29(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 't_or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_simpleexpression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(S, 't_equ', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_is', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_less', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_lesseq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_more', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_moreeq', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 't_sharp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_expression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).
-compile({nowarn_unused_function,  yeccpars2_31/7}).
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function,  yeccpars2_32/7}).
yeccpars2_32(S, 't_arrow', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 't_dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 't_lbrack', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_designator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function,  yeccpars2_33/7}).
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
-compile({nowarn_unused_function,  yeccpars2_34/7}).
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_termlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_constexpression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function,  yeccpars2_36/7}).
yeccpars2_36(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).
-compile({nowarn_unused_function,  yeccpars2_37/7}).
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_constantdeclaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).
-compile({nowarn_unused_function,  yeccpars2_38/7}).
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_number(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function,  yeccpars2_39/7}).
yeccpars2_39(S, 't_dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_qualident(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_number(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_number(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 't_rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_27(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_45: see yeccpars2_27

yeccpars2_46(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_27(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_48: see yeccpars2_46

%% yeccpars2_49: see yeccpars2_46

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_factor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function,  yeccpars2_51/7}).
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_factor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
-compile({nowarn_unused_function,  yeccpars2_52/7}).
yeccpars2_52(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, 't_or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_simpleexpression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_53: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function,  yeccpars2_54/7}).
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_addoperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function,  yeccpars2_55/7}).
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_addoperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_56/7}).
-compile({nowarn_unused_function,  yeccpars2_56/7}).
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_addoperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_57/7}).
-compile({nowarn_unused_function,  yeccpars2_57/7}).
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_simpleexpression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_58/7}).
-compile({nowarn_unused_function,  yeccpars2_58/7}).
yeccpars2_58(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, 't_or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_simpleexpression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function,  yeccpars2_60/7}).
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_simpleexpression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_61/7}).
-compile({nowarn_unused_function,  yeccpars2_61/7}).
yeccpars2_61(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function,  yeccpars2_62/7}).
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_factor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_63/7}).
-compile({nowarn_unused_function,  yeccpars2_63/7}).
yeccpars2_63(S, 't_rbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function,  yeccpars2_64/7}).
yeccpars2_64(S, 't_ddot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_element(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
-compile({nowarn_unused_function,  yeccpars2_65/7}).
yeccpars2_65(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_setlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).
-compile({nowarn_unused_function,  yeccpars2_66/7}).
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_set(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_67: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function,  yeccpars2_68/7}).
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_setlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_69: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_70/7}).
-compile({nowarn_unused_function,  yeccpars2_70/7}).
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function,  yeccpars2_71/7}).
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_set(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function,  yeccpars2_72/7}).
yeccpars2_72(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function,  yeccpars2_73/7}).
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_qualident(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function,  yeccpars2_74/7}).
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_factor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_75(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_27(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function,  yeccpars2_76/7}).
yeccpars2_76(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_exlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function,  yeccpars2_77/7}).
yeccpars2_77(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_78/7}).
-compile({nowarn_unused_function,  yeccpars2_78/7}).
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_explist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function,  yeccpars2_79/7}).
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_actualparameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function,  yeccpars2_80/7}).
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_actualparameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_81: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_82/7}).
-compile({nowarn_unused_function,  yeccpars2_82/7}).
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_exlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function,  yeccpars2_83/7}).
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_selector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function,  yeccpars2_84/7}).
yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_designator(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_85/7}).
-compile({nowarn_unused_function,  yeccpars2_85/7}).
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_selector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
-compile({nowarn_unused_function,  yeccpars2_86/7}).
yeccpars2_86(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_87: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_88/7}).
-compile({nowarn_unused_function,  yeccpars2_88/7}).
yeccpars2_88(S, 't_rbrack', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_89/7}).
-compile({nowarn_unused_function,  yeccpars2_89/7}).
yeccpars2_89(S, 't_lbrack', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_selectorarr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_90/7}).
-compile({nowarn_unused_function,  yeccpars2_90/7}).
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_selectorarr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function,  yeccpars2_91/7}).
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_selector(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_92: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_93/7}).
-compile({nowarn_unused_function,  yeccpars2_93/7}).
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function,  yeccpars2_94/7}).
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_95/7}).
-compile({nowarn_unused_function,  yeccpars2_95/7}).
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function,  yeccpars2_96/7}).
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function,  yeccpars2_97/7}).
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_98/7}).
-compile({nowarn_unused_function,  yeccpars2_98/7}).
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_99/7}).
-compile({nowarn_unused_function,  yeccpars2_99/7}).
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function,  yeccpars2_100/7}).
yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_relation(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_101/7}).
-compile({nowarn_unused_function,  yeccpars2_101/7}).
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_102: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_103/7}).
-compile({nowarn_unused_function,  yeccpars2_103/7}).
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_simpleexpression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_104: see yeccpars2_46

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function,  yeccpars2_105/7}).
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_muloperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function,  yeccpars2_106/7}).
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_muloperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function,  yeccpars2_107/7}).
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_muloperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function,  yeccpars2_108/7}).
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_muloperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function,  yeccpars2_109/7}).
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_muloperator(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
-compile({nowarn_unused_function,  yeccpars2_110/7}).
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_termlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function,  yeccpars2_111/7}).
yeccpars2_111(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_112/7}).
-compile({nowarn_unused_function,  yeccpars2_112/7}).
yeccpars2_112(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_assert', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_for', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_repeat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, 't_while', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_113/7}).
-compile({nowarn_unused_function,  yeccpars2_113/7}).
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_114/7}).
-compile({nowarn_unused_function,  yeccpars2_114/7}).
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_module_begin(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function,  yeccpars2_115/7}).
yeccpars2_115(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_statementsequence(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
-compile({nowarn_unused_function,  yeccpars2_116/7}).
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_117/7}).
-compile({nowarn_unused_function,  yeccpars2_117/7}).
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_118/7}).
-compile({nowarn_unused_function,  yeccpars2_118/7}).
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_119/7}).
-compile({nowarn_unused_function,  yeccpars2_119/7}).
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_120/7}).
-compile({nowarn_unused_function,  yeccpars2_120/7}).
yeccpars2_120(S, 't_assign', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_procedurecall(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_121/7}).
-compile({nowarn_unused_function,  yeccpars2_121/7}).
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function,  yeccpars2_122/7}).
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_123/7}).
-compile({nowarn_unused_function,  yeccpars2_123/7}).
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_statement(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_124/7}).
-compile({nowarn_unused_function,  yeccpars2_124/7}).
yeccpars2_124(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_125: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_126/7}).
-compile({nowarn_unused_function,  yeccpars2_126/7}).
yeccpars2_126(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_127: see yeccpars2_27

%% yeccpars2_128: see yeccpars2_112

%% yeccpars2_129: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_130/7}).
-compile({nowarn_unused_function,  yeccpars2_130/7}).
yeccpars2_130(S, 't_do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_131: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_132/7}).
-compile({nowarn_unused_function,  yeccpars2_132/7}).
yeccpars2_132(S, 't_elsif', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccpars2_133(133, Cat, [132 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function,  yeccpars2_133/7}).
yeccpars2_133(S, 't_elsif', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_134: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_135/7}).
-compile({nowarn_unused_function,  yeccpars2_135/7}).
yeccpars2_135(S, 't_do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_136: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function,  yeccpars2_137/7}).
yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_elsifdosec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_138: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function,  yeccpars2_139/7}).
yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_whilestatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function,  yeccpars2_140/7}).
yeccpars2_140(S, 't_do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_141: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_142/7}).
-compile({nowarn_unused_function,  yeccpars2_142/7}).
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_elsifdosec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_143/7}).
-compile({nowarn_unused_function,  yeccpars2_143/7}).
yeccpars2_143(S, 't_until', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_144: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_145/7}).
-compile({nowarn_unused_function,  yeccpars2_145/7}).
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_repeatstatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function,  yeccpars2_146/7}).
yeccpars2_146(S, 't_then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_147: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function,  yeccpars2_148/7}).
yeccpars2_148(S, 't_elsif', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_149/7}).
-compile({nowarn_unused_function,  yeccpars2_149/7}).
yeccpars2_149(S, 't_else', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_150: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_151/7}).
-compile({nowarn_unused_function,  yeccpars2_151/7}).
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_ifstatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function,  yeccpars2_152/7}).
yeccpars2_152(S, 't_then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_153: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function,  yeccpars2_154/7}).
yeccpars2_154(S, 't_elsif', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_elsifseq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function,  yeccpars2_155/7}).
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_elsifseq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_156/7}).
-compile({nowarn_unused_function,  yeccpars2_156/7}).
yeccpars2_156(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_157: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_158/7}).
-compile({nowarn_unused_function,  yeccpars2_158/7}).
yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_ifstatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function,  yeccpars2_159/7}).
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_ifelse(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
-compile({nowarn_unused_function,  yeccpars2_160/7}).
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_ifstatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
-compile({nowarn_unused_function,  yeccpars2_161/7}).
yeccpars2_161(S, 't_assign', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_162: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_163/7}).
-compile({nowarn_unused_function,  yeccpars2_163/7}).
yeccpars2_163(S, 't_to', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_164: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_165/7}).
-compile({nowarn_unused_function,  yeccpars2_165/7}).
yeccpars2_165(S, 't_by', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccpars2_166(166, Cat, [165 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
-compile({nowarn_unused_function,  yeccpars2_166/7}).
yeccpars2_166(S, 't_do', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_167: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_168/7}).
-compile({nowarn_unused_function,  yeccpars2_168/7}).
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_forby(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_169: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_170/7}).
-compile({nowarn_unused_function,  yeccpars2_170/7}).
yeccpars2_170(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_171/7}).
-compile({nowarn_unused_function,  yeccpars2_171/7}).
yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_forstatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_172/7}).
-compile({nowarn_unused_function,  yeccpars2_172/7}).
yeccpars2_172(S, 't_of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_173/7}).
-compile({nowarn_unused_function,  yeccpars2_173/7}).
yeccpars2_173(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccpars2_176(_S, Cat, [173 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_174/7}).
-compile({nowarn_unused_function,  yeccpars2_174/7}).
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_label(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_175/7}).
-compile({nowarn_unused_function,  yeccpars2_175/7}).
yeccpars2_175(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, 't_vline', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_176/7}).
-compile({nowarn_unused_function,  yeccpars2_176/7}).
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_ntcaselist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_177/7}).
-compile({nowarn_unused_function,  yeccpars2_177/7}).
yeccpars2_177(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccgoto_cllist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_178/7}).
-compile({nowarn_unused_function,  yeccpars2_178/7}).
yeccpars2_178(S, 't_ddot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_labelrange(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_179/7}).
-compile({nowarn_unused_function,  yeccpars2_179/7}).
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccgoto_caselabellist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_180/7}).
-compile({nowarn_unused_function,  yeccpars2_180/7}).
yeccpars2_180(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_181/7}).
-compile({nowarn_unused_function,  yeccpars2_181/7}).
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_label(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_182/7}).
-compile({nowarn_unused_function,  yeccpars2_182/7}).
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_label(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_184/7}).
-compile({nowarn_unused_function,  yeccpars2_184/7}).
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_ntcase(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_185/7}).
-compile({nowarn_unused_function,  yeccpars2_185/7}).
yeccpars2_185(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_186/7}).
-compile({nowarn_unused_function,  yeccpars2_186/7}).
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_labelrange(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_187: see yeccpars2_185

-dialyzer({nowarn_function, yeccpars2_188/7}).
-compile({nowarn_unused_function,  yeccpars2_188/7}).
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_cllist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_189/7}).
-compile({nowarn_unused_function,  yeccpars2_189/7}).
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_189_(Stack),
 yeccgoto_casestatement(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_190/7}).
-compile({nowarn_unused_function,  yeccpars2_190/7}).
yeccpars2_190(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 yeccpars2_191(_S, Cat, [190 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_191/7}).
-compile({nowarn_unused_function,  yeccpars2_191/7}).
yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_ntcaselist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_192: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_193/7}).
-compile({nowarn_unused_function,  yeccpars2_193/7}).
yeccpars2_193(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_194/7}).
-compile({nowarn_unused_function,  yeccpars2_194/7}).
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_assertdeclaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_195/7}).
-compile({nowarn_unused_function,  yeccpars2_195/7}).
yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_procedurecall(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_196: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_197/7}).
-compile({nowarn_unused_function,  yeccpars2_197/7}).
yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_198/7}).
-compile({nowarn_unused_function,  yeccpars2_198/7}).
yeccpars2_198(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_assert', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_for', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_repeat', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(S, 't_while', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_statementsequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_199/7}).
-compile({nowarn_unused_function,  yeccpars2_199/7}).
yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_statementsequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_200/7}).
-compile({nowarn_unused_function,  yeccpars2_200/7}).
yeccpars2_200(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_201/7}).
-compile({nowarn_unused_function,  yeccpars2_201/7}).
yeccpars2_201(S, 't_dot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_202/7}).
-compile({nowarn_unused_function,  yeccpars2_202/7}).
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_module(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_203/7}).
-compile({nowarn_unused_function,  yeccpars2_203/7}).
yeccpars2_203(S, 't_var', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccpars2_288(288, Cat, [203 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_204/7}).
-compile({nowarn_unused_function,  yeccpars2_204/7}).
yeccpars2_204(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccpars2_206(206, Cat, [204 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_205/7}).
-compile({nowarn_unused_function,  yeccpars2_205/7}).
yeccpars2_205(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_206/7}).
-compile({nowarn_unused_function,  yeccpars2_206/7}).
yeccpars2_206(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_declarationsequence_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_207/7}).
-compile({nowarn_unused_function,  yeccpars2_207/7}).
yeccpars2_207(S, 't_equ', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_208(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 't_array', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 't_pointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 't_procedure', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 't_record', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_208(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_208/7}).
-compile({nowarn_unused_function,  yeccpars2_208/7}).
yeccpars2_cont_208(S, 't_boolean', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_208(S, 't_byte', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_208(S, 't_char', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_208(S, 't_integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_208(S, 't_real', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_208(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_209/7}).
-compile({nowarn_unused_function,  yeccpars2_209/7}).
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_typedeclaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_210/7}).
-compile({nowarn_unused_function,  yeccpars2_210/7}).
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_211/7}).
-compile({nowarn_unused_function,  yeccpars2_211/7}).
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_212/7}).
-compile({nowarn_unused_function,  yeccpars2_212/7}).
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_structype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_213/7}).
-compile({nowarn_unused_function,  yeccpars2_213/7}).
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_214/7}).
-compile({nowarn_unused_function,  yeccpars2_214/7}).
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_structype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_215/7}).
-compile({nowarn_unused_function,  yeccpars2_215/7}).
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_structype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_216: see yeccpars2_208

-dialyzer({nowarn_function, yeccpars2_217/7}).
-compile({nowarn_unused_function,  yeccpars2_217/7}).
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_structype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_218/7}).
-compile({nowarn_unused_function,  yeccpars2_218/7}).
yeccpars2_218(S, 'character', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'real', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_false', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_nil', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_tilda', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, 't_true', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_218_(Stack),
 yeccpars2_277(277, Cat, [218 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_219/7}).
-compile({nowarn_unused_function,  yeccpars2_219/7}).
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_simpletype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_220/7}).
-compile({nowarn_unused_function,  yeccpars2_220/7}).
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_simpletype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_221/7}).
-compile({nowarn_unused_function,  yeccpars2_221/7}).
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_simpletype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_222/7}).
-compile({nowarn_unused_function,  yeccpars2_222/7}).
yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_simpletype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_223/7}).
-compile({nowarn_unused_function,  yeccpars2_223/7}).
yeccpars2_223(S, 't_to', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_224/7}).
-compile({nowarn_unused_function,  yeccpars2_224/7}).
yeccpars2_224(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_proceduretype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
-compile({nowarn_unused_function,  yeccpars2_225/7}).
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_simpletype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_226/7}).
-compile({nowarn_unused_function,  yeccpars2_226/7}).
yeccpars2_226(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_227/7}).
-compile({nowarn_unused_function,  yeccpars2_227/7}).
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_identlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_228/7}).
-compile({nowarn_unused_function,  yeccpars2_228/7}).
yeccpars2_228(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_229/7}).
-compile({nowarn_unused_function,  yeccpars2_229/7}).
yeccpars2_229(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_idlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_230/7}).
-compile({nowarn_unused_function,  yeccpars2_230/7}).
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_fieldlistsequence(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
-compile({nowarn_unused_function,  yeccpars2_231/7}).
yeccpars2_231(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_232/7}).
-compile({nowarn_unused_function,  yeccpars2_232/7}).
yeccpars2_232(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_fieldlistsequence_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_233/7}).
-compile({nowarn_unused_function,  yeccpars2_233/7}).
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_recordtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_234/7}).
-compile({nowarn_unused_function,  yeccpars2_234/7}).
yeccpars2_234(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_235/7}).
-compile({nowarn_unused_function,  yeccpars2_235/7}).
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_basetype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_236/7}).
-compile({nowarn_unused_function,  yeccpars2_236/7}).
yeccpars2_236(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_237/7}).
-compile({nowarn_unused_function,  yeccpars2_237/7}).
yeccpars2_237(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_238/7}).
-compile({nowarn_unused_function,  yeccpars2_238/7}).
yeccpars2_238(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_239/7}).
-compile({nowarn_unused_function,  yeccpars2_239/7}).
yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_recordtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_240/7}).
-compile({nowarn_unused_function,  yeccpars2_240/7}).
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_recordtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_241/7}).
-compile({nowarn_unused_function,  yeccpars2_241/7}).
yeccpars2_241(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_fieldlistsequence_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_242/7}).
-compile({nowarn_unused_function,  yeccpars2_242/7}).
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_fieldlistsequence_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_243/7}).
-compile({nowarn_unused_function,  yeccpars2_243/7}).
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_recordtype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_244/7}).
-compile({nowarn_unused_function,  yeccpars2_244/7}).
yeccpars2_244(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_245/7}).
-compile({nowarn_unused_function,  yeccpars2_245/7}).
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_idlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_246: see yeccpars2_208

-dialyzer({nowarn_function, yeccpars2_247/7}).
-compile({nowarn_unused_function,  yeccpars2_247/7}).
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_fieldlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_248/7}).
-compile({nowarn_unused_function,  yeccpars2_248/7}).
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_proceduretype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_249/7}).
-compile({nowarn_unused_function,  yeccpars2_249/7}).
yeccpars2_249(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, 't_var', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_250/7}).
-compile({nowarn_unused_function,  yeccpars2_250/7}).
yeccpars2_250(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_251/7}).
-compile({nowarn_unused_function,  yeccpars2_251/7}).
yeccpars2_251(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 yeccgoto_fpseclist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_252/7}).
-compile({nowarn_unused_function,  yeccpars2_252/7}).
yeccpars2_252(S, 't_rpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_253/7}).
-compile({nowarn_unused_function,  yeccpars2_253/7}).
yeccpars2_253(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_idlist2(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_254/7}).
-compile({nowarn_unused_function,  yeccpars2_254/7}).
yeccpars2_254(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_255/7}).
-compile({nowarn_unused_function,  yeccpars2_255/7}).
yeccpars2_255(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_256/7}).
-compile({nowarn_unused_function,  yeccpars2_256/7}).
yeccpars2_256(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_256(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_257(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, 't_array', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_208(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_258/7}).
-compile({nowarn_unused_function,  yeccpars2_258/7}).
yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_formaltype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_259/7}).
-compile({nowarn_unused_function,  yeccpars2_259/7}).
yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_formaltype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_260/7}).
-compile({nowarn_unused_function,  yeccpars2_260/7}).
yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_fpsection(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_261/7}).
-compile({nowarn_unused_function,  yeccpars2_261/7}).
yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_formaltype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_262(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_208(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_263/7}).
-compile({nowarn_unused_function,  yeccpars2_263/7}).
yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_263_(Stack),
 yeccgoto_proctype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_264/7}).
-compile({nowarn_unused_function,  yeccpars2_264/7}).
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccgoto_proctype(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_265/7}).
-compile({nowarn_unused_function,  yeccpars2_265/7}).
yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_265_(Stack),
 yeccgoto_formalparameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_266: see yeccpars2_255

-dialyzer({nowarn_function, yeccpars2_267/7}).
-compile({nowarn_unused_function,  yeccpars2_267/7}).
yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_267_(Stack),
 yeccgoto_idlist2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_268/7}).
-compile({nowarn_unused_function,  yeccpars2_268/7}).
yeccpars2_268(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_formalparameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_269: see yeccpars2_262

-dialyzer({nowarn_function, yeccpars2_270/7}).
-compile({nowarn_unused_function,  yeccpars2_270/7}).
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_formalparameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_271/7}).
-compile({nowarn_unused_function,  yeccpars2_271/7}).
yeccpars2_271(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, 't_var', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_272/7}).
-compile({nowarn_unused_function,  yeccpars2_272/7}).
yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_272_(Stack),
 yeccgoto_fpseclist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_273: see yeccpars2_257

-dialyzer({nowarn_function, yeccpars2_274/7}).
-compile({nowarn_unused_function,  yeccpars2_274/7}).
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_fpsection(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_275: see yeccpars2_208

-dialyzer({nowarn_function, yeccpars2_276/7}).
-compile({nowarn_unused_function,  yeccpars2_276/7}).
yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_276_(Stack),
 yeccgoto_pointertype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_277/7}).
-compile({nowarn_unused_function,  yeccpars2_277/7}).
yeccpars2_277(S, 't_of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_278/7}).
-compile({nowarn_unused_function,  yeccpars2_278/7}).
yeccpars2_278(S, 't_comma', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_278_(Stack),
 yeccgoto_lenlist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_279/7}).
-compile({nowarn_unused_function,  yeccpars2_279/7}).
yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_length(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_280/7}).
-compile({nowarn_unused_function,  yeccpars2_280/7}).
yeccpars2_280(S, 'character', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'real', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'string', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_false', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_lbrace', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_minus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_nil', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_plus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_tilda', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 't_true', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 yeccpars2_281(_S, Cat, [280 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_281/7}).
-compile({nowarn_unused_function,  yeccpars2_281/7}).
yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_lenlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_282/7}).
-compile({nowarn_unused_function,  yeccpars2_282/7}).
yeccpars2_282(S, 't_array', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_arraytypelist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_283/7}).
-compile({nowarn_unused_function,  yeccpars2_283/7}).
yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_arraytypelist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_284/7}).
-compile({nowarn_unused_function,  yeccpars2_284/7}).
yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_arraytype(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_285/7}).
-compile({nowarn_unused_function,  yeccpars2_285/7}).
yeccpars2_285(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_286/7}).
-compile({nowarn_unused_function,  yeccpars2_286/7}).
yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_type_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_287/7}).
-compile({nowarn_unused_function,  yeccpars2_287/7}).
yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_287_(Stack),
 yeccgoto_type_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_288/7}).
-compile({nowarn_unused_function,  yeccpars2_288/7}).
yeccpars2_288(S, 't_procedure', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_(Stack),
 yeccpars2_300(300, Cat, [288 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_289/7}).
-compile({nowarn_unused_function,  yeccpars2_289/7}).
yeccpars2_289(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_289_(Stack),
 yeccpars2_291(291, Cat, [289 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_290/7}).
-compile({nowarn_unused_function,  yeccpars2_290/7}).
yeccpars2_290(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_290(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_291/7}).
-compile({nowarn_unused_function,  yeccpars2_291/7}).
yeccpars2_291(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_declarationsequence_var(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_292/7}).
-compile({nowarn_unused_function,  yeccpars2_292/7}).
yeccpars2_292(S, 't_colon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_292(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_293: see yeccpars2_208

-dialyzer({nowarn_function, yeccpars2_294/7}).
-compile({nowarn_unused_function,  yeccpars2_294/7}).
yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_variabledeclaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_295/7}).
-compile({nowarn_unused_function,  yeccpars2_295/7}).
yeccpars2_295(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_296/7}).
-compile({nowarn_unused_function,  yeccpars2_296/7}).
yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_296_(Stack),
 yeccgoto_var_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_297/7}).
-compile({nowarn_unused_function,  yeccpars2_297/7}).
yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_297_(Stack),
 yeccgoto_var_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_298/7}).
-compile({nowarn_unused_function,  yeccpars2_298/7}).
yeccpars2_298(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_299/7}).
-compile({nowarn_unused_function,  yeccpars2_299/7}).
yeccpars2_299(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_300/7}).
-compile({nowarn_unused_function,  yeccpars2_300/7}).
yeccpars2_300(S, 't_procedure', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_300_(Stack),
 yeccgoto_declarationsequence(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_301: see yeccpars2_244

-dialyzer({nowarn_function, yeccpars2_302/7}).
-compile({nowarn_unused_function,  yeccpars2_302/7}).
yeccpars2_302(S, 't_lpar', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_302_(Stack),
 yeccgoto_procedureheading(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_303/7}).
-compile({nowarn_unused_function,  yeccpars2_303/7}).
yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_303_(Stack),
 yeccgoto_procedureheading(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_304/7}).
-compile({nowarn_unused_function,  yeccpars2_304/7}).
yeccpars2_304(S, 't_semicolon', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_305/7}).
-compile({nowarn_unused_function,  yeccpars2_305/7}).
yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_305_(Stack),
 yeccgoto_proc_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_306/7}).
-compile({nowarn_unused_function,  yeccpars2_306/7}).
yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_proc_decl_li(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_307/7}).
-compile({nowarn_unused_function,  yeccpars2_307/7}).
yeccpars2_307(S, 't_const', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_307_(Stack),
 yeccpars2_16(16, Cat, [307 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_308/7}).
-compile({nowarn_unused_function,  yeccpars2_308/7}).
yeccpars2_308(S, 'ident', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 317, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_309/7}).
-compile({nowarn_unused_function,  yeccpars2_309/7}).
yeccpars2_309(S, 't_begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_309_(Stack),
 yeccpars2_310(310, Cat, [309 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_310/7}).
-compile({nowarn_unused_function,  yeccpars2_310/7}).
yeccpars2_310(S, 't_return', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_310_(Stack),
 yeccpars2_313(313, Cat, [310 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_311: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_312/7}).
-compile({nowarn_unused_function,  yeccpars2_312/7}).
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_312_(Stack),
 yeccgoto_procedurebody_stat_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_313/7}).
-compile({nowarn_unused_function,  yeccpars2_313/7}).
yeccpars2_313(S, 't_end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_314: see yeccpars2_27

-dialyzer({nowarn_function, yeccpars2_315/7}).
-compile({nowarn_unused_function,  yeccpars2_315/7}).
yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_315_(Stack),
 yeccgoto_procedurebody_ret_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_316/7}).
-compile({nowarn_unused_function,  yeccpars2_316/7}).
yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_316_(Stack),
 yeccgoto_procedurebody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_317/7}).
-compile({nowarn_unused_function,  yeccpars2_317/7}).
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_proceduredeclaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_actualparameters/7}).
-compile({nowarn_unused_function,  yeccgoto_actualparameters/7}).
yeccgoto_actualparameters(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actualparameters(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_addoperator/7}).
-compile({nowarn_unused_function,  yeccgoto_addoperator/7}).
yeccgoto_addoperator(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addoperator(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_addoperator(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(59, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arraytype/7}).
-compile({nowarn_unused_function,  yeccgoto_arraytype/7}).
yeccgoto_arraytype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(273=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_arraytypelist/7}).
-compile({nowarn_unused_function,  yeccgoto_arraytypelist/7}).
yeccgoto_arraytypelist(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(216, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(246, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(273, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(275, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(282=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_arraytypelist(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(216, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assertdeclaration/7}).
-compile({nowarn_unused_function,  yeccgoto_assertdeclaration/7}).
yeccgoto_assertdeclaration(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assertdeclaration(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assignment/7}).
-compile({nowarn_unused_function,  yeccgoto_assignment/7}).
yeccgoto_assignment(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assignment(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_basetype/7}).
-compile({nowarn_unused_function,  yeccgoto_basetype/7}).
yeccgoto_basetype(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_caselabellist/7}).
-compile({nowarn_unused_function,  yeccgoto_caselabellist/7}).
yeccgoto_caselabellist(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_caselabellist(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_casestatement/7}).
-compile({nowarn_unused_function,  yeccgoto_casestatement/7}).
yeccgoto_casestatement(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_casestatement(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_cllist/7}).
-compile({nowarn_unused_function,  yeccgoto_cllist/7}).
yeccgoto_cllist(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cllist(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cllist(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_const_decl_li/7}).
-compile({nowarn_unused_function,  yeccgoto_const_decl_li/7}).
yeccgoto_const_decl_li(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constantdeclaration/7}).
-compile({nowarn_unused_function,  yeccgoto_constantdeclaration/7}).
yeccgoto_constantdeclaration(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constantdeclaration(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_constexpression/7}).
-compile({nowarn_unused_function,  yeccgoto_constexpression/7}).
yeccgoto_constexpression(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constexpression(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constexpression(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constexpression(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarationsequence/7}).
-compile({nowarn_unused_function,  yeccgoto_declarationsequence/7}).
yeccgoto_declarationsequence(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarationsequence(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(309, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarationsequence_const/7}).
-compile({nowarn_unused_function,  yeccgoto_declarationsequence_const/7}).
yeccgoto_declarationsequence_const(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_declarationsequence_const(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarationsequence_type/7}).
-compile({nowarn_unused_function,  yeccgoto_declarationsequence_type/7}).
yeccgoto_declarationsequence_type(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarationsequence_var/7}).
-compile({nowarn_unused_function,  yeccgoto_declarationsequence_var/7}).
yeccgoto_declarationsequence_var(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(288, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_designator/7}).
-compile({nowarn_unused_function,  yeccgoto_designator/7}).
yeccgoto_designator(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_designator(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_element/7}).
-compile({nowarn_unused_function,  yeccgoto_element/7}).
yeccgoto_element(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elsifdosec/7}).
-compile({nowarn_unused_function,  yeccgoto_elsifdosec/7}).
yeccgoto_elsifdosec(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(133, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elsifseq/7}).
-compile({nowarn_unused_function,  yeccgoto_elsifseq/7}).
yeccgoto_elsifseq(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elsifseq(154=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_exlist/7}).
-compile({nowarn_unused_function,  yeccgoto_exlist/7}).
yeccgoto_exlist(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exlist(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exlist(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_explist/7}).
-compile({nowarn_unused_function,  yeccgoto_explist/7}).
yeccgoto_explist(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_explist(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
-compile({nowarn_unused_function,  yeccgoto_expression/7}).
yeccgoto_expression(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(172, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_factor/7}).
-compile({nowarn_unused_function,  yeccgoto_factor/7}).
yeccgoto_factor(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_factor(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fieldlist/7}).
-compile({nowarn_unused_function,  yeccgoto_fieldlist/7}).
yeccgoto_fieldlist(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldlist(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldlist(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fieldlistsequence/7}).
-compile({nowarn_unused_function,  yeccgoto_fieldlistsequence/7}).
yeccgoto_fieldlistsequence(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldlistsequence(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fieldlistsequence_list/7}).
-compile({nowarn_unused_function,  yeccgoto_fieldlistsequence_list/7}).
yeccgoto_fieldlistsequence_list(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldlistsequence_list(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fieldlistsequence_list(241=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_forby/7}).
-compile({nowarn_unused_function,  yeccgoto_forby/7}).
yeccgoto_forby(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(166, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_formalparameters/7}).
-compile({nowarn_unused_function,  yeccgoto_formalparameters/7}).
yeccgoto_formalparameters(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_formalparameters(302=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_formaltype/7}).
-compile({nowarn_unused_function,  yeccgoto_formaltype/7}).
yeccgoto_formaltype(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_formaltype(273=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_forstatement/7}).
-compile({nowarn_unused_function,  yeccgoto_forstatement/7}).
yeccgoto_forstatement(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_forstatement(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fpseclist/7}).
-compile({nowarn_unused_function,  yeccgoto_fpseclist/7}).
yeccgoto_fpseclist(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fpseclist(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_fpsection/7}).
-compile({nowarn_unused_function,  yeccgoto_fpsection/7}).
yeccgoto_fpsection(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(251, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fpsection(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(251, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_identdef/7}).
-compile({nowarn_unused_function,  yeccgoto_identdef/7}).
yeccgoto_identdef(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(206, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identdef(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(302, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_identlist/7}).
-compile({nowarn_unused_function,  yeccgoto_identlist/7}).
yeccgoto_identlist(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identlist(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identlist(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identlist(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(292, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identlist(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(292, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_idlist/7}).
-compile({nowarn_unused_function,  yeccgoto_idlist/7}).
yeccgoto_idlist(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(241=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist(291=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_idlist2/7}).
-compile({nowarn_unused_function,  yeccgoto_idlist2/7}).
yeccgoto_idlist2(249, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(250, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist2(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(256, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist2(266=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_idlist2(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(250, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_ifelse/7}).
-compile({nowarn_unused_function,  yeccgoto_ifelse/7}).
yeccgoto_ifelse(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_ifstatement/7}).
-compile({nowarn_unused_function,  yeccgoto_ifstatement/7}).
yeccgoto_ifstatement(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ifstatement(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_implist/7}).
-compile({nowarn_unused_function,  yeccgoto_implist/7}).
yeccgoto_implist(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_implist(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_import/7}).
-compile({nowarn_unused_function,  yeccgoto_import/7}).
yeccgoto_import(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_importlist/7}).
-compile({nowarn_unused_function,  yeccgoto_importlist/7}).
yeccgoto_importlist(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_label/7}).
-compile({nowarn_unused_function,  yeccgoto_label/7}).
yeccgoto_label(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_label(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_label(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_label(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_labelrange/7}).
-compile({nowarn_unused_function,  yeccgoto_labelrange/7}).
yeccgoto_labelrange(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_labelrange(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_labelrange(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_length/7}).
-compile({nowarn_unused_function,  yeccgoto_length/7}).
yeccgoto_length(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(278, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_length(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(278, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_lenlist/7}).
-compile({nowarn_unused_function,  yeccgoto_lenlist/7}).
yeccgoto_lenlist(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(277, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lenlist(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module/7}).
-compile({nowarn_unused_function,  yeccgoto_module/7}).
yeccgoto_module(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_begin/7}).
-compile({nowarn_unused_function,  yeccgoto_module_begin/7}).
yeccgoto_module_begin(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_importlist/7}).
-compile({nowarn_unused_function,  yeccgoto_module_importlist/7}).
yeccgoto_module_importlist(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_muloperator/7}).
-compile({nowarn_unused_function,  yeccgoto_muloperator/7}).
yeccgoto_muloperator(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(104, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_ntcase/7}).
-compile({nowarn_unused_function,  yeccgoto_ntcase/7}).
yeccgoto_ntcase(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ntcase(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_ntcaselist/7}).
-compile({nowarn_unused_function,  yeccgoto_ntcaselist/7}).
yeccgoto_ntcaselist(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_number/7}).
-compile({nowarn_unused_function,  yeccgoto_number/7}).
yeccgoto_number(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pointertype/7}).
-compile({nowarn_unused_function,  yeccgoto_pointertype/7}).
yeccgoto_pointertype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pointertype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pointertype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pointertype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pointertype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_proc_decl_li/7}).
-compile({nowarn_unused_function,  yeccgoto_proc_decl_li/7}).
yeccgoto_proc_decl_li(288, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(300, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_procedurebody/7}).
-compile({nowarn_unused_function,  yeccgoto_procedurebody/7}).
yeccgoto_procedurebody(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_procedurebody_ret_exp/7}).
-compile({nowarn_unused_function,  yeccgoto_procedurebody_ret_exp/7}).
yeccgoto_procedurebody_ret_exp(310, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_procedurebody_stat_seq/7}).
-compile({nowarn_unused_function,  yeccgoto_procedurebody_stat_seq/7}).
yeccgoto_procedurebody_stat_seq(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(310, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_procedurecall/7}).
-compile({nowarn_unused_function,  yeccgoto_procedurecall/7}).
yeccgoto_procedurecall(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedurecall(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_proceduredeclaration/7}).
-compile({nowarn_unused_function,  yeccgoto_proceduredeclaration/7}).
yeccgoto_proceduredeclaration(288, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proceduredeclaration(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(304, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_procedureheading/7}).
-compile({nowarn_unused_function,  yeccgoto_procedureheading/7}).
yeccgoto_procedureheading(288, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(298, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_procedureheading(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(298, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_proceduretype/7}).
-compile({nowarn_unused_function,  yeccgoto_proceduretype/7}).
yeccgoto_proceduretype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proceduretype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proceduretype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proceduretype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proceduretype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_proctype/7}).
-compile({nowarn_unused_function,  yeccgoto_proctype/7}).
yeccgoto_proctype(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_proctype(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_qualident/7}).
-compile({nowarn_unused_function,  yeccgoto_qualident/7}).
yeccgoto_qualident(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(273=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_qualident(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_recordtype/7}).
-compile({nowarn_unused_function,  yeccgoto_recordtype/7}).
yeccgoto_recordtype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_recordtype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_recordtype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_recordtype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_recordtype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_relation/7}).
-compile({nowarn_unused_function,  yeccgoto_relation/7}).
yeccgoto_relation(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(92, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_repeatstatement/7}).
-compile({nowarn_unused_function,  yeccgoto_repeatstatement/7}).
yeccgoto_repeatstatement(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_repeatstatement(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_selector/7}).
-compile({nowarn_unused_function,  yeccgoto_selector/7}).
yeccgoto_selector(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_selectorarr/7}).
-compile({nowarn_unused_function,  yeccgoto_selectorarr/7}).
yeccgoto_selectorarr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_selectorarr(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_set/7}).
-compile({nowarn_unused_function,  yeccgoto_set/7}).
yeccgoto_set(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(81=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(127=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(192=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_setlist/7}).
-compile({nowarn_unused_function,  yeccgoto_setlist/7}).
yeccgoto_setlist(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_setlist(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_simpleexpression/7}).
-compile({nowarn_unused_function,  yeccgoto_simpleexpression/7}).
yeccgoto_simpleexpression(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpleexpression(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_simpletype/7}).
-compile({nowarn_unused_function,  yeccgoto_simpletype/7}).
yeccgoto_simpletype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(273=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_simpletype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statement/7}).
-compile({nowarn_unused_function,  yeccgoto_statement/7}).
yeccgoto_statement(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statement(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_statementsequence/7}).
-compile({nowarn_unused_function,  yeccgoto_statementsequence/7}).
yeccgoto_statementsequence(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(169, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statementsequence(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_structype/7}).
-compile({nowarn_unused_function,  yeccgoto_structype/7}).
yeccgoto_structype(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_structype(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_structype(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_structype(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_structype(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_term/7}).
-compile({nowarn_unused_function,  yeccgoto_term/7}).
yeccgoto_term(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_term(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_termlist/7}).
-compile({nowarn_unused_function,  yeccgoto_termlist/7}).
yeccgoto_termlist(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(192, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termlist(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type/7}).
-compile({nowarn_unused_function,  yeccgoto_type/7}).
yeccgoto_type(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(216=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(246=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_decl_li/7}).
-compile({nowarn_unused_function,  yeccgoto_type_decl_li/7}).
yeccgoto_type_decl_li(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_typedeclaration/7}).
-compile({nowarn_unused_function,  yeccgoto_typedeclaration/7}).
yeccgoto_typedeclaration(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typedeclaration(206, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(285, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_decl_li/7}).
-compile({nowarn_unused_function,  yeccgoto_var_decl_li/7}).
yeccgoto_var_decl_li(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(291, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_variabledeclaration/7}).
-compile({nowarn_unused_function,  yeccgoto_variabledeclaration/7}).
yeccgoto_variabledeclaration(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(290, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variabledeclaration(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(295, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_whilestatement/7}).
-compile({nowarn_unused_function,  yeccgoto_whilestatement/7}).
yeccgoto_whilestatement(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(136=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_whilestatement(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("priv/obr_parser.yrl", 71).
yeccpars2_4_(__Stack0) ->
 [begin
                                nil
  end | __Stack0].

-compile({inline,yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function,  yeccpars2_5_/1}).
-file("priv/obr_parser.yrl", 91).
yeccpars2_5_(__Stack0) ->
 [begin
                                        nil
  end | __Stack0].

-compile({inline,yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function,  yeccpars2_6_/1}).
-file("priv/obr_parser.yrl", 72).
yeccpars2_6_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                  ___1
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function,  yeccpars2_8_/1}).
-file("priv/obr_parser.yrl", 79).
yeccpars2_8_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    [___1]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function,  yeccpars2_10_/1}).
-file("priv/obr_parser.yrl", 83).
yeccpars2_10_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  'Elixir.T':new({import, str_of(___1), {value_of(___1), value_of(___1)}})
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-dialyzer({nowarn_function, yeccpars2_12_/1}).
-compile({nowarn_unused_function,  yeccpars2_12_/1}).
-file("priv/obr_parser.yrl", 84).
yeccpars2_12_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                'Elixir.T':new({import, str_of(___1), {value_of(___1), value_of(___3)}})
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("priv/obr_parser.yrl", 78).
yeccpars2_13_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                             'Elixir.T':new({importlist, str_of(___1), ___2})
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-dialyzer({nowarn_function, yeccpars2_15_/1}).
-compile({nowarn_unused_function,  yeccpars2_15_/1}).
-file("priv/obr_parser.yrl", 80).
yeccpars2_15_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    [___1] ++ ___3
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-dialyzer({nowarn_function, yeccpars2_16_/1}).
-compile({nowarn_unused_function,  yeccpars2_16_/1}).
-file("priv/obr_parser.yrl", 98).
yeccpars2_16_(__Stack0) ->
 [begin
                                       nil
  end | __Stack0].

-compile({inline,yeccpars2_17_/1}).
-dialyzer({nowarn_function, yeccpars2_17_/1}).
-compile({nowarn_unused_function,  yeccpars2_17_/1}).
-file("priv/obr_parser.yrl", 74).
yeccpars2_17_(__Stack0) ->
 [begin
                           nil
  end | __Stack0].

-compile({inline,yeccpars2_18_/1}).
-dialyzer({nowarn_function, yeccpars2_18_/1}).
-compile({nowarn_unused_function,  yeccpars2_18_/1}).
-file("priv/obr_parser.yrl", 93).
yeccpars2_18_(__Stack0) ->
 [begin
                            []
  end | __Stack0].

-compile({inline,yeccpars2_21_/1}).
-dialyzer({nowarn_function, yeccpars2_21_/1}).
-compile({nowarn_unused_function,  yeccpars2_21_/1}).
-file("priv/obr_parser.yrl", 92).
yeccpars2_21_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                     ___2
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function,  yeccpars2_22_/1}).
-file("priv/obr_parser.yrl", 208).
yeccpars2_22_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({identdef, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("priv/obr_parser.yrl", 209).
yeccpars2_23_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({identdef, str_of(___1), value_of(___1) ++ "*"})
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function,  yeccpars2_25_/1}).
-file("priv/obr_parser.yrl", 95).
yeccpars2_25_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function,  yeccpars2_26_/1}).
-file("priv/obr_parser.yrl", 94).
yeccpars2_26_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                  [___1]
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-dialyzer({nowarn_function, yeccpars2_28_/1}).
-compile({nowarn_unused_function,  yeccpars2_28_/1}).
-file("priv/obr_parser.yrl", 275).
yeccpars2_28_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new({term, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-dialyzer({nowarn_function, yeccpars2_29_/1}).
-compile({nowarn_unused_function,  yeccpars2_29_/1}).
-file("priv/obr_parser.yrl", 259).
yeccpars2_29_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           {simpleexpression_list, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function,  yeccpars2_30_/1}).
-file("priv/obr_parser.yrl", 245).
yeccpars2_30_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                 'Elixir.T':new({expression, str_of(___1), [(___1)]})
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-dialyzer({nowarn_function, yeccpars2_31_/1}).
-compile({nowarn_unused_function,  yeccpars2_31_/1}).
-file("priv/obr_parser.yrl", 292).
yeccpars2_31_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                'Elixir.T':new({factor, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-dialyzer({nowarn_function, yeccpars2_32_/1}).
-compile({nowarn_unused_function,  yeccpars2_32_/1}).
-file("priv/obr_parser.yrl", 62).
yeccpars2_32_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({designator, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-dialyzer({nowarn_function, yeccpars2_33_/1}).
-compile({nowarn_unused_function,  yeccpars2_33_/1}).
-file("priv/obr_parser.yrl", 287).
yeccpars2_33_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new({factor, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-dialyzer({nowarn_function, yeccpars2_34_/1}).
-compile({nowarn_unused_function,  yeccpars2_34_/1}).
-file("priv/obr_parser.yrl", 276).
yeccpars2_34_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     {termlist, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-dialyzer({nowarn_function, yeccpars2_35_/1}).
-compile({nowarn_unused_function,  yeccpars2_35_/1}).
-file("priv/obr_parser.yrl", 120).
yeccpars2_35_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                'Elixir.T':new({constexpression, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-dialyzer({nowarn_function, yeccpars2_36_/1}).
-compile({nowarn_unused_function,  yeccpars2_36_/1}).
-file("priv/obr_parser.yrl", 293).
yeccpars2_36_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new({factor, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-dialyzer({nowarn_function, yeccpars2_37_/1}).
-compile({nowarn_unused_function,  yeccpars2_37_/1}).
-file("priv/obr_parser.yrl", 117).
yeccpars2_37_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        'Elixir.T':new({constantdeclaration, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-dialyzer({nowarn_function, yeccpars2_38_/1}).
-compile({nowarn_unused_function,  yeccpars2_38_/1}).
-file("priv/obr_parser.yrl", 67).
yeccpars2_38_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function,  yeccpars2_39_/1}).
-file("priv/obr_parser.yrl", 205).
yeccpars2_39_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     'Elixir.T':new({qualident, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function,  yeccpars2_40_/1}).
-file("priv/obr_parser.yrl", 65).
yeccpars2_40_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-dialyzer({nowarn_function, yeccpars2_41_/1}).
-compile({nowarn_unused_function,  yeccpars2_41_/1}).
-file("priv/obr_parser.yrl", 66).
yeccpars2_41_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                 'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-dialyzer({nowarn_function, yeccpars2_42_/1}).
-compile({nowarn_unused_function,  yeccpars2_42_/1}).
-file("priv/obr_parser.yrl", 288).
yeccpars2_42_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new({factor, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("priv/obr_parser.yrl", 291).
yeccpars2_43_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new({factor, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function,  yeccpars2_47_/1}).
-file("priv/obr_parser.yrl", 289).
yeccpars2_47_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  'Elixir.T':new({factor, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-dialyzer({nowarn_function, yeccpars2_50_/1}).
-compile({nowarn_unused_function,  yeccpars2_50_/1}).
-file("priv/obr_parser.yrl", 290).
yeccpars2_50_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new({factor, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-dialyzer({nowarn_function, yeccpars2_51_/1}).
-compile({nowarn_unused_function,  yeccpars2_51_/1}).
-file("priv/obr_parser.yrl", 296).
yeccpars2_51_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           'Elixir.T':new({factor, str_of(___1), {t_tilda, ___2}})
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-dialyzer({nowarn_function, yeccpars2_52_/1}).
-compile({nowarn_unused_function,  yeccpars2_52_/1}).
-file("priv/obr_parser.yrl", 260).
yeccpars2_52_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                  {simpleexpression_list, str_of(___2), [___2, 'Elixir.UNARY_OPERATOR':new({t_plus, str_of(___1), value_of(___1)})]}
  end | __Stack].

-compile({inline,yeccpars2_54_/1}).
-dialyzer({nowarn_function, yeccpars2_54_/1}).
-compile({nowarn_unused_function,  yeccpars2_54_/1}).
-file("priv/obr_parser.yrl", 268).
yeccpars2_54_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                         'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function,  yeccpars2_55_/1}).
-file("priv/obr_parser.yrl", 269).
yeccpars2_55_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-dialyzer({nowarn_function, yeccpars2_56_/1}).
-compile({nowarn_unused_function,  yeccpars2_56_/1}).
-file("priv/obr_parser.yrl", 267).
yeccpars2_56_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-dialyzer({nowarn_function, yeccpars2_57_/1}).
-compile({nowarn_unused_function,  yeccpars2_57_/1}).
-file("priv/obr_parser.yrl", 263).
yeccpars2_57_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               {simpleexpression_list, str_of(___3), [___2, 'Elixir.UNARY_OPERATOR':new({t_plus, str_of(___1), value_of(___1)})] ++ value_of(___4) ++ [___3]}
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-dialyzer({nowarn_function, yeccpars2_58_/1}).
-compile({nowarn_unused_function,  yeccpars2_58_/1}).
-file("priv/obr_parser.yrl", 261).
yeccpars2_58_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                   {simpleexpression_list, str_of(___2), [___2, 'Elixir.UNARY_OPERATOR':new({t_minus, str_of(___1), value_of(___1)})]}
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-dialyzer({nowarn_function, yeccpars2_60_/1}).
-compile({nowarn_unused_function,  yeccpars2_60_/1}).
-file("priv/obr_parser.yrl", 264).
yeccpars2_60_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                {simpleexpression_list, str_of(___3), [___2, 'Elixir.UNARY_OPERATOR':new({t_minus, str_of(___1), value_of(___1)})] ++ value_of(___4) ++ [___3]}
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function,  yeccpars2_62_/1}).
-file("priv/obr_parser.yrl", 295).
yeccpars2_62_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                     'Elixir.T':new({factor, str_of(___1), ___2})
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-dialyzer({nowarn_function, yeccpars2_64_/1}).
-compile({nowarn_unused_function,  yeccpars2_64_/1}).
-file("priv/obr_parser.yrl", 333).
yeccpars2_64_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        'Elixir.T':new({element, str_of(___1), (___1)})
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-dialyzer({nowarn_function, yeccpars2_65_/1}).
-compile({nowarn_unused_function,  yeccpars2_65_/1}).
-file("priv/obr_parser.yrl", 329).
yeccpars2_65_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     [___1]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-dialyzer({nowarn_function, yeccpars2_66_/1}).
-compile({nowarn_unused_function,  yeccpars2_66_/1}).
-file("priv/obr_parser.yrl", 327).
yeccpars2_66_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           'Elixir.T':new({set, str_of(___1), []})
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-dialyzer({nowarn_function, yeccpars2_68_/1}).
-compile({nowarn_unused_function,  yeccpars2_68_/1}).
-file("priv/obr_parser.yrl", 330).
yeccpars2_68_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                     [___1] ++ ___3
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-dialyzer({nowarn_function, yeccpars2_70_/1}).
-compile({nowarn_unused_function,  yeccpars2_70_/1}).
-file("priv/obr_parser.yrl", 334).
yeccpars2_70_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         'Elixir.T':new({element, str_of(___1), 'Elixir.ELEMENT_DIAPAZONE':new((___1), (___3))})
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-dialyzer({nowarn_function, yeccpars2_71_/1}).
-compile({nowarn_unused_function,  yeccpars2_71_/1}).
-file("priv/obr_parser.yrl", 328).
yeccpars2_71_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   'Elixir.T':new({set, str_of(___1), ___2})
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-dialyzer({nowarn_function, yeccpars2_73_/1}).
-compile({nowarn_unused_function,  yeccpars2_73_/1}).
-file("priv/obr_parser.yrl", 204).
yeccpars2_73_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                'Elixir.T':new({qualident, str_of(___1), value_of(___1) ++ "." ++ value_of(___3)})
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function,  yeccpars2_74_/1}).
-file("priv/obr_parser.yrl", 294).
yeccpars2_74_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                       'Elixir.T':new({factor, str_of(___1), {___1, ___2}})
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-dialyzer({nowarn_function, yeccpars2_76_/1}).
-compile({nowarn_unused_function,  yeccpars2_76_/1}).
-file("priv/obr_parser.yrl", 338).
yeccpars2_76_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       {exlist, str_of(___1), [(___1)]}
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-dialyzer({nowarn_function, yeccpars2_78_/1}).
-compile({nowarn_unused_function,  yeccpars2_78_/1}).
-file("priv/obr_parser.yrl", 337).
yeccpars2_78_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new({explist, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-dialyzer({nowarn_function, yeccpars2_79_/1}).
-compile({nowarn_unused_function,  yeccpars2_79_/1}).
-file("priv/obr_parser.yrl", 53).
yeccpars2_79_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                    'Elixir.T':new({actualparameters, str_of(___1), []})
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-dialyzer({nowarn_function, yeccpars2_80_/1}).
-compile({nowarn_unused_function,  yeccpars2_80_/1}).
-file("priv/obr_parser.yrl", 54).
yeccpars2_80_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                            'Elixir.T':new({actualparameters, str_of(___1), [value_of(___2)]})
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-dialyzer({nowarn_function, yeccpars2_82_/1}).
-compile({nowarn_unused_function,  yeccpars2_82_/1}).
-file("priv/obr_parser.yrl", 339).
yeccpars2_82_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      {exlist, str_of(___1),[(___1)] ++ value_of(___3)}
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-dialyzer({nowarn_function, yeccpars2_83_/1}).
-compile({nowarn_unused_function,  yeccpars2_83_/1}).
-file("priv/obr_parser.yrl", 299).
yeccpars2_83_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-dialyzer({nowarn_function, yeccpars2_84_/1}).
-compile({nowarn_unused_function,  yeccpars2_84_/1}).
-file("priv/obr_parser.yrl", 61).
yeccpars2_84_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                  {deslist, str_of(___1), ['Elixir.T':new({ident, str_of(___1), value_of(___1)})]++ ___2}
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-dialyzer({nowarn_function, yeccpars2_85_/1}).
-compile({nowarn_unused_function,  yeccpars2_85_/1}).
-file("priv/obr_parser.yrl", 300).
yeccpars2_85_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      ['Elixir.UNARY_OPERATOR':new({t_arrow, str_of(___1), value_of(___1)})]
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-dialyzer({nowarn_function, yeccpars2_89_/1}).
-compile({nowarn_unused_function,  yeccpars2_89_/1}).
-file("priv/obr_parser.yrl", 303).
yeccpars2_89_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                           ___2
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-dialyzer({nowarn_function, yeccpars2_90_/1}).
-compile({nowarn_unused_function,  yeccpars2_90_/1}).
-file("priv/obr_parser.yrl", 302).
yeccpars2_90_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                      {___2, ___4}
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-dialyzer({nowarn_function, yeccpars2_91_/1}).
-compile({nowarn_unused_function,  yeccpars2_91_/1}).
-file("priv/obr_parser.yrl", 298).
yeccpars2_91_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                          ___2
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-dialyzer({nowarn_function, yeccpars2_93_/1}).
-compile({nowarn_unused_function,  yeccpars2_93_/1}).
-file("priv/obr_parser.yrl", 249).
yeccpars2_93_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function,  yeccpars2_94_/1}).
-file("priv/obr_parser.yrl", 255).
yeccpars2_94_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-dialyzer({nowarn_function, yeccpars2_95_/1}).
-compile({nowarn_unused_function,  yeccpars2_95_/1}).
-file("priv/obr_parser.yrl", 256).
yeccpars2_95_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function,  yeccpars2_96_/1}).
-file("priv/obr_parser.yrl", 251).
yeccpars2_96_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-dialyzer({nowarn_function, yeccpars2_97_/1}).
-compile({nowarn_unused_function,  yeccpars2_97_/1}).
-file("priv/obr_parser.yrl", 252).
yeccpars2_97_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-dialyzer({nowarn_function, yeccpars2_98_/1}).
-compile({nowarn_unused_function,  yeccpars2_98_/1}).
-file("priv/obr_parser.yrl", 253).
yeccpars2_98_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-dialyzer({nowarn_function, yeccpars2_99_/1}).
-compile({nowarn_unused_function,  yeccpars2_99_/1}).
-file("priv/obr_parser.yrl", 254).
yeccpars2_99_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-dialyzer({nowarn_function, yeccpars2_100_/1}).
-compile({nowarn_unused_function,  yeccpars2_100_/1}).
-file("priv/obr_parser.yrl", 250).
yeccpars2_100_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-dialyzer({nowarn_function, yeccpars2_101_/1}).
-compile({nowarn_unused_function,  yeccpars2_101_/1}).
-file("priv/obr_parser.yrl", 246).
yeccpars2_101_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          'Elixir.T':new({expression, str_of(___1), [___1, ___3, ___2]})
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-dialyzer({nowarn_function, yeccpars2_103_/1}).
-compile({nowarn_unused_function,  yeccpars2_103_/1}).
-file("priv/obr_parser.yrl", 262).
yeccpars2_103_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                        {simpleexpression_list, str_of(___2), [___1] ++ value_of(___3) ++ [___2]}
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function,  yeccpars2_105_/1}).
-file("priv/obr_parser.yrl", 284).
yeccpars2_105_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-dialyzer({nowarn_function, yeccpars2_106_/1}).
-compile({nowarn_unused_function,  yeccpars2_106_/1}).
-file("priv/obr_parser.yrl", 282).
yeccpars2_106_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function,  yeccpars2_107_/1}).
-file("priv/obr_parser.yrl", 281).
yeccpars2_107_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-dialyzer({nowarn_function, yeccpars2_108_/1}).
-compile({nowarn_unused_function,  yeccpars2_108_/1}).
-file("priv/obr_parser.yrl", 283).
yeccpars2_108_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-dialyzer({nowarn_function, yeccpars2_109_/1}).
-compile({nowarn_unused_function,  yeccpars2_109_/1}).
-file("priv/obr_parser.yrl", 280).
yeccpars2_109_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       'Elixir.T':new(___1)
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-dialyzer({nowarn_function, yeccpars2_110_/1}).
-compile({nowarn_unused_function,  yeccpars2_110_/1}).
-file("priv/obr_parser.yrl", 277).
yeccpars2_110_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         {termlist, str_of(___1), value_of(___1) ++[___3] ++ [___2]}
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-dialyzer({nowarn_function, yeccpars2_113_/1}).
-compile({nowarn_unused_function,  yeccpars2_113_/1}).
-file("priv/obr_parser.yrl", 346).
yeccpars2_113_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-dialyzer({nowarn_function, yeccpars2_114_/1}).
-compile({nowarn_unused_function,  yeccpars2_114_/1}).
-file("priv/obr_parser.yrl", 75).
yeccpars2_114_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                            ___2
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-dialyzer({nowarn_function, yeccpars2_115_/1}).
-compile({nowarn_unused_function,  yeccpars2_115_/1}).
-file("priv/obr_parser.yrl", 358).
yeccpars2_115_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                 {sslist, str_of(___1), [value_of(___1)]}
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-dialyzer({nowarn_function, yeccpars2_116_/1}).
-compile({nowarn_unused_function,  yeccpars2_116_/1}).
-file("priv/obr_parser.yrl", 347).
yeccpars2_116_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-dialyzer({nowarn_function, yeccpars2_117_/1}).
-compile({nowarn_unused_function,  yeccpars2_117_/1}).
-file("priv/obr_parser.yrl", 343).
yeccpars2_117_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-dialyzer({nowarn_function, yeccpars2_118_/1}).
-compile({nowarn_unused_function,  yeccpars2_118_/1}).
-file("priv/obr_parser.yrl", 344).
yeccpars2_118_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-dialyzer({nowarn_function, yeccpars2_119_/1}).
-compile({nowarn_unused_function,  yeccpars2_119_/1}).
-file("priv/obr_parser.yrl", 348).
yeccpars2_119_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-dialyzer({nowarn_function, yeccpars2_120_/1}).
-compile({nowarn_unused_function,  yeccpars2_120_/1}).
-file("priv/obr_parser.yrl", 58).
yeccpars2_120_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                              'Elixir.T':new({procedurecall, str_of(___1), {___1, nil }})
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-dialyzer({nowarn_function, yeccpars2_121_/1}).
-compile({nowarn_unused_function,  yeccpars2_121_/1}).
-file("priv/obr_parser.yrl", 345).
yeccpars2_121_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-dialyzer({nowarn_function, yeccpars2_122_/1}).
-compile({nowarn_unused_function,  yeccpars2_122_/1}).
-file("priv/obr_parser.yrl", 342).
yeccpars2_122_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-dialyzer({nowarn_function, yeccpars2_123_/1}).
-compile({nowarn_unused_function,  yeccpars2_123_/1}).
-file("priv/obr_parser.yrl", 349).
yeccpars2_123_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                 'Elixir.T':new({statement, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-dialyzer({nowarn_function, yeccpars2_132_/1}).
-compile({nowarn_unused_function,  yeccpars2_132_/1}).
-file("priv/obr_parser.yrl", 404).
yeccpars2_132_(__Stack0) ->
 [begin
                         []
  end | __Stack0].

-compile({inline,yeccpars2_137_/1}).
-dialyzer({nowarn_function, yeccpars2_137_/1}).
-compile({nowarn_unused_function,  yeccpars2_137_/1}).
-file("priv/obr_parser.yrl", 405).
yeccpars2_137_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          ['Elixir.T':new({while_else_do, ___2, ___4})]
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-dialyzer({nowarn_function, yeccpars2_139_/1}).
-compile({nowarn_unused_function,  yeccpars2_139_/1}).
-file("priv/obr_parser.yrl", 403).
yeccpars2_139_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                              'Elixir.T':new({whilestatement, str_of(___1), ['Elixir.T':new({while_do, ___2, ___4})] ++ ___5})
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-dialyzer({nowarn_function, yeccpars2_142_/1}).
-compile({nowarn_unused_function,  yeccpars2_142_/1}).
-file("priv/obr_parser.yrl", 406).
yeccpars2_142_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                     ___1 ++ ['Elixir.T':new({while_else_do, ___3, ___5})]
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-dialyzer({nowarn_function, yeccpars2_145_/1}).
-compile({nowarn_unused_function,  yeccpars2_145_/1}).
-file("priv/obr_parser.yrl", 409).
yeccpars2_145_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   'Elixir.T':new({repeatstatement, str_of(___1), {___2, ___4}})
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-dialyzer({nowarn_function, yeccpars2_151_/1}).
-compile({nowarn_unused_function,  yeccpars2_151_/1}).
-file("priv/obr_parser.yrl", 363).
yeccpars2_151_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                ___1
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-dialyzer({nowarn_function, yeccpars2_154_/1}).
-compile({nowarn_unused_function,  yeccpars2_154_/1}).
-file("priv/obr_parser.yrl", 367).
yeccpars2_154_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          ___1
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-dialyzer({nowarn_function, yeccpars2_155_/1}).
-compile({nowarn_unused_function,  yeccpars2_155_/1}).
-file("priv/obr_parser.yrl", 368).
yeccpars2_155_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                   ___1
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-dialyzer({nowarn_function, yeccpars2_158_/1}).
-compile({nowarn_unused_function,  yeccpars2_158_/1}).
-file("priv/obr_parser.yrl", 364).
yeccpars2_158_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                         ___1
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-dialyzer({nowarn_function, yeccpars2_159_/1}).
-compile({nowarn_unused_function,  yeccpars2_159_/1}).
-file("priv/obr_parser.yrl", 370).
yeccpars2_159_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                     ___1
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-dialyzer({nowarn_function, yeccpars2_160_/1}).
-compile({nowarn_unused_function,  yeccpars2_160_/1}).
-file("priv/obr_parser.yrl", 365).
yeccpars2_160_(__Stack0) ->
 [___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                ___1
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-dialyzer({nowarn_function, yeccpars2_165_/1}).
-compile({nowarn_unused_function,  yeccpars2_165_/1}).
-file("priv/obr_parser.yrl", 413).
yeccpars2_165_(__Stack0) ->
 [begin
                    nil
  end | __Stack0].

-compile({inline,yeccpars2_168_/1}).
-dialyzer({nowarn_function, yeccpars2_168_/1}).
-compile({nowarn_unused_function,  yeccpars2_168_/1}).
-file("priv/obr_parser.yrl", 414).
yeccpars2_168_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                ___2
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-dialyzer({nowarn_function, yeccpars2_171_/1}).
-compile({nowarn_unused_function,  yeccpars2_171_/1}).
-file("priv/obr_parser.yrl", 412).
yeccpars2_171_(__Stack0) ->
 [___10,___9,___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                     'Elixir.T':new({forstatement, str_of(___1), {___2, ___4, ___6, ___7, ___9}})
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-dialyzer({nowarn_function, yeccpars2_173_/1}).
-compile({nowarn_unused_function,  yeccpars2_173_/1}).
-file("priv/obr_parser.yrl", 385).
yeccpars2_173_(__Stack0) ->
 [begin
                     'Elixir.T':new({ntcase, nil, nil})
  end | __Stack0].

-compile({inline,yeccpars2_174_/1}).
-dialyzer({nowarn_function, yeccpars2_174_/1}).
-compile({nowarn_unused_function,  yeccpars2_174_/1}).
-file("priv/obr_parser.yrl", 400).
yeccpars2_174_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     'Elixir.T':new({label, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-dialyzer({nowarn_function, yeccpars2_176_/1}).
-compile({nowarn_unused_function,  yeccpars2_176_/1}).
-file("priv/obr_parser.yrl", 381).
yeccpars2_176_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       {ntcaselist, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-dialyzer({nowarn_function, yeccpars2_177_/1}).
-compile({nowarn_unused_function,  yeccpars2_177_/1}).
-file("priv/obr_parser.yrl", 390).
yeccpars2_177_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       {cllist, str_of(___1), [(___1)]}
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-dialyzer({nowarn_function, yeccpars2_178_/1}).
-compile({nowarn_unused_function,  yeccpars2_178_/1}).
-file("priv/obr_parser.yrl", 394).
yeccpars2_178_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      'Elixir.T':new({labelrange, str_of(___1), {___1, ___1}})
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-dialyzer({nowarn_function, yeccpars2_179_/1}).
-compile({nowarn_unused_function,  yeccpars2_179_/1}).
-file("priv/obr_parser.yrl", 389).
yeccpars2_179_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({caselabellist, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-dialyzer({nowarn_function, yeccpars2_181_/1}).
-compile({nowarn_unused_function,  yeccpars2_181_/1}).
-file("priv/obr_parser.yrl", 398).
yeccpars2_181_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   'Elixir.T':new({label, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-dialyzer({nowarn_function, yeccpars2_182_/1}).
-compile({nowarn_unused_function,  yeccpars2_182_/1}).
-file("priv/obr_parser.yrl", 399).
yeccpars2_182_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                  'Elixir.T':new({label, str_of(___1), 'Elixir.T':new(___1)})
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-dialyzer({nowarn_function, yeccpars2_184_/1}).
-compile({nowarn_unused_function,  yeccpars2_184_/1}).
-file("priv/obr_parser.yrl", 386).
yeccpars2_184_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    'Elixir.T':new({ntcase, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-dialyzer({nowarn_function, yeccpars2_186_/1}).
-compile({nowarn_unused_function,  yeccpars2_186_/1}).
-file("priv/obr_parser.yrl", 395).
yeccpars2_186_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                  'Elixir.T':new({labelrange, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_188_/1}).
-dialyzer({nowarn_function, yeccpars2_188_/1}).
-compile({nowarn_unused_function,  yeccpars2_188_/1}).
-file("priv/obr_parser.yrl", 391).
yeccpars2_188_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      {cllist, str_of(___1),[(___1)] ++ value_of(___3)}
  end | __Stack].

-compile({inline,yeccpars2_189_/1}).
-dialyzer({nowarn_function, yeccpars2_189_/1}).
-compile({nowarn_unused_function,  yeccpars2_189_/1}).
-file("priv/obr_parser.yrl", 380).
yeccpars2_189_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                           'Elixir.T':new({casestatement, str_of(___1), {___2, value_of(___4)}})
  end | __Stack].

-compile({inline,yeccpars2_190_/1}).
-dialyzer({nowarn_function, yeccpars2_190_/1}).
-compile({nowarn_unused_function,  yeccpars2_190_/1}).
-file("priv/obr_parser.yrl", 385).
yeccpars2_190_(__Stack0) ->
 [begin
                     'Elixir.T':new({ntcase, nil, nil})
  end | __Stack0].

-compile({inline,yeccpars2_191_/1}).
-dialyzer({nowarn_function, yeccpars2_191_/1}).
-compile({nowarn_unused_function,  yeccpars2_191_/1}).
-file("priv/obr_parser.yrl", 382).
yeccpars2_191_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          {ntcaselist, str_of(___1), value_of(___1) ++ [___3]}
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-dialyzer({nowarn_function, yeccpars2_194_/1}).
-compile({nowarn_unused_function,  yeccpars2_194_/1}).
-file("priv/obr_parser.yrl", 228).
yeccpars2_194_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                         ___3
  end | __Stack].

-compile({inline,yeccpars2_195_/1}).
-dialyzer({nowarn_function, yeccpars2_195_/1}).
-compile({nowarn_unused_function,  yeccpars2_195_/1}).
-file("priv/obr_parser.yrl", 57).
yeccpars2_195_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               'Elixir.T':new({procedurecall, str_of(___1), {___1, value_of(___1) }})
  end | __Stack].

-compile({inline,yeccpars2_197_/1}).
-dialyzer({nowarn_function, yeccpars2_197_/1}).
-compile({nowarn_unused_function,  yeccpars2_197_/1}).
-file("priv/obr_parser.yrl", 353).
yeccpars2_197_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               'Elixir.T':new({assignment, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-dialyzer({nowarn_function, yeccpars2_198_/1}).
-compile({nowarn_unused_function,  yeccpars2_198_/1}).
-file("priv/obr_parser.yrl", 359).
yeccpars2_198_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             {sslist, str_of(___1), [value_of(___1)]}
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-dialyzer({nowarn_function, yeccpars2_199_/1}).
-compile({nowarn_unused_function,  yeccpars2_199_/1}).
-file("priv/obr_parser.yrl", 357).
yeccpars2_199_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               ___1
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-dialyzer({nowarn_function, yeccpars2_202_/1}).
-compile({nowarn_unused_function,  yeccpars2_202_/1}).
-file("priv/obr_parser.yrl", 70).
yeccpars2_202_(__Stack0) ->
 [___9,___8,___7,___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                            'Elixir.T':new({module, nil, {___2, ___4, ___5, ___6, ___8}})
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-dialyzer({nowarn_function, yeccpars2_203_/1}).
-compile({nowarn_unused_function,  yeccpars2_203_/1}).
-file("priv/obr_parser.yrl", 105).
yeccpars2_203_(__Stack0) ->
 [begin
                                      nil
  end | __Stack0].

-compile({inline,yeccpars2_204_/1}).
-dialyzer({nowarn_function, yeccpars2_204_/1}).
-compile({nowarn_unused_function,  yeccpars2_204_/1}).
-file("priv/obr_parser.yrl", 100).
yeccpars2_204_(__Stack0) ->
 [begin
                           []
  end | __Stack0].

-compile({inline,yeccpars2_206_/1}).
-dialyzer({nowarn_function, yeccpars2_206_/1}).
-compile({nowarn_unused_function,  yeccpars2_206_/1}).
-file("priv/obr_parser.yrl", 99).
yeccpars2_206_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                  ___2
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-dialyzer({nowarn_function, yeccpars2_209_/1}).
-compile({nowarn_unused_function,  yeccpars2_209_/1}).
-file("priv/obr_parser.yrl", 123).
yeccpars2_209_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                         'Elixir.T':new({typedeclaration, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-dialyzer({nowarn_function, yeccpars2_210_/1}).
-compile({nowarn_unused_function,  yeccpars2_210_/1}).
-file("priv/obr_parser.yrl", 217).
yeccpars2_210_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new({type, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_211_/1}).
-dialyzer({nowarn_function, yeccpars2_211_/1}).
-compile({nowarn_unused_function,  yeccpars2_211_/1}).
-file("priv/obr_parser.yrl", 218).
yeccpars2_211_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     ___1
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-dialyzer({nowarn_function, yeccpars2_212_/1}).
-compile({nowarn_unused_function,  yeccpars2_212_/1}).
-file("priv/obr_parser.yrl", 127).
yeccpars2_212_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({structype, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-dialyzer({nowarn_function, yeccpars2_213_/1}).
-compile({nowarn_unused_function,  yeccpars2_213_/1}).
-file("priv/obr_parser.yrl", 216).
yeccpars2_213_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    'Elixir.T':new({type, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-dialyzer({nowarn_function, yeccpars2_214_/1}).
-compile({nowarn_unused_function,  yeccpars2_214_/1}).
-file("priv/obr_parser.yrl", 128).
yeccpars2_214_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                             'Elixir.T':new({structype, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_215_/1}).
-dialyzer({nowarn_function, yeccpars2_215_/1}).
-compile({nowarn_unused_function,  yeccpars2_215_/1}).
-file("priv/obr_parser.yrl", 129).
yeccpars2_215_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           'Elixir.T':new({structype, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_217_/1}).
-dialyzer({nowarn_function, yeccpars2_217_/1}).
-compile({nowarn_unused_function,  yeccpars2_217_/1}).
-file("priv/obr_parser.yrl", 126).
yeccpars2_217_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                         'Elixir.T':new({structype, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-dialyzer({nowarn_function, yeccpars2_218_/1}).
-compile({nowarn_unused_function,  yeccpars2_218_/1}).
-file("priv/obr_parser.yrl", 133).
yeccpars2_218_(__Stack0) ->
 [begin
                      dynamic
  end | __Stack0].

-compile({inline,yeccpars2_219_/1}).
-dialyzer({nowarn_function, yeccpars2_219_/1}).
-compile({nowarn_unused_function,  yeccpars2_219_/1}).
-file("priv/obr_parser.yrl", 222).
yeccpars2_219_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-dialyzer({nowarn_function, yeccpars2_220_/1}).
-compile({nowarn_unused_function,  yeccpars2_220_/1}).
-file("priv/obr_parser.yrl", 223).
yeccpars2_220_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       ___1
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-dialyzer({nowarn_function, yeccpars2_221_/1}).
-compile({nowarn_unused_function,  yeccpars2_221_/1}).
-file("priv/obr_parser.yrl", 224).
yeccpars2_221_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       ___1
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-dialyzer({nowarn_function, yeccpars2_222_/1}).
-compile({nowarn_unused_function,  yeccpars2_222_/1}).
-file("priv/obr_parser.yrl", 221).
yeccpars2_222_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-dialyzer({nowarn_function, yeccpars2_224_/1}).
-compile({nowarn_unused_function,  yeccpars2_224_/1}).
-file("priv/obr_parser.yrl", 172).
yeccpars2_224_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({proceduretype, str_of(___1), []})
  end | __Stack].

-compile({inline,yeccpars2_225_/1}).
-dialyzer({nowarn_function, yeccpars2_225_/1}).
-compile({nowarn_unused_function,  yeccpars2_225_/1}).
-file("priv/obr_parser.yrl", 225).
yeccpars2_225_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                       ___1
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-dialyzer({nowarn_function, yeccpars2_227_/1}).
-compile({nowarn_unused_function,  yeccpars2_227_/1}).
-file("priv/obr_parser.yrl", 164).
yeccpars2_227_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      'Elixir.T':new({identlist, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-dialyzer({nowarn_function, yeccpars2_229_/1}).
-compile({nowarn_unused_function,  yeccpars2_229_/1}).
-file("priv/obr_parser.yrl", 165).
yeccpars2_229_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                     {idlist, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-dialyzer({nowarn_function, yeccpars2_230_/1}).
-compile({nowarn_unused_function,  yeccpars2_230_/1}).
-file("priv/obr_parser.yrl", 153).
yeccpars2_230_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                              'Elixir.T':new({fieldlistsequence, str_of(___1), value_of(___1)})
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-dialyzer({nowarn_function, yeccpars2_232_/1}).
-compile({nowarn_unused_function,  yeccpars2_232_/1}).
-file("priv/obr_parser.yrl", 155).
yeccpars2_232_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                                      {fieldlistsequence_list, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-dialyzer({nowarn_function, yeccpars2_233_/1}).
-compile({nowarn_unused_function,  yeccpars2_233_/1}).
-file("priv/obr_parser.yrl", 144).
yeccpars2_233_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                               'Elixir.T':new({recordtype, str_of(___1), {nil, []}})
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-dialyzer({nowarn_function, yeccpars2_235_/1}).
-compile({nowarn_unused_function,  yeccpars2_235_/1}).
-file("priv/obr_parser.yrl", 150).
yeccpars2_235_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        {basetype, str_of(___1), (___1)}
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-dialyzer({nowarn_function, yeccpars2_239_/1}).
-compile({nowarn_unused_function,  yeccpars2_239_/1}).
-file("priv/obr_parser.yrl", 146).
yeccpars2_239_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                      'Elixir.T':new({recordtype, str_of(___1), {value_of(___3), []}})
  end | __Stack].

-compile({inline,yeccpars2_240_/1}).
-dialyzer({nowarn_function, yeccpars2_240_/1}).
-compile({nowarn_unused_function,  yeccpars2_240_/1}).
-file("priv/obr_parser.yrl", 147).
yeccpars2_240_(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                        'Elixir.T':new({recordtype, str_of(___1), {value_of(___3), ___5}})
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-dialyzer({nowarn_function, yeccpars2_241_/1}).
-compile({nowarn_unused_function,  yeccpars2_241_/1}).
-file("priv/obr_parser.yrl", 156).
yeccpars2_241_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                  {fieldlistsequence_list, str_of(___1), [___1]}
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-dialyzer({nowarn_function, yeccpars2_242_/1}).
-compile({nowarn_unused_function,  yeccpars2_242_/1}).
-file("priv/obr_parser.yrl", 157).
yeccpars2_242_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                         {fieldlistsequence_list, str_of(___1), [___1] ++ value_of(___3)}
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-dialyzer({nowarn_function, yeccpars2_243_/1}).
-compile({nowarn_unused_function,  yeccpars2_243_/1}).
-file("priv/obr_parser.yrl", 145).
yeccpars2_243_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                 'Elixir.T':new({recordtype, str_of(___1), {nil, ___2}})
  end | __Stack].

-compile({inline,yeccpars2_245_/1}).
-dialyzer({nowarn_function, yeccpars2_245_/1}).
-compile({nowarn_unused_function,  yeccpars2_245_/1}).
-file("priv/obr_parser.yrl", 166).
yeccpars2_245_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   {idlist, str_of(___1), [___1] ++ value_of(___3)}
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-dialyzer({nowarn_function, yeccpars2_247_/1}).
-compile({nowarn_unused_function,  yeccpars2_247_/1}).
-file("priv/obr_parser.yrl", 161).
yeccpars2_247_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      'Elixir.T':new({fieldlist, str_of(___1), {value_of(___1), value_of(___3)}})
  end | __Stack].

-compile({inline,yeccpars2_248_/1}).
-dialyzer({nowarn_function, yeccpars2_248_/1}).
-compile({nowarn_unused_function,  yeccpars2_248_/1}).
-file("priv/obr_parser.yrl", 173).
yeccpars2_248_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               'Elixir.T':new({proceduretype, str_of(___1), ___2})
  end | __Stack].

-compile({inline,yeccpars2_251_/1}).
-dialyzer({nowarn_function, yeccpars2_251_/1}).
-compile({nowarn_unused_function,  yeccpars2_251_/1}).
-file("priv/obr_parser.yrl", 183).
yeccpars2_251_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                         [___1]
  end | __Stack].

-compile({inline,yeccpars2_253_/1}).
-dialyzer({nowarn_function, yeccpars2_253_/1}).
-compile({nowarn_unused_function,  yeccpars2_253_/1}).
-file("priv/obr_parser.yrl", 190).
yeccpars2_253_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   {idlist2, str_of(___1), ['Elixir.T':new(___1)]}
  end | __Stack].

-compile({inline,yeccpars2_258_/1}).
-dialyzer({nowarn_function, yeccpars2_258_/1}).
-compile({nowarn_unused_function,  yeccpars2_258_/1}).
-file("priv/obr_parser.yrl", 196).
yeccpars2_258_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                           'Elixir.T':new({formaltype, str_of(___1), {___1, 0}})
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-dialyzer({nowarn_function, yeccpars2_259_/1}).
-compile({nowarn_unused_function,  yeccpars2_259_/1}).
-file("priv/obr_parser.yrl", 194).
yeccpars2_259_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          'Elixir.T':new({formaltype, str_of(___1), {___1, 0}})
  end | __Stack].

-compile({inline,yeccpars2_260_/1}).
-dialyzer({nowarn_function, yeccpars2_260_/1}).
-compile({nowarn_unused_function,  yeccpars2_260_/1}).
-file("priv/obr_parser.yrl", 188).
yeccpars2_260_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                'Elixir.T':new({fpsection, str_of(___1), {var, value_of(___2), ___4}})
  end | __Stack].

-compile({inline,yeccpars2_261_/1}).
-dialyzer({nowarn_function, yeccpars2_261_/1}).
-compile({nowarn_unused_function,  yeccpars2_261_/1}).
-file("priv/obr_parser.yrl", 198).
yeccpars2_261_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                          ___1
  end | __Stack].

-compile({inline,yeccpars2_263_/1}).
-dialyzer({nowarn_function, yeccpars2_263_/1}).
-compile({nowarn_unused_function,  yeccpars2_263_/1}).
-file("priv/obr_parser.yrl", 181).
yeccpars2_263_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                         ___1
  end | __Stack].

-compile({inline,yeccpars2_264_/1}).
-dialyzer({nowarn_function, yeccpars2_264_/1}).
-compile({nowarn_unused_function,  yeccpars2_264_/1}).
-file("priv/obr_parser.yrl", 180).
yeccpars2_264_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        ___1
  end | __Stack].

-compile({inline,yeccpars2_265_/1}).
-dialyzer({nowarn_function, yeccpars2_265_/1}).
-compile({nowarn_unused_function,  yeccpars2_265_/1}).
-file("priv/obr_parser.yrl", 178).
yeccpars2_265_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                     'Elixir.T':new({formalparameters, str_of(___1), {___2, ___4}})
  end | __Stack].

-compile({inline,yeccpars2_267_/1}).
-dialyzer({nowarn_function, yeccpars2_267_/1}).
-compile({nowarn_unused_function,  yeccpars2_267_/1}).
-file("priv/obr_parser.yrl", 191).
yeccpars2_267_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                   {idlist2, str_of(___1), ['Elixir.T':new(___1)] ++ value_of(___3)}
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-dialyzer({nowarn_function, yeccpars2_268_/1}).
-compile({nowarn_unused_function,  yeccpars2_268_/1}).
-file("priv/obr_parser.yrl", 177).
yeccpars2_268_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                              'Elixir.T':new({formalparameters, str_of(___1), {___2, []}})
  end | __Stack].

-compile({inline,yeccpars2_270_/1}).
-dialyzer({nowarn_function, yeccpars2_270_/1}).
-compile({nowarn_unused_function,  yeccpars2_270_/1}).
-file("priv/obr_parser.yrl", 176).
yeccpars2_270_(__Stack0) ->
 [___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                               'Elixir.T':new({formalparameters, str_of(___1), {___2, ___5}})
  end | __Stack].

-compile({inline,yeccpars2_272_/1}).
-dialyzer({nowarn_function, yeccpars2_272_/1}).
-compile({nowarn_unused_function,  yeccpars2_272_/1}).
-file("priv/obr_parser.yrl", 184).
yeccpars2_272_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                               [___1] ++ ___3
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-dialyzer({nowarn_function, yeccpars2_274_/1}).
-compile({nowarn_unused_function,  yeccpars2_274_/1}).
-file("priv/obr_parser.yrl", 187).
yeccpars2_274_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                          'Elixir.T':new({fpsection, str_of(___1), {not_var, value_of(___1), ___3}})
  end | __Stack].

-compile({inline,yeccpars2_276_/1}).
-dialyzer({nowarn_function, yeccpars2_276_/1}).
-compile({nowarn_unused_function,  yeccpars2_276_/1}).
-file("priv/obr_parser.yrl", 169).
yeccpars2_276_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                     'Elixir.T':new({pointertype, str_of(___1), ___3})
  end | __Stack].

-compile({inline,yeccpars2_278_/1}).
-dialyzer({nowarn_function, yeccpars2_278_/1}).
-compile({nowarn_unused_function,  yeccpars2_278_/1}).
-file("priv/obr_parser.yrl", 134).
yeccpars2_278_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    [___1]
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-dialyzer({nowarn_function, yeccpars2_279_/1}).
-compile({nowarn_unused_function,  yeccpars2_279_/1}).
-file("priv/obr_parser.yrl", 141).
yeccpars2_279_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                            'Elixir.T':new({length, str_of(___1), ___1})
  end | __Stack].

-compile({inline,yeccpars2_280_/1}).
-dialyzer({nowarn_function, yeccpars2_280_/1}).
-compile({nowarn_unused_function,  yeccpars2_280_/1}).
-file("priv/obr_parser.yrl", 133).
yeccpars2_280_(__Stack0) ->
 [begin
                      dynamic
  end | __Stack0].

-compile({inline,yeccpars2_281_/1}).
-dialyzer({nowarn_function, yeccpars2_281_/1}).
-compile({nowarn_unused_function,  yeccpars2_281_/1}).
-file("priv/obr_parser.yrl", 135).
yeccpars2_281_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    [___1] ++ ___3
  end | __Stack].

-compile({inline,yeccpars2_282_/1}).
-dialyzer({nowarn_function, yeccpars2_282_/1}).
-compile({nowarn_unused_function,  yeccpars2_282_/1}).
-file("priv/obr_parser.yrl", 138).
yeccpars2_282_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                        {arraytype, [___2]}
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-dialyzer({nowarn_function, yeccpars2_283_/1}).
-compile({nowarn_unused_function,  yeccpars2_283_/1}).
-file("priv/obr_parser.yrl", 137).
yeccpars2_283_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                      {arraytype, {___4, [___2]}}
  end | __Stack].

-compile({inline,yeccpars2_284_/1}).
-dialyzer({nowarn_function, yeccpars2_284_/1}).
-compile({nowarn_unused_function,  yeccpars2_284_/1}).
-file("priv/obr_parser.yrl", 132).
yeccpars2_284_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                  'Elixir.T':new({arraytype, str_of(___1), {___1, ___2}})
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-dialyzer({nowarn_function, yeccpars2_286_/1}).
-compile({nowarn_unused_function,  yeccpars2_286_/1}).
-file("priv/obr_parser.yrl", 102).
yeccpars2_286_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                          ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_287_/1}).
-dialyzer({nowarn_function, yeccpars2_287_/1}).
-compile({nowarn_unused_function,  yeccpars2_287_/1}).
-file("priv/obr_parser.yrl", 101).
yeccpars2_287_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                             [___1]
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-dialyzer({nowarn_function, yeccpars2_288_/1}).
-compile({nowarn_unused_function,  yeccpars2_288_/1}).
-file("priv/obr_parser.yrl", 112).
yeccpars2_288_(__Stack0) ->
 [begin
                            []
  end | __Stack0].

-compile({inline,yeccpars2_289_/1}).
-dialyzer({nowarn_function, yeccpars2_289_/1}).
-compile({nowarn_unused_function,  yeccpars2_289_/1}).
-file("priv/obr_parser.yrl", 107).
yeccpars2_289_(__Stack0) ->
 [begin
                          []
  end | __Stack0].

-compile({inline,yeccpars2_291_/1}).
-dialyzer({nowarn_function, yeccpars2_291_/1}).
-compile({nowarn_unused_function,  yeccpars2_291_/1}).
-file("priv/obr_parser.yrl", 106).
yeccpars2_291_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                               'Elixir.T':new({variabledeclaration, str_of(___1), ___2})
  end | __Stack].

-compile({inline,yeccpars2_294_/1}).
-dialyzer({nowarn_function, yeccpars2_294_/1}).
-compile({nowarn_unused_function,  yeccpars2_294_/1}).
-file("priv/obr_parser.yrl", 212).
yeccpars2_294_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                'Elixir.T':new({variabledeclaration, str_of(___1), {___1, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_296_/1}).
-dialyzer({nowarn_function, yeccpars2_296_/1}).
-compile({nowarn_unused_function,  yeccpars2_296_/1}).
-file("priv/obr_parser.yrl", 109).
yeccpars2_296_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_297_/1}).
-dialyzer({nowarn_function, yeccpars2_297_/1}).
-compile({nowarn_unused_function,  yeccpars2_297_/1}).
-file("priv/obr_parser.yrl", 108).
yeccpars2_297_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                [___1]
  end | __Stack].

-compile({inline,yeccpars2_300_/1}).
-dialyzer({nowarn_function, yeccpars2_300_/1}).
-compile({nowarn_unused_function,  yeccpars2_300_/1}).
-file("priv/obr_parser.yrl", 87).
yeccpars2_300_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                                                 'Elixir.T':new({declarationsequence, nil, {___1, ___2, ___3, ___4}})
  end | __Stack].

-compile({inline,yeccpars2_302_/1}).
-dialyzer({nowarn_function, yeccpars2_302_/1}).
-compile({nowarn_unused_function,  yeccpars2_302_/1}).
-file("priv/obr_parser.yrl", 234).
yeccpars2_302_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                           'Elixir.T':new({procedureheading, str_of(___1), {___2, []}})
  end | __Stack].

-compile({inline,yeccpars2_303_/1}).
-dialyzer({nowarn_function, yeccpars2_303_/1}).
-compile({nowarn_unused_function,  yeccpars2_303_/1}).
-file("priv/obr_parser.yrl", 235).
yeccpars2_303_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                            'Elixir.T':new({procedureheading, str_of(___1), {___2, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_305_/1}).
-dialyzer({nowarn_function, yeccpars2_305_/1}).
-compile({nowarn_unused_function,  yeccpars2_305_/1}).
-file("priv/obr_parser.yrl", 114).
yeccpars2_305_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                ___1 ++ [___2]
  end | __Stack].

-compile({inline,yeccpars2_306_/1}).
-dialyzer({nowarn_function, yeccpars2_306_/1}).
-compile({nowarn_unused_function,  yeccpars2_306_/1}).
-file("priv/obr_parser.yrl", 113).
yeccpars2_306_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                   [___1]
  end | __Stack].

-compile({inline,yeccpars2_307_/1}).
-dialyzer({nowarn_function, yeccpars2_307_/1}).
-compile({nowarn_unused_function,  yeccpars2_307_/1}).
-file("priv/obr_parser.yrl", 91).
yeccpars2_307_(__Stack0) ->
 [begin
                                        nil
  end | __Stack0].

-compile({inline,yeccpars2_309_/1}).
-dialyzer({nowarn_function, yeccpars2_309_/1}).
-compile({nowarn_unused_function,  yeccpars2_309_/1}).
-file("priv/obr_parser.yrl", 239).
yeccpars2_309_(__Stack0) ->
 [begin
                                     nil
  end | __Stack0].

-compile({inline,yeccpars2_310_/1}).
-dialyzer({nowarn_function, yeccpars2_310_/1}).
-compile({nowarn_unused_function,  yeccpars2_310_/1}).
-file("priv/obr_parser.yrl", 241).
yeccpars2_310_(__Stack0) ->
 [begin
                                     nil
  end | __Stack0].

-compile({inline,yeccpars2_312_/1}).
-dialyzer({nowarn_function, yeccpars2_312_/1}).
-compile({nowarn_unused_function,  yeccpars2_312_/1}).
-file("priv/obr_parser.yrl", 240).
yeccpars2_312_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                      ___2
  end | __Stack].

-compile({inline,yeccpars2_315_/1}).
-dialyzer({nowarn_function, yeccpars2_315_/1}).
-compile({nowarn_unused_function,  yeccpars2_315_/1}).
-file("priv/obr_parser.yrl", 242).
yeccpars2_315_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                                                ___2
  end | __Stack].

-compile({inline,yeccpars2_316_/1}).
-dialyzer({nowarn_function, yeccpars2_316_/1}).
-compile({nowarn_unused_function,  yeccpars2_316_/1}).
-file("priv/obr_parser.yrl", 238).
yeccpars2_316_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                                          'Elixir.T':new({procedurebody, str_of(___1), {___1, ___2, ___3}})
  end | __Stack].

-compile({inline,yeccpars2_317_/1}).
-dialyzer({nowarn_function, yeccpars2_317_/1}).
-compile({nowarn_unused_function,  yeccpars2_317_/1}).
-file("priv/obr_parser.yrl", 231).
yeccpars2_317_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                                           'Elixir.T':new({proceduredeclaration, str_of(___1), {___1, ___3, 'Elixir.T':new(___4)}})
  end | __Stack].


-file("priv/obr_parser.yrl", 445).
