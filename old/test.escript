#!/usr/local/bin/escript

main(_) ->
  io:format('Leex test~n'),
  Result_leex = leex:file('./obr.xrl'),
  io:format('leex:file(\'./obr.xrl\'). -> result = ~p~n', [Result_leex]),
  Result_c = c:c('./obr.erl'),
  io:format('c(\'./obr.erl\').         -> result = ~p~n', [Result_c]),
  Res1 = obr:string("123"),
  io:format('123 -> ~p~n', [Res1]),
  Res2 = obr:string("123H"),
  io:format('123H -> ~p~n', [Res2]),
  Res3 = obr:string("DEH"),
  io:format('DEH -> ~p~n', [Res3]),
  Res4 = obr:string("0DEH"),
  io:format('0DEH -> ~p~n', [Res4]),
  Res5 = obr:string("000000123"),
  io:format('000000123 -> ~p~n', [Res5]),
  Res6 = obr:string("0000000CH"),
  io:format('0000000CH -> ~p~n', [Res6]),
  Res7 = obr:string("abcdedsdfsfsdjfsijdfisdjfisjdfijsdifjisjdfijisdjfivsjdivfjsdvsdfvs"),
  io:format('long id -> ~p~n', [Res7]),
  Res8 = obr:string("000000000000000CAFE"),
  io:format('hex without H -> ~p~n', [Res8]),
  Res9 = obr:string("'Some \"text'"),
  io:format('String -> ~p~n', [Res9])
.
