defmodule OberLexxTest do
  use ExUnit.Case

  require Logger

  # mix test --only erleex
  @tag erleex: true  

  test "the truth" do
    
    {:ok, _} = :leex.file('./priv/obr.xrl')

    {:ok, :obr} = :c.c('./priv/obr.erl')

    # число
    res1 = :obr.string('123')
    Logger.debug ">>>>>> res1=#{inspect res1}"
    assert {:ok, [{:integer, 1, '123'}], 1} == res1

    # hex число
    res2 = :obr.string('123H')
    Logger.debug ">>>>>> res2=#{inspect res2}"
    assert {:ok, [{:integer, 1, '123H'}], 1} == res2

    # нет ноля перед символами, значит это не число, а ID
    res3 = :obr.string('DEH')
    Logger.debug ">>>>>> res3=#{inspect res3}"
    assert {:ok, [{:ident, 1, 'DEH'}], 1} == res3

    # шестнадцатиричное число но тоже INT незначащй нолик убрался
    res4 = :obr.string('0DEH')
    Logger.debug ">>>>>> res4=#{inspect res4}"
    assert {:ok, [{:integer, 1, 'DEH'}], 1} == res4

    # число, но без ноликов
    res5 = :obr.string('000000123')
    Logger.debug ">>>>>> res5=#{inspect res5}"
    assert {:ok, [{:integer, 1, '123'}], 1} == res5

    # hex число, но без ноликов
    res6 = :obr.string('0000000CH')
    Logger.debug ">>>>>> res6=#{inspect res6}"
    assert {:ok, [{:integer, 1, 'CH'}], 1} == res6

    # сокращается длина ИД до 40 символов
    res7 = :obr.string('abcdedsdfsfsdjfsijdfisdjfisjdfijsdifjisjdfijisdjfivsjdivfjsdvsdfvs')
    Logger.debug ">>>>>> res7=#{inspect res7}"
    assert {:ok, [{:ident, 1, 'abcdedsdfsfsdjfsijdfisdjfisjdfijsdifjisj'}], 1} == res7
    assert {:ok, [{:ident, 1, id40}], 1} = res7
    assert length(id40) == 40

    # исправляет, добавляя H в конце
    res8 = :obr.string('000000000000000CAFE')
    Logger.debug ">>>>>> res8=#{inspect res8}"
    assert {:ok, [{:integer, 1, 'CAFEH'}], 1} == res8

    # тесты на real
    res9 = :obr.string('12.012')
    Logger.debug ">>>>>> res9=#{inspect res9}"
    assert {:ok, [{:real, 1, '12.012'}], 1} == res9

    res10 = :obr.string('3.402823466E38')
    Logger.debug ">>>>>> res10=#{inspect res10}"
    assert {:ok, [{:real, 1, '3.402823466E38'}], 1} == res10

    res11 = :obr.string('1.7976931348623158D307')
    Logger.debug ">>>>>> res11=#{inspect res11}"
    assert {:ok, [{:real, 1, '1.7976931348623158D307'}], 1} == res11

    # строка
    str = '"abc\\123"'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '"abc\"123"'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '"abc\'123"'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc123\''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc\"123\''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc\'123\''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '\'  \''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'русские буквы\''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\' \n \''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '\' \r \''
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    # char - заканчивается на X
    str = '0X'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '01X'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '0AX'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '01AX'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '0A1X'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = 'ARRAY'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_array, 1, str}], 1} == res

    str = 'array'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:ident, 1, str}], 1} == res

    str = '+'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_plus, 1, str}], 1} == res

    str = '-'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_minus, 1, str}], 1} == res

    str = '*'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_mul, 1, str}], 1} == res

    str = '/'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_divide, 1, str}], 1} == res

    str = '~'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_tilda, 1, str}], 1} == res

    str = '&'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_and, 1, str}], 1} == res

    str = '.'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_dot, 1, str}], 1} == res

    str = '..'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_ddot, 1, str}], 1} == res

    str = ','
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_comma, 1, str}], 1} == res

    str = ';'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_semicolon, 1, str}], 1} == res

    str = '|'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_vline, 1, str}], 1} == res

    str = '()'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_lpar, 1, '('}, {:t_rpar, 1, ')'}], 1} == res

    str = '[]'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok,  [{:t_lbrack, 1, '['}, {:t_rbrack, 1, ']'}], 1} == res

    str = '{}'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok,  [{:t_lbrace, 1, '{'}, {:t_rbrace, 1, '}'}], 1} == res

    str = ':='
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_assign, 1, str}], 1} == res

    str = '^'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_arrow, 1, str}], 1} == res

    str = '='
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_equ, 1, str}], 1} == res

    str = '#'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_sharp, 1, str}], 1} == res

    str = '< >'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_less, 1, '<'}, {:t_more, 1, '>'}], 1} == res

    str = '<= >='
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_lesseq, 1, '<='}, {:t_moreeq, 1, '>='}], 1} == res

    str = ':'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_colon, 1, ':'}], 1} == res

    assert {:ok, [{:t_array, 1, _}], 1}     = :obr.string('ARRAY')
    assert {:ok, [{:t_array, 1, _}], 1}     = :obr.string(' ARRAY ')
    assert {:ok, [{:t_begin, 1, _}], 1}     = :obr.string(' BEGIN ')
    assert {:ok, [{:t_by, 1, _}], 1}        = :obr.string(' BY ')
    assert {:ok, [{:t_case, 1, _}], 1}      = :obr.string(' CASE ')
    assert {:ok, [{:t_const, 1, _}], 1}     = :obr.string(' CONST ')
    assert {:ok, [{:t_div, 1, _}], 1}       = :obr.string(' DIV ')
    assert {:ok, [{:t_do, 1, _}], 1}        = :obr.string(' DO ')
    assert {:ok, [{:t_else, 1, _}], 1}      = :obr.string(' ELSE ')
    assert {:ok, [{:t_elseif, 1, _}], 1}    = :obr.string(' ELSEIF ')
    assert {:ok, [{:t_end, 1, _}], 1}       = :obr.string(' END ')
    assert {:ok, [{:t_false, 1, _}], 1}     = :obr.string(' FALSE ')
    assert {:ok, [{:t_for, 1, _}], 1}       = :obr.string(' FOR ')
    assert {:ok, [{:t_if, 1, _}], 1}        = :obr.string(' IF ')
    assert {:ok, [{:t_import, 1, _}], 1}    = :obr.string(' IMPORT ')
    assert {:ok, [{:t_in, 1, _}], 1}        = :obr.string(' IN ')
    assert {:ok, [{:t_is, 1, _}], 1}        = :obr.string(' IS ')
    assert {:ok, [{:t_mod, 1, _}], 1}       = :obr.string(' MOD ')
    assert {:ok, [{:t_module, 1, _}], 1}    = :obr.string(' MODULE ')
    assert {:ok, [{:t_nil, 1, _}], 1}       = :obr.string(' NIL ')
    assert {:ok, [{:t_of, 1, _}], 1}        = :obr.string(' OF ')
    assert {:ok, [{:t_or, 1, _}], 1}        = :obr.string(' OR ')
    assert {:ok, [{:t_pointer, 1, _}], 1}   = :obr.string(' POINTER ')
    assert {:ok, [{:t_procedure, 1, _}], 1} = :obr.string(' PROCEDURE ')
    assert {:ok, [{:t_record, 1, _}], 1}    = :obr.string(' RECORD ')
    assert {:ok, [{:t_repeat, 1, _}], 1}    = :obr.string(' REPEAT ')
    assert {:ok, [{:t_return, 1, _}], 1}    = :obr.string(' RETURN ')
    assert {:ok, [{:t_to, 1, _}], 1}        = :obr.string(' TO ')
    assert {:ok, [{:t_true, 1, _}], 1}      = :obr.string(' TRUE ')
    assert {:ok, [{:t_type, 1, _}], 1}      = :obr.string(' TYPE ')
    assert {:ok, [{:t_var, 1, _}], 1}       = :obr.string(' VAR ')
    assert {:ok, [{:t_while, 1, _}], 1}     = :obr.string(' WHILE ')
    assert {:ok, [{:t_then, 1, _}], 1}      = :obr.string(' THEN ')
    assert {:ok, [{:t_until, 1, _}], 1}     = :obr.string(' UNTIL ')

    assert {:ok, [{:t_abs, 1, _}], 1}     = :obr.string(' ABS ')
    assert {:ok, [{:t_asr, 1, _}], 1}     = :obr.string(' ASR ')
    assert {:ok, [{:t_assert, 1, _}], 1}  = :obr.string(' ASSERT ')
    assert {:ok, [{:t_boolean, 1, _}], 1} = :obr.string(' BOOLEAN ')
    assert {:ok, [{:t_byte, 1, _}], 1}    = :obr.string(' BYTE ')
    assert {:ok, [{:t_char, 1, _}], 1}    = :obr.string(' CHAR ')
    assert {:ok, [{:t_chr, 1, _}], 1}     = :obr.string(' CHR ')
    assert {:ok, [{:t_dec, 1, _}], 1}     = :obr.string(' DEC ')
    assert {:ok, [{:t_excl, 1, _}], 1}    = :obr.string(' EXCL ')
    assert {:ok, [{:t_floor, 1, _}], 1}   = :obr.string(' FLOOR ')
    assert {:ok, [{:t_flt, 1, _}], 1}     = :obr.string(' FLT ')
    assert {:ok, [{:t_incl, 1, _}], 1}    = :obr.string(' INCL ')
    assert {:ok, [{:t_inc, 1, _}], 1}     = :obr.string(' INC ')
    assert {:ok, [{:t_integer, 1, _}], 1} = :obr.string(' INTEGER ')
    assert {:ok, [{:t_len, 1, _}], 1}     = :obr.string(' LEN ')
    assert {:ok, [{:t_lsl, 1, _}], 1}     = :obr.string(' LSL ')
    assert {:ok, [{:t_new, 1, _}], 1}     = :obr.string(' NEW ')
    assert {:ok, [{:t_odd, 1, _}], 1}     = :obr.string(' ODD ')
    assert {:ok, [{:t_ord, 1, _}], 1}     = :obr.string(' ORD ')
    assert {:ok, [{:t_pack, 1, _}], 1}    = :obr.string(' PACK ')
    assert {:ok, [{:t_real, 1, _}], 1}    = :obr.string(' REAL ')
    assert {:ok, [{:t_ror, 1, _}], 1}     = :obr.string(' ROR ')
    assert {:ok, [{:t_set, 1, _}], 1}     = :obr.string(' SET ')
    assert {:ok, [{:t_unpk, 1, _}], 1}    = :obr.string(' UNPK ')

    # PLUS       = \+
    assert {:ok, [{:t_plus, 1, _}], 1}         = :obr.string(' + ')
    # MINUS      = \-
    assert {:ok, [{:t_minus, 1, _}], 1}        = :obr.string(' - ')
    # MUL        = \*
    assert {:ok, [{:t_mul, 1, _}], 1}          = :obr.string(' * ')
    # DIVIDE     = /
    assert {:ok, [{:t_divide, 1, _}], 1}       = :obr.string(' / ')
    # TILDA      = ~
    assert {:ok, [{:t_tilda, 1, _}], 1}        = :obr.string(' ~ ')
    # AND        = &
    assert {:ok, [{:t_and, 1, _}], 1}          = :obr.string(' & ')
    # DDOT       = \.\.
    assert {:ok, [{:t_ddot, 1, _}], 1}         = :obr.string(' .. ')
    # DOT        = \.
    assert {:ok, [{:t_dot, 1, _}], 1}          = :obr.string(' . ')
    # COMMA      = ,
    assert {:ok, [{:t_comma, 1, _}], 1}        = :obr.string(' , ')
    # SEMICOLON  = ;
    assert {:ok, [{:t_semicolon, 1, _}], 1}    = :obr.string(' ; ')
    # VLINE      = \|
    assert {:ok, [{:t_vline, 1, _}], 1}        = :obr.string(' | ')
    # LPAR       = \(
    assert {:ok, [{:t_lpar, 1, _}], 1}         = :obr.string(' ( ')
    # RPAR       = \)
    assert {:ok, [{:t_rpar, 1, _}], 1}         = :obr.string(' ) ')
    # LBRACK     = \[
    assert {:ok, [{:t_lbrack, 1, _}], 1}       = :obr.string(' [ ')
    # RBRACK     = \]
    assert {:ok, [{:t_rbrack, 1, _}], 1}       = :obr.string(' ] ')
    # LBRACE     = \{
    assert {:ok, [{:t_lbrace, 1, _}], 1}       = :obr.string(' { ')
    # RBRACE     = \}
    assert {:ok, [{:t_rbrace, 1, _}], 1}       = :obr.string(' } ')
    # ASSIGN     = \:\=
    assert {:ok, [{:t_assign, 1, _}], 1}       = :obr.string(' := ')
    # COLON      = \:
    assert {:ok, [{:t_colon, 1, _}], 1}        = :obr.string(' : ')
    # ARROW      = \^
    assert {:ok, [{:t_arrow, 1, _}], 1}        = :obr.string(' ^ ')
    # EQU        = \=
    assert {:ok, [{:t_equ, 1, _}], 1}          = :obr.string(' = ')
    # SHARP      = \#
    assert {:ok, [{:t_sharp, 1, _}], 1}        = :obr.string(' # ')
    # LESSEQ     = \<\=
    assert {:ok, [{:t_lesseq, 1, _}], 1}       = :obr.string(' <= ')
    # MOREEQ     = \>\=
    assert {:ok, [{:t_moreeq, 1, _}], 1}       = :obr.string(' >= ')
    # LESS       = \<
    assert {:ok, [{:t_less, 1, _}], 1}         = :obr.string(' < ')
    # MORE       = \>
    assert {:ok, [{:t_more, 1, _}], 1}         = :obr.string(' > ')


    str =  'MODULE ASCII;\n'
        ++ 'IMPORT Console;\n'
        ++ 'VAR\n'
        ++ '  n: SHORTINT;\n'
        ++ 'BEGIN\n'
        ++ '  FOR n := 32-1 TO 127-1 DO Console.WriteCh(CHR(n+1)) END;\n'
        ++ 'END ASCII.\n'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"

    # str = '(* some comment *)'
    # res = :obr.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], 1} == res

    # str = '(* some (*  \r\n  comment2 *)\r\n comment *)'
    # res = :obr.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], _} = res

    # str =  '(* (* 1 *)\n'
    #     ++ '(* 2 *) *)'
    # res = :obr.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], _} = res

    # # должно быть невалидно
    # str =  '(* ляляля\n'
    #     ++ 'Print("    *)       ");\n'
    #     ++ '*)'
    # res = :obr.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # {:error, {1, :obr, {:illegal, sym}}, 1} = res

    # # assert {:ok, [{:comments, 1, str}], _} = res
    # Logger.debug ">>>>>> sym=#{sym}"




    # TODO должно быть невалидно
    # str =  '(*DEFINITION Abc;\n'
    #     ++ '\n'
    #     ++ 'PROCEDURE A1; (* Эта процедура делает тото1 *)\n'
    #     ++ 'PROCEDURE A2; (* Эта процедура делает тото1 *)\n'
    #     ++ '123\n'
    # res = :obr.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # assert {:ok, [{:comments, 1, str}], _} = res
    # Logger.debug ">>>>>> str=#{str}"

    # TODO должно быть валидно
    # Oleg N. Cher, [08.02.18 02:19]
    # Вот в этом кейсе:

    # (* ляляля

    # Print("    *)       ");

    # *)

    # Коммент вот:

    # (* ляляля

    # Print("    *)

    # Oleg N. Cher, [08.02.18 02:20]
    # А это:

    #        ");

    # *)
    # уже не коммент


    # Oleg N. Cher, [08.02.18 02:20]
    # притом это то же самое что и:

    # (*
    # "
    # *)


  end
end
