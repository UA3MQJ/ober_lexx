defmodule OberLexxTest do
  use ExUnit.Case

  require Logger

  # mix test --only erleex
  @tag erleex: true  

  test "the truth" do
    
    time1 = :os.system_time(:millisecond)
    {:ok, _} = :leex.file('./priv/obr_lexer.xrl')
    time2 = :os.system_time(:millisecond)
    {:ok, :obr_lexer} = :c.c('./priv/obr_lexer.erl')
    time3 = :os.system_time(:millisecond)


    # число
    res1 = :obr_lexer.string('123')
    Logger.debug ">>>>>> res1=#{inspect res1}"
    assert {:ok, [{:integer, 1, '123'}], 1} == res1

    # hex число
    res2 = :obr_lexer.string('123H')
    Logger.debug ">>>>>> res2=#{inspect res2}"
    assert {:ok, [{:integer, 1, '123H'}], 1} == res2

    # нет ноля перед символами, значит это не число, а ID
    res3 = :obr_lexer.string('DEH')
    Logger.debug ">>>>>> res3=#{inspect res3}"
    assert {:ok, [{:ident, 1, 'DEH'}], 1} == res3

    # шестнадцатиричное число но тоже INT незначащй нолик убрался
    res4 = :obr_lexer.string('0DEH')
    Logger.debug ">>>>>> res4=#{inspect res4}"
    assert {:ok, [{:integer, 1, 'DEH'}], 1} == res4

    # число, но без ноликов
    res5 = :obr_lexer.string('000000123')
    Logger.debug ">>>>>> res5=#{inspect res5}"
    assert {:ok, [{:integer, 1, '123'}], 1} == res5

    # hex число, но без ноликов
    res6 = :obr_lexer.string('0000000CH')
    Logger.debug ">>>>>> res6=#{inspect res6}"
    assert {:ok, [{:integer, 1, 'CH'}], 1} == res6

    # сокращается длина ИД до 40 символов
    res7 = :obr_lexer.string('abcdedsdfsfsdjfsijdfisdjfisjdfijsdifjisjdfijisdjfivsjdivfjsdvsdfvs')
    Logger.debug ">>>>>> res7=#{inspect res7}"
    assert {:ok, [{:ident, 1, 'abcdedsdfsfsdjfsijdfisdjfisjdfijsdifjisj'}], 1} == res7
    assert {:ok, [{:ident, 1, id40}], 1} = res7
    assert length(id40) == 40

    # исправляет, добавляя H в конце
    res8 = :obr_lexer.string('000000000000000CAFE')
    Logger.debug ">>>>>> res8=#{inspect res8}"
    assert {:ok, [{:integer, 1, 'CAFEH'}], 1} == res8

    # тесты на real
    res9 = :obr_lexer.string('12.012')
    Logger.debug ">>>>>> res9=#{inspect res9}"
    assert {:ok, [{:real, 1, '12.012'}], 1} == res9

    res10 = :obr_lexer.string('3.402823466E38')
    Logger.debug ">>>>>> res10=#{inspect res10}"
    assert {:ok, [{:real, 1, '3.402823466E38'}], 1} == res10

    res11 = :obr_lexer.string('1.7976931348623158D307')
    Logger.debug ">>>>>> res11=#{inspect res11}"
    assert {:ok, [{:real, 1, '1.7976931348623158D307'}], 1} == res11

    # строка
    str = '"abc\\123"'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '"abc\"123"'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '"abc\'123"'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc123\''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc\"123\''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'abc\'123\''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '\'  \''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\'русские буквы\''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:string, 1, str}], 1} == res

    str = '\' \n \''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    str = '\' \r \''
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:error, _, _} = res

    # char - заканчивается на X
    str = '0X'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '01X'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '0AX'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '01AX'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = '0A1X'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:character, 1, str}], 1} == res

    str = 'ARRAY'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_array, 1, str}], 1} == res

    str = 'array'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:ident, 1, str}], 1} == res

    str = '+'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_plus, 1, str}], 1} == res

    str = '-'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_minus, 1, str}], 1} == res

    str = '*'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_mul, 1, str}], 1} == res

    str = '/'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_divide, 1, str}], 1} == res

    str = '~'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_tilda, 1, str}], 1} == res

    str = '&'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_and, 1, str}], 1} == res

    str = '.'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_dot, 1, str}], 1} == res

    str = '..'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_ddot, 1, str}], 1} == res

    str = ','
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_comma, 1, str}], 1} == res

    str = ';'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_semicolon, 1, str}], 1} == res

    str = '|'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_vline, 1, str}], 1} == res

    str = '()'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_lpar, 1, '('}, {:t_rpar, 1, ')'}], 1} == res

    str = '[]'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok,  [{:t_lbrack, 1, '['}, {:t_rbrack, 1, ']'}], 1} == res

    str = '{}'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok,  [{:t_lbrace, 1, '{'}, {:t_rbrace, 1, '}'}], 1} == res

    str = ':='
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_assign, 1, str}], 1} == res

    str = '^'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_arrow, 1, str}], 1} == res

    str = '='
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_equ, 1, str}], 1} == res

    str = '#'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_sharp, 1, str}], 1} == res

    str = '< >'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_less, 1, '<'}, {:t_more, 1, '>'}], 1} == res

    str = '<= >='
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_lesseq, 1, '<='}, {:t_moreeq, 1, '>='}], 1} == res

    str = ':'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:t_colon, 1, ':'}], 1} == res

    assert {:ok, [{:t_array, 1, _}], 1}     = :obr_lexer.string('ARRAY')
    assert {:ok, [{:t_array, 1, _}], 1}     = :obr_lexer.string(' ARRAY ')
    assert {:ok, [{:t_begin, 1, _}], 1}     = :obr_lexer.string(' BEGIN ')
    assert {:ok, [{:t_by, 1, _}], 1}        = :obr_lexer.string(' BY ')
    assert {:ok, [{:t_case, 1, _}], 1}      = :obr_lexer.string(' CASE ')
    assert {:ok, [{:t_const, 1, _}], 1}     = :obr_lexer.string(' CONST ')
    assert {:ok, [{:t_div, 1, _}], 1}       = :obr_lexer.string(' DIV ')
    assert {:ok, [{:t_do, 1, _}], 1}        = :obr_lexer.string(' DO ')
    assert {:ok, [{:t_else, 1, _}], 1}      = :obr_lexer.string(' ELSE ')
    assert {:ok, [{:t_elseif, 1, _}], 1}    = :obr_lexer.string(' ELSEIF ')
    assert {:ok, [{:t_end, 1, _}], 1}       = :obr_lexer.string(' END ')
    assert {:ok, [{:t_false, 1, _}], 1}     = :obr_lexer.string(' FALSE ')
    assert {:ok, [{:t_for, 1, _}], 1}       = :obr_lexer.string(' FOR ')
    assert {:ok, [{:t_if, 1, _}], 1}        = :obr_lexer.string(' IF ')
    assert {:ok, [{:t_import, 1, _}], 1}    = :obr_lexer.string(' IMPORT ')
    assert {:ok, [{:t_in, 1, _}], 1}        = :obr_lexer.string(' IN ')
    assert {:ok, [{:t_is, 1, _}], 1}        = :obr_lexer.string(' IS ')
    assert {:ok, [{:t_mod, 1, _}], 1}       = :obr_lexer.string(' MOD ')
    assert {:ok, [{:t_module, 1, _}], 1}    = :obr_lexer.string(' MODULE ')
    assert {:ok, [{:t_nil, 1, _}], 1}       = :obr_lexer.string(' NIL ')
    assert {:ok, [{:t_of, 1, _}], 1}        = :obr_lexer.string(' OF ')
    assert {:ok, [{:t_or, 1, _}], 1}        = :obr_lexer.string(' OR ')
    assert {:ok, [{:t_pointer, 1, _}], 1}   = :obr_lexer.string(' POINTER ')
    assert {:ok, [{:t_procedure, 1, _}], 1} = :obr_lexer.string(' PROCEDURE ')
    assert {:ok, [{:t_record, 1, _}], 1}    = :obr_lexer.string(' RECORD ')
    assert {:ok, [{:t_repeat, 1, _}], 1}    = :obr_lexer.string(' REPEAT ')
    assert {:ok, [{:t_return, 1, _}], 1}    = :obr_lexer.string(' RETURN ')
    assert {:ok, [{:t_to, 1, _}], 1}        = :obr_lexer.string(' TO ')
    assert {:ok, [{:t_true, 1, _}], 1}      = :obr_lexer.string(' TRUE ')
    assert {:ok, [{:t_type, 1, _}], 1}      = :obr_lexer.string(' TYPE ')
    assert {:ok, [{:t_var, 1, _}], 1}       = :obr_lexer.string(' VAR ')
    assert {:ok, [{:t_while, 1, _}], 1}     = :obr_lexer.string(' WHILE ')
    assert {:ok, [{:t_then, 1, _}], 1}      = :obr_lexer.string(' THEN ')
    assert {:ok, [{:t_until, 1, _}], 1}     = :obr_lexer.string(' UNTIL ')

    assert {:ok, [{:t_abs, 1, _}], 1}     = :obr_lexer.string(' ABS ')
    assert {:ok, [{:t_asr, 1, _}], 1}     = :obr_lexer.string(' ASR ')
    assert {:ok, [{:t_assert, 1, _}], 1}  = :obr_lexer.string(' ASSERT ')
    assert {:ok, [{:t_boolean, 1, _}], 1} = :obr_lexer.string(' BOOLEAN ')
    assert {:ok, [{:t_byte, 1, _}], 1}    = :obr_lexer.string(' BYTE ')
    assert {:ok, [{:t_char, 1, _}], 1}    = :obr_lexer.string(' CHAR ')
    assert {:ok, [{:t_chr, 1, _}], 1}     = :obr_lexer.string(' CHR ')
    assert {:ok, [{:t_dec, 1, _}], 1}     = :obr_lexer.string(' DEC ')
    assert {:ok, [{:t_excl, 1, _}], 1}    = :obr_lexer.string(' EXCL ')
    assert {:ok, [{:t_floor, 1, _}], 1}   = :obr_lexer.string(' FLOOR ')
    assert {:ok, [{:t_flt, 1, _}], 1}     = :obr_lexer.string(' FLT ')
    assert {:ok, [{:t_incl, 1, _}], 1}    = :obr_lexer.string(' INCL ')
    assert {:ok, [{:t_inc, 1, _}], 1}     = :obr_lexer.string(' INC ')
    assert {:ok, [{:t_integer, 1, _}], 1} = :obr_lexer.string(' INTEGER ')
    assert {:ok, [{:t_len, 1, _}], 1}     = :obr_lexer.string(' LEN ')
    assert {:ok, [{:t_lsl, 1, _}], 1}     = :obr_lexer.string(' LSL ')
    assert {:ok, [{:t_new, 1, _}], 1}     = :obr_lexer.string(' NEW ')
    assert {:ok, [{:t_odd, 1, _}], 1}     = :obr_lexer.string(' ODD ')
    assert {:ok, [{:t_ord, 1, _}], 1}     = :obr_lexer.string(' ORD ')
    assert {:ok, [{:t_pack, 1, _}], 1}    = :obr_lexer.string(' PACK ')
    assert {:ok, [{:t_real, 1, _}], 1}    = :obr_lexer.string(' REAL ')
    assert {:ok, [{:t_ror, 1, _}], 1}     = :obr_lexer.string(' ROR ')
    assert {:ok, [{:t_set, 1, _}], 1}     = :obr_lexer.string(' SET ')
    assert {:ok, [{:t_unpk, 1, _}], 1}    = :obr_lexer.string(' UNPK ')

    # PLUS       = \+
    assert {:ok, [{:t_plus, 1, _}], 1}         = :obr_lexer.string(' + ')
    # MINUS      = \-
    assert {:ok, [{:t_minus, 1, _}], 1}        = :obr_lexer.string(' - ')
    # MUL        = \*
    assert {:ok, [{:t_mul, 1, _}], 1}          = :obr_lexer.string(' * ')
    # DIVIDE     = /
    assert {:ok, [{:t_divide, 1, _}], 1}       = :obr_lexer.string(' / ')
    # TILDA      = ~
    assert {:ok, [{:t_tilda, 1, _}], 1}        = :obr_lexer.string(' ~ ')
    # AND        = &
    assert {:ok, [{:t_and, 1, _}], 1}          = :obr_lexer.string(' & ')
    # DDOT       = \.\.
    assert {:ok, [{:t_ddot, 1, _}], 1}         = :obr_lexer.string(' .. ')
    # DOT        = \.
    assert {:ok, [{:t_dot, 1, _}], 1}          = :obr_lexer.string(' . ')
    # COMMA      = ,
    assert {:ok, [{:t_comma, 1, _}], 1}        = :obr_lexer.string(' , ')
    # SEMICOLON  = ;
    assert {:ok, [{:t_semicolon, 1, _}], 1}    = :obr_lexer.string(' ; ')
    # VLINE      = \|
    assert {:ok, [{:t_vline, 1, _}], 1}        = :obr_lexer.string(' | ')
    # LPAR       = \(
    assert {:ok, [{:t_lpar, 1, _}], 1}         = :obr_lexer.string(' ( ')
    # RPAR       = \)
    assert {:ok, [{:t_rpar, 1, _}], 1}         = :obr_lexer.string(' ) ')
    # LBRACK     = \[
    assert {:ok, [{:t_lbrack, 1, _}], 1}       = :obr_lexer.string(' [ ')
    # RBRACK     = \]
    assert {:ok, [{:t_rbrack, 1, _}], 1}       = :obr_lexer.string(' ] ')
    # LBRACE     = \{
    assert {:ok, [{:t_lbrace, 1, _}], 1}       = :obr_lexer.string(' { ')
    # RBRACE     = \}
    assert {:ok, [{:t_rbrace, 1, _}], 1}       = :obr_lexer.string(' } ')
    # ASSIGN     = \:\=
    assert {:ok, [{:t_assign, 1, _}], 1}       = :obr_lexer.string(' := ')
    # COLON      = \:
    assert {:ok, [{:t_colon, 1, _}], 1}        = :obr_lexer.string(' : ')
    # ARROW      = \^
    assert {:ok, [{:t_arrow, 1, _}], 1}        = :obr_lexer.string(' ^ ')
    # EQU        = \=
    assert {:ok, [{:t_equ, 1, _}], 1}          = :obr_lexer.string(' = ')
    # SHARP      = \#
    assert {:ok, [{:t_sharp, 1, _}], 1}        = :obr_lexer.string(' # ')
    # LESSEQ     = \<\=
    assert {:ok, [{:t_lesseq, 1, _}], 1}       = :obr_lexer.string(' <= ')
    # MOREEQ     = \>\=
    assert {:ok, [{:t_moreeq, 1, _}], 1}       = :obr_lexer.string(' >= ')
    # LESS       = \<
    assert {:ok, [{:t_less, 1, _}], 1}         = :obr_lexer.string(' < ')
    # MORE       = \>
    assert {:ok, [{:t_more, 1, _}], 1}         = :obr_lexer.string(' > ')


    str =  'MODULE ASCII;\n'
        ++ 'IMPORT Console;\n'
        ++ 'VAR\n'
        ++ '  n: SHORTINT;\n'
        ++ 'BEGIN\n'
        ++ '  FOR n := 32-1 TO 127-1 DO Console.WriteCh(CHR(n+1)) END;\n'
        ++ 'END ASCII.\n'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"

    str = '"some test"'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"

    str = '"какой то текст"'
    res = :obr_lexer.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    {:ok, [{:string, 1, res_string}], 1}  = res
    Logger.debug ">>>>>> res_string=#{res_string}"

    Logger.debug "Leex - generate erl time = #{time2 - time1} ms"
    Logger.debug "Compile erl time = #{time3 - time2} ms"

    # str = '(* some comment *)'
    # res = :obr_lexer.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], 1} == res

    # str = '(* some (*  \r\n  comment2 *)\r\n comment *)'
    # res = :obr_lexer.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], _} = res

    # str =  '(* (* 1 *)\n'
    #     ++ '(* 2 *) *)'
    # res = :obr_lexer.string(str)
    # Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    # # assert {:ok, [{:comments, 1, str}], _} = res

    # # должно быть невалидно
    # str =  '(* ляляля\n'
    #     ++ 'Print("    *)       ");\n'
    #     ++ '*)'
    # res = :obr_lexer.string(str)
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
    # res = :obr_lexer.string(str)
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
