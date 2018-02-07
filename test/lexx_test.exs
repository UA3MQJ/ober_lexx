defmodule OberLexxTest do
  use ExUnit.Case

  require Logger

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

    str = '.'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:dot, 1, str}], 1} == res

    str = '['
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:lbrack, 1, str}], 1} == res

    str = '^'
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:arrow, 1, str}], 1} == res

    str = '('
    res = :obr.string(str)
    Logger.debug ">>>>>> str=#{str} res=#{inspect res}"
    assert {:ok, [{:lpar, 1, str}], 1} == res

  end
end
