defmodule T do
  def new({:integer, _str, _value} = tuple), do: NUMBER.new(tuple)
  def new({:real, _str, _value} = tuple), do: NUMBER.new(tuple)
  def new({:string, _str, _value} = tuple), do: STRING.new(tuple)
  def new({:char, _str, _value} = tuple), do: CHAR.new(tuple)
  def new({:character, _str, _value} = tuple), do: CHARACTER.new(tuple)
  def new({:importlist, _str, _value} = tuple), do: IMPORT_LIST.new(tuple)
  def new({:import, _str, _value} = tuple), do: IMPORT.new(tuple)
  def new({:qualident, _str, _value} = tuple), do: QUAL_IDENT.new(tuple)
  def new({:ident, _str, _value} = tuple), do: IDENT.new(tuple)
  def new({:identlist, _str, _value} = tuple), do: IDENT_LIST.new(tuple)
  def new({:identdef, _str, _value} = tuple), do: IDENT_DEF.new(tuple)
  def new({:factor, _str, _value} = tuple), do: FACTOR.new(tuple)
  def new({:term, _str, _value} = tuple), do: TERM.new(tuple)
  def new({:t_nil, _str, _value} = tuple), do: NIL.new(tuple)
  def new({:t_true, _str, _value} = tuple), do: TRUE.new(tuple)
  def new({:t_false, _str, _value} = tuple), do: FALSE.new(tuple)
  def new({:set, _str, _value} = tuple), do: SET.new(tuple)
  def new({:element, _str, _value} = tuple), do: ELEMENT.new(tuple)
  def new({:expression, _str, _value} = tuple), do: EXPRESSION.new(tuple)
  def new({:simpleexpression, _str, _value} = tuple), do: SIMPLE_EXPRESSION.new(tuple)
  def new({:t_mul = type, str, value}), do: MUL_OPERATOR.new({type, str, value})
  def new({:t_divide = type, str, value}), do: MUL_OPERATOR.new({type, str, value})
  def new({:t_div = type, str, value}), do: MUL_OPERATOR.new({type, str, value})
  def new({:t_mod = type, str, value}), do: MUL_OPERATOR.new({type, str, value})
  def new({:t_and = type, str, value}), do: MUL_OPERATOR.new({type, str, value})
  def new({:t_plus = type, str, value}), do: ADD_OPERATOR.new({type, str, value})
  def new({:t_minus = type, str, value}), do: ADD_OPERATOR.new({type, str, value})
  def new({:t_or = type, str, value}), do: ADD_OPERATOR.new({type, str, value})

  def new({:designator, _str, _value} = tuple), do: DESIGNATOR.new(tuple)

  def new({:t_equ = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_sharp = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_less = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_lesseq = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_more = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_moreeq = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_in = type, str, value}), do: RELATION.new({type, str, value})
  def new({:t_is = type, str, value}), do: RELATION.new({type, str, value})

  def new({:assignment, _str, _value} = tuple), do: ASSIGMENT.new(tuple)
  def new({:statement, _str, _value} = tuple), do: STATEMENT.new(tuple)
  def new({:constexpression, _str, _value} = tuple), do: CONST_EXPRESSION.new(tuple)
  def new({:constantdeclaration, _str, _value} = tuple), do: CONSTANT_DECLARATION.new(tuple)
  def new({:explist, _str, _value} = tuple), do: EXP_LIST.new(tuple)
  def new({:actualparameters, _str, _value} = tuple), do: ACTUAL_PARAMETERS.new(tuple)

  def new({:fieldlist, _str, _value} = tuple), do: FIELD_LIST.new(tuple)
  def new({:fieldlistsequence, _str, _value} = tuple), do: FIELD_LIST_SEQUENCE.new(tuple)

  def new({:type, _str, _value} = tuple), do: TYPE.new(tuple)
  def new({:structype, _str, _value} = tuple), do: STRUC_TYPE.new(tuple)
  def new({:arraytype, _str, _value} = tuple), do: ARRAY_TYPE.new(tuple)
  def new({:length, _str, _value} = tuple), do: LENGTH.new(tuple)
  def new({:recordtype, _str, _value} = tuple), do: RECORD_TYPE.new(tuple)
  def new({:proceduretype, _str, _value} = tuple), do: PROCEDURE_TYPE.new(tuple)
  def new({:formaltype, _str, _value} = tuple), do: FORMAL_TYPE.new(tuple)
  def new({:pointertype, _str, _value} = tuple), do: POINTER_TYPE.new(tuple)

  def new({:fpsection, _str, _value} = tuple), do: FP_SECTION.new(tuple)
  def new({:formalparameters, _str, _value} = tuple), do: FORMAL_PARAMETERS.new(tuple)

  def new({:variabledeclaration, _str, _value} = tuple), do: VARIABLE_DECLARATION.new(tuple)

  def new({:procedureheading, _str, _value} = tuple), do: PROCEDURE_HEADING.new(tuple)

  def new({:typedeclaration, _str, _value} = tuple), do: TYPE_DECLARATION.new(tuple)
  def new({:label, _str, _value} = tuple), do: LABEL.new(tuple)
  def new({:labelrange, _str, _value} = tuple), do: LABEL_RANGE.new(tuple)
  def new({:caselabellist, _str, _value} = tuple), do: CASE_LABEL_LIST.new(tuple)
  
  def new({:procedurecall, _str, _value} = tuple), do: PROCEDURE_CALL.new(tuple)
  def new({:statementsequence, _str, _value} = tuple), do: STATEMENT_SEQUENCE.new(tuple)
  def new({:ntcase, _str, _value} = tuple), do: CASE.new(tuple)
  def new({:casestatement, _str, _value} = tuple), do: CASE_STATEMENT.new(tuple)
  def new({:ifstatement, _str, _value} = tuple), do: IF_STATEMENT.new(tuple)

  def new({:va_t_if, _, _} = tuple), do: IF_CASE_IF.new(tuple)
  def new({:va_t_elsif, _, _} = tuple), do: IF_CASE_ELSIF.new(tuple)
  def new({:va_t_else, _} = tuple), do: IF_CASE_ELSE.new(tuple)

  def new({:repeatstatement, _str, _value} = tuple), do: REPEAT_STATEMENT.new(tuple)

  def new({:while_do, _, _} = tuple), do: WHILE_DO.new(tuple)
  def new({:while_else_do, _, _} = tuple), do: WHILE_ELSE_DO.new(tuple)

  def new({:whilestatement, _str, _value} = tuple), do: WHILE_STATEMENT.new(tuple)
  def new({:forstatement, _str, _value} = tuple), do: FOR_STATEMENT.new(tuple)
  
  def new({:proceduredeclaration, _str, _value} = tuple), do: PROCEDURE_DECLARATION.new(tuple)
  def new({:procedurebody, _str, _value} = tuple), do: PROCEDURE_BODY.new(tuple)

  def new({:declarationsequence, _str, _value} = tuple), do: DECLARATION_SEQUENCE.new(tuple)

  def new({:decl_const, _str, _value} = tuple), do: DECLARATION_SEQUENCE.new(tuple)
  def new({:decl_type, _str, _value} = tuple), do: DECLARATION_SEQUENCE.new(tuple)
  def new({:decl_var, _str, _value} = tuple), do: DECLARATION_SEQUENCE.new(tuple)

  def new({:module, _str, _value} = tuple), do: MODULE.new(tuple)
end

defmodule MODULE do
  defstruct ident: nil, import_list: nil, declaration_sequence: nil, statement_sequence: nil, ident_end: nil
  def new({:module, _str, {ident, import_list, declaration_sequence, statement_sequence, ident_end}}), do: %__MODULE__{ident: IDENT.new(ident), import_list: import_list, declaration_sequence: declaration_sequence, statement_sequence: statement_sequence, ident_end: IDENT.new(ident_end)}
end

defmodule DECLARATION_SEQUENCE do
  defstruct constant_declaration: nil, type_declaration: nil, variable_declaration: nil, procedure_declaration: nil
  def new({:declarationsequence, _str, {constant_declaration, type_declaration, variable_declaration, procedure_declaration}}), do: %__MODULE__{constant_declaration: constant_declaration, type_declaration: type_declaration, variable_declaration: variable_declaration, procedure_declaration: procedure_declaration}
end

defmodule VARIABLE_DECLARATION do
  defstruct str: nil, value: nil
  def new({:variabledeclaration, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule IDENT do
  defstruct str: nil, value: nil
  def new({:ident, str, value}), do: %__MODULE__{str: str, value: to_string(value)}
end

defmodule NUMBER do
  defstruct str: nil, value: nil, type: nil
  def new({type, str, value}), do: %__MODULE__{str: str, value: value, type: type}
end

defmodule STRING do
  defstruct str: nil, value: nil
  def new({:string, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule CHARACTER do
  defstruct str: nil, value: nil
  def new({:character, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule CHAR do
  defstruct str: nil, value: nil, type: nil
  def new({type, str, value}), do: %__MODULE__{str: str, value: value, type: type}
end

defmodule IMPORT do
  defstruct str: nil, module_name: nil, module_alias: nil
  def new({:import, str, {module_name, module_alias}}), do: %__MODULE__{str: str, module_name: to_string(module_name), module_alias: to_string(module_alias)}
end

defmodule IMPORT_LIST do
  defstruct str: nil, value: nil
  def new({:importlist, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule QUAL_IDENT do
  defstruct str: nil, value: nil
  def new({:qualident, str, value}), do: %__MODULE__{str: str, value: to_string(value)}
end

defmodule IDENT_DEF do
  defstruct str: nil, value: nil
  def new({:identdef, str, value}), do: %__MODULE__{str: str, value: to_string(value)}
end

defmodule FACTOR do
  defstruct str: nil, value: nil
  def new({:factor, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule TERM do
  defstruct str: nil, value: nil
  def new({:term, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule NIL do
  defstruct str: nil
  def new({:t_nil, str, _}), do: %__MODULE__{str: str}
end

defmodule TRUE do
  defstruct str: nil
  def new({:t_true, str, _}), do: %__MODULE__{str: str}
end

defmodule FALSE do
  defstruct str: nil
  def new({:t_false, str, _}), do: %__MODULE__{str: str}
end

defmodule SET do
  defstruct str: nil, value: nil
  def new({:set, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule ELEMENT do
  defstruct str: nil, value: nil
  def new({:element, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule ELEMENT_DIAPAZONE do
  defstruct value_start: nil, value_end: nil
  def new(value_start, value_end), do: %__MODULE__{value_start: value_start, value_end: value_end}
end

defmodule EXPRESSION do
  defstruct str: nil, value: nil
  def new({:expression, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule SIMPLE_EXPRESSION do
  defstruct str: nil, value: nil
  def new({:simpleexpression, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule MUL_OPERATOR do
  defstruct str: nil, value: nil
  def new({_type, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule ADD_OPERATOR do
  defstruct str: nil, value: nil
  def new({_type, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule UNARY_OPERATOR do
  defstruct str: nil, value: nil
  def new({_type, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule DESIGNATOR do
  defstruct str: nil, value: nil
  def new({:designator, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule RELATION do
  defstruct str: nil, value: nil
  def new({_type, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule ASSIGMENT do
  defstruct str: nil, designator: nil, expression: nil
  def new({:assignment, str, {designator, expression}}), do: %__MODULE__{str: str, designator: designator, expression: expression}
end

defmodule STATEMENT do
  defstruct str: nil, value: nil
  def new({:statement, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule CONST_EXPRESSION do
  defstruct str: nil, expression: nil
  def new({:constexpression, str, expression}), do: %__MODULE__{str: str, expression: expression}
end

defmodule CONSTANT_DECLARATION do
  defstruct str: nil, ident_def: nil, const_expression: nil
  def new({:constantdeclaration, str, {ident_def, const_expression}}), do: %__MODULE__{str: str,  ident_def: ident_def, const_expression: const_expression}
end

defmodule EXP_LIST do
  defstruct str: nil, value: nil
  def new({:explist, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule ACTUAL_PARAMETERS do
  defstruct str: nil, value: nil
  def new({:actualparameters, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule IDENT_LIST do
  defstruct str: nil, value: nil
  def new({:identlist, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule FIELD_LIST do
  defstruct str: nil, ident_list: nil, type: nil
  def new({:fieldlist, str, {ident_list, type}}), do: %__MODULE__{str: str, ident_list: ident_list, type: type}
end

defmodule FIELD_LIST_SEQUENCE do
  defstruct str: nil, value: nil
  def new({:fieldlistsequence, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule TYPE do
  defstruct str: nil, value: nil
  def new({:type, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule STRUC_TYPE do
  defstruct str: nil, type: nil
  def new({:structype, str, type}), do: %__MODULE__{str: str, type: type}
end

defmodule ARRAY_TYPE do
  defstruct str: nil, len: nil, type: nil
  def new({:arraytype, str, {len, type}}), do: %__MODULE__{str: str, len: len, type: type}
end

defmodule LENGTH do
  defstruct str: nil, value: nil
  def new({:length, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule RECORD_TYPE do
  defstruct str: nil, field_list_sequence: nil, base_type: nil
  def new({:recordtype, str, {base_type, field_list_sequence}}), do: %__MODULE__{str: str, field_list_sequence: field_list_sequence, base_type: base_type}
end

defmodule PROCEDURE_TYPE do
  defstruct str: nil, value: nil
  def new({:proceduretype, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule FORMAL_TYPE do
  defstruct str: nil, qual_ident: nil, array_of_steps: nil
  def new({:formaltype, str, {qual_ident, array_of_steps}}), do: %__MODULE__{str: str, qual_ident: qual_ident, array_of_steps: array_of_steps}
end

defmodule POINTER_TYPE do
  defstruct str: nil, pointer_type: nil
  def new({:pointertype, str, pointer_type}), do: %__MODULE__{str: str, pointer_type: pointer_type}
end

defmodule FP_SECTION do
  defstruct str: nil, ident_list: nil, var: nil, type: nil
  def new({:fpsection, str, {var, ident_list, type}}), do: %__MODULE__{str: str, ident_list: ident_list, var: var==:var, type: type}
end

defmodule FORMAL_PARAMETERS do
  defstruct str: nil, formal_parameters_list: nil, qual_ident: nil
  def new({:formalparameters, str, {formal_parameters_list, qual_ident}}), do: %__MODULE__{str: str, formal_parameters_list: formal_parameters_list, qual_ident: qual_ident}
end

defmodule PROCEDURE_HEADING do
  defstruct str: nil, ident_def: nil, formal_parameters: nil
  def new({:procedureheading, str, {ident_def, formal_parameters}}), do: %__MODULE__{str: str, ident_def: ident_def, formal_parameters: formal_parameters}
end

defmodule TYPE_DECLARATION do
  defstruct str: nil, ident_def: nil, struc_type: nil
  def new({:typedeclaration, str, {ident_def, struc_type}}), do: %__MODULE__{str: str, ident_def: ident_def, struc_type: struc_type}
end

defmodule LABEL do
  defstruct str: nil, value: nil
  def new({:label, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule LABEL_RANGE do
  defstruct str: nil, start: nil, finish: nil
  def new({:labelrange, str, {start, finish}}), do: %__MODULE__{str: str, start: start, finish: finish}
end

defmodule CASE_LABEL_LIST do
  defstruct str: nil, value: nil
  def new({:caselabellist, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule PROCEDURE_CALL do
  defstruct str: nil, designator: nil, actualparameters: nil
  def new({:procedurecall, str, {designator, actualparameters}}), do: %__MODULE__{str: str, designator: designator, actualparameters: actualparameters}
end

defmodule STATEMENT_SEQUENCE do
  defstruct str: nil, value: nil
  def new({:statementsequence, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule CASE do
  defstruct str: nil, case_label_list: nil, statement_sequence: nil
  def new({:ntcase, nil, nil}), do: %__MODULE__{}
  def new({:ntcase, str, {case_label_list, statement_sequence}}), do: %__MODULE__{str: str, case_label_list: case_label_list, statement_sequence: statement_sequence}
end

defmodule CASE_STATEMENT do
  defstruct str: nil, expression: nil, case_list: nil
  def new({:casestatement, str, {expression, case_list}}), do: %__MODULE__{str: str, expression: expression, case_list: case_list}
end

defmodule IF_STATEMENT do
  defstruct str: nil, value: nil
  def new({:ifstatement, str, value}), do: %__MODULE__{str: str, value: value}
end





defmodule IF_CASE_IF do
  defstruct str: nil, expression: nil, sequence: nil
  def new({:va_t_if, expression, sequence}), do: %__MODULE__{expression: expression, sequence: sequence}
end

defmodule IF_CASE_ELSIF do
  defstruct str: nil, expression: nil, sequence: nil
  def new({:va_t_elsif, expression, sequence}), do: %__MODULE__{expression: expression, sequence: sequence}
end

defmodule IF_CASE_ELSE do
  defstruct str: nil, sequence: nil
  def new({:va_t_else, sequence}), do: %__MODULE__{sequence: sequence}
end


defmodule REPEAT_STATEMENT do
  defstruct str: nil, sequence: nil, expression: nil
  def new({:repeatstatement, str, {sequence, expression}}), do: %__MODULE__{str: str, expression: expression, sequence: sequence}
end

defmodule WHILE_DO do
  defstruct str: nil, expression: nil, sequence: nil
  def new({:while_do, expression, sequence}), do: %__MODULE__{expression: expression, sequence: sequence}
end

defmodule WHILE_ELSE_DO do
  defstruct str: nil, expression: nil, sequence: nil
  def new({:while_else_do, expression, sequence}), do: %__MODULE__{expression: expression, sequence: sequence}
end


defmodule WHILE_STATEMENT do
  defstruct str: nil, value: nil
  def new({:whilestatement, str, value}), do: %__MODULE__{str: str, value: value}
end

defmodule FOR_STATEMENT do
  defstruct str: nil, ident: nil, start_expression: nil, finish_expression: nil, const_expression: nil, sequence: nil
  def new({:forstatement, str, {ident, start_expression, finish_expression, const_expression, sequence}}), do: %__MODULE__{str: str, ident: ident, start_expression: start_expression, finish_expression: finish_expression, const_expression: const_expression, sequence: sequence}
end

defmodule PROCEDURE_DECLARATION do
  defstruct str: nil, procedure_heading: nil, procedure_body: nil, ident: nil
  def new({:proceduredeclaration, str, {procedure_heading, procedure_body, ident}}), do: %__MODULE__{str: str, procedure_heading: procedure_heading, procedure_body: procedure_body, ident: ident}
end

defmodule PROCEDURE_BODY do
  defstruct str: nil, declaration_sequence: nil, statement_sequence: nil, expression: nil
  def new({:procedurebody, str, {declaration_sequence, statement_sequence, expression}}), do: %__MODULE__{str: str, declaration_sequence: declaration_sequence, statement_sequence: statement_sequence, expression: expression}
end
