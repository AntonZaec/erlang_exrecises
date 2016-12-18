-module(calc).
-export([parse/1, compute/1, print/1, 
	compile/1, simulate/1, simplify/1, eval/1]).

%% Parsing of string with arithmetic expression. Brackets are required.
%% For example:
%% {exps, {unary_minus, {SubExpression}}} = parse("~(1+(3+((4+5)*7))").
%% if statement is allowed.
%% For example: "(1+(if (2-1) then (3+4) else (5-6)))"
parse(String) ->
	Tokens = tokenize(String),
	{Result, _} = parse_impl(Tokens, {}),
	{exps, Result}.

%% Computing result of parsing function.
compute(Expression) ->
	{exps, ExpressionData} = Expression,
	compute_impl(ExpressionData).

%% Printing result of parsing function.
print(Expression) ->
	{exps, ExpressionData} = Expression,
	print_impl(ExpressionData),
	io:format("~n").

%% Transformation of parsing result to stack form 
%% (reverse polish notation).
compile(Expression) ->
	{exps, ExpressionData} = Expression,
	{stack_actions, compile_impl(ExpressionData, [])}.

%% Simulation of compilation result.
%% compute(parse("(1+2)")) =:= simulate(compile(parse("(1+2)"))).
simulate(StackActions) ->
	{stack_actions, Stack} = StackActions,
	{Result, _} = simulate_impl(Stack),
	Result.

%% Simplification of parsing result.
%% For example, "(3+0)*1" will be "3".
simplify(Expression) ->
	{exps, ExpressionData} = Expression,
	{exps, simplify_impl(simplify_impl(ExpressionData))}.

%% Evaluate computing of expression in string.
eval(String) ->
	compute(simplify(parse(String))).

print_impl({number, Number}) ->
	io:format("~B", [Number]);
print_impl({Operator, Operand}) ->
	io:format("(~s", [get_operator_str(Operator)]),
	print_impl(Operand),
	io:format(")");
print_impl({Operator, Operand1, Operand2}) ->
	io:format("("),
	print_impl(Operand1),
	io:format("~s", [get_operator_str(Operator)]),
	print_impl(Operand2),
	io:format(")");
print_impl({if_then_else, Operand1, Operand2, Operand3}) ->
	io:format("if "),
	print_impl(Operand1),
	io:format(" then "),
	print_impl(Operand2),
	io:format(" else "),
	print_impl(Operand3).

compute_impl({number, Number}) ->
	Number;
compute_impl({unary_minus, Expression}) ->
	Operand = compute_impl(Expression),
	do_unary_operation(unary_minus, Operand);
compute_impl({Operator, LeftExpression, RightExpression}) ->
	Operand1 = compute_impl(LeftExpression),
	Operand2 = compute_impl(RightExpression),
	do_binary_operation(Operator, Operand1, Operand2);
compute_impl({if_then_else, Expression1, Expression2, Expression3}) ->
	Operand1 = compute_impl(Expression1),
	case Operand1 of
		0 -> compute_impl(Expression2);
		_ -> compute_impl(Expression3)
	end.

simplify_impl({number, Value}) ->
	{number, Value};
simplify_impl({unary_minus, Expression}) ->
	SimplifiedExpression = simplify_impl(Expression),
	case SimplifiedExpression of
		{number, 0} -> {number, 0};
		{number, Value} -> {number, -Value};
		_ -> {unary_minus, SimplifiedExpression}
	end;
simplify_impl({if_then_else, Operand1, Operand2, Operand3}) ->
	SimplifiedOperand1 = simplify_impl(Operand1),
	case SimplifiedOperand1 of
		{number, 0} -> simplify_impl(Operand3);
		{number, _} -> simplify_impl(Operand2);
		_ -> 
			{if_then_else, SimplifiedOperand1, 
				simplify_impl(Operand2), simplify_impl(Operand3)}
	end;
simplify_impl({Operator, Operand1, Operand2}) ->
	SimplifiedOperand1 = simplify_impl(Operand1),
	SimplifiedOperand2 = simplify_impl(Operand2),
	simplify_operation({Operator, SimplifiedOperand1, SimplifiedOperand2}).

simplify_operation({minus, Operand1, Operand2}) ->
	case {Operand1, Operand2} of
		{{number, 0}, {number, 0}} -> {number, 0};
		{{number, 0}, _} -> {unary_minus, Operand2};
		{_, {number, 0}} -> Operand1;
		_ -> {minus, Operand1, Operand2}
	end;
simplify_operation({plus, Operand1, Operand2}) ->
	case {Operand1, Operand2} of
		{{number, 0}, {number, 0}} -> {number, 0};
		{{number, 0}, _} -> Operand2;
		{_, {number, 0}} -> Operand1;
		_ -> {plus, Operand1, Operand2}
	end;
simplify_operation({multiply, Operand1, Operand2}) ->
	case {Operand1, Operand2} of
		{{number, 0}, _} -> {number, 0};
		{_, {number, 0}} -> {number, 0};
		{{number, 1}, {number, 1}} -> {number, 1};
		{{number, 1}, _} -> Operand2;
		{_, {number, 1}} -> Operand1;
		{{number, -1}, {number, -1}} -> {number, 1};
		{{number, -1}, _} -> {unary_minus, Operand2};
		{_, {number, -1}} -> {unary_minus, Operand1};
		_ -> {multiply, Operand1, Operand2}
	end;
simplify_operation({division, Operand1, Operand2}) ->
	case {Operand1, Operand2} of
		{_, {number, 0}} -> {division, Operand1, Operand2};
		{{number, 0}, _} -> {number, 0};
		{{number, 1}, {number, 1}} -> {number, 1};
		{_, {number, 1}} -> Operand1;
		{{number, -1}, {number, -1}} -> {number, 1};
		{_, {number, -1}} -> {unary_minus, Operand1};
		_ -> {division, Operand1, Operand2}
	end.

simulate_impl([H|T]) when is_number(H)->
	{H, T};
simulate_impl([unary_minus|T]) ->
	{Operand, NewStack} = simulate_impl(T),
	{do_unary_operation(unary_minus, Operand), NewStack};
simulate_impl([if_then_else|T]) ->
	{Operand1, NewStack} = simulate_impl(T),
	case Operand1 of
		0 -> simulate_impl(unroll_stack(NewStack));
		_ -> 
			{ThenResult, StackWithElse} = simulate_impl(NewStack),
			StackWithoutElse = unroll_stack(StackWithElse),
			{ThenResult, StackWithoutElse}
	end;
simulate_impl([H|T]) ->
	{Operand2, NewStack} = simulate_impl(T),
	{Operand1, NewStack2} = simulate_impl(NewStack),
	{do_binary_operation(H, Operand1, Operand2), NewStack2}.

unroll_stack([H|T]) when is_number(H)-> T;
unroll_stack([unary_minus|T]) -> unroll_stack(T);
unroll_stack([if_then_else|T]) ->
	%Unroll three operands
	unroll_stack(unroll_stack(unroll_stack(T)));
unroll_stack([_|T]) ->
	%Unroll two operands
	unroll_stack(unroll_stack(T));
unroll_stack([]) -> [].

do_unary_operation(Operation, Operand) when Operation =:= unary_minus ->
	-Operand.

do_binary_operation(Operation, Operand1, Operand2) -> 
	case Operation of
		plus -> Operand1 + Operand2;
		minus -> Operand1 - Operand2;
		multiply -> Operand1*Operand2;
		division -> Operand1/Operand2
	end.

compile_impl({number, Value}, Stack) ->
	[Value|Stack];
compile_impl({Operator, Operand1}, Stack) ->
	[Operator|compile_impl(Operand1, Stack)];
compile_impl({Operator, Operand1, Operand2}, Stack) ->
	[Operator|compile_impl(Operand2, compile_impl(Operand1, Stack))];
compile_impl({if_then_else, Operand1, Operand2, Operand3}, Stack) ->
	[if_then_else|compile_impl(Operand1, 
		compile_impl(Operand2, compile_impl(Operand3, Stack)))].
	
tokenize([]) ->
	[];
tokenize([H|T]) ->
	State = if 
			H >= $0 andalso H =< $9 -> read_number;
			H >= $A andalso H =< $z -> read_string;
			H =:= 32 -> skip_space;
			true -> read_operator
		end,
	String = [H|T],
	{NewToken, NextCharacters} = case State of
		read_operator -> {{operator, get_operator_name(H)}, T};
		read_number -> 
			{Number, StringWithoutNumber} = extrude_number(String, ""),
			{{number, Number}, StringWithoutNumber};
		read_string -> 
			{NewString, NextCharactersTmp} = extrude_string(String, ""),
			{convert_str_to_token(NewString), NextCharactersTmp};
		skip_space -> {none, T}
	end,
	case State of
		skip_space -> tokenize(T);
		_ -> [NewToken|tokenize(NextCharacters)]
	end.

parse_impl([], Result) -> {Result, []};
parse_impl(Tokens, Result) ->
	{NewResult, NewNextTokens, MustContinue} = case Tokens of
		[{keyword, 'if'}, {operator, open_bracket}|IfExpressionTokens] ->
			{SubResultAfterIf, NextTokensAfterIf} = 
				parse_impl(IfExpressionTokens, {}),
			[{keyword, 'then'}, {operator, open_bracket}|ThenExpressionTokens] = 
				NextTokensAfterIf,
			{SubResultAfterThen, NextTokensAfterThen} = 
				parse_impl(ThenExpressionTokens, {}),
			[{keyword, 'else'}|ElseExpressionTokens] = NextTokensAfterThen,
			{SubResultAfterElse, NextTokensAfterElse} = 
				parse_impl(ElseExpressionTokens, {}),
			{{if_then_else, SubResultAfterIf, SubResultAfterThen, SubResultAfterElse},
				NextTokensAfterElse, false};
		[{operator, unary_minus},{operator, open_bracket}|T] -> 
			%This case is needed for parsing expressions like "(~(1+2)-3)".
			%If we don't think about unary minus and bracket as one symbol
			%we would have a problem with recursion level. 
			%I don't know another way to solve this problem.
			{SubResult, NextTokens} = parse_impl(T, {}),
			{{unary_minus, SubResult}, NextTokens, true};
		[{operator, unary_minus}|T] -> 
			{SubResult, NextTokens} = parse_impl(T, {}),
			{{unary_minus, SubResult}, NextTokens, true};
		[{operator, open_bracket}|T] -> 
			{SubResult, NextTokens} = parse_impl(T, {}),
			{SubResult, NextTokens, true};
		[{operator, close_bracket}|T] -> {Result, T, false};
		[{operator, Operator}|T] ->
			{RightSubResult, NextTokens} = parse_impl(T, {}),
			SubResult = {Operator, Result, RightSubResult},
			{SubResult, NextTokens, false};
		[{number, Operand}|T] ->
			{{number, Operand}, T, true}
	end,
	case MustContinue of
		true -> parse_impl(NewNextTokens, NewResult);
		false -> {NewResult, NewNextTokens}
	end.

get_operator_name(OpChar) ->
	case OpChar of
		$+ -> plus;
		$~ -> unary_minus;
		$- -> minus;
		$* -> multiply;
		$/ -> division;
		$( -> open_bracket;
		$) -> close_bracket
	end.	

get_operator_str(Operator) ->
	case Operator of
		plus -> "+";
		unary_minus -> "~";
		minus -> "-";
		multiply -> "*";
		division -> "/"
	end.	

extrude_string([H|T], Acc) when H >= $A andalso H =< $z ->
	extrude_string(T, [H|Acc]);
extrude_string(L, Acc) ->
	{lists:reverse(Acc), L}.

convert_str_to_token("if") ->
	{keyword, 'if'};
convert_str_to_token("then") ->
	{keyword, 'then'};
convert_str_to_token("else") ->
	{keyword, 'else'}.

extrude_number([H|T], Acc) when H >= $0 andalso H =< $9 ->
	extrude_number(T, [H|Acc]);
extrude_number(L, Acc) ->
	{list_to_integer(lists:reverse(Acc)), L}.