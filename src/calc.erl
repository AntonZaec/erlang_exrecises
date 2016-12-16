-module(calc).
-export([parse/1]).

parse(String) ->
	Tokens = tokenize(String),
	{Result, _} = parse_impl(Tokens, {}),
	Result.

tokenize([]) ->
	[];
tokenize([H|T]) ->
	State = if 
			H >= $0 andalso H =< $9 -> read_number;
			true -> read_operator
		end,
	String = [H|T],
	case State of
		read_operator -> [{operator, get_operator_name(H)}|tokenize(T)];
		read_number -> Number = list_to_integer(extrude_number(String)),
						StringWithoutNumber = remove_number(String),
						[{number, Number}|tokenize(StringWithoutNumber)]
	end.

parse_impl([], Result) -> {Result, []};
parse_impl(Tokens, Result) ->
	{NewResult, NewNextTokens, MustContinue} = case Tokens of
		[{operator, unary_minus},{operator, open_bracket}|T] -> 
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

extrude_number([H|T]) when H >= $0 andalso H =< $9 ->
	[H|extrude_number(T)];
extrude_number(_) ->
	[].

remove_number([H|T]) when H >= $0 andalso H =< $9 ->
	remove_number(T);
remove_number(L) ->
	L.