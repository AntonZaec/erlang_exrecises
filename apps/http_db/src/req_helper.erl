-module(req_helper).
-export([get_db_name/1, get_record_key/1, get_record/1]).

get_db_name(Req) ->
	list_to_atom(bitstring_to_list(cowboy_req:binding(db_name, Req))).

get_record_key(Req) ->
	list_to_atom(bitstring_to_list(cowboy_req:binding(key, Req))).

get_record(Req) ->
	{ok, Data, _NewReq} = cowboy_req:read_body(Req),
	{FieldList} = jiffy:decode(Data),
	Dict = maps:from_list(FieldList),
	case Dict of
		#{<<"value">> := Value} -> {ok, Value};
		_ -> {error, wrong_json}
	end.