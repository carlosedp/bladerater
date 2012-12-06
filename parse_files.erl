-module(parse_files).
-compile([export_all]).
-export([parse_tsv/1]).

parse_tsv(Filename) when is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    parse_tsv(Bin);
parse_tsv(Bin) when is_binary(Bin) ->
    parse_tsv(Bin, [], [], []).

parse_tsv(<<$\t, Rest/binary>>, Field, Line, Acc) ->
    parse_tsv(Rest, [], [lists:reverse(Field)|Line], Acc);
parse_tsv(<<$\r, Rest/binary>>, Field, Line, Acc) ->
    parse_tsv(Rest, Field, Line, Acc);
parse_tsv(<<$\n, Rest/binary>>, Field, Line, Acc) ->
    FieldList = list_to_tuple(lists:reverse([lists:reverse(Field)|Line])),
    parse_tsv(Rest, [], [], [FieldList|Acc]);
parse_tsv(<<Char, Rest/binary>>, Field, Line, Acc) ->
    parse_tsv(Rest, [Char|Field], Line, Acc);
parse_tsv(<<>>, [], [], Acc) ->
    lists:reverse(Acc);
parse_tsv(<<>>, Field, Line, Acc) ->
    parse_tsv(<<$\n>>, Field, Line, Acc).
