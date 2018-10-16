-module(avltree).

%%====================================================================
%% API exports
%%====================================================================
-export([new/0, insert/3, find/2, find_next/2, find_previous/2, union/2, from_list/1, to_list/1]).

%%====================================================================
%% Records, types, and macros
%%====================================================================
-record(node, {key :: term(),
               value :: term(),
               left = nil :: tree(),
               right = nil :: tree(),
               height = 1 :: non_neg_integer()}).

-type tree() :: #node{} | nil.
-type operation() :: insert_left | insert_right.
-type key() :: term().
-type value() :: term().

%%====================================================================
%% API functions
%%====================================================================
%% @doc
-spec new() -> tree().
new() -> nil.

-spec insert(key(), value(), tree()) -> tree().
insert(Key, Value, nil) ->
    #node{key = Key, value = Value};
insert(InsertingKey, InsertingValue, Tree = #node{key = Key,
                                                  left = Left,
                                                  right = Right}) ->
    if
        Key == InsertingKey ->
            Tree#node{value = InsertingValue};
        InsertingKey < Key ->
            balance(insert_left,
                    InsertingKey,
                    Tree#node{left = insert(InsertingKey,
                                            InsertingValue,
                                            Left)});
        InsertingValue > Key  ->
            balance(insert_right,
                    InsertingKey,
                    Tree#node{right = insert(InsertingKey,
                                             InsertingValue,
                                             Right)})
    end.

-spec find(key(), tree()) -> {ok, value()} | {error, not_found}.
find(_, nil) -> {error, not_found};
find(FindingKey, #node{key = Key, value = Value, left = Left, right = Right}) ->
    if
        FindingKey == Key ->
            {ok, Value};
        FindingKey < Key ->
            find(FindingKey, Left);
        Key < FindingKey ->
            find(FindingKey, Right)
    end.

-spec find_next(key(), tree()) -> {ok, {key(), value()}} | {error, not_found}.
find_next(_, nil) -> {error, not_found};
find_next(FindingKey, #node{key = Key, value = Value, left = Left, right = Right} = Tree) ->
    if
        FindingKey == Key ->
            {ok, {Key, Value}};
        FindingKey < Key ->
            case Left of
                nil ->
                    {ok, {Key, Value}};
                _ ->
                    case max_key(Left) < FindingKey of
                        true ->
                            {ok, {Key, Value}};
                        false ->
                            find_next(FindingKey, Left)
                    end
            end;
        Key < FindingKey ->
            find_next(FindingKey, Right)
    end.

-spec find_previous(key(), tree()) -> {ok, {key(), value()}} | {error, not_found}.
find_previous(_, nil) -> {error, not_found};
find_previous(FindingKey, #node{key = Key, value = Value, left = Left, right = Right}) ->
    if
        FindingKey == Key ->
            {ok, {Key, Value}};
        FindingKey < Key ->
            find_previous(FindingKey, Left);
        Key < FindingKey ->
            case Right of
                nil ->
                    {ok, {Key, Value}};
                _ ->
                    case FindingKey < min_key(Right) of
                        true ->
                            {ok, {Key, Value}};
                        false ->
                            find_previous(FindingKey, Right)
                    end
            end
    end.

-spec union(tree(), tree()) -> tree().
union(nil, Tree) -> Tree;
union(#node{key = Key,
            value = Value,
            left = Left,
            right = Right}, Tree) ->
    union(Right, insert(Key, Value, union(Left, Tree))).

-spec map(fun((key(), value()) -> value()), tree()) -> tree().
map(F, nil) -> nil;
map(F, Tree = #node{key = Key,
                    value = Value,
                    left = Left,
                    right = Right}) ->
    Tree#node{value = F(Key, Value),
              left = map(F, Left),
              right = map(F, Right)}.

-spec from_list([{key(), value()}]) -> tree().
from_list(KeyValueList) ->
    lists:foldl(fun({Key, Value}, Accumulated) ->
                        insert(Key, Value, Accumulated)
                end, new(), KeyValueList).

-spec to_list(tree()) -> [{key(), value()}].
to_list(nil) -> [];
to_list(#node{key = Key, value = Value, left = Left, right = Right}) ->
    to_list(Left) ++ [{Key, Value}] ++ to_list(Right).

%%====================================================================
%% Internal functions
%%====================================================================
-spec min_key(tree()) -> key().
min_key(#node{left = nil, key = Key}) -> Key;
min_key(#node{left = Left}) -> min_key(Left).

-spec max_key(tree()) -> {key(), value()}.
max_key(#node{right = nil, key = Key}) -> Key;
max_key(#node{right = Right}) -> max_key(Right).

-spec balance(operation(), key(), tree()) -> tree().
balance(insert_left, InsertingKey, Tree = #node{left = Left,
                                                right = Right}) ->
    case height(Left) - height(Right) =:= 2 of
        true ->
            case InsertingKey < Left#node.key of
                true ->
                    left_left_rotation(Tree);
                false ->
                    left_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end;

balance(insert_right, InsertingKey, Tree = #node{left = Left,
                                                 right = Right}) ->
    case height(Right) - height(Left) =:= 2 of
        true ->
            case InsertingKey < Right#node.key of
                true ->
                    right_left_rotation(Tree);
                false ->
                    right_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

-spec left_left_rotation(tree()) -> tree().
left_left_rotation(Tree = #node{left = L = #node{right = LR}}) ->
    update_height(L#node{right = update_height(Tree#node{left = LR})}).

-spec left_right_rotation(tree()) -> tree().
left_right_rotation(Tree = #node{left = L = #node{right = LR = #node{left = LRL, right = LRR}}}) ->
    update_height(LR#node{left = update_height(L#node{right = LRL}),
                          right = update_height(Tree#node{left = LRR})}).

-spec right_left_rotation(tree()) -> tree().
right_left_rotation(Tree = #node{right = R = #node{left = RL = #node{left = RLL, right = RLR}}}) ->
    update_height(RL#node{left = update_height(Tree#node{right = RLL}),
                          right = update_height(R#node{left = RLR})}).

-spec right_right_rotation(tree()) -> tree().
right_right_rotation(Tree = #node{right = R = #node{left = RL}}) ->
    update_height(R#node{left = update_height(Tree#node{right = RL})}).

-spec update_height(tree()) -> tree().
update_height(Tree = #node{left = Left, right = Right}) ->
    Tree#node{height = max(height(Left), height(Right)) + 1}.

-spec height(tree()) -> non_neg_integer().
height(nil) -> 0;
height(#node{height = Height}) -> Height.
