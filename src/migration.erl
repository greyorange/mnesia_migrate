-module(migration).

-callback init(Args :: list(term())) -> 'ok'.

%
%-callback handle(Event :: atom()) -> NextEvent :: atom().
%
%-callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 'ok'.
%

