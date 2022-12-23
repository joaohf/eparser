Nonterminals
expression values.

Terminals
command value time.

Rootsymbol
expression.

expression -> command : '$1'.

values -> value : '$1'.
values -> value values : ['$1' ++ '$2'].

expression -> command value value : {unwrap('$1'), unwrap('$2'), unwrap('$3')}.
expression -> command time : {unwrap('$1'), unwrap('$2')}.

Header
"%%%-------------------------------------------------------------------"
"%% @hidden"
"%% @doc ZL30702 incremental configuration script scanner."
"%% @end"
"%%%-------------------------------------------------------------------".

Erlang code.

unwrap({_,_,V}) -> V.