type interpreter_state
val step: interpreter_state -> bool
val call_fn: Code.fn -> Val.t array -> Val.t
