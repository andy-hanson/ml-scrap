type interpreter_state
val step: interpreter_state -> bool
val call_fn: Code.func -> Val.t array -> Val.t
