open Token

val keyword: Sym.t -> t option
val output: (t, 'o) OutputU.printer
