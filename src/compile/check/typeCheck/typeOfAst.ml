module Rcs = AstU.RcLookup
type rcs = Type.rc Rcs.t
module Uns = AstU.UnLookup
type uns = Type.un Uns.t
module Fts = AstU.FtLookup
type fts = Type.ft Fts.t
type t = {rcs: rcs; uns: uns; fts: fts}

let rc_of_ast({rcs; _}: t) = Rcs.get rcs
let un_of_ast({uns; _}: t) = Uns.get uns
let ft_of_ast({fts; _}: t) = Fts.get fts
let all({rcs; uns; fts}: t): Type.rc array * Type.un array * Type.ft array = Rcs.values rcs, Uns.values uns, Fts.values fts
