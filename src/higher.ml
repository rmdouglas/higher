
(* Representation of type application.  This corresponds to the "apply"
   variant type in a defunctionalized program.  Application is postfix. *)
type ('p, 'f) app

module type Newtype0 = sig
  type s
  type ty
  external inj : s -> ty 
    = "%identity"
  external prj : ty -> s
    = "%identity"
end

module type Newtype1 = sig
  type 'a s
  type ty
  external inj : 'a s -> ('a, ty) app 
    = "%identity"
  external prj : ('a, ty) app -> 'a s
    = "%identity"
end

module type Newtype2 = sig
  type ('a, 'b) s
  type ty
  external inj : ('a, 'b) s -> ('a, ('b, ty) app) app 
    = "%identity"
  external prj : ('a, ('b, ty) app) app -> ('a, 'b) s
    = "%identity"
end

module type Newtype3 = sig
  type ('a, 'b, 'c) s
  type ty
  external inj : ('a, 'b, 'c) s -> ('a, ('b, ('c, ty) app) app) app 
    = "%identity"
  external prj : ('a, ('b, ('c, ty) app) app) app -> ('a, 'b, 'c) s
    = "%identity"
end

module type Newtype4 = sig
  type ('a, 'b, 'c, 'd) s
  type ty
  external inj : ('a, 'b, 'c, 'd) s -> ('a, ('b, ('c, ('d, ty) app) app) app) app 
    = "%identity"
  external prj : ('a, ('b, ('c, ('d, ty) app) app) app) app -> ('a, 'b, 'c, 'd) s
    = "%identity"
end

module type Newtype5 = sig
  type ('a, 'b, 'c, 'd, 'e) s
  type ty
  external inj : ('a, 'b, 'c, 'd, 'e) s -> ('a, ('b, ('c, ('d, ('e, ty) app) app) app) app) app 
    = "%identity"
  external prj : ('a, ('b, ('c, ('d, ('e, ty) app) app) app) app) app -> ('a, 'b, 'c, 'd, 'e) s
    = "%identity"
end

module type Newtype6 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f) s
  type ty
  external inj : ('a, 'b, 'c, 'd, 'e, 'f) s -> ('a, ('b, ('c, ('d, ('e, ('f, ty) app) app) app) app) app) app 
    = "%identity"
  external prj : ('a, ('b, ('c, ('d, ('e, ('f, ty) app) app) app) app) app) app -> ('a, 'b, 'c, 'd, 'e, 'f) s
    = "%identity"
end

module Common =
struct
  type ty
  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Newtype0 (T : sig type t end) =
struct
  type s = T.t
  include Common
end

module Newtype1 (T : sig type 'a t end) =
struct
  type 'a s = 'a T.t
  include Common
end

module Newtype2 (T : sig type ('a, 'b) t end) =
struct
  type ('a, 'b) s = ('a, 'b) T.t
  include Common
end

module Newtype3 (T : sig type ('a, 'b, 'c) t end) =
struct
  type ('a, 'b, 'c) s = ('a, 'b, 'c) T.t
  include Common
end

module Newtype4 (T : sig type ('a, 'b, 'c, 'd) t end) =
struct
  type ('a, 'b, 'c, 'd) s = ('a, 'b, 'c, 'd) T.t
  include Common
end

module Newtype5 (T : sig type ('a, 'b, 'c, 'd, 'e) t end) =
struct
  type ('a, 'b, 'c, 'd, 'e) s = ('a, 'b, 'c, 'd, 'e) T.t
  include Common
end

module Newtype6 (T : sig type ('a, 'b, 'c, 'd, 'e, 'f) t end) =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f) s = ('a, 'b, 'c, 'd, 'e, 'f) T.t
  include Common
end
