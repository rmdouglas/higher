
(* Representation of type application.  This corresponds to the "apply"
   variant type in a defunctionalized program.  Application is postfix. *)
type ('p, 'f) app

module type Newtype0 = sig
  type s
  type ty
  type t = ty
  external inj : s -> t 
    = "%identity"
  external prj : t -> s
    = "%identity"
end

module type Newtype1 = sig
  type 'a s
  type ty
  type 'a t = ('a, ty) app
  external inj : 'a s -> 'a t 
    = "%identity"
  external prj : 'a t -> 'a s
    = "%identity"
end

module type Newtype2 = sig
  type ('a, 'b) s
  type ty
  type ('a, 'b) t = ('a, ('b, ty) app) app
  external inj : ('a, 'b) s -> ('a, 'b) t
    = "%identity"
  external prj : ('a, 'b) t-> ('a, 'b) s
    = "%identity"
end

module type Newtype3 = sig
  type ('a, 'b, 'c) s
  type ty
  type ('a, 'b, 'c) t = ('a, ('b, ('c, ty) app) app) app
  external inj : ('a, 'b, 'c) s -> ('a, 'b, 'c) t 
    = "%identity"
  external prj : ('a, 'b, 'c) t -> ('a, 'b, 'c) s
    = "%identity"
end

module type Newtype4 = sig
  type ('a, 'b, 'c, 'd) s
  type ty
  type ('a, 'b, 'c, 'd) t = ('a, ('b, ('c, ('d, ty) app) app) app) app
  external inj : ('a, 'b, 'c, 'd) s -> ('a, 'b, 'c, 'd) t 
    = "%identity"
  external prj : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) s
    = "%identity"
end

module type Newtype5 = sig
  type ('a, 'b, 'c, 'd, 'e) s
  type ty
  type ('a, 'b, 'c, 'd, 'e) t = ('a, ('b, ('c, ('d, ('e, ty) app) app) app) app) app
  external inj : ('a, 'b, 'c, 'd, 'e) s -> ('a, 'b, 'c, 'd, 'e) t 
    = "%identity"
  external prj : ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e) s
    = "%identity"
end

module type Newtype6 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f) s
  type ty
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, ('b, ('c, ('d, ('e, ('f, ty) app) app) app) app) app) app
  external inj : ('a, 'b, 'c, 'd, 'e, 'f) s -> ('a, 'b, 'c, 'd, 'e, 'f) t
    = "%identity"
  external prj : ('a, 'b, 'c, 'd, 'e, 'f) t -> ('a, 'b, 'c, 'd, 'e, 'f) s
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
  type t = ty
end

module Newtype1 (T : sig type 'a t end) =
struct
  type 'a s = 'a T.t
  include Common
  type 'a t = ('a, ty) app
end

module Newtype2 (T : sig type ('a, 'b) t end) =
struct
  type ('a, 'b) s = ('a, 'b) T.t
  include Common
  type ('a, 'b) t = ('a, ('b, ty) app) app
end

module Newtype3 (T : sig type ('a, 'b, 'c) t end) =
struct
  type ('a, 'b, 'c) s = ('a, 'b, 'c) T.t
  include Common
  type ('a, 'b, 'c) t = ('a, ('b, ('c, ty) app) app) app
end

module Newtype4 (T : sig type ('a, 'b, 'c, 'd) t end) =
struct
  type ('a, 'b, 'c, 'd) s = ('a, 'b, 'c, 'd) T.t
  include Common
  type ('a, 'b, 'c, 'd) t = ('a, ('b, ('c, ('d, ty) app) app) app) app
end

module Newtype5 (T : sig type ('a, 'b, 'c, 'd, 'e) t end) =
struct
  type ('a, 'b, 'c, 'd, 'e) s = ('a, 'b, 'c, 'd, 'e) T.t
  include Common
  type ('a, 'b, 'c, 'd, 'e) t = ('a, ('b, ('c, ('d, ('e, ty) app) app) app) app) app
end

module Newtype6 (T : sig type ('a, 'b, 'c, 'd, 'e, 'f) t end) =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f) s = ('a, 'b, 'c, 'd, 'e, 'f) T.t
  include Common
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, ('b, ('c, ('d, ('e, ('f, ty) app) app) app) app) app) app
end
