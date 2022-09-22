(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type modifiers =
  | Public
  | Private
  | Protected
  | Static
  | Const
  | Virtual
  | Override
  | Abstract
  | New

val pp_modifiers : Ppx_show_runtime.Format.formatter -> modifiers -> unit
val show_modifiers : modifiers -> string

type types = TInt | TVoid | TString | TRef of string | TBool

val pp_types : Ppx_show_runtime.Format.formatter -> types -> unit
val show_types : types -> string

type values =
  | VInt of int
  | VBool of bool
  | VString of string
  | VVoid
  | VObjectReference of object_references

and field_references = {
  key : string;
  field_type : types;
  field_value : values;
  is_const : bool;
  assignments_count : int;
}

and object_references =
  | NullObjectReference
  | ObjectReference of {
      class_key : string;
      ext_interface : string option;
      field_references_table : field_references KeyMap.KeyMap.t;
      number : int;
    }

val pp_values : Ppx_show_runtime.Format.formatter -> values -> unit

val pp_field_references :
  Ppx_show_runtime.Format.formatter -> field_references -> unit

val pp_object_references :
  Ppx_show_runtime.Format.formatter -> object_references -> unit

val show_values : values -> string
val show_field_references : field_references -> string
val show_object_references : object_references -> string

type names = Name of string

val pp_names : Ppx_show_runtime.Format.formatter -> names -> unit
val show_names : names -> string

type expressions =
  | Add of expressions * expressions
  | Sub of expressions * expressions
  | Mult of expressions * expressions
  | Div of expressions * expressions
  | PostInc of expressions
  | PostDec of expressions
  | PrefInc of expressions
  | PrefDec of expressions
  | Mod of expressions * expressions
  | And of expressions * expressions
  | Or of expressions * expressions
  | Not of expressions
  | Equal of expressions * expressions
  | NotEqual of expressions * expressions
  | Less of expressions * expressions
  | More of expressions * expressions
  | LessOrEqual of expressions * expressions
  | MoreOrEqual of expressions * expressions
  | This
  | Base
  | Null
  | Value of values
  | Identifier of string
  | ClassCreation of names * expressions list
  | CallMethod of expressions * expressions list
  | AccessByPoint of expressions * expressions
  | Assign of expressions * expressions
  | Cast of types * expressions

and statements =
  | Expression of expressions
  | StatementBlock of statements list
  | If of expressions * statements * statements option
  | While of expressions * statements
  | For of
      statements option * expressions option * expressions list * statements
  | Break
  | Continue
  | Return of expressions option
  | VariableDecl of modifiers option * types * (names * expressions option) list
  | Print of expressions

and fields =
  | Field of types * (names * expressions option) list
  | Method of types * names * (types * names) list * statements option
  | Constructor of
      names * (types * names) list * expressions option * statements

and objects =
  | Class of
      modifiers list * names * names list * (modifiers list * fields) list
  | Interface of modifiers * names * names list * (modifiers list * fields) list

val pp_expressions : Ppx_show_runtime.Format.formatter -> expressions -> unit
val pp_statements : Ppx_show_runtime.Format.formatter -> statements -> unit
val pp_fields : Ppx_show_runtime.Format.formatter -> fields -> unit
val pp_objects : Ppx_show_runtime.Format.formatter -> objects -> unit
val show_expressions : expressions -> string
val show_statements : statements -> string
val show_fields : fields -> string
val show_objects : objects -> string

type signal = WasBreak | WasContinue | WasReturn | NoSignal

val pp_signal : Ppx_show_runtime.Format.formatter -> signal -> unit
val show_signal : signal -> string

type table_constructor = {
  key : string;
  arguments : (types * names) list;
  call_constructor : expressions option;
  body : statements;
}

val pp_table_constructor :
  Ppx_show_runtime.Format.formatter -> table_constructor -> unit

val show_table_constructor : table_constructor -> string

type table_field = {
  field_type : types;
  field_modifiers : modifiers list;
  key : string;
  sub_tree : expressions option;
}

val pp_table_field : Ppx_show_runtime.Format.formatter -> table_field -> unit
val show_table_field : table_field -> string

type table_method = {
  method_type : types;
  method_modifiers : modifiers list;
  arguments : (types * names) list;
  key : string;
  interface_key : string option;
  body : statements option;
  is_overriden : bool;
}

val pp_table_method : Ppx_show_runtime.Format.formatter -> table_method -> unit
val show_table_method : table_method -> string

type table_class = {
  this_key : string;
  fields_table : table_field KeyMap.KeyMap.t;
  methods_table : table_method KeyMap.KeyMap.t;
  constructors_table : table_constructor KeyMap.KeyMap.t;
  children_keys : string list;
  is_abstract : bool;
  is_static : bool;
  is_class : bool;
  parent_key : string list;
  decl_tree : objects;
}

val pp_table_class : Ppx_show_runtime.Format.formatter -> table_class -> unit
val show_table_class : table_class -> string
