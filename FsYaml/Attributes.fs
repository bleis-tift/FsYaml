module Attributes

open System

[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type AllowEmpty() =
  inherit Attribute()