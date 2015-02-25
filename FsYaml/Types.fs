namespace FsYaml

open System

/// Yamlをロードするときに、メンバが省略可能であることを示します。
[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type OptionalMemberAttribute() = inherit Attribute()

/// YamlのRepresentation層の型を提供します。
module RepresentationTypes =
  /// Scalar Nodeの値
  type Scalar =
    /// Plain形式のScalar
    | Plain of string
    /// Plain形式以外のScalar
    | NonPlain of string

  /// Scalarを操作するモジュールです。
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Scalar =
    /// Scalarの値を取得します。
    let value = function
      | Plain x -> x
      | NonPlain x -> x

  /// Yaml文字列中の位置
  type Position = { Line: int; Column: int; }

  /// Yamlを表現します。
  type YamlObject =
    /// Scalar node
    | Scalar of Scalar * Position option
    /// Sequence node
    | Sequence of YamlObject list * Position option
    /// Mapping node
    | Mapping of Map<YamlObject, YamlObject> * Position option
    /// Scalarを特殊化したNullの値
    | Null of Position option

  /// YamlObjectを操作するモジュールです。
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module YamlObject =
    open Microsoft.FSharp.Reflection

    /// 対象のオブジェクトのYaml文字列中の位置を取得します。
    let position = function
      | Scalar (_, p) -> p
      | Sequence (_, p) -> p
      | Mapping (_, p) -> p
      | Null p -> p

    /// Nodeの名前を取得します。
    let nodeTypeName (x: YamlObject) = let caseInfo, _ = FSharpValue.GetUnionFields(x, typeof<YamlObject>) in caseInfo.Name

  /// Mappingを操作するモジュールです。
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Mapping =
    let private pickF name = (fun k v ->
      match k with
      | Scalar (k, _) -> if Scalar.value k = name then Some v else None
      | _ -> None)
    /// Mapping内の要素を検索します。要素が存在しない場合はNoneを返します。
    let tryFind key (mapping: Map<YamlObject, YamlObject>) = Map.tryPick (pickF key) mapping
    /// <summary>
    /// Mapping内の要素を検索します。
    /// </summary>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">キーが存在しない場合</exception>
    let find key (mapping: Map<YamlObject, YamlObject>) = Map.pick (pickF key) mapping

/// YamlのNative層の型を提供します。
module NativeTypes =
  open RepresentationTypes

  /// <summary>
  /// Yamlから<c>Type</c>が表す型のオブジェクトに変換します。
  /// </summary>
  type Constructor = Type -> YamlObject -> obj
  /// <summary>
  /// 再帰的にYamlからオブジェクトに変換します。ジェネリック型の要素や、メンバーの変換に使用します。
  /// </summary>
  type RecursiveConstructor = Constructor
  /// <summary>
  /// 指定された型のデフォルト値を返します。デフォルト値を定義できない場合はNoneを返します。
  /// </summary>
  type ZeroConstructor = Type -> obj option

  /// <summary>
  /// <c>Type</c>が表す型のオブジェクトをYamlに変換します。
  /// </summary>
  type Representer = Type -> obj -> YamlObject
  /// <summary>
  /// 再帰的にオブジェクトからYamlに変換します。ジェネリック型の要素や、メンバーの変換に使用します。
  /// </summary>
  type RecursiveRepresenter = Representer

  /// Yamlとオブジェクト間の変換に関する情報です。
  type TypeDefinition = {
    /// 渡された型が変換可能か返します。
    Accept: Type -> bool
    /// Yamlからオブジェクトに変換します。
    Construct: RecursiveConstructor -> ZeroConstructor -> Constructor
    /// 型のデフォルト値を返します。デフォルト値を定義できない場合はNoneを返します。
    Zero: ZeroConstructor
    /// オブジェクトからYamlに変換します。
    Represent: RecursiveRepresenter -> Representer
  }