/// <summary>
/// ユーザ定義の<see cref="FsYaml.NativeTypes.TypeDefinition" />を作成するためのヘルパ関数を提供します。
/// </summary>
module FsYaml.CustomTypeDefinition

open System
open FsYaml.Utility
open FsYaml.RepresentationTypes
open FsYaml.NativeTypes

/// <summary>
/// Constructorが<c>YamlObject.Scalar</c>を期待したがそうでなかった事を表す例外を生成します。
/// </summary>
/// <param name="t">変換しようとした型</param>
/// <param name="actual">実際のYaml</param>
let mustBeScalar t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeScalar", Type.print t, YamlObject.nodeTypeName actual)
/// <summary>
/// Constructorが<c>YamlObject.Sequence</c>を期待したがそうでなかった事を表す例外を生成します。
/// </summary>
/// <param name="t">変換しようとした型</param>
/// <param name="actual">実際のYaml</param>
let mustBeSequence t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeSequence", Type.print t, YamlObject.nodeTypeName actual)
/// <summary>
/// Constructorが<c>YamlObject.Mapping</c>を期待したがそうでなかった事を表す例外を生成します。
/// </summary>
/// <param name="t">変換しようとした型</param>
/// <param name="actual">実際のYaml</param>
let mustBeMapping t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeMapping", Type.print t, YamlObject.nodeTypeName actual)

type internal ScalarPreference =
  | PrefersPlain
  | PrefersNonPlain

/// スカラー値とスカラー形式の好みに基づいて、値に存在の度合いを付加します。
let internal fuzzyFromScalar preference s x =
  match (s, preference) with
  | (Plain _, PrefersPlain)
  | (NonPlain _, PrefersNonPlain) -> Fuzzy.result x
  | (Plain _, PrefersNonPlain)
  | (NonPlain _, PrefersPlain) -> Fuzzy.create 0.5 x

/// <summary>
/// <c>YamlObject.Scalar</c>からオブジェクトを生成します。
/// Plain/NonPlain形式の好みに応じて、変換結果の好ましさを計算します。
/// </summary>
let internal fuzzyConstructFromScalar preference f = fun _ t yaml ->
  match yaml with
  | Scalar (s, _) -> Scalar.value s |> f |> box |> fuzzyFromScalar preference s
  | otherwise -> raise (mustBeScalar t otherwise)

/// <summary>
/// <c>YamlObject.Scalar</c>からオブジェクトを生成します。
/// </summary>
/// <param name="f"><c>YamlObject.Scalar</c>の値をオブジェクトへ変換する関数</param>
let constructFromScalar f = fun construct' t yaml ->
  match yaml with
  | Scalar (s, _) -> Scalar.value s |> f |> box
  | otherwise -> raise (mustBeScalar t otherwise)
/// <summary>
/// オブジェクトからPlainな<c>YamlObject.Scalar</c>へ変換します。
/// </summary>
/// <param name="f">オブジェクトを<c>YamlObject.Scalar</c>の値へ変換する関数</param>
let representAsPlain f = fun represent t obj -> Scalar (Plain (f obj), None)
/// <summary>
/// オブジェクトからNonPlainな<c>YamlObject.Scalar</c>へ変換します。
/// </summary>
/// <param name="f">オブジェクトを<c>YamlObject.Scalar</c>の値へ変換する関数</param>
let representAsNonPlain f = fun represent t obj -> Scalar (NonPlain (f obj), None)

/// <summary>
/// Seq型のオブジェクトから<c>YamlObject.Sequence</c>へ変換します。
/// </summary>
let representSeqAsSequence = fun represent t obj ->
  let elementType = RuntimeSeq.elementType t
  let values = RuntimeSeq.map (represent elementType) t obj |> Seq.toList
  Sequence (values, None)

/// Nullの可能性がある値のConstructor/Representerを提供します。
module MaybeNull =
  /// <summary>
  /// <c>YamlObject.Scalar</c>からオブジェクトを生成します。値が<c>YamlObject.Null</c>の場合はnullを返します。
  /// </summary>
  let internal fuzzyConstructFromScalar preference f = fun _ t yaml ->
    match yaml with
    | Scalar (s, _) ->
      Scalar.value s |> f |> box |> fuzzyFromScalar preference s
    | Null _ -> Fuzzy.result null
    | otherwise -> raise (mustBeScalar t otherwise)

  /// <summary>
  /// <c>YamlObject.Scalar</c>からオブジェクトを生成します。値が<c>YamlObject.Null</c>の場合はnullを返します。
  /// </summary>
  /// <param name="f"><c>YamlObject.Scalar</c>の値をオブジェクトへ変換する関数</param>
  let constructFromScalar f = fun construct' t yaml ->
    match yaml with
    | Scalar (s, _) -> Scalar.value s |> f |> box
    | Null _ -> null
    | otherwise -> raise (mustBeScalar t otherwise)
  /// <summary>
  /// オブジェクトからPlainな<c>YamlObject.Scalar</c>へ変換します。オブジェクトのnullの場合は<c>YamlObject.Null</c>を返します。
  /// </summary>
  /// <param name="f">オブジェクトを<c>YamlObject.Scalar</c>の値へ変換する関数</param>
  let representAsPlain f = fun represent t obj ->
    match obj with
    | null -> Null None
    | _ -> Scalar (Plain (f obj), None)
  /// <summary>
  /// オブジェクトからNonPlainな<c>YamlObject.Scalar</c>へ変換します。オブジェクトのnullの場合は<c>YamlObject.Null</c>を返します。
  /// </summary>
  /// <param name="f">オブジェクトを<c>YamlObject.Scalar</c>の値へ変換する関数</param>
  let representAsNonPlain f = fun represent t obj ->
    match obj with
    | null -> Null None
    | _ -> Scalar (NonPlain (f obj), None)

/// <summary>
/// 指定した型がジェネリック型定義と一致するか返します。
/// </summary>
/// <param name="genericTypeDef">期待するジェネリック型定義</param>
/// <param name="x">テストする型</param>
let isGenericTypeDef genericTypeDef (x: Type) = x.IsGenericType && x.GetGenericTypeDefinition() = genericTypeDef

let internal fuzzyConstructorFromConstructor construct fuzzyConstruct' t yaml =
  let construct' t yaml = fuzzyConstruct' t yaml |> Fuzzy.value
  construct construct' t yaml |> Fuzzy.result

let internal internalTypeDefinitionFromTypeDefinition (d: TypeDefinition) = {
  Accept = d.Accept
  FuzzyConstruct = fuzzyConstructorFromConstructor d.Construct
  Represent = d.Represent
}

let internal typeDefinitionFromInternalTypeDefinition (d: InternalTypeDefinition) = {
  Accept = d.Accept
  Construct = fun construct' t yaml ->
    let fuzzyConstruct' t yaml =
      construct' t yaml |> Fuzzy.result
    d.FuzzyConstruct fuzzyConstruct' t yaml |> Fuzzy.value
  Represent = d.Represent
}