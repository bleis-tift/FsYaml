/// <summary>
/// オブジェクトとYamlの相互変換を提供します。
/// </summary>
/// <remarks>
/// 変換は以下の型に対応しています。これら以外の型を変換するには、<see cref="FsYaml.NativeTypes.TypeDefinition" />を実装し、<c>loadWith</c>、<c>dumpWith</c>に渡します。<br />
/// <list type="bullet">
///   <item><description>int</description></item>
///   <item><description>int64</description></item>
///   <item><description>float</description></item>
///   <item><description>string</description></item>
///   <item><description>bool</description></item>
///   <item><description>decimal</description></item>
///   <item><description>System.DateTime</description></item>
///   <item><description>System.TimeSpan</description></item>
///   <item><description>レコード型</description></item>
///   <item><description>タプル</description></item>
///   <item><description>list&lt;'T&gt;</description></item>
///   <item><description>Map&lt;'Key, 'Value&gt;</description></item>
///   <item><description>配列</description></item>
///   <item><description>seq&lt;'T&gt;</description></item>
///   <item><description>Option&lt;'T&gt;</description></item>
///   <item><description>判別共用体</description></item>
/// </list>
/// </remarks>
module FsYaml.Yaml

open FsYaml.Utility

/// <summary>
/// Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。
/// </summary>
/// <param name="yamlStr">ロードするYaml文字列</param>
/// <exception cref="FsYaml.FsYamlException">ロードに失敗した場合</exception>
let load<'a> yamlStr = Representation.parse yamlStr |> Native.fuzzyConstruct<'a> TypeDefinitions.defaultInternalDefinitions |> Fuzzy.value

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
/// <exception cref="FsYaml.FsYamlException">ロードに失敗した場合</exception>
let loadWith<'a> customDefinitions yamlStr =
  let definitions = Seq.append (customDefinitions |> Seq.map CustomTypeDefinition.internalTypeDefinitionFromTypeDefinition) TypeDefinitions.defaultInternalDefinitions
  Representation.parse yamlStr |> Native.fuzzyConstruct<'a> definitions |> Fuzzy.value

/// <summary>
/// Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// </summary>
/// <param name="yamlStr">ロードするYaml文字列</param>
let tryLoad<'a> yamlStr =
  try
    Some (load<'a> yamlStr)
  with
    _ -> None

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
let tryLoadWith<'a> customDefinitions yamlStr =
  try
    Some (loadWith<'a> customDefinitions yamlStr)
  with
    _ -> None

/// <summary>
/// オブジェクトをYaml文字列へダンプします。
/// </summary>
/// <param name="obj">ダンプするオブジェクト</param>
/// <exception cref="FsYaml.FsYamlException">ダンプに失敗した場合</exception>
let dump<'a> obj = Native.represent<'a> TypeDefinitions.defaultDefinitions obj |> Representation.present

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、オブジェクトをYaml文字列へダンプします。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ダンプの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="obj">ダンプするオブジェクト</param>
/// <exception cref="FsYaml.FsYamlException">ダンプに失敗した場合</exception>
let dumpWith<'a> customDefinitions obj = Native.represent<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions) obj |> Representation.present