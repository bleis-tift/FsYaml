/// トップレベルのAPIのためのモジュールです。
module Yaml

/// <summary>
/// YAML文字列を指定した型として読み込みます。
/// 解析に失敗した場合や、YAML文字列を指定した型に変換できなかった場合は例外が発生します。
/// </summary>
val load<'a> : string -> 'a

/// <summary>
/// YAM文字列を指定した型として読み込みます。
/// 解析に失敗した場合や、YAML文字列を指定した型に変換できなかった場合はNoneを返します。
/// </summary>
val tryLoad<'a> : string -> 'a option

/// <summary>
/// 型をYAML文字列に変換します。
/// 変換に失敗した場合は例外が発生します。
/// </summary>
val dump<'a> : 'a -> string