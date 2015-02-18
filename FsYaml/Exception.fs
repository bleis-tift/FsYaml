namespace FsYaml

open System
open FsYaml.RepresentationTypes

/// <summary>
/// <c>load</c>または<c>dump</c>に失敗した時にスローされる例外。
/// </summary>
type FsYamlException(msg, ex: exn) =
  inherit Exception(msg, ex)

  new (msg) = FsYamlException(msg, null)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="innerException">原因となった例外</param>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member Create(innerException, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    FsYamlException(msg, innerException)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member Create(format, [<ParamArray>] args: obj[]) = FsYamlException.Create(null, format, args)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="innerException">原因となった例外</param>
  /// <param name="position">この例外が発生するYaml中の位置</param>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member WithPosition(innerException, position, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    let msg =
      match position with
      | Some p -> sprintf "%s (Line=%d, Column=%d)" msg p.Line p.Column
      | None -> msg
    FsYamlException(msg, innerException)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="position">この例外が発生するYaml中の位置</param>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member WithPosition(position, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(null, position, format, args)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="innerException">原因となった例外</param>
  /// <param name="yaml">この例外が発生した原因となったYaml</param>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member WithYaml(innerException, yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(innerException, YamlObject.position yaml, format, args)

  /// <summary>
  /// このクラスのインスタンスを生成します。
  /// </summary>
  /// <param name="yaml">この例外が発生した原因となったYaml</param>
  /// <param name="format">複合書式指定文字列</param>
  /// <param name="args">書式指定するオブジェクト</param>
  static member WithYaml(yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithYaml(null, yaml, format, args)

open FSharp.Configuration

type private R = ResXProvider<file = "Resources.resx">
type internal Messages = R.Resources
