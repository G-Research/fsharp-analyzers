#r "nuget: editorconfig, 0.18.0"

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Xml
open System.Xml.Linq
open EditorConfig.Core

let private repositoryRoot = __SOURCE_DIRECTORY__

let private editorConfig = EditorConfigParser ()

let private processOutput (fileName : string) (arguments : string) =
    let startInfo =
        ProcessStartInfo (fileName, arguments, WorkingDirectory = repositoryRoot, RedirectStandardOutput = true)

    use proc = Process.Start startInfo
    let output = proc.StandardOutput.ReadToEnd ()
    proc.WaitForExit ()

    if proc.ExitCode <> 0 then
        failwith $"%s{fileName} %s{arguments} exited with %i{proc.ExitCode}"

    output

let private fantomas (arguments : string) =
    let startInfo =
        ProcessStartInfo ("dotnet", $"fantomas %s{arguments}", WorkingDirectory = repositoryRoot)

    use proc = Process.Start startInfo
    proc.WaitForExit ()

    if proc.ExitCode <> 0 then
        failwith $"dotnet fantomas %s{arguments} exited with %i{proc.ExitCode}"

/// Only the files git knows about, which keeps obj, bin and output out of the picture
/// without a second ignore list to maintain.
let trackedFiles () =
    processOutput "git" "ls-files"
    |> fun output -> output.Split ('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun path -> Path.Combine (repositoryRoot, path.Trim ()))

let private newLineOf (config : FileConfiguration) =
    match Option.ofNullable config.EndOfLine with
    | Some EndOfLine.CRLF -> "\r\n"
    | Some EndOfLine.CR -> "\r"
    | _ -> "\n"

let private indentOf (config : FileConfiguration) =
    match Option.ofNullable config.IndentStyle with
    | Some IndentStyle.Tab -> "\t"
    | _ ->

    let size = config.IndentSize

    let columns =
        if isNull (box size) then
            None
        else
            Option.ofNullable size.NumberOfColumns

    String (' ', Option.defaultValue 4 columns)

let private encodingOf (config : FileConfiguration) =
    match Option.ofNullable config.Charset with
    | Some Charset.UTF8BOM -> UTF8Encoding true
    | _ -> UTF8Encoding false

let private withFinalNewline (config : FileConfiguration) (content : string) =
    let newLine = newLineOf config

    let wanted = Option.ofNullable config.InsertFinalNewline |> Option.defaultValue true

    if wanted && not (content.EndsWith (newLine, StringComparison.Ordinal)) then
        String.Concat (content, newLine)
    else
        content

/// XmlWriter preserves comments and keeps the space in "<Foo />", which is why this
/// beats reaching for an external XML formatter.
let private formatXml (config : FileConfiguration) (path : string) =
    let document = XDocument.Load (path, LoadOptions.None)

    let settings = XmlWriterSettings ()
    settings.Indent <- true
    settings.IndentChars <- indentOf config
    settings.NewLineChars <- newLineOf config
    settings.OmitXmlDeclaration <- isNull document.Declaration
    // Writing through a stream keeps the declaration honest: a StringWriter claims utf-16.
    settings.Encoding <- UTF8Encoding false

    use buffer = new MemoryStream ()
    using (XmlWriter.Create (buffer, settings)) document.Save
    UTF8Encoding(false).GetString(buffer.ToArray ())

/// System.Text.Json on .NET 8 always indents with two spaces and offers no knob for it,
/// so rescale the indentation afterwards. Every newline in JSON is structural, because a
/// string literal cannot contain a raw one.
let private formatJson (config : FileConfiguration) (path : string) =
    let node = JsonNode.Parse (File.ReadAllText path)
    let indent = indentOf config

    node.ToJsonString (JsonSerializerOptions (WriteIndented = true))
    |> fun json -> json.Split '\n'
    |> Array.map (fun line ->
        let content = line.TrimStart ' '
        let depth = (line.Length - content.Length) / 2
        String.Concat (String.replicate depth indent, content)
    )
    |> String.concat (newLineOf config)

let private formatters =
    Map.ofList
        [
            ".props", formatXml
            ".targets", formatXml
            ".fsproj", formatXml
            ".csproj", formatXml
            ".json", formatJson
        ]

let private targets () =
    trackedFiles ()
    |> Array.filter (fun path -> Map.containsKey (Path.GetExtension path) formatters)
    |> Array.sort

/// The bytes the file should hold, preamble included, so a stray BOM counts as a difference.
let private desiredBytes (path : string) =
    let config = editorConfig.Parse path
    let encoding = encodingOf config

    let content =
        formatters.[Path.GetExtension path] config path |> withFinalNewline config

    Array.append (encoding.GetPreamble ()) (encoding.GetBytes content)

let private isFormatted (path : string) =
    File.ReadAllBytes path = desiredBytes path

let private relative (path : string) =
    Path.GetRelativePath (repositoryRoot, path)

/// Rewrite every tracked XML and JSON file, then hand the F# files to Fantomas.
let format _ =
    for path in targets () do
        if not (isFormatted path) then
            File.WriteAllBytes (path, desiredBytes path)
            printfn "formatted %s" (relative path)

    fantomas "."

/// Report anything unformatted without touching the working tree.
let checkFormat _ =
    let unformatted = targets () |> Array.filter (isFormatted >> not)

    if not (Array.isEmpty unformatted) then
        for path in unformatted do
            eprintfn "needs formatting: %s" (relative path)

        failwith $"%i{unformatted.Length} file(s) need formatting, run: dotnet fsi build.fsx -p Format"

    fantomas ". --check"
