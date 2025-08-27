module FSharp.Analyzers.Comments

open System
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

/// A standard pattern within an analyzer is to look for a specific comment preceding a problematic line,
/// indicating "suppress the analyzer on this line".
/// This function performs that common check.
let isSwitchedOffPerComment
    (magicComment : string)
    (comments : CommentTrivia list)
    (sourceText : ISourceText)
    (analyzerTriggeredOn : Range)
    : bool
    =
    comments
    |> List.exists (fun c ->
        match c with
        | CommentTrivia.LineComment r ->
            if r.StartLine <> analyzerTriggeredOn.StartLine - 1 then
                false
            else
                let lineOfComment = sourceText.GetLineString (r.StartLine - 1) // 0-based

                lineOfComment.Contains (magicComment, StringComparison.OrdinalIgnoreCase)
        | _ -> false
    )
