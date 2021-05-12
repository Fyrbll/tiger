1. the position of the EOF token is reported inaccurately if last character in
a file is not a newline
2. when using `Parse.parse` on two files one after the other in the SMLNJ REPL
run the line `ErrorMsg.lineNum := 1; ErrorMsg.linePos := [];` between calls
3. I had to replace the `sources.cm` entry `smlnj-lib.cm` with
`$SMLNJ-BASIS/basis.cm` to get `CM.make "sources.cm"` to work correctly
4. I have added three files to the `testcases` folder specifically to test the
lexer on strings, nested comments, unclosed strings and unclosed comments
    - comments-and-strings.tig
    - unclosed-comment.tig
    - unclosed-string.tig
