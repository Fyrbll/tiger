type pos = int

type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum

val linePos = ErrorMsg.linePos

val commentDepth = ref 0

val inStr = ref false

val curStr = ref ""

val strPos = ref 0

val valOfString = valOf o Int.fromString

fun set x v = x := v

fun cat s t = s := !s ^ t

fun clear s = s := ""

fun flip x = x := (not o !) x

fun inc x = x := !x + 1

fun dec x = x := !x - 1

fun push p ps = ps := p :: !ps

fun zerop x = !x = 0

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let
    val p = (hd o !) linePos
  in
    if zerop commentDepth
    then if (not o !) inStr
         then Tokens.EOF (p, p)
         else (ErrorMsg.error p "unclosed string"; Tokens.EOF (p, p))
    else (ErrorMsg.error p "unclosed comment"; Tokens.EOF (p, p))
  end

fun badChar t p = ErrorMsg.error p ("illegal character " ^ t)

fun handleNumEsc t p = case String.fromString t
    of SOME s => cat curStr s
     | NONE   => ErrorMsg.error p ("numeric escape outside ascii range " ^ t)

%%

%s COMMENT STRING FORMAT;

numLit=[0-9]+;

id=[a-zA-Z][a-zA-Z0-9_]*;

esc=\\(n|t|"^"[@A-Z\[\\\]^_]|\"|\\);

ws=[ \t];

%%

<FORMAT>\\ => (YYBEGIN STRING; continue());
<FORMAT>[ \f\t] => (continue());
<FORMAT>\n => (inc lineNum;
               push yypos linePos;
               continue());
<FORMAT>. => (ErrorMsg.error yypos ("illegal format character " ^ yytext);
              continue());
<COMMENT>"/*" => (inc commentDepth;
                  continue());
<COMMENT>"*/" => (dec commentDepth;
                  if zerop commentDepth
                  then (YYBEGIN INITIAL; continue())
                  else continue());
<COMMENT>\n => (inc lineNum; 
                push yypos linePos;
                continue());
<COMMENT>. => (continue());
<STRING>\\[ \f\t] => (YYBEGIN FORMAT; continue());
<STRING>\\\n => (inc lineNum;
                 push (yypos + 1) linePos;
                 YYBEGIN FORMAT;
                 continue());
<STRING>\" => (flip inStr;
               YYBEGIN INITIAL;
               Tokens.STRING(!curStr, !strPos, yypos + size yytext));
<STRING>{esc} => ((cat curStr o valOf o String.fromString) yytext;
                  continue());
<STRING>\\[0-9]{3} => (handleNumEsc yytext yypos; continue());
<STRING>\n => (cat curStr yytext;
               inc lineNum;
               push yypos linePos;
               continue());
<STRING>\\. => (ErrorMsg.error yypos ("illegal escape " ^ yytext); 
                continue());
<STRING>. => (cat curStr yytext;
              continue());
<INITIAL>\" => (flip inStr;
                clear curStr;
                set strPos yypos;
                YYBEGIN STRING;
                continue());
<INITIAL>"/*" => (inc commentDepth;
                  YYBEGIN COMMENT;
                  continue());
<INITIAL>type => (Tokens.TYPE (yypos, yypos + 4));
<INITIAL>var => (Tokens.VAR (yypos, yypos + 3));
<INITIAL>function => (Tokens.FUNCTION (yypos, yypos + 8));
<INITIAL>break => (Tokens.BREAK (yypos, yypos + 5));
<INITIAL>of => (Tokens.OF (yypos, yypos + 2));
<INITIAL>end => (Tokens.END (yypos, yypos + 3));
<INITIAL>in => (Tokens.IN (yypos, yypos + 2));
<INITIAL>nil => (Tokens.NIL (yypos, yypos + 3));
<INITIAL>let => (Tokens.LET (yypos, yypos + 3));
<INITIAL>do => (Tokens.DO (yypos, yypos + 2));
<INITIAL>to => (Tokens.TO (yypos, yypos + 2));
<INITIAL>for => (Tokens.FOR (yypos, yypos + 3));
<INITIAL>while => (Tokens.WHILE (yypos, yypos + 5));
<INITIAL>else => (Tokens.ELSE (yypos, yypos + 4));
<INITIAL>then => (Tokens.THEN (yypos, yypos + 4));
<INITIAL>if => (Tokens.IF (yypos, yypos + 2));
<INITIAL>array => (Tokens.ARRAY (yypos, yypos + 5));
<INITIAL>assign => (Tokens.ASSIGN (yypos, yypos + 6));
<INITIAL>"|" => (Tokens.OR (yypos, yypos + 2));
<INITIAL>& => (Tokens.AND (yypos, yypos + 3));
<INITIAL>">=" => (Tokens.GE (yypos, yypos + 2));
<INITIAL>">" => (Tokens.GT (yypos, yypos + 1));
<INITIAL>"<=" => (Tokens.LE (yypos, yypos + 2));
<INITIAL>"<" => (Tokens.LT (yypos, yypos + 1));
<INITIAL>"<>" => (Tokens.NEQ (yypos, yypos + 2));
<INITIAL>"=" => (Tokens.EQ (yypos, yypos + 1));
<INITIAL>"/" => (Tokens.DIVIDE (yypos, yypos + 1));
<INITIAL>"*" => (Tokens.TIMES (yypos, yypos + 1));
<INITIAL>"-" => (Tokens.MINUS (yypos, yypos + 1));
<INITIAL>"+" => (Tokens.PLUS (yypos, yypos + 1));
<INITIAL>"." => (Tokens.DOT (yypos, yypos + 1));
<INITIAL>} => (Tokens.RBRACE (yypos, yypos + 1));
<INITIAL>"{" => (Tokens.LBRACE (yypos, yypos + 1));
<INITIAL>] => (Tokens.RBRACK (yypos, yypos + 1));
<INITIAL>"[" => (Tokens.LBRACK (yypos, yypos + 1));
<INITIAL>")" => (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>"(" => (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL>";" => (Tokens.SEMICOLON (yypos, yypos + 1));
<INITIAL>: => (Tokens.COLON (yypos, yypos + 1));
<INITIAL>, => (Tokens.COMMA (yypos, yypos + 1));
<INITIAL>{numLit} => (Tokens.INT (valOfString yytext
                                 ,yypos
                                 ,yypos + size yytext));
<INITIAL>{id} => (Tokens.ID (yytext, yypos, yypos + size yytext));
<INITIAL>{ws} => (continue());
<INITIAL>\n => (inc lineNum; 
                push yypos linePos;
                continue());
<INITIAL>. => (badChar yytext yypos; continue());
