program QuokkaCompilerVM;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TTokenKind = (
    tkEOF, tkIdent, tkNumber,
    tkLet, tkPrint, tkIf, tkElse, tkWhile, tkFunc, tkReturn,
    tkLParen, tkRParen, tkLBrace, tkRBrace,
    tkComma, tkSemicolon, tkAssign,
    tkPlus, tkMinus, tkStar, tkSlash, tkPercent,
    tkBang, tkEq, tkNe, tkLt, tkLe, tkGt, tkGe
  );

  TToken = record
    kind: TTokenKind;
    lex: string;
    ival: Int64;
    pos: integer;
    line: integer;
    col: integer;
  end;

  TIntArray = array of Int64;
  TStringArray = array of string;
  TInt32Array = array of integer;

const
  OP_HALT   = 0;
  OP_ICONST = 1;
  OP_ADD    = 2;
  OP_SUB    = 3;
  OP_MUL    = 4;
  OP_DIV    = 5;
  OP_MOD    = 6;
  OP_NEG    = 7;
  OP_NOT    = 8;
  OP_EQ     = 9;
  OP_NE     = 10;
  OP_LT     = 11;
  OP_LE     = 12;
  OP_GT     = 13;
  OP_GE     = 14;
  OP_JMP    = 15;
  OP_JZ     = 16;
  OP_LOAD   = 17;
  OP_STORE  = 18;
  OP_CALL   = 19;
  OP_RET    = 20;
  OP_PRINT  = 21;
  OP_POP    = 22;
  OP_ENTER  = 23;

type
  TFunctionInfo = record
    name: string;
    paramCount: integer;
    localCount: integer;
    addr: integer;
  end;

  TFuncArray = array of TFunctionInfo;

var
  Source: string;
  SLen: integer;
  SPos: integer;
  SLine, SCol: integer;
  Cur: TToken;

  Code: TIntArray;
  Funcs: TFuncArray;

  LocalNames: TStringArray;
  CurrentEnterPos: integer = -1;
  CurrentFuncIndex: integer = -1;

  OptDumpOnly: boolean = false;
  BootIP: integer = 0;

procedure ErrorAt(line, col: integer; const msg: string);
begin
  Writeln(StdErr, 'Error [', line, ':', col, ']: ', msg);
  Halt(1);
end;

procedure ErrorTok(const T: TToken; const msg: string);
begin
  ErrorAt(T.line, T.col, msg);
end;

function IsAlpha(ch: char): boolean; inline;
begin
  Result := (ch in ['A'..'Z','a'..'z','_']);
end;

function IsDigit(ch: char): boolean; inline;
begin
  Result := (ch in ['0'..'9']);
end;

function PeekChar(offset: integer = 0): char; inline;
var p: integer;
begin
  p := SPos + offset;
  if (p >= 1) and (p <= SLen) then Result := Source[p]
  else Result := #0;
end;

procedure AdvChar;
var ch: char;
begin
  if SPos <= SLen then
  begin
    ch := Source[SPos];
    Inc(SPos);
    if ch = #10 then
    begin
      Inc(SLine);
      SCol := 1;
    end
    else
      Inc(SCol);
  end;
end;

function MakeToken(k: TTokenKind; const lex: string = ''; ival: Int64 = 0): TToken;
begin
  Result.kind := k; Result.lex := lex; Result.ival := ival;
  Result.pos := SPos; Result.line := SLine; Result.col := SCol;
end;

function KeywordKind(const s: string): TTokenKind;
begin
  if s = 'let' then exit(tkLet);
  if s = 'print' then exit(tkPrint);
  if s = 'if' then exit(tkIf);
  if s = 'else' then exit(tkElse);
  if s = 'while' then exit(tkWhile);
  if s = 'func' then exit(tkFunc);
  if s = 'return' then exit(tkReturn);
  Result := tkIdent;
end;

procedure NextToken;
var ch: char; startCol, startLine: integer; s: string; num: Int64;
begin
  while true do
  begin
    ch := PeekChar;
    while (ch <> #0) and (ch <= ' ') do begin AdvChar; ch := PeekChar; end;
    if (ch = '/') and (PeekChar(1) = '/') then
    begin
      while (ch <> #0) and (ch <> #10) do begin AdvChar; ch := PeekChar; end;
      continue;
    end;
    if (ch = '/') and (PeekChar(1) = '*') then
    begin
      AdvChar; AdvChar;
      while true do
      begin
        ch := PeekChar;
        if ch = #0 then ErrorAt(SLine, SCol, 'Unterminated block comment');
        if (ch = '*') and (PeekChar(1) = '/') then begin AdvChar; AdvChar; break; end;
        AdvChar;
      end;
      continue;
    end;
    break;
  end;

  startLine := SLine; startCol := SCol;

  ch := PeekChar;
  if ch = #0 then begin Cur := MakeToken(tkEOF); Cur.line:=startLine; Cur.col:=startCol; exit; end;

  if IsAlpha(ch) then
  begin
    s := '';
    while IsAlpha(PeekChar) or IsDigit(PeekChar) do
    begin
      s := s + PeekChar; AdvChar;
    end;
    Cur := MakeToken(KeywordKind(s), s);
    Cur.line := startLine; Cur.col := startCol;
    exit;
  end;

  if IsDigit(ch) then
  begin
    s := '';
    while IsDigit(PeekChar) do begin s := s + PeekChar; AdvChar; end;
    num := StrToInt64(s);
    Cur := MakeToken(tkNumber, s, num); Cur.line := startLine; Cur.col := startCol;
    exit;
  end;

  case ch of
    '(': begin AdvChar; Cur := MakeToken(tkLParen); end;
    ')': begin AdvChar; Cur := MakeToken(tkRParen); end;
    '{': begin AdvChar; Cur := MakeToken(tkLBrace); end;
    '}': begin AdvChar; Cur := MakeToken(tkRBrace); end;
    ',': begin AdvChar; Cur := MakeToken(tkComma); end;
    ';': begin AdvChar; Cur := MakeToken(tkSemicolon); end;
    '+': begin AdvChar; Cur := MakeToken(tkPlus); end;
    '-': begin AdvChar; Cur := MakeToken(tkMinus); end;
    '*': begin AdvChar; Cur := MakeToken(tkStar); end;
    '/': begin AdvChar; Cur := MakeToken(tkSlash); end;
    '%': begin AdvChar; Cur := MakeToken(tkPercent); end;
    '!':
      begin
        AdvChar;
        if PeekChar = '=' then begin AdvChar; Cur := MakeToken(tkNe); end
        else Cur := MakeToken(tkBang);
      end;
    '=':
      begin
        AdvChar;
        if PeekChar = '=' then begin AdvChar; Cur := MakeToken(tkEq); end
        else Cur := MakeToken(tkAssign);
      end;
    '<':
      begin
        AdvChar;
        if PeekChar = '=' then begin AdvChar; Cur := MakeToken(tkLe); end
        else Cur := MakeToken(tkLt);
      end;
    '>':
      begin
        AdvChar;
        if PeekChar = '=' then begin AdvChar; Cur := MakeToken(tkGe); end
        else Cur := MakeToken(tkGt);
      end;
  else
    ErrorAt(startLine, startCol, 'Unexpected character: "'+ch+'"');
  end;
  Cur.line := startLine; Cur.col := startCol;
end;

procedure Expect(k: TTokenKind; const msg: string = '');
begin
  if Cur.kind <> k then
    ErrorTok(Cur, 'Expected token kind ' + IntToStr(Ord(k)) + ' ' + msg + ', got ' + IntToStr(Ord(Cur.kind)));
  NextToken;
end;

function Accept(k: TTokenKind): boolean;
begin
  if Cur.kind = k then begin NextToken; exit(true); end;
  Result := false;
end;

function Emit(op: Int64): integer;
begin
  Result := Length(Code);
  SetLength(Code, Result+1);
  Code[Result] := op;
end;

function Emit2(op, a: Int64): integer;
begin
  Result := Emit(op);
  Emit(a);
end;

function Emit3(op, a, b: Int64): integer;
begin
  Result := Emit(op);
  Emit(a);
  Emit(b);
end;

procedure Patch(atIndex: integer; value: Int64);
begin
  if (atIndex < 0) or (atIndex >= Length(Code)) then
    ErrorAt(0,0,'Internal patch out of range');
  Code[atIndex] := value;
end;

function LocalIndexOf(const name: string): integer;
var i: integer;
begin
  for i := 0 to High(LocalNames) do
    if LocalNames[i] = name then exit(i);
  Result := -1;
end;

function AddLocal(const name: string): integer;
var idx: integer;
begin
  idx := LocalIndexOf(name);
  if idx >= 0 then exit(idx);
  idx := Length(LocalNames);
  SetLength(LocalNames, idx+1);
  LocalNames[idx] := name;
  Result := idx;
  if idx+1 > Funcs[CurrentFuncIndex].localCount then
    Funcs[CurrentFuncIndex].localCount := idx+1;
end;

function FuncIndexOf(const name: string): integer;
var i: integer;
begin
  for i := 0 to High(Funcs) do
    if Funcs[i].name = name then exit(i);
  Result := -1;
end;

function AddFuncShell(const name: string; paramCount: integer): integer;
var idx: integer;
begin
  idx := FuncIndexOf(name);
  if idx >= 0 then exit(idx);
  idx := Length(Funcs);
  SetLength(Funcs, idx+1);
  Funcs[idx].name := name;
  Funcs[idx].paramCount := paramCount;
  Funcs[idx].localCount := paramCount;
  Funcs[idx].addr := -1;
  Result := idx;
end;

procedure ParseProgram; forward;
procedure ParseFunction; forward;
procedure ParseBlock; forward;
procedure ParseStmt; forward;
procedure ParseIfStmt; forward;
procedure ParseWhileStmt; forward;
procedure ParseReturnStmt; forward;
procedure ParseLetOrAssignOrCall; forward;
procedure ParsePrintStmt; forward;

procedure Expr; forward;
procedure EqualityExpr; forward;
procedure ComparisonExpr; forward;
procedure TermExpr; forward;
procedure FactorExpr; forward;
procedure UnaryExpr; forward;
procedure PrimaryExpr; forward;

function IsStartOfStmt: boolean;
begin
  case Cur.kind of
    tkLet, tkIf, tkWhile, tkReturn, tkPrint, tkLBrace, tkIdent: exit(true);
  else
    exit(false);
  end;
end;

// ---- Expression parsers ----

procedure Expr;
begin
  EqualityExpr;
end;

procedure EqualityExpr;
begin
  ComparisonExpr;
  while (Cur.kind = tkEq) or (Cur.kind = tkNe) do
  begin
    if Cur.kind = tkEq then
    begin
      NextToken;
      ComparisonExpr;
      Emit(OP_EQ);
    end
    else
    begin
      NextToken;
      ComparisonExpr;
      Emit(OP_NE);
    end;
  end;
end;

procedure ComparisonExpr;
begin
  TermExpr;
  while (Cur.kind = tkLt) or (Cur.kind = tkLe) or (Cur.kind = tkGt) or (Cur.kind = tkGe) do
  begin
    case Cur.kind of
      tkLt: begin NextToken; TermExpr; Emit(OP_LT); end;
      tkLe: begin NextToken; TermExpr; Emit(OP_LE); end;
      tkGt: begin NextToken; TermExpr; Emit(OP_GT); end;
      tkGe: begin NextToken; TermExpr; Emit(OP_GE); end;
    end;
  end;
end;

procedure TermExpr;
begin
  FactorExpr;
  while (Cur.kind = tkPlus) or (Cur.kind = tkMinus) do
  begin
    if Cur.kind = tkPlus then begin NextToken; FactorExpr; Emit(OP_ADD); end
    else begin NextToken; FactorExpr; Emit(OP_SUB); end;
  end;
end;

procedure FactorExpr;
begin
  UnaryExpr;
  while (Cur.kind = tkStar) or (Cur.kind = tkSlash) or (Cur.kind = tkPercent) do
  begin
    case Cur.kind of
      tkStar:    begin NextToken; UnaryExpr; Emit(OP_MUL); end;
      tkSlash:   begin NextToken; UnaryExpr; Emit(OP_DIV); end;
      tkPercent: begin NextToken; UnaryExpr; Emit(OP_MOD); end;
    end;
  end;
end;

procedure UnaryExpr;
begin
  if Accept(tkMinus) then
  begin
    UnaryExpr;
    Emit(OP_NEG);
  end
  else if Accept(tkBang) then
  begin
    UnaryExpr;
    Emit(OP_NOT);
  end
  else
    PrimaryExpr;
end;

procedure ParseArgList(out argc: integer; funcIndex: integer);
begin
  argc := 0;
  if not Accept(tkRParen) then
  begin
    repeat
      Expr;
      Inc(argc);
    until not Accept(tkComma);
    Expect(tkRParen, ')');
  end;
end;

procedure PrimaryExpr;
var name: string; idx, fidx, argc: integer;
begin
  if Cur.kind = tkNumber then
  begin
    Emit2(OP_ICONST, Cur.ival);
    NextToken;
  end
  else if Accept(tkLParen) then
  begin
    Expr;
    Expect(tkRParen, ') expected');
  end
  else if Cur.kind = tkIdent then
  begin
    name := Cur.lex; NextToken;
    if Accept(tkLParen) then
    begin
      fidx := FuncIndexOf(name);
      if fidx < 0 then fidx := AddFuncShell(name, -1);
      ParseArgList(argc, fidx);
      Emit3(OP_CALL, fidx, argc);
    end
    else
    begin
      idx := LocalIndexOf(name);
      if idx < 0 then ErrorTok(Cur, 'Unknown variable "'+name+'"');
      Emit2(OP_LOAD, idx);
    end;
  end
  else
    ErrorTok(Cur, 'Expression expected');
end;

// ---- Statements ----

procedure ParseBlock;
begin
  Expect(tkLBrace, '{');
  while IsStartOfStmt do ParseStmt;
  Expect(tkRBrace, '}');
end;

procedure ParseIfStmt;
var jzPos, jmpEndPos: integer;
begin
  Expect(tkIf, 'if');
  Expect(tkLParen, '(');
  Expr;
  Expect(tkRParen, ')');
  jzPos := Emit2(OP_JZ, -1);
  ParseBlock;
  jmpEndPos := Emit2(OP_JMP, -1);
  Patch(jzPos+1, Length(Code));
  if Accept(tkElse) then
    ParseBlock;
  Patch(jmpEndPos+1, Length(Code));
end;

procedure ParseWhileStmt;
var startPos, jzPos: integer;
begin
  Expect(tkWhile, 'while');
  startPos := Length(Code);
  Expect(tkLParen, '(');
  Expr;
  Expect(tkRParen, ')');
  jzPos := Emit2(OP_JZ, -1);
  ParseBlock;
  Emit2(OP_JMP, startPos);
  Patch(jzPos+1, Length(Code));
end;

procedure ParseReturnStmt;
begin
  Expect(tkReturn, 'return');
  Expr;
  Expect(tkSemicolon, ';');
  Emit(OP_RET);
end;

procedure ParsePrintStmt;
begin
  Expect(tkPrint, 'print');
  Expr;
  Expect(tkSemicolon, ';');
  Emit(OP_PRINT);
end;

procedure ParseLetOrAssignOrCall;
var name: string; idx, fidx, argc: integer;
begin
  if Accept(tkLet) then
  begin
    if Cur.kind <> tkIdent then ErrorTok(Cur,'identifier expected after let');
    name := Cur.lex; NextToken;
    Expect(tkAssign, '=');
    Expr;
    Expect(tkSemicolon, ';');
    idx := AddLocal(name);
    Emit2(OP_STORE, idx);
  end
  else if Cur.kind = tkIdent then
  begin
    name := Cur.lex; NextToken;
    if Accept(tkAssign) then
    begin
      idx := LocalIndexOf(name);
      if idx < 0 then ErrorTok(Cur, 'Unknown variable "'+name+'"');
      Expr;
      Expect(tkSemicolon, ';');
      Emit2(OP_STORE, idx);
    end
    else if Accept(tkLParen) then
    begin
      fidx := FuncIndexOf(name);
      if fidx < 0 then fidx := AddFuncShell(name, -1);
      ParseArgList(argc, fidx);
      Emit3(OP_CALL, fidx, argc);
      Emit(OP_POP);
      Expect(tkSemicolon, ';');
    end
    else
      ErrorTok(Cur, 'Expected "=" or "(" after identifier');
  end
  else
    ErrorTok(Cur, 'Statement expected');
end;

procedure ParseStmt;
begin
  case Cur.kind of
    tkLBrace: ParseBlock;
    tkIf: ParseIfStmt;
    tkWhile: ParseWhileStmt;
    tkReturn: ParseReturnStmt;
    tkPrint: ParsePrintStmt;
    tkLet, tkIdent: ParseLetOrAssignOrCall;
  else
    ErrorTok(Cur, 'Unexpected token in statement');
  end;
end;

procedure ParseParamList(out names: TStringArray; out count: integer);
begin
  SetLength(names, 0);
  count := 0;
  if Accept(tkRParen) then exit;
  repeat
    if Cur.kind <> tkIdent then ErrorTok(Cur, 'parameter name expected');
    SetLength(names, Length(names)+1);
    names[High(names)] := Cur.lex;
    Inc(count);
    NextToken;
  until not Accept(tkComma);
  Expect(tkRParen, ')');
end;

procedure ParseFunction;
var fname: string; paramNames: TStringArray; paramCount,i: integer;
begin
  Expect(tkFunc, 'func');
  if Cur.kind <> tkIdent then ErrorTok(Cur,'function name expected');
  fname := Cur.lex; NextToken;
  Expect(tkLParen, '(');
  ParseParamList(paramNames, paramCount);

  CurrentFuncIndex := AddFuncShell(fname, paramCount);
  Funcs[CurrentFuncIndex].paramCount := paramCount;
  Funcs[CurrentFuncIndex].localCount := paramCount;
  Funcs[CurrentFuncIndex].addr := Length(Code);

  SetLength(LocalNames, 0);
  for i := 0 to paramCount-1 do
    AddLocal(paramNames[i]);

  CurrentEnterPos := Emit2(OP_ENTER, -1);

  ParseBlock;

  Emit2(OP_ICONST, 0);
  Emit(OP_RET);

  Patch(CurrentEnterPos+1, Funcs[CurrentFuncIndex].localCount - Funcs[CurrentFuncIndex].paramCount);

  CurrentFuncIndex := -1;
  CurrentEnterPos := -1;
  SetLength(LocalNames, 0);
end;

procedure ParseProgram;
var mainIdx: integer;
begin
  while Cur.kind <> tkEOF do
    ParseFunction;

  mainIdx := FuncIndexOf('main');
  if mainIdx < 0 then ErrorAt(1,1,'No entry point: define func main()');

  BootIP := Length(Code);
  Emit3(OP_CALL, mainIdx, 0);
  Emit(OP_HALT);
end;

procedure DumpCode;
var i: integer;
begin
  i := 0;
  Writeln('== Functions ==');
  for i := 0 to High(Funcs) do
    Writeln('  [', i, '] ', Funcs[i].name, ' params=', Funcs[i].paramCount,
            ' locals=', Funcs[i].localCount, ' addr=', Funcs[i].addr);
  Writeln('== Bytecode ==');
  i := 0;
  while i < Length(Code) do
  begin
    Write(Format('%5d: ', [i]));
    case Code[i] of
      OP_HALT:   begin Writeln('HALT'); Inc(i,1); end;
      OP_ICONST: begin Writeln('ICONST ', Code[i+1]); Inc(i,2); end;
      OP_ADD:    begin Writeln('ADD'); Inc(i,1); end;
      OP_SUB:    begin Writeln('SUB'); Inc(i,1); end;
      OP_MUL:    begin Writeln('MUL'); Inc(i,1); end;
      OP_DIV:    begin Writeln('DIV'); Inc(i,1); end;
      OP_MOD:    begin Writeln('MOD'); Inc(i,1); end;
      OP_NEG:    begin Writeln('NEG'); Inc(i,1); end;
      OP_NOT:    begin Writeln('NOT'); Inc(i,1); end;
      OP_EQ:     begin Writeln('EQ'); Inc(i,1); end;
      OP_NE:     begin Writeln('NE'); Inc(i,1); end;
      OP_LT:     begin Writeln('LT'); Inc(i,1); end;
      OP_LE:     begin Writeln('LE'); Inc(i,1); end;
      OP_GT:     begin Writeln('GT'); Inc(i,1); end;
      OP_GE:     begin Writeln('GE'); Inc(i,1); end;
      OP_JMP:    begin Writeln('JMP ', Code[i+1]); Inc(i,2); end;
      OP_JZ:     begin Writeln('JZ  ', Code[i+1]); Inc(i,2); end;
      OP_LOAD:   begin Writeln('LOAD ', Code[i+1]); Inc(i,2); end;
      OP_STORE:  begin Writeln('STORE ', Code[i+1]); Inc(i,2); end;
      OP_CALL:   begin Writeln('CALL f=', Code[i+1], ' argc=', Code[i+2]); Inc(i,3); end;
      OP_RET:    begin Writeln('RET'); Inc(i,1); end;
      OP_PRINT:  begin Writeln('PRINT'); Inc(i,1); end;
      OP_POP:    begin Writeln('POP'); Inc(i,1); end;
      OP_ENTER:  begin Writeln('ENTER ', Code[i+1], ' locals'); Inc(i,2); end;
    else
      Writeln('??? (', Code[i], ')'); Inc(i,1);
    end;
  end;
end;

procedure RunVM;
var
  ip: integer;
  sp: integer;
  stack: TIntArray;

  function push(v: Int64): integer;
  begin
    sp := sp + 1;
    if sp >= Length(stack) then
    begin
      if Length(stack)=0 then SetLength(stack, 1024)
      else SetLength(stack, Length(stack)*2);
    end;
    stack[sp] := v; Result := sp;
  end;

  function pop: Int64;
  begin
    if sp < 0 then ErrorAt(0,0,'VM stack underflow');
    Result := stack[sp]; sp := sp - 1;
  end;

var
  a,b: Int64; tgt: integer; fidx, argc, nlocals: integer;
  bp: integer; // base pointer
  retIPStack, bpStack: TInt32Array;
  rsp: integer;
  retToIP, oldbp: integer;

  procedure pushRet(ipv: integer); inline;
  begin
    Inc(rsp);
    if rsp >= Length(retIPStack) then
    begin
      if Length(retIPStack)=0 then
      begin
        SetLength(retIPStack, 64);
        SetLength(bpStack, 64);
      end
      else
      begin
        SetLength(retIPStack, Length(retIPStack)*2);
        SetLength(bpStack, Length(bpStack)*2);
      end;
    end;
    retIPStack[rsp] := ipv;
    bpStack[rsp] := bp;
  end;

  procedure popRet(out ipv: integer; out oldbp: integer); inline;
  begin
    if rsp < 0 then ErrorAt(0,0,'VM call stack underflow');
    ipv := retIPStack[rsp];
    oldbp := bpStack[rsp];
    Dec(rsp);
  end;

  procedure doCall(findex, argcount: integer);
  var f: TFunctionInfo;
  begin
    if (findex < 0) or (findex > High(Funcs)) then ErrorAt(0,0,'Bad function index');
    f := Funcs[findex];
    pushRet(ip);
    bp := sp - argcount + 1; // args live at bp..bp+paramCount-1
    ip := f.addr;
  end;

begin
  SetLength(stack, 1024); sp := -1;
  SetLength(retIPStack, 64); SetLength(bpStack, 64); rsp := -1;
  bp := 0;
  ip := BootIP;

  while true do
  begin
    if (ip < 0) or (ip >= Length(Code)) then ErrorAt(0,0,'VM ip out of range');
    case Code[ip] of
      OP_HALT: begin Inc(ip); break; end;
      OP_ICONST: begin Inc(ip); push(Code[ip]); Inc(ip); end;
      OP_ADD: begin Inc(ip); b:=pop; a:=pop; push(a+b); end;
      OP_SUB: begin Inc(ip); b:=pop; a:=pop; push(a-b); end;
      OP_MUL: begin Inc(ip); b:=pop; a:=pop; push(a*b); end;
      OP_DIV: begin Inc(ip); b:=pop; a:=pop; if b=0 then ErrorAt(0,0,'Division by zero'); push(a div b); end;
      OP_MOD: begin Inc(ip); b:=pop; a:=pop; if b=0 then ErrorAt(0,0,'Modulo by zero'); push(a mod b); end;
      OP_NEG: begin Inc(ip); a:=pop; push(-a); end;
      OP_NOT: begin Inc(ip); a:=pop; if a=0 then push(1) else push(0); end;
      OP_EQ:  begin Inc(ip); b:=pop; a:=pop; if a=b then push(1) else push(0); end;
      OP_NE:  begin Inc(ip); b:=pop; a:=pop; if a<>b then push(1) else push(0); end;
      OP_LT:  begin Inc(ip); b:=pop; a:=pop; if a<b then push(1) else push(0); end;
      OP_LE:  begin Inc(ip); b:=pop; a:=pop; if a<=b then push(1) else push(0); end;
      OP_GT:  begin Inc(ip); b:=pop; a:=pop; if a>b then push(1) else push(0); end;
      OP_GE:  begin Inc(ip); b:=pop; a:=pop; if a>=b then push(1) else push(0); end;
      OP_JMP: begin Inc(ip); tgt := Code[ip]; ip := tgt; end;
      OP_JZ:  begin Inc(ip); tgt := Code[ip]; Inc(ip); a:=pop; if a=0 then ip := tgt; end;
      OP_LOAD:  begin Inc(ip); a := stack[bp + Code[ip]]; Inc(ip); push(a); end;
      OP_STORE: begin Inc(ip); a:=pop; stack[bp + Code[ip]] := a; Inc(ip); end;
      OP_CALL:  begin Inc(ip); fidx := Code[ip]; argc := Code[ip+1]; Inc(ip,2); doCall(fidx, argc); end;
      OP_RET:
        begin
          Inc(ip);
          a := pop;
          popRet(retToIP, oldbp);
          sp := bp - 1; // drop params + locals
          bp := oldbp;
          ip := retToIP;
          push(a);
        end;
      OP_PRINT: begin Inc(ip); a := pop; Writeln(a); end;
      OP_POP:   begin Inc(ip); pop; end;
      OP_ENTER:
        begin
          Inc(ip);
          nlocals := Code[ip]; Inc(ip);
          while nlocals > 0 do begin push(0); Dec(nlocals); end;
        end;
    else
      ErrorAt(0,0,'Unknown opcode: ' + IntToStr(Code[ip]));
    end;
  end;
end;

function ReadAllText(const path: string): string;
var fs: TFileStream; ss: TStringStream;
begin
  fs := TFileStream.Create(path, fmOpenRead or fmShareDenyNone);
  try
    ss := TStringStream.Create('');
    try
      ss.CopyFrom(fs, fs.Size);
      Result := ss.DataString;
    finally ss.Free; end;
  finally fs.Free; end;
end;

procedure Usage;
begin
  Writeln('Quokka: tiny language compiler+VM in Pascal');
  Writeln('Usage:  ./compiler [--dump] <file.qk>');
  Halt(1);
end;

var
  srcPath: string;

begin
  if ParamCount < 1 then Usage;

  OptDumpOnly := false;
  if (ParamStr(1) = '--dump') then
  begin
    if ParamCount < 2 then Usage;
    OptDumpOnly := true;
    srcPath := ParamStr(2);
  end
  else
    srcPath := ParamStr(1);

  Source := ReadAllText(srcPath);
  SLen := Length(Source);
  SPos := 1; SLine := 1; SCol := 1;

  SetLength(Code, 0);
  SetLength(Funcs, 0);

  NextToken;
  ParseProgram;

  if OptDumpOnly then
    DumpCode
  else
    RunVM;
end.

