{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHLEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHLEditor
description : JvEditor with built-in highlighting for:
              pascal, cbuilder, sql, python, jscript,
              vbscript, perl, ini, html, not quite c

Known Issues:
  (rom) source cleaning incomplete
  (rom) GetAttr should be broken up further
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ history
 (JVCL Library versions) :
  1.03:
    - first release;
  1.11:
    - improvements in custom reserved words support;
    - comments works better in custom reserved words;
  1.17:
    - python highlighting by Rafal Smotrzyk - rsmotrzyk@mikroplan.com.pl;
  1.17.2:
    - jscript, vbscript highlighting by Rafal Smotrzyk - rsmotrzyk@mikroplan.com.pl;
  1.17.6:
    - html highlighting;
  1.12.2:
    - fixed bug with pressing End-key if CursorBeoyondEOF enabled
      (greetings to Andre N Belokon)
  1.23:
    - fixed another bug in comment checking (range check error)
    (greetings to Willo vd Merwe)
  1.23.1:
    - first version of perl highlighter;
  1.41:
    - fixed another bug in comment checking;
  1.51.3 (JVCL Library 1.51 with Update 3):
    - fixed bug: exception on comments in "c++, java, sql" - mode;
  1.51.4 (JVCL Library 1.51 with Update 4):
    - ini-file highlighter;
    - fixed bug: custom reserved words not working;
  1.61:
    - new: in html-highlighter unknown (not html) tag highlighted with
      "statement" color. This allows to use html-highlighter to display
      xml-files.
  2.10.2: (changes by Andreas Hausladen)
    - C/C++ line continuation symbol '\' extends the highlight colors to the
      next line (LongToken=True)
    - "Not Quite C" highlighter (C similar, for programming LEGO MindStorm(R) robots)
    - fixed bug: SetBlockColor raise an exception (AV) if iEnd becomes greater than
      MAX_X
    - in TRAHLEditor.GetAttr all IsXxxKeyWord get a CONST string, so no compiler
      magic LStrAddRef is needed.
    - some speed optimations
    - new property DelphiColors: Boolean
    - renamed all "Identifer" to "Identifier". published property "Identifer"
      still exists but uses "FIdentifier"
    - added some new DelphiKeyWord
    - fixed bug: RescanLong() may exceed FLongDesc[] dimension
  2.10.3
    - faster RescanLong
    - faster KeyWord search for drawing

}

unit JvHLEditor;

interface

uses
  SysUtils, Classes, Graphics,
  JvEditor, JvHLParser;

const
  { Max_Line - maximum line numbers, scanned by editor for comments }
  Max_Line = 64 * 1024;

type
  THighLighter = (hlNone, hlPascal, hlCBuilder, hlSql, hlPython, hlJava, hlVB,
    hlHtml, hlPerl, hlIni, hlCocoR, hlPhp, hlNQC);

  TJvSymbolColor = class(TPersistent)
  private
    FStyle: TFontStyles;
    FForeColor: TColor;
    FBackColor: TColor;
  public
    constructor Create;
    procedure SetColor(const ForeColor, BackColor: TColor; const Style: TFontStyles);
    procedure Assign(Source: TPersistent); override;
  published
    // (rom) defaults and constructor added
    property Style: TFontStyles read FStyle write FStyle default [];
    property ForeColor: TColor read FForeColor write FForeColor default clWindowText;
    property BackColor: TColor read FBackColor write FBackColor default clWindow;
  end;

  TJvColors = class(TPersistent)
  private
    FComment: TJvSymbolColor;
    FNumber: TJvSymbolColor;
    FString: TJvSymbolColor;
    FSymbol: TJvSymbolColor;
    FReserved: TJvSymbolColor;
    FIdentifier: TJvSymbolColor;
    FPreproc: TJvSymbolColor;
    FFunctionCall: TJvSymbolColor;
    FDeclaration: TJvSymbolColor;
    FStatement: TJvSymbolColor;
    FPlainText: TJvSymbolColor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Comment: TJvSymbolColor read FComment write FComment;
    property Number: TJvSymbolColor read FNumber write FNumber;
    property Strings: TJvSymbolColor read FString write FString;
    property Symbol: TJvSymbolColor read FSymbol write FSymbol;
    property Reserved: TJvSymbolColor read FReserved write FReserved;
    property Identifier: TJvSymbolColor read FIdentifier write FIdentifier;
    property Preproc: TJvSymbolColor read FPreproc write FPreproc;
    property FunctionCall: TJvSymbolColor read FFunctionCall write FFunctionCall;
    property Declaration: TJvSymbolColor read FDeclaration write FDeclaration;
    property Statement: TJvSymbolColor read FStatement write FStatement;
    property PlainText: TJvSymbolColor read FPlainText write FPlainText;
  end;

  TOnReservedWord = procedure(Sender: TObject; Token: string;
    var Reserved: Boolean) of object;

  TJvHLEditor = class(TJvEditor)
  private
    Parser: TJvIParser;
    FHighLighter: THighLighter;
    FColors: TJvColors;
    FLine: string;
    FLineNum: Integer;
    FLong: Integer;
    FLongTokens: Boolean;
    FLongDesc: array [0..Max_Line] of Byte;
    FSyntaxHighlighting: Boolean;
    FOnReservedWord: TOnReservedWord;

//    FCachedHighligher: THighLighter;

    // Coco/R
    ProductionsLine: Integer;
    function RescanLong(iLine: Integer): Boolean;
    procedure CheckInLong;
    function FindLongEnd: Integer;
    procedure SetHighLighter(Value: THighLighter);
    function GetDelphiColors: Boolean;
    procedure SetDelphiColors(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure GetAttr(Line, ColBeg, ColEnd: Integer); override;
    procedure TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
      const Text: string); override;
    function GetReservedWord(const Token: string; var Reserved: Boolean): Boolean; virtual;
    function UserReservedWords: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property HighLighter: THighLighter read FHighLighter write SetHighLighter default hlPascal;
    property Colors: TJvColors read FColors write FColors;
    property DelphiColors: Boolean read GetDelphiColors write SetDelphiColors;
    property LongTokens: Boolean read FLongTokens write FLongTokens default True;
    property OnReservedWord: TOnReservedWord read FOnReservedWord write FOnReservedWord;
    property SyntaxHighlighting: Boolean read FSyntaxHighlighting write FSyntaxHighlighting stored False;
  end;

implementation

uses
  Math,
  JvStrUtil;

const
  lgNone            = 0;
  lgComment1        = 1;
  lgComment2        = 2;
  lgString          = 4;
  lgTag             = 5;
  lgPreproc         = 6;
  lgUndefined       = 255;

function StrScan(const Str: PChar; Chr: Char): PChar;
{ faster than the SysUtils.StrScan function on Pentium CPUs }
begin
  Result := Str;
  while (Result[0] <> #0) do
  begin
    if Result[0] = Chr then Exit;
    Inc(Result);
  end;
  Result := nil;
end;

function LastNoSpaceChar(const S: String): Char;
var i: integer;
begin
  Result := #0;
  i := Length(S);
  while (i > 0) and (S[i] = ' ') do Dec(i);
  if i > 0 then Result := S[i];
end;

function GetTrimChar(const S: String; Index: Integer): Char;
var LS, l: Integer;
begin
  LS := Length(S);
  if LS <> 0 then
  begin
    l := 1;
    while (l <= LS) and (s[l] = ' ') do Inc(l);
    if l <= LS then
      Result := S[l - 1 + Index]
    else
      Result := S[Index];
  end
  else
    Result := #0;
end;

function HasStringOpenEnd(Lines: TStrings; iLine: Integer): Boolean;
{ find C/C++ "line breaker" '\' }
var
  i: integer;
  IsOpen: Boolean;
  P, F: PChar;
  S: string;
begin
  Result := False;
  if (iLine < 0) or (iLine >= Lines.Count) then exit;
  i := iLine - 1;
  IsOpen := False;
  if (i >= 0) and (LastNoSpaceChar(Lines[i]) = '\') then // check prior lines
    IsOpen := HasStringOpenEnd(Lines, i);
  S := Lines[iLine];
  F := PChar(S);
  P := F;
  repeat
    P := StrScan(P, '"');
    if P <> nil then
    begin
      if (P = F) or (P[-1] <> '\') then
        IsOpen := not IsOpen
      else
      begin
       // count the backslashes
        i := 1;
        while (P-1-i > F) and (P[-1-i] = '\') do inc(i);
        if i and $01 = 0 then IsOpen := not IsOpen; { faster than: if i mod 2 = 0 then IsOpen := not IsOpen; }
      end;
      inc(P);
    end;
  until P = nil;
  Result := IsOpen;
end;

//=== TJvSymbolColor =========================================================

constructor TJvSymbolColor.Create;
begin
  inherited Create;
  FStyle :=  [];
  FForeColor := clWindowText;
  FBackColor := clWindow;
end;

procedure TJvSymbolColor.SetColor(const ForeColor, BackColor: TColor; const Style: TFontStyles);
begin
  FForeColor := ForeColor;
  FBackColor := BackColor;
  FStyle := Style;
end;

procedure TJvSymbolColor.Assign(Source: TPersistent);
begin
  if Source is TJvSymbolColor then
  begin
    FForeColor := TJvSymbolColor(Source).FForeColor;
    FBackColor := TJvSymbolColor(Source).FBackColor;
    FStyle := TJvSymbolColor(Source).FStyle;
  end
  else
    inherited Assign(Source);
end;

//=== TJvColors ==============================================================

constructor TJvColors.Create;
begin
  inherited Create;
  FComment := TJvSymbolColor.Create;
  FNumber := TJvSymbolColor.Create;
  FString := TJvSymbolColor.Create;
  FSymbol := TJvSymbolColor.Create;
  FReserved := TJvSymbolColor.Create;
  FStatement := TJvSymbolColor.Create;
  FIdentifier := TJvSymbolColor.Create;
  FPreproc := TJvSymbolColor.Create;
  FFunctionCall := TJvSymbolColor.Create;
  FDeclaration := TJvSymbolColor.Create;
  FPlainText := TJvSymbolColor.Create;
  FComment.SetColor(clOlive, clWindow, [fsItalic]);
  FNumber.SetColor(clNavy, clWindow, []);
  FString.SetColor(clPurple, clWindow, []);
  FSymbol.SetColor(clBlue, clWindow, []);
  FReserved.SetColor(clWindowText, clWindow, [fsBold]);
  FStatement.SetColor(clWindowText, clWindow, [fsBold]);
  FIdentifier.SetColor(clWindowText, clWindow, []);
  FPreproc.SetColor(clGreen, clWindow, []);
  FFunctionCall.SetColor(clWindowText, clWindow, []);
  FDeclaration.SetColor(clWindowText, clWindow, []);
  FPlainText.SetColor(clWindowText, clWindow, []);
end;

destructor TJvColors.Destroy;
begin
  FComment.Free;
  FNumber.Free;
  FString.Free;
  FSymbol.Free;
  FReserved.Free;
  FStatement.Free;
  FIdentifier.Free;
  FPreproc.Free;
  FFunctionCall.Free;
  FDeclaration.Free;
  FPlainText.Free;
  inherited Destroy;
end;

procedure TJvColors.Assign(Source: TPersistent);
begin
  if Source is TJvColors then
  begin
    FComment.Assign(TJvColors(Source).FComment);
    FNumber.Assign(TJvColors(Source).FNumber);
    FString.Assign(TJvColors(Source).FString);
    FSymbol.Assign(TJvColors(Source).FSymbol);
    FReserved.Assign(TJvColors(Source).FReserved);
    FStatement.Assign(TJvColors(Source).FStatement);
    FIdentifier.Assign(TJvColors(Source).FIdentifier);
    FPreproc.Assign(TJvColors(Source).FPreproc);
    FFunctionCall.Assign(TJvColors(Source).FFunctionCall);
    FDeclaration.Assign(TJvColors(Source).FDeclaration);
    FPlainText.Assign(TJvColors(Source).FPlainText);
  end
  else
    inherited Assign(Source);
end;


//=== TJvHLEditor ============================================================

constructor TJvHLEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parser := TJvIParser.Create;
  Parser.ReturnComments := True;
  FHighLighter := hlPascal;
  FColors := TJvColors.Create;
  FLongTokens := True;
  FSyntaxHighlighting := True;
  ProductionsLine := High(Integer);
end;

destructor TJvHLEditor.Destroy;
begin
  Parser.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure TJvHLEditor.Loaded;
begin
  inherited Loaded;
  RescanLong(0);
end;

procedure TJvHLEditor.SetHighLighter(Value: THighLighter);
begin
  if FHighLighter <> Value then
  begin
    FHighLighter := Value;
    case FHighLighter of
      hlPascal:
        Parser.Style := psPascal;
      hlCBuilder, hlSql, hlJava, hlNQC:
        Parser.Style := psCpp;
      hlPython:
        Parser.Style := psPython;
      hlVB:
        Parser.Style := psVB;
      hlHtml:
        Parser.Style := psHtml;
      hlPerl:
        Parser.Style := psPerl;
      hlIni:
        Parser.Style := psPascal;
      hlCocoR:
        Parser.Style := psCocoR;
      hlPhp:
        Parser.Style := psPhp;
    end;
    RescanLong(0);
    Invalidate;
  end;
end;

procedure TJvHLEditor.GetAttr(Line, ColBeg, ColEnd: Integer);
var
  Token: string;
  i: Integer;
const
  Symbols = [',', ':', ';', '.', '[', ']', '(', ')', '=', '+',
    '-', '/', '<', '>', '%', '*', '~', '''', '\', '^', '@', '{', '}',
    '#', '|', '&'];
const
  DelphiKeyWords =
    ' constructor destructor string record procedure with of' +
    ' repeat until try finally except for to downto case' +
    ' type interface implementation initialization finalization' +
    ' default private public protected published automated property' +
    ' program read write override object nil raise' +
    ' on set xor shr shl begin end args if then else' +
    ' endif goto while do var or and not mod div unit' +
    ' function uses external const class inherited' +
    ' register stdcall cdecl safecall pascal is as package program' +
    ' external overload platform deprecated implements export contains' +
    ' requires resourcestring';

  BuilderKeyWords =
    ' __asm _asm asm auto __automated break bool case catch __cdecl' +
    ' _cdecl cdecl char class __classid __closure const const_cast' +
    ' continue __declspec default delete __dispid do double dynamic_cast' +
    ' else enum __except explicit _export __export extern false __fastcall' +
    ' _fastcall __finally float for friend goto if __import _import inline' +
    ' int __int8 __int16 __int32 __int64 long mutable namespace new operator' +
    ' __pascal _pascal pascal private protected __property public __published' +
    ' register reinterpret_cast return __rtti short signed sizeof static static_cast' +
    ' __stdcall _stdcall struct switch template this __thread throw true __try' +
    ' try typedef typename typeid union using unsigned virtual void volatile' +
    ' wchar_t while ';

  NQCKeyWords: string = {Not Quite C - a C similar language for programming LEGO MindStorm(R) robots }
    ' __event_src __type acquire break __sensor abs asm case catch const' +
    ' continue default do else false for if inline' +
    ' int monitor repeat return signed start stop sub switch task true' +
    ' until void while ';

  SQLKeyWords: string =
    ' active as add asc after ascending all at alter auto' +
    ' and autoddl any avg based between basename blob' +
    ' base_name blobedit before buffer begin by cache  compiletime' +
    ' cast  computed char  close character  conditional character_length  connect' +
    ' char_length  constraint check  containing check_point_len  continue check_point_length  count' +
    ' collate  create collation  cstring column  current commit  cursor' +
    ' committed database  descending date  describe db_key  descriptor debug  disconnect' +
    ' dec  display decimal distinct declare do default  domain' +
    ' delete  double desc drop echo exception edit execute' +
    ' else  exists end  exit entry_point  extern escape  external' +
    ' event  extract fetch foreign file  found filter  from' +
    ' float  full for  function gdscode grant generator group' +
    ' gen_id commit_group_wait global group_commit_wait_time goto' +
    ' having help if  input_type immediate  insert in int' +
    ' inactive  integer index into indicator  is init  isolation' +
    ' inner isql input join key' +
    ' lc_messages  like lc_type  logfile left log_buffer_size length log_buf_size' +
    ' lev  long level manual  merge max  message' +
    ' maximum  min maximum_segment minimum max_segment  module_name names not' +
    ' national  null natural  numeric nchar num_log_bufs no num_log_buffers' +
    ' noauto octet_length or of  order on  outer only output' +
    ' open output_type option overflow page post_event pagelength  precision' +
    ' pages  prepare page_size procedure parameter  protected password  primary' +
    ' plan  privileges position  public quit' +
    ' raw_partitions  retain rdb db_key  return read  returning_values real  returns' +
    ' record_version revoke references  right release  rollback reserv runtime' +
    ' reserving schema  sql segment  sqlcode select  sqlerror set  sqlwarning' +
    ' shadow  stability shared  starting shell  starts show  statement' +
    ' singular  static size  statistics smallint  sub_type snapshot  sum' +
    ' some suspend sort table  translate terminator  translation then  trigger to  trim' +
    ' transaction uncommitted upper union  user unique using update' +
    ' value varying values version varchar view variable' +
    ' wait while when with whenever work where write' +
    ' term new old ';

  PythonKeyWords =
    ' and del for is raise' +
    ' assert elif from lambda return' +
    ' break else global not try' +
    ' class except if or while' +
    ' continue exec import pass' +
    ' def finally in print ';

  JavaKeyWords =
    ' abstract delegate if boolean do implements break double import' +
    ' byte else instanceof case extends int catch false interface' +
    ' char final long class finally multicast continue float' +
    ' default for native short transient new static true' +
    ' null super try package switch void private synchronized volatile' +
    ' protected this while public throw return throws ';

  VBKeyWords =
    ' as and base binary byref byval call case class compare const date debug declare deftype dim do each else elseif ' +
    ' empty end endif enum eqv erase error event execute exit explicit false for friend function get' +
    ' global gosub goto if imp implements input is kill len let line load lock loop lset me mid mod name new next not nothing null on open option optional' +
    ' or paramarray preserve print private property public raiseevent randomize redim rem' +
    ' resume return seek select set static step' +
    ' string sub then time to true unlock until wend while with withevents xor ';

  VBStatements =
    ' access alias any beep ccur cdbl chdir chdrive choose' +
    ' chr cint clear clng clone close cls command compare' +
    ' cos csng cstr curdir currency cvar cvdate ' +
    ' defcur defdbl defint deflng defsng defstr deftype defvar delete deletesetting' +
    ' doevents double dynaset edit environ eof erl err exp fix format ' +
    ' hex int integer isdate isempty isnull isnumeric lbound lcase' +
    ' lib like loc local lof long mkdir oct output pset put' +
    ' random read refresh reset restore rmdir rnd rset savesetting ' +
    ' sendkeys shared single stop system text type typeof ubound unload ' +
    ' using variant vartype write';

  HTMLTags =
    ' doctype a address applet area b base basefont bgsound big blink ' +
    ' blockquote body br caption center cite code col colgroup comment ' +
    ' dfn dir li div dl dt dd em embed font form frame frameset h align ' +
    ' h1 h2 h3 h4 h5 h6 head hr html i iframe img input isindex kbd link ' +
    ' listing map marquee menu meta multicol nextid nobr noframes noscript ' +
    ' object ol option p plaintext pre s samp script select small sound ' +
    ' spacer span strike strong style sub sup table tbody td textarea tfoot' +
    ' th thead title tr tt u ul var wbr xmp ';

  HtmlSpecChars =
    ' Aacute aacute acirc Acirc acute AElig aelig agrave Agrave alefsym ' +
    ' alpha Alpha AMP amp and ang Aring aring asymp atilde Atilde Auml ' +
    ' auml bdquo beta Beta brvbar bull cap Ccedil ccedil cedil cent chi ' +
    ' Chi circ clubs cong copy COPY crarr cup curren dagger Dagger dArr ' +
    ' darr deg Delta delta diams divide eacute Eacute ecirc Ecirc Egrave ' +
    ' egrave empty emsp ensp Epsilon epsilon equiv eta Eta ETH eth Euml ' +
    ' euml euro exist fnof forall frac12 frac14 frac34 frasl Gamma gamma ' +
    ' ge gt GT harr hArr hearts hellip iacute Iacute Icirc icirc iexcl Igrave ' +
    ' igrave image infin int Iota iota iquest isin Iuml iuml kappa Kappa Lambda ' +
    ' lambda lang laquo larr lArr lceil ldquo le lfloor lowast loz lrm lsaquo ' +
    ' lsquo lt LT macr mdash micro middot minus mu Mu nabla nbsp ndash ne ' +
    ' ni not notin nsub Ntilde ntilde Nu nu oacute Oacute ocirc Ocirc oelig ' +
    ' OElig ograve Ograve oline Omega omega omicron Omicron oplus or ordf ' +
    ' ordm Oslash oslash Otilde otilde otimes ouml Ouml para part permil ' +
    ' perp phi Phi Pi pi piv plusmn pound Prime prime prod prop psi Psi quot ' +
    ' QUOT radic rang raquo rArr rarr rceil rdquo real REG reg rfloor Rho ' +
    ' rho rlm rsaquo rsquo sbquo scaron Scaron sdot sect shy Sigma sigma ' +
    ' sigmaf sim spades sub sube sum sup sup1 sup2 sup3 supe szlig Tau ' +
    ' tau there4 Theta theta thetasym thinsp THORN thorn tilde times trade ' +
    ' Uacute uacute uArr uarr ucirc Ucirc ugrave Ugrave uml upsih upsilon ' +
    ' Upsilon uuml Uuml weierp xi Xi Yacute yacute yen yuml Yuml zeta Zeta ' +
    ' zwj zwnj ';

  PerlKeyWords =
    ' sub if else unless foreach next local ' +
    ' return defined until while do elsif eq ';

  PerlStatements =
    ' stat die open print push close defined chdir last read chop ' +
    ' keys sort bind unlink select length ';

  CocoKeyWords = DelphiKeyWords +
    ' compiler productions delphi end_delphi ignore case characters ' +
    ' tokens create destroy errors comments from nested chr any ' +
    ' description ';

  function PosI(const S1, S2: string): Boolean;
  var
    F, P: PChar;
    Len: Integer;
  begin
    Len := Length(S1);
    Result := True;
    P := PChar(S2);
    while P[0] <> #0 do
    begin
      while P[0] = ' ' do Inc(P);
      F := P;
      while not (P[0] <= #32) do Inc(P);
      if (P - F) = Len then
        if StrLIComp(Pointer(S1), F, Len) = 0 then Exit;
    end;
    Result := False;
  end;

  function PosNI(const S1, S2: string): Boolean;
  var
    F, P: PChar;
    Len: Integer;
  begin
    Len := Length(S1);
    Result := True;
    P := PChar(S2);
    while P[0] <> #0 do
    begin
      while P[0] = ' ' do Inc(P);
      F := P;
      while not (P[0] <= #32) do Inc(P);
      if (P - F) = Len then
        if StrLComp(Pointer(S1), F, Len) = 0 then Exit;
    end;
    Result := False;
  end;

  function IsDelphiKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + AnsiLowerCase(St) + ' ', DelphiKeyWords) <> 0;
    Result := PosI(St, DelphiKeyWords);
  end;

  function IsBuilderKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', BuilderKeyWords) <> 0;
    Result := PosNI(St, BuilderKeyWords);
  end;

  function IsNQCKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', NQCKeyWords) <> 0;
    Result := PosNI(St, NQCKeyWords);
  end;

  function IsJavaKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', JavaKeyWords) <> 0;
    Result := PosNI(St, JavaKeyWords);
  end;

  function IsVBKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + LowerCase(St) + ' ', VBKeyWords) <> 0;
    Result := PosI(St, VBKeyWords);
  end;

  function IsVBStatement(const St: string): Boolean;
  begin
//    Result := Pos(' ' + LowerCase(St) + ' ', VBStatements) <> 0;
    Result := PosI(St, VBStatements);
  end;

  function IsSQLKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + AnsiLowerCase(St) + ' ', SQLKeyWords) <> 0;
    Result := PosI(St, SQLKeyWords);
  end;

  function IsPythonKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', PythonKeyWords) <> 0;
    Result := PosNI(St, PythonKeyWords);
  end;

  function IsHtmlTag(const St: string): Boolean;
  begin
//    Result := Pos(' ' + AnsiLowerCase(St) + ' ', HtmlTags) <> 0;
    Result := PosI(St, HtmlTags);
  end;

  function IsHtmlSpecChar(const St: string): Boolean;
  begin
//    Result := Pos(' ' + AnsiLowerCase(St) + ' ', HtmlSpecChars) <> 0;
    Result := PosI(St, HtmlSpecChars);
  end;

  function IsPerlKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', PerlKeyWords) <> 0;
    Result := PosNI(St, PerlKeyWords);
  end;

  function IsPerlStatement(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', PerlStatements) <> 0;
    Result := PosNI(St, PerlStatements);
  end;

  function IsCocoKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + AnsiLowerCase(St) + ' ', CocoKeyWords) <> 0;
    Result := PosI(St, CocoKeyWords);
  end;

  function IsPhpKeyWord(const St: string): Boolean;
  begin
//    Result := Pos(' ' + St + ' ', PerlKeyWords) <> 0;
    Result := PosNI(St, PerlKeyWords);
  end;

  function IsComment(const St: string): Boolean;
  var
    LS: Integer;
  begin
    LS := Length(St);
    case HighLighter of
      hlPascal:
        Result := ((LS > 0) and (St[1] = '{')) or
          ((LS > 1) and (((St[1] = '(') and (St[2] = '*')) or
          ((St[1] = '/') and (St[2] = '/'))));
      hlCBuilder, hlSQL, hlJava, hlPhp, hlNQC:
        Result := (LS > 1) and (St[1] = '/') and
          ((St[2] = '*') or (St[2] = '/'));
      hlVB:
        Result := (LS > 0) and (St[1] = '''');
      hlPython, hlPerl:
        Result := (LS > 0) and (St[1] = '#');
      hlIni:
        Result := (LS > 0) and (St[1] in ['#', ';']);
      hlCocoR:
        Result := (LS > 1) and (((St[1] = '/') and (St[2] = '/')) or
          ((St[1] = '(') and (St[2] = '*')) or
          ((St[1] = '/') and (St[2] = '*'))
          );
    else
      Result := False;
    end;
  end;

  function IsStringConstant(const St: string): Boolean;
  var
    LS: Integer;
  begin
    LS := Length(St);
    case FHighLighter of
      hlPascal, hlCBuilder, hlSql, hlPython, hlJava, hlPerl, hlCocoR, hlPhp, hlNQC:
        Result := (LS > 0) and ((St[1] = '''') or (St[1] = '"'));
      hlVB:
        Result := (LS > 0) and (St[1] = '"');
      hlHtml:
        Result := False;
    else
      Result := False; { unknown highlighter ? }
    end;
  end;

  procedure SetBlockColor(iBeg, iEnd: Integer; Color: TJvSymbolColor);
  var
    I: Integer;
  begin
    if iEnd > Max_X then iEnd := Max_X;
    for I := iBeg to iEnd do
      with LineAttrs[I] do
      begin
        FC := Color.ForeColor;
        BC := Color.BackColor;
        Style := Color.Style;
      end;
  end;

  procedure SetColor(Color: TJvSymbolColor);
  begin
    SetBlockColor(Parser.PosBeg[0] + 1, Parser.PosEnd[0], Color);
  end;

  function NextSymbol: string;
  var
    I: Integer;
  begin
    I := 0;
    while (Parser.PCPos[I] <> #0) and (Parser.PCPos[I] in [' ', #9, #13, #10]) do
      Inc(I);
    Result := Parser.PCPos[I];
  end;

  procedure TestHtmlSpecChars;
  var
    i, j, iBeg, iEnd: Integer;
    S1: string;
    F1: Integer;
  begin
    i := 1;
    F1 := Parser.PosBeg[0];
    while i <= Length(Token) do
    begin
      if Token[i] = '&' then
      begin
        iBeg := i;
        iEnd := iBeg;
        Inc(i);
        while i <= Length(Token) do
        begin
          if Token[i] = ';' then
          begin
            iEnd := i;
            Break;
          end;
          Inc(i);
        end;
        if iEnd > iBeg + 1 then
        begin
          S1 := Copy(Token, iBeg + 1, iEnd - iBeg - 1);
          if IsHtmlSpecChar(S1) then
            for j := iBeg to iEnd do
              with LineAttrs[F1 + j] do
              begin
                FC := Colors.Preproc.ForeColor;
                BC := Colors.Preproc.BackColor;
                Style := Colors.Preproc.Style;
              end;
        end;
      end;
      Inc(i);
    end;
  end;

var
  S: string;
  LS: Integer;

  procedure SetIniColors;
  var
    EquPos: Integer;
  begin
    if (LS > 0) and (S[1] = '[') and (S[LS] = ']') then
      SetBlockColor(0, LS, FColors.FDeclaration)
    else
    begin
      EquPos := Pos('=', S);
      if EquPos > 0 then
      begin
        SetBlockColor(0, EquPos, FColors.FIdentifier);
        SetBlockColor(EquPos, EquPos, FColors.FSymbol);
        SetBlockColor(EquPos + 1, LS, FColors.FString);
      end;
    end;
  end;

  // for Coco/R

  procedure HighlightGrammarName;
  var
    P: Integer;
  begin
    P := Pos('-->Grammar<--', S);
    if P > 0 then
      SetBlockColor(P, P + Length('-->Grammar<--') - 1, FColors.FPreproc);
  end;

// (rom) const, var, local function sequence not cleaned up yet
var
  F: Boolean;
  C: TJvSymbolColor;
  Reserved: Boolean;
  PrevToken: string;
  PrevToken2: string;
  NextToken: string;
  InTag: Boolean;
  N: Integer;
begin
  if not FSyntaxHighlighting then Exit;
  S := Lines[Line];
  if (FHighLighter = hlNone) and not UserReservedWords then
    C := Colors.PlainText
  else
  begin
    FLine := S;
    FLineNum := Line;
    Parser.pcProgram := PChar(S);
    Parser.pcPos := Parser.pcProgram;
    CheckInLong;
    LS := Length(S);
    if (FHighLighter in [hlCBuilder, hlNQC]) and (LS > 0) and
      (((GetTrimChar(S, 1) = '#') and (FLong = 0)) or (FLong = lgPreproc)) then
      C := FColors.FPreproc
    else
    if ((FHighLighter in [hlPython, hlPerl]) and (LS > 0) and
      (S[1] = '#') and (FLong = 0)) or
      ((FHighLighter = hlIni) and (LS > 0) and (S[1] in ['#', ';'])) then
      C := FColors.FComment
    else
      C := FColors.FPlainText;
    if (FLong <> 0) and (FHighLighter <> hlHtml) then
    begin
      Parser.pcPos := Parser.pcProgram + FindLongEnd + 1;
      if (FHighLighter in [hlCBuilder, hlPython, hlPerl, hlNQC]) then
        case FLong of
          lgString:
            C := FColors.FString;
          lgComment1, lgComment2:
            C := FColors.FComment;
          lgPreproc:
            C := FColors.Preproc;
        end
      else
        C := FColors.FComment;
    end;
  end;

  LineAttrs[1].FC := C.ForeColor;
  LineAttrs[1].Style := C.Style;
  LineAttrs[1].BC := C.BackColor;
  N := Min(Max_X, Length(S));
  for i := 2 to N do
    Move(LineAttrs[1], LineAttrs[i], SizeOf(LineAttrs[1]));
  if Length(S) < Max_X then
  begin
    LineAttrs[N + 1].FC := Font.Color;
    LineAttrs[N + 1].Style := Font.Style;
    LineAttrs[N + 1].BC := Color;
    for i := N + 1 + 1 to Max_X do
      Move(LineAttrs[N + 1], LineAttrs[i], SizeOf(LineAttrs[1]));
  end;

  if (FHighLighter = hlNone) and not UserReservedWords then
    Exit;
  if (Length(S) > 0) and (((GetTrimChar(S, 1) = '#') and
    (FHighLighter in [hlCBuilder, hlPython, hlPerl, hlNQC])) or
    ((GetTrimChar(S, 1) in ['#', ';']) and (FHighLighter = hlIni))) then
    Exit;

  if FHighLighter = hlIni then
    SetIniColors
  else
  try
    InTag := FLong = lgTag;
    PrevToken := '';
    PrevToken2 := '';
    Token := Parser.Token;
    while Token <> '' do
    begin
      F := True;
      if GetReservedWord(Token, Reserved) then
      begin
        if Reserved then
          SetColor(FColors.FReserved)
        else
          F := False;
      end
      else
        case FHighLighter of
          hlPascal:
            if IsDelphiKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
              F := False;
          hlCBuilder:
            if IsBuilderKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
              F := False;
          hlNQC:
            if IsNQCKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
              F := False;
          hlSql:
            if IsSQLKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
              F := False;
          hlPython:
            if IsPythonKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
            if Token = 'None' then
              SetColor(FColors.FNumber)
            else
            if (PrevToken = 'def') or (PrevToken = 'class') then
              SetColor(FColors.FDeclaration)
            else
            if (NextSymbol = '(') and IsIdentifier(Token) then
              SetColor(FColors.FFunctionCall)
            else
              F := False;
          hlJava:
            if IsJavaKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
            if PrevToken = 'function' then
              SetColor(FColors.FDeclaration)
            else
              F := False;
          hlVB:
            if IsVBKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
            if IsVBStatement(Token) then
              SetColor(FColors.FStatement)
            else
            if Cmp(PrevToken, 'function') or Cmp(PrevToken, 'sub') or
              Cmp(PrevToken, 'class') then
              SetColor(FColors.FDeclaration)
            else
              F := False;
          hlHtml:
            if not InTag then
            begin
              if Token = '<' then
              begin
                InTag := True;
                SetColor(FColors.FReserved)
              end;
              F := True;
            end
            else
            begin
              if Token = '>' then
              begin
                InTag := False;
                SetColor(FColors.FReserved)
              end
              else
              if (Token = '/') and (PrevToken = '<') then
                SetColor(FColors.FReserved)
              else
              if (NextSymbol = '=') and IsIdentifier(Token) then
                SetColor(FColors.FIdentifier)
              else
              if PrevToken = '=' then
                SetColor(FColors.FString)
              else
              if IsHtmlTag(Token) then
                SetColor(FColors.FReserved)
              else
              if (PrevToken = '<') or ((PrevToken = '/') and (PrevToken2 = '<')) then
                SetColor(FColors.FStatement)
              else
                F := False;
            end;
          hlPerl:
            if IsPerlKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
            if IsPerlStatement(Token) then
              SetColor(FColors.FStatement)
            else
            if Token[1] in ['$', '@', '%', '&'] then
              SetColor(FColors.FFunctionCall)
            else
              F := False;
          hlCocoR:
            if IsCocoKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
            if (Parser.PosBeg[0] = 0) and (Line > ProductionsLine) and
              IsIdentifier(Token) then
            begin
              NextToken := Parser.Token;
              Parser.RollBack(1);
              SetColor(FColors.FDeclaration)
            end
            else
              F := False;
          hlPhp:
            if IsPhpKeyWord(Token) then
              SetColor(FColors.FReserved)
            else
              F := False;
        else
          F := False;
        end;
      if F then
        {Ok}
      else
      if IsComment(Token) then
        SetColor(FColors.FComment)
      else
      if IsStringConstant(Token) then
        SetColor(FColors.FString)
      else
      if (Length(Token) = 1) and (Token[1] in Symbols) then
        SetColor(FColors.FSymbol)
      else
      if IsIntConstant(Token) or IsRealConstant(Token) then
        SetColor(FColors.FNumber)
      else
      if (FHighLighter in [hlCBuilder, hlJava, hlPython, hlPhp, hlNQC]) and
        (PrevToken = '0') and (Token[1] in ['x', 'X']) then
        SetColor(FColors.FNumber)
      else
      if FHighLighter = hlHtml then
        SetColor(FColors.FPlainText)
      else
        SetColor(FColors.FIdentifier);
      if FHighLighter = hlHtml then
        { found special chars starting with '&' and ending with ';' }
        TestHtmlSpecChars;
      PrevToken2 := PrevToken;
      PrevToken := Token;
      Token := Parser.Token;
    end;

    if Highlighter = hlCocoR then
      HighlightGrammarName;
  except
  end;
end;

procedure TJvHLEditor.CheckInLong;
begin
  if not FLongTokens then
  begin
    FLong := lgNone;
    Exit;
  end;
  if FLineNum < Length(FLongDesc) then
  begin
    FLong := FLongDesc[FLineNum];
    if FLong = lgUndefined then
    begin
      RescanLong(FLineNum); // scan the line
      FLong := FLongDesc[FLineNum];
    end;
  end
  else
    { oh my god!, it's very big text }
    FLong := lgNone;
end;

function TJvHLEditor.RescanLong(iLine: Integer): Boolean;
const
  MaxScanLinesAtOnce = 5000;
var
  P, F: PChar;
  MaxLine, MaxScanLine: Integer;
  S: string;
  i, i1, L1: Integer;
begin
  FLong := lgNone;
  Result := False; // no Invalidate

  if (not FSyntaxHighlighting) or
     (not FLongTokens or (FHighLighter in [hlNone, hlIni])) or
     (Lines.Count = 0) then
    Exit;

  ProductionsLine := High(Integer);
  MaxLine := Lines.Count - 1;
  if MaxLine > High(FLongDesc) then
    MaxLine := High(FLongDesc);
  if iLine > MaxLine then Exit;;

  MaxScanLine := MaxLine;
  FLong := lgNone;
  if iLine < 0 then
  begin
    FillChar(FLongDesc[0], SizeOf(FLongDesc[0]) * (1 + MaxLine), lgUndefined);
    FLongDesc[0] := lgNone;
    iLine := 0;
  end
  else
  begin
    FLong := FLongDesc[iLine];
    if FLong = lgUndefined then
    begin
      if (iLine > 0) and (FLongDesc[iLine - 1] = lgUndefined) then
      begin
        iLine := 0; // scan all
        FLong := lgNone;
      end
      else
      begin
        Dec(iLine);
        FLong := FLongDesc[iLine];
        MaxScanLine := Min(iLine + MaxScanLinesAtOnce, MaxLine);
      end;
    end
    else
      MaxScanLine := Min(iLine + MaxScanLinesAtOnce, MaxLine);
  end;

  while iLine < MaxScanLine do
  begin
    { only real programmer can write loop on 5 pages }
    // (rom) real programmers do not add comments to end ;-)
    S := Lines[iLine];
    P := Pointer(S);
    F := P;
    L1 := Length(S);
    if (L1 = 0) and (FLong in [lgPreproc, lgString]) then FLong := lgNone;
    i := 1;
    while i <= L1 do
    begin
      case FHighLighter of
        hlPascal:
          case FLong of
            lgNone: //  not in comment
              case S[i] of
                '{':
                  begin
                    P := StrScan(F + i, '}');
                    if P = nil then
                    begin
                      FLong := lgComment1;
                      Break;
                    end
                    else
                      i := P - F + 1;
                  end;
                '(':
                  if {S[i + 1]} F[i] = '*' then
                  begin
                    FLong := lgComment2;
                    P := StrScan(F + i + 2, ')');
                    if P = nil then
                      Break
                    else
                    begin
                      if P[-1] = '*' then
                        FLong := lgNone;
                      i := P - F + 1;
                    end;
                  end;
                '''':
                  begin
                    P := StrScan(F + i + 1, '''');
                    if P <> nil then
                    begin
                      i1 := P - F;
                      if P[1] <> '''' then
                        i := i1
                      else
                        { ?? }
                    end
                    else
                      i := L1 + 1;
                  end;
              end;
            lgComment1:
              begin //  {
                P := StrScan(F + i - 1, '}');
                if P <> nil then
                begin
                  FLong := lgNone;
                  i := P - F + 1;
                end
                else
                  i := L1 + 1;
              end;
            lgComment2:
              begin //  (*
                P := StrScan(F + i, ')');
                if P = nil then
                  Break
                else
                begin
                  if P[-1] = '*' then
                    FLong := lgNone;
                  i := P - F + 1;
                end;
              end;
          end;
        hlCBuilder, hlSql, hlJava, hlPhp, hlNQC:
          case FLong of
            lgNone: //  not in comment
              case S[i] of
                '/':
                  if {S[i + 1]} F[i] = '*' then
                  begin
                    FLong := lgComment2;
                    P := StrScan(F + i + 2, '/');
                    if P = nil then
                      Break
                    else
                    begin
                      if P[-1] = '*' then
                        FLong := lgNone;
                      i := P - F + 1;
                    end;
                  end;
                '"':
                  begin
                    P := StrScan(F + i + 1, '"');
                    if P <> nil then
                    begin
                      i1 := P - F;
                      if P[1] <> '"' then
                        i := i1
                      else
                        { ?? }
                    end
                    else
                    if FHighlighter in [hlCBuilder, hlJava, hlNQC] then
                    begin
                      if (LastNoSpaceChar(S) = '\') and (HasStringOpenEnd(Lines, iLine)) then
                        FLong := lgString;
                      i := L1 + 1;
                    end
                    else
                      i := L1 + 1;
                  end;
                '#':
                  begin
                    if (GetTrimChar(S, 1) = '#') and (LastNoSpaceChar(S) = '\') then
                    begin
                      FLong := lgPreproc;
                      Break;
                    end;
                  end;
              end;
            lgComment2:
              begin //  /*
                P := StrScan(F + i, '/');
                if P = nil then
                  Break
                else
                begin
                  if P[-1] = '*' then
                    FLong := lgNone;
                  i := P - F + 1;
                end;
              end;
            lgString:
              begin
                P := StrScan(F + i + 1, '"');
                if P <> nil then
                begin
                  i1 := P - F;
                  if P[1] <> '"' then
                    i := i1
                  else
                    { ?? }
                end
                else
                begin
                  if FHighlighter in [hlCBuilder, hlJava, hlNQC] then
                  begin
                    if (LastNoSpaceChar(S) <> '\') or (not HasStringOpenEnd(Lines, iLine)) then
                      FLong := lgNone;
                  end;
                  i := L1 + 1;
                end;
              end;
            lgPreproc:
              begin
                if LastNoSpaceChar(S) <> '\' then
                  FLong := lgNone;
              end;
          end;
        hlPython, hlPerl:
          case FLong of
            lgNone: //  not in comment
              case S[i] of
                '#':
                  i := L1;
                '"':
                  begin
                    P := StrScan(F + i, '"');
                    if P = nil then
                    begin
                      FLong := lgString;
                      Break;
                    end
                    else
                      i := P - F + 1;
                  end;
              end;
            lgString: // python and perl long string
              begin
                P := StrScan(F + i - 1, '"');
                if P <> nil then
                begin
                  FLong := lgNone;
                  i := P - F + 1;
                end
                else
                  i := L1 + 1;
              end;
          end;
        hlHtml:
          case FLong of
            lgNone: //  not in comment
              case S[i] of
                '<':
                  begin
                    P := StrScan(F + i, '>');
                    if P = nil then
                    begin
                      FLong := lgTag;
                      Break;
                    end
                    else
                      i := P - F + 1;
                  end;
              end;
            lgTag: // html tag
              begin
                P := StrScan(F + i - 1, '>');
                if P <> nil then
                begin
                  FLong := lgNone;
                  i := P - F + 1;
                end
                else
                  i := L1 + 1;
              end;
          end;
        hlCocoR:
          case FLong of
            lgNone: //  not in comment
              case S[i] of
                '(':
                  if {S[i + 1]} F[i] = '*' then
                  begin
                    FLong := lgComment2;
                    P := StrScan(F + i + 2, ')');
                    if P = nil then
                      Break
                    else
                    begin
                      if P[-1] = '*' then
                        FLong := lgNone;
                      i := P - F + 1;
                    end;
                  end;
                '"':
                  begin
                    P := StrScan(F + i + 1, '"');
                    if P <> nil then
                    begin
                      i1 := P - F;
                      if P[1] <> '"' then
                        i := i1
                      else
                        { ?? }
                    end
                    else
                      i := L1 + 1;
                  end;
                '''':
                  begin
                    P := StrScan(F + i + 1, '''');
                    if P <> nil then
                    begin
                      i1 := P - F;
                      if P[1] <> '''' then
                        i := i1
                      else
                        { ?? }
                    end
                    else
                      i := L1 + 1;
                  end;
                '/':
                  if {S[i + 1]} F[i] = '*' then
                  begin
                    FLong := lgComment2;
                    P := StrScan(F + i + 2, '/');
                    if P = nil then
                      Break
                    else
                    begin
                      if P[-1] = '*' then
                        FLong := lgNone;
                      i := P - F + 1;
                    end;
                  end;
              end;
            lgComment2:
              begin //  (*
                P := StrScan(F + i, ')');
                if P = nil then
                  Break
                else
                begin
                  if P[-1] = '*' then
                    FLong := lgNone;
                  i := P - F + 1;
                end;
              end;
          end;
      end;
      Inc(i);
    end;

    if (FHighLighter = hlCocoR) and
      (StrLIComp(PChar(S), 'productions', Length('productions')) = 0) then
    begin
      ProductionsLine := iLine;
    end;


    Inc(iLine);
    if FLongDesc[iLine] <> FLong then
    begin
      FLongDesc[iLine] := FLong;
      Result := True; // Invalidate
    end;
  end;
 // undefine following lines
  if MaxScanLine < MaxLine then
    FillChar(FLongDesc[MaxScanLine + 1], SizeOf(FLongDesc[0]) * (MaxLine - MaxScanLine), lgUndefined);
end;

function TJvHLEditor.FindLongEnd: Integer;
var
  P, F: PChar;
  i: integer;
begin
  P := PChar(FLine);
  Result := Length(FLine);
  case FHighLighter of
    hlPascal:
      case FLong of
        lgComment1:
          begin
            P := StrScan(P, '}');
            if P <> nil then
              Result := P - PChar(FLine);
          end;
        lgComment2:
          begin
            F := P;
            while True do
            begin
              F := StrScan(F, '*');
              if F = nil then
                Exit;
              if F[1] = ')' then
                Break;
              Inc(F);
            end;
            P := F + 1;
            Result := P - PChar(FLine);
          end;
      end;
    hlCBuilder, hlSql, hlJava, hlPhp, hlNQC:
      begin
        case FLong of
          lgComment2:
            begin
              F := P;
              while True do
              begin
                F := StrScan(F, '*');
                if F = nil then
                  Exit;
                if F[1] = '/' then
                  Break;
                Inc(F);
              end;
              P := F + 1;
              Result := P - PChar(FLine);
            end;
          lgString:
            begin
              F := P;
              repeat
                P := StrScan(P, '"');
                if P <> nil then
                begin
                  if (P = F) or (P[-1] <> '\') then
                  begin
                    Result := P - F;
                    Break;
                  end
                  else
                  begin
                   // count the backslashes
                    i := 1;
                    while (P - 1 - i > F) and (P[-1 - i] = '\') do Inc(i);
                    if i and $01 = 0 then {faster than: if i mod 2 = 0 then}
                    begin
                      Result := P - F;
                      Break;
                    end;
                  end;
                  Inc(P);
                end;
              until P = nil;
            end;
          end;  // case
      end;
    hlPython, hlPerl:
      case FLong of
        lgString:
          begin
            P := StrScan(P, '"');
            if P <> nil then
              Result := P - PChar(FLine);
          end;
      end;
    hlHtml:
      case FLong of
        lgTag:
          begin
            P := StrScan(P, '>');
            if P <> nil then
              Result := P - PChar(FLine);
          end;
      end;
  end;
end;

procedure TJvHLEditor.TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
  const Text: string);
var
  S: string;
{  LP, i: Integer;
  P: PChar;
  OldProductionsLine: Integer; }
begin
  if not FLongTokens then
    Exit;
  case FHighLighter of
    hlPascal:
      S := '{}*()/'#13;
    hlCBuilder, hlJava, hlSql, hlPhp, hlNQC:
      S := '*/'#13'\';
    hlVB:
      S := '''' + #13;
    hlPython, hlPerl:
      S := '#"'#13;
    hlHtml:
      S := '<>'#13;
    hlCocoR:
      S := '*()/'#13;
  else
    S := #13; { unknown highlighter ? }
  end;
  if (Action in [maAll, maReplace]) or HasAnyChar(S, Text) then
  begin
    if Action = maAll then
      ACaretY := -1;  // rescan all lines

    if RescanLong(ACaretY) then
      Invalidate;
  end;
 {
  if (FHighLighter = hlCocoR) and (HasAnyChar('productions'#13, Text)) then
  begin
    LP := Length('productions');
    OldProductionsLine := ProductionsLine;
    ProductionsLine := High(Integer);
    for i := 0 to Lines.Count - 1 do
    begin
      P := PChar(Lines[i]);
      if (StrLIComp(P, 'productions', LP) = 0) and
         ((Length(P) = LP) or (P[LP] = ' ')) then
      begin
        ProductionsLine := i;
        Break;
      end;
    end;
    if ProductionsLine <> OldProductionsLine then
      Invalidate;
  end; }
end;

function TJvHLEditor.GetReservedWord(const Token: string;
  var Reserved: Boolean): Boolean;
begin
  Result := Assigned(FOnReservedWord);
  if Result then
  begin
    Reserved := False;
    FOnReservedWord(Self, Token, Reserved);
  end
end;

function TJvHLEditor.UserReservedWords: Boolean;
begin
  Result := Assigned(FOnReservedWord);
end;

procedure TJvHLEditor.Assign(Source: TPersistent);
begin
  if Source is TJvHLEditor then
  begin
    Colors.Assign((Source as TJvHLEditor).Colors);
    SelForeColor := (Source as TJvHLEditor).SelForeColor;
    SelBackColor := (Source as TJvHLEditor).SelBackColor;
    Color := (Source as TJvHLEditor).Color;
    RightMarginColor := (Source as TJvHLEditor).RightMarginColor;
    Invalidate;
  end
  else
    inherited Assign(Source);
end;


type
  TDelphiColor = record
    ForeColor, BackColor: TColor;
    Style: TFontStyles;
  end;

const
  DelphiColor_Comment: TDelphiColor = (ForeColor: clNavy; BackColor: clWindow; Style: [fsItalic]);
  DelphiColor_Number: TDelphiColor = (ForeColor: clNavy; BackColor: clWindow; Style: []);
  DelphiColor_Strings: TDelphiColor = (ForeColor: clBlue; BackColor: clWindow; Style: []);
  DelphiColor_Symbol: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: []);
  DelphiColor_Reserved: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: [fsBold]);
  DelphiColor_Identifier: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: []);
  DelphiColor_PlainText: TDelphiColor = (ForeColor: clWindowText; BackColor: clWindow; Style: []);

function TJvHLEditor.GetDelphiColors: Boolean;
  function CompareColor(Symbol: TJvSymbolColor; const DelphiColor: TDelphiColor): Boolean;
  begin
    Result := (Symbol.ForeColor = DelphiColor.ForeColor) and
              (Symbol.BackColor = DelphiColor.BackColor) and
              (Symbol.Style = DelphiColor.Style);
  end;
begin
  Result := False;
  if not CompareColor(FColors.Comment, DelphiColor_Comment) then Exit;
  if not CompareColor(FColors.Number, DelphiColor_Number) then Exit;
  if not CompareColor(FColors.Strings, DelphiColor_Strings) then Exit;
  if not CompareColor(FColors.Symbol, DelphiColor_Symbol) then Exit;
  if not CompareColor(FColors.Reserved, DelphiColor_Reserved) then Exit;
  if not CompareColor(FColors.Identifier, DelphiColor_Identifier) then Exit;
  if not CompareColor(FColors.PlainText, DelphiColor_PlainText) then Exit;
  Result := True;
end;

procedure TJvHLEditor.SetDelphiColors(Value: Boolean);
  procedure SetColor(Symbol: TJvSymbolColor; const DelphiColor: TDelphiColor);
  begin
    with DelphiColor do
      Symbol.SetColor(ForeColor, BackColor, Style);
  end;
begin
  if Value then
  begin
    SetColor(FColors.FComment, DelphiColor_Comment);
    SetColor(FColors.FNumber, DelphiColor_Number);
    SetColor(FColors.Strings, DelphiColor_Strings);
    SetColor(FColors.Symbol, DelphiColor_Symbol);
    SetColor(FColors.Reserved, DelphiColor_Reserved);
    SetColor(FColors.Identifier, DelphiColor_Identifier);
    SetColor(FColors.PlainText, DelphiColor_PlainText);
  end;
end;

end.

