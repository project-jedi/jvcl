{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUnicodeHLEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvWideHLEditor
description : JvEditor with built-in highlighting for:
              pascal, cbuilder, sql, python, jscript,
              vbscript, perl, ini, html, not quite c
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvUnicodeHLEditor;

interface

uses
  {$IFNDEF COMPILER6_UP}
  Windows,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, Graphics,
  JvEditorCommon, JvUnicodeEditor, JvHLParser, JvWStrUtils;

type
  TJvWideHLEditor = class;

  TOnReservedWord = procedure(Sender: TObject; Token: WideString;
    var Reserved: Boolean) of object;

  TJvWideEditorHighlighter = class(TComponent)
  protected
    procedure GetAttr(Editor: TJvWideHLEditor; Lines: TWStrings; Line, ColBeg, ColEnd: Integer;
      LongToken: TLongTokenType; var LineAttrs: TLineAttrs); virtual; abstract;
    procedure ScanLongTokens(Editor: TJvWideHLEditor; Lines: TWStrings; Line: Integer;
      var FLong: TLongTokenType); virtual; abstract;
    function GetRescanLongKeys(Editor: TJvWideHLEditor; Action: TModifiedAction;
      ACaretX, ACaretY: Integer; const Text: WideString): Boolean; virtual; abstract;
  end;

  TJvWideHLEditor = class(TJvWideEditor, IJvHLEditor)
  private
    Parser: TJvIParserW;
    FHighlighter: TJvHighlighter;
    FColors: TJvColors;
    FLine: WideString;
    FLineNum: Integer;
    FLong: TLongTokenType;
    FLongTokens: Boolean;
    FLongDesc: array[0..Max_Line] of TLongTokenType;
    FSyntaxHighlighting: Boolean;
    FSyntaxHighlighter: TJvWideEditorHighlighter;
    FOnReservedWord: TOnReservedWord;

    // Coco/R
    ProductionsLine: Integer;
    function RescanLong(iLine: Integer): Boolean;
    procedure CheckInLong;
    function FindLongEnd: Integer;
    procedure SetHighlighter(const Value: TJvHighlighter);
    function GetDelphiColors: Boolean;
    procedure SetDelphiColors(Value: Boolean);
    function GetColors: TJvColors;
    procedure SetColors(const Value: TJvColors);
    function GetSyntaxHighlighting: Boolean;
    procedure SetSyntaxHighlighting(Value: Boolean);
    function GetHighlighter: TJvHighlighter;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetAttr(Line, ColBeg, ColEnd: Integer); override;
    procedure TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
      const Text: WideString); override;
    function GetReservedWord(const Token: WideString; var Reserved: Boolean): Boolean; virtual;
    function UserReservedWords: Boolean; virtual;
    procedure SetSyntaxHighlighter(const Value: TJvWideEditorHighlighter);
    procedure AssignTo(Source: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Highlighter: TJvHighlighter read GetHighlighter write SetHighlighter default hlPascal;
    property Colors: TJvColors read GetColors write SetColors;
    property DelphiColors: Boolean read GetDelphiColors write SetDelphiColors stored False;
    property LongTokens: Boolean read FLongTokens write FLongTokens default True;
    property OnReservedWord: TOnReservedWord read FOnReservedWord write FOnReservedWord;
    property SyntaxHighlighting: Boolean read GetSyntaxHighlighting write SetSyntaxHighlighting stored False;
    property SyntaxHighlighter: TJvWideEditorHighlighter read FSyntaxHighlighter write SetSyntaxHighlighter;
  end;

implementation

uses
  Math,
  JvHLEditor, // for Assign
  JvJCLUtils, JvConsts;

function LastNonSpaceChar(const S: WideString): WideChar;
var
  I: Integer;
begin
  Result := #0;
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  if I > 0 then
    Result := S[I];
end;

function GetTrimChar(const S: WideString; Index: Integer): WideChar;
var
  LS, L: Integer;
begin
  LS := Length(S);
  if LS <> 0 then
  begin
    L := 1;
    while (L <= LS) and (S[L] = ' ') do
      Inc(L);
    if L <= LS then
      Result := S[L - 1 + Index]
    else
      Result := S[Index];
  end
  else
    Result := #0;
end;

function HasStringOpenEnd(Lines: TWStrings; iLine: Integer): Boolean;
{ find C/C++ "line breaker" '\' }
var
  I: Integer;
  IsOpen: Boolean;
  P, F: PWideChar;
  S: WideString;
begin
  Result := False;
  if (iLine < 0) or (iLine >= Lines.Count) then
    Exit;
  I := iLine - 1;
  IsOpen := False;
  if (I >= 0) and (LastNonSpaceChar(Lines[I]) = '\') then // check prior lines
    IsOpen := HasStringOpenEnd(Lines, I);
  S := Lines[iLine];
  F := PWideChar(S);
  P := F;
  repeat
    P := StrScanW(P, WideChar('"'));
    if P <> nil then
    begin
      if (P = F) or (P[-1] <> '\') then
        IsOpen := not IsOpen
      else
      begin
       // count the backslashes
        I := 1;
        while (P-1-I > F) and (P[-1-I] = '\') do
          Inc(I);
        if I mod 2 = 0 then
          IsOpen := not IsOpen;
      end;
      Inc(P);
    end;
  until P = nil;
  Result := IsOpen;
end;

function StrScanW(P: PWideChar; Ch: WideChar): PWideChar;
begin
  Result := P;
  while True do
  begin
    if Result[0] = Ch then
      Exit
    else
    if Result[0] = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

//=== { TJvWideHLEditor } ========================================================

constructor TJvWideHLEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parser := TJvIParserW.Create;
  Parser.ReturnComments := True;
  FHighlighter := hlPascal;
  FColors := TJvColors.Create;
  FLongTokens := True;
  FSyntaxHighlighting := True;
  ProductionsLine := High(Integer);
end;

destructor TJvWideHLEditor.Destroy;
begin
  Parser.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure TJvWideHLEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSyntaxHighlighter) then
    SyntaxHighlighter := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvWideHLEditor.Loaded;
begin
  inherited Loaded;
  RescanLong(0);
end;

procedure TJvWideHLEditor.SetHighlighter(const Value: TJvHighlighter);
begin
  if FHighlighter <> Value then
  begin
    FHighlighter := Value;
    case FHighlighter of
      hlPascal:
        Parser.Style := psPascal;
      hlCBuilder, hlSql, hlJava, hlNQC, hlCSharp:
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

procedure TJvWideHLEditor.GetAttr(Line, ColBeg, ColEnd: Integer);
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

  NQCKeyWords = {Not Quite C - a C similar language for programming LEGO MindStorm(R) robots }
    ' __event_src __type acquire break __sensor abs asm case catch const' +
    ' continue default do else false for if inline' +
    ' int monitor repeat return signed start stop sub switch task true' +
    ' until void while ';

  SQLKeyWords =
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

  HTMLSpecChars =
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

  function PosI(const S1, S2: WideString): Boolean;
  var
    F, P: PWideChar;
    Len: Integer;
  begin
    Len := Length(S1);
    Result := True;
    P := PWideChar(S2);
    while P[0] <> #0 do
    begin
      while P[0] = ' ' do
        Inc(P);
      F := P;
      while not (P[0] <= #32) do
        Inc(P);
      if (P - F) = Len then
        if StrLICompW2(PWideChar(S1), F, Len) = 0 then
          Exit;
    end;
    Result := False;
  end;

  function PosNI(const S1, S2: WideString): Boolean;
  var
    F, P: PWideChar;
    Len: Integer;
  begin
    Len := Length(S1);
    Result := True;
    P := PWideChar(S2);
    while P[0] <> #0 do
    begin
      while P[0] = ' ' do
        Inc(P);
      F := P;
      while not (P[0] <= #32) do
        Inc(P);
      if (P - F) = Len then
        if StrLCompW(PWideChar(S1), F, Len) = 0 then
          Exit;
    end;
    Result := False;
  end;

  function IsDelphiKeyWord(const St: WideString): Boolean;
  begin
    Result := PosI(St, DelphiKeyWords);
  end;

  function IsBuilderKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, BuilderKeyWords);
  end;

  function IsNQCKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, NQCKeyWords);
  end;

  function IsJavaKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, JavaKeyWords);
  end;

  function IsVBKeyWord(const St: WideString): Boolean;
  begin
    Result := PosI(St, VBKeyWords);
  end;

  function IsVBStatement(const St: WideString): Boolean;
  begin
    Result := PosI(St, VBStatements);
  end;

  function IsSQLKeyWord(const St: WideString): Boolean;
  begin
    Result := PosI(St, SQLKeyWords);
  end;

  function IsPythonKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, PythonKeyWords);
  end;

  function IsHtmlTag(const St: WideString): Boolean;
  begin
    Result := PosI(St, HTMLTags);
  end;

  function IsHtmlSpecChar(const St: WideString): Boolean;
  begin
    Result := PosI(St, HTMLSpecChars);
  end;

  function IsPerlKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, PerlKeyWords);
  end;

  function IsPerlStatement(const St: WideString): Boolean;
  begin
    Result := PosNI(St, PerlStatements);
  end;

  function IsCocoKeyWord(const St: WideString): Boolean;
  begin
    Result := PosI(St, CocoKeyWords);
  end;

  function IsPhpKeyWord(const St: WideString): Boolean;
  begin
    Result := PosNI(St, PerlKeyWords);
  end;

  function IsComment(const St: WideString): Boolean;
  var
    LS: Integer;
  begin
    LS := Length(St);
    case Highlighter of
      hlPascal:
        Result := ((LS > 0) and (St[1] = '{')) or
          ((LS > 1) and (((St[1] = '(') and (St[2] = '*')) or
          ((St[1] = '/') and (St[2] = '/'))));
      hlCBuilder, hlSql, hlJava, hlPhp, hlNQC:
        Result := (LS > 1) and (St[1] = '/') and
          ((St[2] = '*') or (St[2] = '/'));
      hlVB:
        Result := (LS > 0) and (St[1] = '''');
      hlPython, hlPerl:
        Result := (LS > 0) and (St[1] = '#');
      hlIni:
        Result := (LS > 0) and ((St[1] = '#') or (St[1] = ';'));
      hlCocoR:
        Result := (LS > 1) and (((St[1] = '/') and (St[2] = '/')) or
          ((St[1] = '(') and (St[2] = '*')) or
          ((St[1] = '/') and (St[2] = '*'))
          );
    else
      Result := False;
    end;
  end;

  function IsPreproc(const St: WideString): Boolean;
  var
    LS: Integer;
  begin
    LS := Length(St);
    case Highlighter of
      hlPascal:
        Result := ((LS > 0) and ((St[1] = '{') and (St[2] = '$'))) or
          ((LS > 1) and (((St[1] = '(') and (St[2] = '*') and (St[3] = '$'))));
      {hlCBuilder, hlSql, hlJava, hlPhp, hlNQC:
      hlVB:
      hlPython, hlPerl:
      hlIni:
      hlCocoR:}
    else
      Result := False;
    end;
  end;

  function IsStringConstant(const St: WideString): Boolean;
  var
    LS: Integer;
  begin
    LS := Length(St);
    case FHighlighter of
      hlPascal, hlCBuilder, hlSql, hlPython, hlJava, hlPerl, hlCocoR, hlPhp, hlNQC:
        Result := (LS > 0) and ((St[1] = '''') or (St[1] = '"'));
      hlVB:
        Result := (LS > 0) and (St[1] = '"');
      hlHtml:
        Result := False;
    else
      Result := False; { unknown Highlighter ? }
    end;
  end;

  procedure SetBlockColor(iBeg, iEnd: Integer; Color: TJvSymbolColor);
  var
    I: Integer;
  begin
    if iEnd > Max_X then
      iEnd := Max_X;
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

  function NextSymbol: WideString;
  var
    I: Integer;
  begin
    I := 0;
    while (Parser.pcPos[I] <> #0) and CharInSetW(Parser.pcPos[I], [' ', Tab, Lf, Cr]) do
      Inc(I);
    Result := Parser.pcPos[I];
  end;

  procedure TestHtmlSpecChars(const Token: WideString);
  var
    I, J, iBeg, iEnd: Integer;
    S1: WideString;
    F1: Integer;
  begin
    I := 1;
    F1 := Parser.PosBeg[0];
    while I <= Length(Token) do
    begin
      if Token[I] = '&' then
      begin
        iBeg := I;
        iEnd := iBeg;
        Inc(I);
        while I <= Length(Token) do
        begin
          if Token[I] = ';' then
          begin
            iEnd := I;
            Break;
          end;
          Inc(I);
        end;
        if iEnd > iBeg + 1 then
        begin
          S1 := Copy(Token, iBeg + 1, iEnd - iBeg - 1);
          if IsHtmlSpecChar(S1) then
            for J := iBeg to iEnd do
              with LineAttrs[F1 + J] do
              begin
                FC := Colors.Preproc.ForeColor;
                BC := Colors.Preproc.BackColor;
                Style := Colors.Preproc.Style;
              end;
        end;
      end;
      Inc(I);
    end;
  end;

  procedure SetIniColors(const S: WideString);
  var
    EquPos: Integer;
    LS: Integer;
  begin
    LS := Length(S);
    if (LS > 0) and (S[1] = '[') and (S[LS] = ']') then
      SetBlockColor(0, LS, Colors.Declaration)
    else
    begin
      EquPos := Pos('=', S);
      if EquPos > 0 then
      begin
        SetBlockColor(0, EquPos, Colors.Identifier);
        SetBlockColor(EquPos, EquPos, Colors.Symbol);
        SetBlockColor(EquPos + 1, LS, Colors.Strings);
      end;
    end;
  end;

  // for Coco/R

  procedure HighlightGrammarName(S: WideString);
  var
    P: Integer;
  begin
    P := Pos('-->Grammar<--', S);
    if P > 0 then
      SetBlockColor(P, P + Length('-->Grammar<--') - 1, Colors.Preproc);
  end;

// (rom) const, var, local function sequence not cleaned up yet
var
  F: Boolean;
  C: TJvSymbolColor;
  Reserved: Boolean;
  PrevToken: WideString;
  PrevToken2: WideString;
  NextToken: WideString;
  Ch: WideChar;
  InTag: Boolean;
  N: Integer;

var
  S: WideString;
  LS: Integer;
  Token: WideString;
  I: Integer;

begin
  if not FSyntaxHighlighting then
    Exit;
  S := Lines[Line];
  if (FHighlighter = hlNone) and not UserReservedWords then
    C := Colors.PlainText
  else
  begin
    FLine := S;
    FLineNum := Line;
    CheckInLong;

    if (FHighlighter = hlSyntaxHighlighter) and (FSyntaxHighlighter <> nil) then
    begin
     // user defined syntax highlighting
      FSyntaxHighlighter.GetAttr(Self, Lines, Line, ColBeg, ColEnd, FLong, LineAttrs);
      Exit;
    end;

    Parser.pcProgram := PWideChar(S);
    Parser.pcPos := Parser.pcProgram;

    LS := Length(S);
    Ch := GetTrimChar(S, 1);
    if (Highlighter in [hlCBuilder, hlNQC]) and (LS > 0) and
      (((Ch = '#') and (FLong = 0)) or (FLong = lgPreproc)) then
      C := Colors.Preproc
    else
    if ((FHighlighter in [hlPython, hlPerl]) and (LS > 0) and
      (Ch = '#') and (FLong = 0)) or
      ((Highlighter = hlIni) and (LS > 0) and ((Ch = '#') or (Ch = ';'))) then
      C := Colors.Comment
    else
      C := Colors.PlainText;
    if (FLong <> 0) and (FHighlighter <> hlHtml) then
    begin
      Parser.pcPos := Parser.pcProgram + FindLongEnd + 1;
      case Highlighter of
        hlCBuilder, hlPython, hlPerl, hlNQC:
          case FLong of
            lgString:
              C := Colors.Strings;
            lgComment1, lgComment2:
              C := Colors.Comment;
            lgPreproc:
              C := Colors.Preproc;
          end;
        hlPascal:
          case FLong of
            lgComment1, lgComment2:
              C := Colors.Comment;
            lgPreproc1, lgPreproc2:
              C := Colors.Preproc;
          end;
      else
        C := Colors.Comment;
      end;
    end;
  end;

  LineAttrs[1].FC := C.ForeColor;
  LineAttrs[1].Style := C.Style;
  LineAttrs[1].BC := C.BackColor;
  N := Min(Max_X, Length(S));
  for I := 2 to N do
    Move(LineAttrs[1], LineAttrs[I], SizeOf(LineAttrs[1]));
  if Length(S) < Max_X then
  begin
    LineAttrs[N + 1].FC := Font.Color;
    LineAttrs[N + 1].Style := Font.Style;
    LineAttrs[N + 1].BC := Color;
    for I := N + 1 + 1 to Max_X do
      Move(LineAttrs[N + 1], LineAttrs[I], SizeOf(LineAttrs[1]));
  end;

  if (FHighlighter = hlNone) and not UserReservedWords then
    Exit;
  if (Length(S) > 0) then
  begin
    Ch := GetTrimChar(S, 1);
    if ((Ch = '#') and (FHighlighter in [hlCBuilder, hlPython, hlPerl, hlNQC])) or
       (((Ch = '#') or (Ch = ';')) and (FHighlighter = hlIni)) then
      Exit;
  end;

  if FHighlighter = hlIni then
    SetIniColors(S)
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
          SetColor(Colors.Reserved)
        else
          F := False;
      end
      else
        case FHighlighter of
          hlPascal:
            if IsDelphiKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
              F := False;
          hlCBuilder:
            if IsBuilderKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
              F := False;
          hlNQC:
            if IsNQCKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
              F := False;
          hlSql:
            if IsSQLKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
              F := False;
          hlPython:
            if IsPythonKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
            if Token = 'None' then
              SetColor(Colors.Number)
            else
            if (PrevToken = 'def') or (PrevToken = 'class') then
              SetColor(Colors.Declaration)
            else
            if (NextSymbol = '(') and IsIdentifierW(Token) then
              SetColor(Colors.FunctionCall)
            else
              F := False;
          hlJava:
            if IsJavaKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
            if PrevToken = 'function' then
              SetColor(Colors.Declaration)
            else
              F := False;
          hlVB:
            if IsVBKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
            if IsVBStatement(Token) then
              SetColor(Colors.Statement)
            else
            if Cmp(PrevToken, 'function') or Cmp(PrevToken, 'sub') or
              Cmp(PrevToken, 'class') then
              SetColor(Colors.Declaration)
            else
              F := False;
          hlHtml:
            if not InTag then
            begin
              if Token = '<' then
              begin
                InTag := True;
                SetColor(Colors.Reserved)
              end;
              F := True;
            end
            else
            begin
              if Token = '>' then
              begin
                InTag := False;
                SetColor(Colors.Reserved)
              end
              else
              if (Token = '/') and (PrevToken = '<') then
                SetColor(Colors.Reserved)
              else
              if (NextSymbol = '=') and IsIdentifierW(Token) then
                SetColor(Colors.Identifier)
              else
              if PrevToken = '=' then
                SetColor(Colors.Strings)
              else
              if IsHtmlTag(Token) then
                SetColor(Colors.Reserved)
              else
              if (PrevToken = '<') or ((PrevToken = '/') and (PrevToken2 = '<')) then
                SetColor(Colors.Statement)
              else
                F := False;
            end;
          hlPerl:
            if IsPerlKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
            if IsPerlStatement(Token) then
              SetColor(Colors.Statement)
            else
            if CharInSetW(Token[1], ['$', '@', '%', '&']) then
              SetColor(Colors.FunctionCall)
            else
              F := False;
          hlCocoR:
            if IsCocoKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
            if (Parser.PosBeg[0] = 0) and (Line > ProductionsLine) and
              IsIdentifierW(Token) then
            begin
              NextToken := Parser.Token;
              Parser.RollBack(1);
              SetColor(Colors.Declaration)
            end
            else
              F := False;
          hlPhp:
            if IsPhpKeyWord(Token) then
              SetColor(Colors.Reserved)
            else
              F := False;
        else
          F := False;
        end;
      if F then
        {Ok}
      else
      if IsPreproc(Token) then
        SetColor(Colors.Preproc)
      else
      if IsComment(Token) then
        SetColor(Colors.Comment)
      else
      if IsStringConstant(Token) then
        SetColor(Colors.Strings)
      else
      if (Length(Token) = 1) and CharInSetW(Token[1], Symbols) then
        SetColor(Colors.Symbol)
      else
      if IsIntConstantW(Token) or IsRealConstantW(Token) then
        SetColor(Colors.Number)
      else
      if (FHighlighter in [hlCBuilder, hlJava, hlPython, hlPhp, hlNQC]) and
        (PrevToken = '0') and ((Token[1] = 'x') or (Token[1] = 'X')) then
        SetColor(Colors.Number)
      else
      if FHighlighter = hlHtml then
        SetColor(Colors.PlainText)
      else
        SetColor(Colors.Identifier);
      if FHighlighter = hlHtml then
        { found special chars starting with '&' and ending with ';' }
        TestHtmlSpecChars(Token);
      PrevToken2 := PrevToken;
      PrevToken := Token;
      Token := Parser.Token;
    end;

    if Highlighter = hlCocoR then
      HighlightGrammarName(S);
  except
  end;
end;

procedure TJvWideHLEditor.CheckInLong;
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

function TJvWideHLEditor.RescanLong(iLine: Integer): Boolean;
const
  MaxScanLinesAtOnce = 5000;
var
  P, F: PWideChar;
  MaxLine, MaxScanLine: Integer;
  S: WideString;
  I, i1, L1: Integer;
begin
  FLong := lgNone;
  Result := False; // no Invalidate

  if (not FSyntaxHighlighting) or
     (not FLongTokens or (FHighlighter in [hlNone, hlIni])) or
     (Lines.Count = 0) then
    Exit;

  ProductionsLine := High(Integer);
  MaxLine := Lines.Count - 1;
  if MaxLine > High(FLongDesc) then
    MaxLine := High(FLongDesc);
  if iLine > MaxLine then
    Exit;

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
    if (FHighlighter = hlSyntaxHighlighter) and (FSyntaxHighlighter <> nil) then
      FSyntaxHighlighter.ScanLongTokens(Self, Lines, iLine, FLong)
    else
    begin
      S := Lines[iLine];
      P := Pointer(S);
      F := P;
      L1 := Length(S);
      if (L1 = 0) then
      begin
        case Highlighter of
          hlPascal:
            if FLong in [lgString] then
              FLong := lgNone;
          hlCBuilder, hlPython, hlPerl, hlNQC:
            if FLong in [lgPreproc] then
              FLong := lgNone;
        else
          if FLong in [lgPreproc1, lgPreproc2, lgString] then
            FLong := lgNone;
        end;
      end;
      I := 1;
      while I <= L1 do
      begin
        case FHighlighter of
          hlPascal:
            case FLong of
              lgNone: //  not in comment
                case S[I] of
                  '{':
                    begin
                      P := StrScanW(F + I, WideChar('}'));
                      if P = nil then
                      begin
                        if S[I + 1] = '$' then
                          FLong := lgPreproc1
                        else
                          FLong := lgComment1;
                        Break;
                      end
                      else
                        I := P - F + 1;
                    end;
                  '(':
                    if {S[I + 1]} F[I] = '*' then
                    begin
                      if {S[I + 2]} F[I + 1] = '$' then
                        FLong := lgPreproc2
                      else
                        FLong := lgComment2;
                      P := StrScanW(F + I + 2, WideChar(')'));
                      if P = nil then
                        Break
                      else
                      begin
                        if P[-1] = '*' then
                          FLong := lgNone;
                        I := P - F + 1;
                      end;
                    end;
                  '''':
                    begin
                      P := StrScanW(F + I + 1, WideChar(''''));
                      if P <> nil then
                      begin
                        i1 := P - F;
                        if P[1] <> '''' then
                          I := i1
                        else
                          { ?? }
                      end
                      else
                        I := L1 + 1;
                    end;
                end;
              lgPreproc1, lgComment1:
                begin //  {
                  P := StrScanW(F + I - 1, WideChar('}'));
                  if P <> nil then
                  begin
                    FLong := lgNone;
                    I := P - F + 1;
                  end
                  else
                    I := L1 + 1;
                end;
              lgPreproc2, lgComment2:
                begin //  (*
                  P := StrScanW(F + I, WideChar(')'));
                  if P = nil then
                    Break
                  else
                  begin
                    if P[-1] = '*' then
                      FLong := lgNone;
                    I := P - F + 1;
                  end;
                end;
            end;
          hlCBuilder, hlSql, hlJava, hlPhp, hlNQC:
            case FLong of
              lgNone: //  not in comment
                case S[I] of
                  '/':
                    if {S[I + 1]} F[I] = '*' then
                    begin
                      FLong := lgComment2;
                      P := StrScanW(F + I + 2, WideChar('/'));
                      if P = nil then
                        Break
                      else
                      begin
                        if P[-1] = '*' then
                          FLong := lgNone;
                        I := P - F + 1;
                      end;
                    end;
                  '"':
                    begin
                      P := StrScanW(F + I + 1, WideChar('"'));
                      if P <> nil then
                      begin
                        i1 := P - F;
                        if P[1] <> '"' then
                          I := i1
                        else
                          { ?? }
                      end
                      else
                      if FHighlighter in [hlCBuilder, hlJava, hlNQC] then
                      begin
                        if (LastNonSpaceChar(S) = '\') and (HasStringOpenEnd(Lines, iLine)) then
                          FLong := lgString;
                        I := L1 + 1;
                      end
                      else
                        I := L1 + 1;
                    end;
                  '#':
                    begin
                      if (GetTrimChar(S, 1) = '#') and (LastNonSpaceChar(S) = '\') then
                      begin
                        FLong := lgPreproc;
                        Break;
                      end;
                    end;
                end;
              lgComment2:
                begin //  /*
                  P := StrScanW(F + I, WideChar('/'));
                  if P = nil then
                    Break
                  else
                  begin
                    if P[-1] = '*' then
                      FLong := lgNone;
                    I := P - F + 1;
                  end;
                end;
              lgString:
                begin
                  P := StrScanW(F + I + 1, WideChar('"'));
                  if P <> nil then
                  begin
                    i1 := P - F;
                    if P[1] <> '"' then
                      I := i1
                    else
                      { ?? }
                  end
                  else
                  begin
                    if FHighlighter in [hlCBuilder, hlJava, hlNQC] then
                    begin
                      if (LastNonSpaceChar(S) <> '\') or (not HasStringOpenEnd(Lines, iLine)) then
                        FLong := lgNone;
                    end;
                    I := L1 + 1;
                  end;
                end;
              lgPreproc:
                begin
                  if LastNonSpaceChar(S) <> '\' then
                    FLong := lgNone;
                end;
            end;
          hlPython, hlPerl:
            case FLong of
              lgNone: //  not in comment
                case S[I] of
                  '#':
                    I := L1;
                  '"':
                    begin
                      P := StrScanW(F + I, WideChar('"'));
                      if P = nil then
                      begin
                        FLong := lgString;
                        Break;
                      end
                      else
                        I := P - F + 1;
                    end;
                end;
              lgString: // python and perl long string
                begin
                  P := StrScanW(F + I - 1, WideChar('"'));
                  if P <> nil then
                  begin
                    FLong := lgNone;
                    I := P - F + 1;
                  end
                  else
                    I := L1 + 1;
                end;
            end;
          hlHtml:
            case FLong of
              lgNone: //  not in comment
                case S[I] of
                  '<':
                    begin
                      P := StrScanW(F + I, WideChar('>'));
                      if P = nil then
                      begin
                        FLong := lgTag;
                        Break;
                      end
                      else
                        I := P - F + 1;
                    end;
                end;
              lgTag: // html tag
                begin
                  P := StrScanW(F + I - 1, WideChar('>'));
                  if P <> nil then
                  begin
                    FLong := lgNone;
                    I := P - F + 1;
                  end
                  else
                    I := L1 + 1;
                end;
            end;
          hlCocoR:
            case FLong of
              lgNone: //  not in comment
                case S[I] of
                  '(':
                    if {S[I + 1]} F[I] = '*' then
                    begin
                      FLong := lgComment2;
                      P := StrScanW(F + I + 2, WideChar(')'));
                      if P = nil then
                        Break
                      else
                      begin
                        if P[-1] = '*' then
                          FLong := lgNone;
                        I := P - F + 1;
                      end;
                    end;
                  '"':
                    begin
                      P := StrScanW(F + I + 1, WideChar('"'));
                      if P <> nil then
                      begin
                        i1 := P - F;
                        if P[1] <> '"' then
                          I := i1
                        else
                          { ?? }
                      end
                      else
                        I := L1 + 1;
                    end;
                  '''':
                    begin
                      P := StrScanW(F + I + 1, WideChar(''''));
                      if P <> nil then
                      begin
                        i1 := P - F;
                        if P[1] <> '''' then
                          I := i1
                        else
                          { ?? }
                      end
                      else
                        I := L1 + 1;
                    end;
                  '/':
                    if {S[I + 1]} F[I] = '*' then
                    begin
                      FLong := lgComment2;
                      P := StrScanW(F + I + 2, WideChar('/'));
                      if P = nil then
                        Break
                      else
                      begin
                        if P[-1] = '*' then
                          FLong := lgNone;
                        I := P - F + 1;
                      end;
                    end;
                end;
              lgComment2:
                begin //  (*
                  P := StrScanW(F + I, WideChar(')'));
                  if P = nil then
                    Break
                  else
                  begin
                    if P[-1] = '*' then
                      FLong := lgNone;
                    I := P - F + 1;
                  end;
                end;
            end;
        end;
        Inc(I);
      end;

      if (FHighlighter = hlCocoR) and
        (StrLICompW2(PWideChar(S), 'productions', Length('productions')) = 0) then
      begin
        ProductionsLine := iLine;
      end;
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

function TJvWideHLEditor.FindLongEnd: Integer;
var
  P, F: PWideChar;
  I: Integer;
begin
  P := PWideChar(FLine);
  Result := Length(FLine);
  case FHighlighter of
    hlPascal:
      case FLong of
        lgPreproc1, lgComment1:
          begin
            P := StrScanW(P, WideChar('}'));
            if P <> nil then
              Result := P - PWideChar(FLine);
          end;
        lgPreproc2, lgComment2:
          begin
            F := P;
            while True do
            begin
              F := StrScanW(F, WideChar('*'));
              if F = nil then
                Exit;
              if F[1] = ')' then
                Break;
              Inc(F);
            end;
            P := F + 1;
            Result := P - PWideChar(FLine);
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
                F := StrScanW(F, WideChar('*'));
                if F = nil then
                  Exit;
                if F[1] = '/' then
                  Break;
                Inc(F);
              end;
              P := F + 1;
              Result := P - PWideChar(FLine);
            end;
          lgString:
            begin
              F := P;
              repeat
                P := StrScanW(P, WideChar('"'));
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
                    I := 1;
                    while (P - 1 - I > F) and (P[-1 - I] = '\') do
                      Inc(I);
                    if I and $01 = 0 then {faster than: if I mod 2 = 0 then}
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
            P := StrScanW(P, WideChar('"'));
            if P <> nil then
              Result := P - PWideChar(FLine);
          end;
      end;
    hlHtml:
      case FLong of
        lgTag:
          begin
            P := StrScanW(P, WideChar('>'));
            if P <> nil then
              Result := P - PWideChar(FLine);
          end;
      end;
  end;
end;

procedure TJvWideHLEditor.TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
  const Text: WideString);
var
  S: WideString;
  L: Integer;
{  LP, I: Integer;
  P: PChar;
  OldProductionsLine: Integer; }
begin
  if not FLongTokens then
    Exit;
  case FHighlighter of
    hlPascal:
      S := #13'{}*()/';
    hlCBuilder, hlJava, hlSql, hlPhp, hlNQC:
      S := #13'*/\';
    hlVB:
      S := #13'''';
    hlPython, hlPerl:
      S := #13'#"';
    hlHtml:
      S := #13'<>';
    hlCocoR:
      S := #13'*()/';
    hlSyntaxHighlighter:
      if FSyntaxHighlighter <> nil then
      begin
        if FSyntaxHighlighter.GetRescanLongKeys(Self, Action, ACaretX, ACaretY, Text) then
        begin
          if RescanLong(ACaretY) then
            Invalidate;
        end;
        Exit;
      end
      else
        S := #13;
  else
    S := #13; { unknown Highlighter ? }
  end;

  if Action = maAll then
    ACaretY := -1;  // rescan all lines

  if (Action in [maAll, maReplace]) or HasAnyChar(S, Text) then
  begin
    if RescanLong(ACaretY) then
      Invalidate;
  end
  else
  begin
    if (Highlighter = hlPascal) and (Cardinal(ACaretY) < Max_Line) then
    begin
     // comment <-> preproc
      S := Lines[ACaretY];
      L := Length(S);
         // [Backspace, "insert"]
      if ((ACaretX > 1) and (ACaretX <= L + 1) and (S[ACaretX - 1] = '{')) or
         ((ACaretX > 2) and (ACaretX <= L + 2) and (S[ACaretX - 2] = '(') and (S[ACaretX - 1] = '*')) or
         // [Delete]
         ((ACaretX > 0) and (ACaretX <= L) and (S[ACaretX] = '{')) or
         ((ACaretX > 1) and (ACaretX <= L + 1) and (S[ACaretX - 1] = '(') and (S[ACaretX] = '*')) then
      begin
        if RescanLong(ACaretY) then
          Invalidate;
      end;
    end;
  end;
 {
  if (FHighlighter = hlCocoR) and (HasAnyChar('productions'#13, Text)) then
  begin
    LP := Length('productions');
    OldProductionsLine := ProductionsLine;
    ProductionsLine := High(Integer);
    for I := 0 to Lines.Count - 1 do
    begin
      P := PWideChar(Lines[I]);
      if (StrLICompW2(P, 'productions', LP) = 0) and
         ((Length(P) = LP) or (P[LP] = ' ')) then
      begin
        ProductionsLine := I;
        Break;
      end;
    end;
    if ProductionsLine <> OldProductionsLine then
      Invalidate;
  end; }
end;

function TJvWideHLEditor.GetReservedWord(const Token: WideString;
  var Reserved: Boolean): Boolean;
begin
  Result := Assigned(FOnReservedWord);
  if Result then
  begin
    Reserved := False;
    FOnReservedWord(Self, Token, Reserved);
  end
end;

function TJvWideHLEditor.UserReservedWords: Boolean;
begin
  Result := Assigned(FOnReservedWord);
end;

procedure TJvWideHLEditor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvWideHLEditor then
  begin
    FHighlighter := TJvWideHLEditor(Source).Highlighter;
    Colors.Assign(TJvWideHLEditor(Source).Colors);
    //FSyntaxHighlighting := TJvWideHLEditor(Source).SyntaxHighlighting;
    Invalidate;
  end
  else
  if Source is TJvHLEditor then
  begin
    FHighlighter := TJvHLEditor(Source).Highlighter;
    Colors.Assign(TJvHLEditor(Source).Colors);
    //FSyntaxHighlighting := TJvHLEditor(Source).SyntaxHighlighting;
    Invalidate;
  end;
end;

procedure TJvWideHLEditor.AssignTo(Source: TPersistent);
begin
  if Source is TJvHLEditor then
  begin
    TJvHLEditor(Source).BeginUpdate;
    try
      TJvHLEditor(Source).Assign(TJvCustomEditorBase(Self));

      TJvHLEditor(Source).Highlighter := Highlighter;
      TJvHLEditor(Source).Colors.Assign(Colors);
      TJvHLEditor(Source).SyntaxHighlighting := SyntaxHighlighting;
    finally
      TJvHLEditor(Source).EndUpdate;
    end;
  end;
end;


function TJvWideHLEditor.GetDelphiColors: Boolean;
  function CompareColor(Symbol: TJvSymbolColor; const DelphiColor: TDelphiColor): Boolean;
  begin
    Result :=
     (Symbol.ForeColor = DelphiColor.ForeColor) and
     (Symbol.BackColor = DelphiColor.BackColor) and
     (Symbol.Style = DelphiColor.Style);
  end;
begin
  Result := False;
  if not CompareColor(Colors.Comment, DelphiColor_Comment) then
    Exit;
  if not CompareColor(Colors.Preproc, DelphiColor_Preproc) then
    Exit;
  if not CompareColor(Colors.Number, DelphiColor_Number) then
    Exit;
  if not CompareColor(Colors.Strings, DelphiColor_Strings) then
    Exit;
  if not CompareColor(Colors.Symbol, DelphiColor_Symbol) then
    Exit;
  if not CompareColor(Colors.Reserved, DelphiColor_Reserved) then
    Exit;
  if not CompareColor(Colors.Identifier, DelphiColor_Identifier) then
    Exit;
  if not CompareColor(Colors.PlainText, DelphiColor_PlainText) then
    Exit;
  Result := True;
end;

procedure TJvWideHLEditor.SetDelphiColors(Value: Boolean);

  procedure SetColor(Symbol: TJvSymbolColor; const DelphiColor: TDelphiColor);
  begin
    with DelphiColor do
      Symbol.SetColor(ForeColor, BackColor, Style);
  end;

begin
  if Value then
  begin
    SetColor(Colors.Comment, DelphiColor_Comment);
    SetColor(Colors.Preproc, DelphiColor_Preproc);
    SetColor(Colors.Number, DelphiColor_Number);
    SetColor(Colors.Strings, DelphiColor_Strings);
    SetColor(Colors.Symbol, DelphiColor_Symbol);
    SetColor(Colors.Reserved, DelphiColor_Reserved);
    SetColor(Colors.Identifier, DelphiColor_Identifier);
    SetColor(Colors.PlainText, DelphiColor_PlainText);
  end;
end;

procedure TJvWideHLEditor.SetSyntaxHighlighter(const Value: TJvWideEditorHighlighter);
begin
  if Value <> FSyntaxHighlighter then
  begin
    if Value <> nil then
      FHighlighter := hlSyntaxHighlighter
    else
      if FHighlighter = hlSyntaxHighlighter then
        FHighlighter := hlNone;

    FSyntaxHighlighter := Value;
    RescanLong(0);
    Invalidate;
  end;
end;

function TJvWideHLEditor.GetColors: TJvColors;
begin
  Result := FColors;
end;

procedure TJvWideHLEditor.SetColors(const Value: TJvColors);
begin
  FColors.Assign(Value);
end;

function TJvWideHLEditor.GetSyntaxHighlighting: Boolean;
begin
  Result := FSyntaxHighlighting;
end;

procedure TJvWideHLEditor.SetSyntaxHighlighting(Value: Boolean);
begin
  FSyntaxHighlighting := Value;
  Invalidate;
end;

function TJvWideHLEditor.GetHighlighter: TJvHighlighter;
begin
  Result := FHighlighter;
end;

end.

