{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvForth.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}

unit JvForth;

interface

uses
  Windows, shellapi, Messages, SysUtils, Classes, forms, Dialogs, math,
  fileCtrl, JvXMLTree, Jvstrings{$IFDEF DELPHI6_UP}, Variants{$ENDIF};

const
  StackMax = 1000;

type
  EJvjanScriptError = class(Exception);

  TToken = (dfoError, dfonop,
    // flow actions
    dfoIf, dfoElse, dfoEndIf, dfoRepeat, dfoUntil,
    // sub routines
    dfosub, dfoEndSub, dfoCall,
    // stack operations
    dfodup, dfodrop, dfoswap,
    // conversion
    dfoCstr,
    // data source object, symbols starting with _
    dfoDSO, dfoSelDir, dfoDSOBase,
    // xmldso starts with ?
    dfoXML,
    // system io
    dfoSystem,
    // internal variables
    dfoIntVar,
    // external variables
    dfoExtVar,
    // direct action
    dfoInteger, dfoFloat, dfoSet, dfoString, dfoBoolean,
    dfoDate,
    // end direct action
    dfoEq, dfoNe, dfoGt, dfoLt, dfoGe, dfoLe, dfoLike, dfoUnlike,
    dfoNot, dfoAnd, dfoXor, dfoOr,
    dfoIn,
    dfoAdd, dfoSubtract, dfoMultiply, dfoDivide, dfoPower,
    dfoAbs,
    // some usefull constants
    dfocrlf,
    // some gonio functions
    dfosin, dfocos, dfopi, dfotan,
    dfoarcsin, dfoarccos, dfoarctan, dfoarctan2,

    dfonegate, dfosqr, dfosqrt,
    dfoleft, dforight,
    // windows api
    dfoshellexecute,
    // date and time
    dfonow, dfotime, dfodatestr, dfotimestr
    );

  Tprocvar = procedure of object;

  TonGetVariable = procedure(Sender: TObject; Symbol: string; var value: variant; var handled: boolean; var ErrorStr: string) of object;
  TonSetVariable = procedure(Sender: TObject; Symbol: string; value: variant; var handled: boolean; var ErrorStr: string) of object;
  TonGetSystem = procedure(Sender: TObject; Symbol, Prompt: string; var value: variant; var handled: boolean; var ErrorStr: string) of object;
  TonSetSystem = procedure(Sender: TObject; Symbol: string; value: variant; var handled: boolean; var ErrorStr: string) of object;
  TonInclude = procedure(Sender: TObject; includefile: string; var value: string; var handled: boolean; var ErrorStr: string) of object;

  TJvJanDSO = class(TStringList)
  private
    function InternalGetValue(index: integer; aField: string): string;
    procedure InternalSetValue(index: integer; aField, aValue: string);
  public
    // when a key is not found it will be added
    procedure SetValue(aKey: variant; aField, aValue: string);
    function GetValue(aKey: variant; aField: string): string;

  end;

  TJvJanDSOList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearTables;
    function Table(aName: string): TJvJanDSO;
  end;

  TJvJanXMLList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearXMLS;
    function xml(aName: string): TJvXMLTree;
  end;

  TVariantObject = class(TObject)
  private
    Fvalue: variant;
    procedure Setvalue(const Value: variant);
  protected
  public
    property value: variant read Fvalue write Setvalue;
  end;

  TVariantList = class(TStringList)
  private
  protected
  public
    destructor Destroy; override;
    procedure ClearObjects;
    procedure SetVariable(symbol: string; Avalue: variant);
    function GetVariable(symbol: string): variant;
    function GetObject(symbol: string): TvariantObject;
  end;

  TAtom = class(TObject)
  private
    FToken: TToken;
    Fsymbol: string;
    Fvalue: variant;
    FProc: TProcVar;
    FIsOperant: boolean;

    procedure SetToken(const Value: TToken);
    procedure Setsymbol(const Value: string);
    procedure Setvalue(const Value: variant);
    procedure SetProc(const Value: TProcVar);
    procedure SetIsOperant(const Value: boolean);
  protected
  public
    property Token: TToken read FToken write SetToken;
    property Proc: TProcVar read FProc write SetProc;
    property symbol: string read Fsymbol write Setsymbol;
    property value: variant read Fvalue write Setvalue;
    property IsOperant: boolean read FIsOperant write SetIsOperant;
  end;

  TAtomList = class(TList)
  private
  protected
  public
    destructor Destroy; override;
    procedure clearObjects;
  end;

  TJvForthScript = class(TComponent)
  private
    FScript: string;
    FIncludes: TstringList;
    FInDevice: string;
    FOutDevice: string;
    FSubsList: TStringList;
    FVarsList: TVariantList;
    FDSOList: TJvJanDSOList;
    FXMLList: TJvJanXMLList;
    FXMLSelect: TList;
    FXMLSelectRecord: integer;
    FDSOBase: string; // root directory for DSO tables
    Atoms: TAtomList;
    // rstack if the return stack for loop, sub etc.
    rstack: array[0..StackMax] of integer;
    rsp: integer;
    vstack: array[0..StackMax] of variant;
    vsp: integer;
    ostack: array[0..StackMax] of TToken;
    osp: integer;
    pstack: array[0..StackMax] of TToken;
    psp: integer;
    pc: integer;
    CurrentSymbol: string;
    CurrentValue: variant;
    FonGetVariable: TonGetVariable;
    FonSetVariable: TonSetVariable;
    FScriptTimeOut: integer;
    FonGetSystem: TonGetSystem;
    FonSetSystem: TonSetSystem;
    FonInclude: TonInclude;
    procedure ClearAtoms;
    procedure SetScript(const Value: string);
    procedure SetonGetVariable(const Value: TonGetVariable);
    procedure SetonSetVariable(const Value: TonSetVariable);
    // expresssion procedures

       // constants
    procedure proccrlf;
    // date and time
    procedure procnow;
    procedure procdatestr;
    procedure proctimestr;
    // shell
    procedure procshellexecute;
    // xml variables
    procedure procXML;
    // data source variables
    procedure procDSO;
    procedure procSelDir;
    procedure procDSOBase;
    // external variables
    procedure procExtVar; // general dispatcher
    procedure procassign;
    procedure procVariable;

    // internal variables
    procedure procIntVar; // general dispatcher
    procedure procVarGet;
    procedure procVarSet;
    procedure procVarInc;
    procedure procVarIncIndex;
    procedure procVarDec;
    procedure procVarDecTestZero;
    procedure procVarAdd;
    procedure procVarSub;
    procedure procVarMul;
    procedure procVarDiv;
    procedure procVarNeg;
    procedure procVarLoad;
    procedure procVarSave;
    // system io
    procedure procSystem; // general dispatcher
    procedure procSysGet;
    procedure procSysSet;
    // flow expressions
    procedure procIf;
    procedure procElse;
    procedure procEndif;
    procedure procUntil;
    procedure procRepeat;
    // end flow expressions

    // sub expressions
    procedure procSub;
    procedure procEndsub;
    procedure procCall;
    // conversion expressions
    procedure procCStr;
    procedure procNop;
    procedure procDup;
    procedure procDrop;
    procedure procSwap;
    procedure procInteger;
    procedure procFloat;
    procedure procSet;
    procedure procString;
    procedure procBoolean;
    procedure procDate;
    procedure procEq;
    procedure procNe;
    procedure procGt;
    procedure procLt;
    procedure procGe;
    procedure procLe;
    procedure procLike;
    procedure procUnlike;
    procedure procNot;
    procedure procAnd;
    procedure procXor;
    procedure procOr;
    procedure procIn;
    procedure procAdd;
    procedure procSubtract;
    procedure procMultiply;
    procedure procDivide;
    procedure procPower;
    procedure procAbs;
    // some gonio functions
    procedure procpi;
    procedure procSin;
    procedure procCos;
    procedure procTan;
    procedure procarcsin;
    procedure procarccos;
    procedure procarctan;
    procedure procarctan2;

    procedure procNegate;
    procedure procSqr;
    procedure procSqrt;
    procedure procLeft;
    procedure procRight;
    function vpop: variant;
    procedure vpush(aValue: variant);
    function opop: TToken;
    procedure opush(aValue: TToken);
    function ppop: TToken;
    procedure ppush(aValue: TToken);
    function rpop: integer;
    procedure rpush(aValue: Integer);
    procedure doproc;
    procedure doToken(aToken: TToken);
    procedure SetScriptTimeOut(const Value: integer);
    procedure ParseScript;
    procedure SetonGetSystem(const Value: TonGetSystem);
    procedure SetonSetSystem(const Value: TonSetSystem);
    procedure SetonInclude(const Value: TonInclude);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: variant;
    function popValue: variant;
    function canPopValue: boolean;
    procedure pushValue(aValue: variant);
    function canPushValue: boolean;
  published
    { Published declarations }
    property Script: string read FScript write SetScript;
    property ScriptTimeOut: integer read FScriptTimeOut write SetScriptTimeOut;
    property onGetVariable: TonGetVariable read FonGetVariable write SetonGetVariable;
    property onSetVariable: TonSetVariable read FonSetVariable write SetonSetVariable;
    property onSetSystem: TonSetSystem read FonSetSystem write SetonSetSystem;
    property onGetSystem: TonGetSystem read FonGetSystem write SetonGetSystem;
    property onInclude: TonInclude read FonInclude write SetonInclude;
  end;

  { Loadstring returns a string loaded from a file}
function LoadString(aFile: string): string;

{ Savestring saves a stirng variable aText to a file }
procedure SaveString(aFile, aText: string);

{ PosStr searches the first occurrence of a substring FindString in a string
  given by SourceString with case sensitivity (upper and lower case characters
  are differed). This function returns the index value of the first character
  of a specified substring from which it occurs in a given string starting with
  StartPos character index. If a specified substring is not found Q_PosStr
  returns zero. The author of algorithm is Peter Morris (UK) (FastStrings unit
  from www.torry.ru). }

function PosStr(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;

{ PosText searches the first occurrence of a substring FindString in a string
  given by SourceString without case sensitivity (upper and lower case
  characters are not differed). This function returns the index value of the
  first character of a specified substring from which it occurs in a given
  string starting with StartPos character index. If a specified substring is
  not found Q_PosStr returns zero.  The author of algorithm is Peter Morris (UK) (FastStrings unit
  from www.torry.ru). }

function PosText(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;

function LastPosChar(const FindChar: char; SourceString: string): integer;

// runs an external file or progam
procedure Launch(Afile: string);

implementation

const

  cr = chr(13) + chr(10);
  tab = chr(9);

  ToUpperChars: array[0..255] of Char =
  (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,
    #$60, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$7B, #$7C, #$7D, #$7E, #$7F,
    #$80, #$81, #$82, #$81, #$84, #$85, #$86, #$87, #$88, #$89, #$8A, #$8B, #$8C, #$8D, #$8E, #$8F,
    #$80, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$8A, #$9B, #$8C, #$8D, #$8E, #$8F,
    #$A0, #$A1, #$A1, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$AA, #$AB, #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B2, #$A5, #$B5, #$B6, #$B7, #$A8, #$B9, #$AA, #$BB, #$A3, #$BD, #$BD, #$AF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF);

  ToLowerChars: array[0..255] of Char =
  (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
    #$40, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$5B, #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,
    #$90, #$83, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$9A, #$8B, #$9C, #$9D, #$9E, #$9F,
    #$90, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$9A, #$9B, #$9C, #$9D, #$9E, #$9F,
    #$A0, #$A2, #$A2, #$BC, #$A4, #$B4, #$A6, #$A7, #$B8, #$A9, #$BA, #$AB, #$AC, #$AD, #$AE, #$BF,
    #$B0, #$B1, #$B3, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB, #$BC, #$BE, #$BE, #$BF,
    #$E0, #$E1, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF,
    #$E0, #$E1, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF);

  { some utility functions }

procedure Launch(Afile: string);
var
  command, params, workdir: string;
begin
  command := afile;
  params := #0;
  workdir := #0;
  shellexecute(application.handle, 'open', @command[1],
    @params[1], @workdir[1], SW_SHOWNORMAL);
end;

function LastPosChar(const FindChar: char; SourceString: string): integer;
var
  i: integer;
begin
  result := 0;
  i := length(sourcestring);
  if i = 0 then exit;
  while (i > 0) and (sourceString[i] <> Findchar) do
    dec(i);
  result := i;
end;

function LoadString(aFile: string): string;
var
  s: string;
begin
  with TFileStream.Create(aFile, fmOpenRead) do
  try
    SetLength(s, Size);
    ReadBuffer(s[1], Size);
  finally free;
  end;
  result := s;
end;

procedure SaveString(aFile, aText: string);
begin
  with TFileStream.Create(aFile, fmCreate) do
  try
    writeBuffer(aText[1], length(aText));
  finally free;
  end;
end;

function PosStr(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        MOV     EBX,EAX
        XCHG    EAX,EDX
        NOP
        ADD     EDI,ECX
        MOV     ECX,EAX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNZ     @@lp1
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOV     AL,BYTE PTR [ESI]
        MOV     EBX,EDX
        JMP     @@fr
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        XOR     AL,BYTE PTR [EDI+EBX]
        JNE     @@ms
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function PosText(const FindString, SourceString: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        NOP
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        PUSH    EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        PUSH    EAX
        SUB     EDX,ECX
        JNG     @@qtx
        ADD     EDI,ECX
        MOV     ECX,EDX
        MOV     EDX,EAX
        MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qtx:  ADD     ESP,$08
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOV     EDX,[ESP]
        JMP     @@fr
        NOP
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     BL,BYTE PTR [ESI+EDX]
        MOV     AH,BYTE PTR [EDI+EDX]
        CMP     BL,AH
        JE      @@eq
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOVZX   EBX,AH
        XOR     AL,BYTE PTR [EBX+ToUpperChars]
        JNE     @@ms
@@eq:   DEC     EDX
        JNZ     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        POP     ECX
        SUB     EAX,[ESP]
        POP     ECX
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

procedure GlobalSetValue(var aText: string; aName, aValue: string);
var
  p, p2, L: integer;
begin
  l := length(aName) + 2;
  if aText = '' then
  begin
    aText := aName + '="' + aValue + '"';
  end
  else
  begin
    p := PosText(aName + '="', aText);
    if p = 0 then
    begin
      aText := aText + ' ' + aName + '="' + aValue + '"';
    end
    else
    begin
      p2 := PosStr('"', aText, p + L);
      if p2 = 0 then exit;
      Delete(aText, p + L, p2 - (p + L));
      insert(aValue, aText, p + L);
    end;
  end;
end;

function GlobalGetValue(aText, aName: string): string;
var
  p, p2, L: integer;
begin
  result := '';
  L := length(aName) + 2;
  p := PosText(aName + '="', aText);
  if p = 0 then exit;
  p2 := PosStr('"', aText, p + L);
  if p2 = 0 then exit;
  result := copy(atext, p + L, p2 - (p + L));
  result := stringreplace(result, '~~', cr, [rfreplaceall]);
end;

// some special expression functions

// returns the index of integer v in aList

function IndexOfInteger(aList: Tstringlist; v: variant): integer;
var
  c, i, index, p: integer;
  s, s1, s2: string;
begin
  result := -1;
  i := v;
  c := AList.count;
  if c = 0 then exit;
  for index := 0 to c - 1 do
  begin
    try
      s := aList[index];
      p := pos('..', s);
      if p = 0 then
      begin
        if strtoint(aList[index]) = i then
        begin
          result := index;
          exit;
        end;
      end
      else
      begin // have range
        s1 := trim(copy(s, 1, p - 1));
        s2 := trim(copy(s, p + 2, length(s)));
        if (i >= strtoint(s1)) and (i <= strtoint(s2)) then
        begin
          result := index;
          exit;
        end;
      end;
    except
      exit;
    end;
  end;
end;

// returns the index of float v (single or double)in aList

function IndexOfFloat(aList: Tstringlist; v: variant): integer;
var
  c, index, p: integer;
  f: extended;
  s, s1, s2: string;
begin
  result := -1;
  f := v;
  c := AList.count;
  if c = 0 then exit;
  for index := 0 to c - 1 do
  begin
    try
      s := aList[index];
      p := pos('..', s);
      if p = 0 then
      begin
        if strtofloat(s) = f then
        begin
          result := index;
          exit;
        end;
      end
      else
      begin // have range
        s1 := trim(copy(s, 1, p - 1));
        s2 := trim(copy(s, p + 2, length(s)));
        if (f >= strtofloat(s1)) and (f <= strtofloat(s2)) then
        begin
          result := index;
          exit;
        end;
      end;
    except
      raise EJvjanScriptError.Create('invalid number ' + s);
    end;
  end;
end;

// returns the index of date v in aList

function IndexOfDate(aList: Tstringlist; v: variant): integer;
var
  c, index, p: integer;
  d: TDatetime;
  s, s1, s2: string;

begin
  result := -1;
  d := v;
  c := AList.count;
  if c = 0 then exit;
  for index := 0 to c - 1 do
  begin
    try
      s := aList[index];
      p := pos('..', s);
      if p = 0 then
      begin
        if strtodate(aList[index]) = d then
        begin
          result := index;
          exit;
        end;
      end
      else
      begin
        s1 := trim(copy(s, 1, p - 1));
        s2 := trim(copy(s, p + 2, length(s)));
        if (d >= strtoDate(s1)) and (d <= strtoDate(s2)) then
        begin
          result := index;
          exit;
        end;
      end;
    except
      exit;
    end;
  end;
end;

// returns the index of string v in aList

function IndexOfString(aList: Tstringlist; v: variant): integer;
var
  c, index, p: integer;
  sv: string;
  s, s1, s2: string;
begin
  result := -1;
  sv := v;
  c := AList.count;
  if c = 0 then exit;
  for index := 0 to c - 1 do
  begin
    try
      s := aList[index];
      p := pos('..', s);
      if p = 0 then
      begin
        if (aList[index]) = sv then
        begin
          result := index;
          exit;
        end;
      end
      else
      begin
        s1 := trim(copy(s, 1, p - 1));
        s2 := trim(copy(s, p + 2, length(s)));
        if (sv >= s1) and (sv <= s2) then
        begin
          result := index;
          exit;
        end;
      end;
    except
      exit;
    end;
  end;
end;

// used by dfoIN
// tests if Avalue is in aSet

function FuncIn(Avalue: variant; aSet: variant): boolean;
var
  list: TStringlist;
  s: string;
  p: integer;
  token: string;

  function GetToken: boolean;
  begin
    result := false;
    s := trimleft(s);
    if s = '' then exit;
    p := 1;
    if s[1] = '"' then
    begin // get string
      p := posstr('"', s, 2);
      if p = 0 then
        raise EJvjanScriptError.Create('unterminated string near ' + s);
      token := copy(s, 2, p - 2);
      delete(s, 1, p);
      result := true;
    end
    else
    begin
      p := pos(' ', s);
      if p = 0 then
      begin
        token := s;
        result := true;
        s := '';
      end
      else
      begin
        token := copy(s, 1, p - 1);
        delete(s, 1, p);
        result := true
      end;
    end
  end;

begin
  result := false;
  s := aSet;
  if s = '' then exit;
  list := tstringlist.create;
  try
    while gettoken do
      list.append(token);
    //    c:=list.count;
    case vartype(AValue) of
      varString:
        begin
          result := IndexOfString(list, Avalue) > -1;
        end;
      varInteger, varByte:
        begin
          result := IndexOfInteger(list, Avalue) > -1;
        end;
      varSingle, varDouble:
        begin
          result := IndexOfFloat(list, Avalue) > -1;
        end;
      varDate:
        begin
          result := IndexOfDate(list, Avalue) > -1;
        end;
    else
      raise EJvjanScriptError.Create('unrecognized data type in set operation');
    end;
  finally
    list.free;
  end;
end;

// parse number returns the last position, starting from 1

function parseNumber(s: string): integer;
var
  i, e, e2, c: integer;
begin
  result := 0;
  i := 0;
  c := length(s);
  if c = 0 then exit;
  while (i + 1 <= c) and (s[i + 1] in ['0'..'9', ',', '.']) do
    inc(i);
  if (i + 1 <= c) and (s[i + 1] in ['e', 'E']) then
  begin
    e := i;
    inc(i);
    if (i + 1 <= c) and (s[i + 1] in ['+', '-']) then inc(i);
    e2 := i;
    while (i + 1 <= c) and (s[i + 1] in ['0'..'9']) do
      inc(i);
    if i = e2 then i := e;
  end;
  result := i;
end;

// parse a SQL style data string from positions 1,
// starts and ends with #

function parseDate(s: string): integer;
var
  p: integer;
begin
  result := 0;
  if length(s) < 2 then exit;
  p := posstr('#', s, 2);
  if p = 0 then exit;
  try
    strtodate(copy(s, 2, p - 2));
    result := p;
  except
    result := 0;
  end;
end;

{ TJvForthScript }

procedure TJvForthScript.vpush(aValue: variant);
begin
  //Vstack.push(aValue);
  vstack[vsp] := avalue;
  if vsp < StackMax then
    inc(vsp)
  else
    raise EJvjanScriptError.Create('stack overflow');
end;

procedure TJvForthScript.opush(aValue: TToken);
begin
  ostack[osp] := avalue;
  if osp < StackMax then
    inc(osp);
end;

function TJvForthScript.opop: TToken;
begin
  showmessage('opop');
  if osp <= 0 then
    result := dfonop
  else
  begin
    dec(osp);
    result := ostack[osp];
  end;
end;

procedure TJvForthScript.ppush(aValue: TToken);
begin
  pstack[psp] := avalue;
  if psp < StackMax then
    inc(psp);
end;

function TJvForthScript.ppop: TToken;
begin
  if psp = 0 then
    result := dfoError
  else
  begin
    dec(psp);
    result := pstack[psp];
  end;
end;

function TJvForthScript.vpop: variant;
begin
  if vsp = 0 then
    raise EJvjanScriptError.Create('stack underflow')
  else
  begin
    dec(vsp);
    result := vstack[vsp];
  end;
end;

procedure TJvForthScript.SetScript(const Value: string);
begin
  if value <> FScript then
  begin
    FScript := Value;
    ParseScript;
  end;

end;

procedure TJvForthScript.ParseScript;
var
  s: string;
  i, p, p2: integer;
  atom: TAtom;
  //  atomoperation:TToken;
  atomsymbol: string;
  atomvalue: variant;
  //  atomproc:TProcVar;
  token: string;
  vinteger: integer;
  vfloat: double;
  vdate: TdateTime;
  // handling of includes:
  incfile: string;
  handled: boolean;
  incScript: string;
  errStr: string;
  TimeOutTicks: Cardinal;
  deltaTicks: Cardinal;

  function pushatom(aToken: TToken): integer;
    //    var cc:integer;
  begin
    atom := TAtom.Create;
    atom.Token := atoken;
    atom.symbol := atomsymbol;
    atom.value := atomvalue;
    result := atoms.Add(atom);
  end;

  procedure opush(aToken: TToken);
    //    var cc:integer;
  begin
    atom := TAtom.Create;
    atom.Token := atoken;
    atom.symbol := token;
    atom.value := atomvalue;
    atoms.Add(atom);
  end;

  procedure brcpush(proc: TProcVar);
    //    var cc:integer;
  begin
    atom := TAtom.Create;
    atom.Proc := proc;
    atom.symbol := atomsymbol;
    atom.value := atomvalue;
    atom.IsOperant := false;
    atoms.Add(atom);
  end;

  function GetToken: boolean;
  begin
    result := false;
    s := trimleft(s);
    if s = '' then exit;
    p := 1;
    if s[1] = '"' then
    begin // get string
      p := posstr('"', s, 2);
      if p = 0 then
        raise EJvjanScriptError.Create('unterminated string near ' + s);
      token := copy(s, 1, p);
      delete(s, 1, p);
      result := true;
    end
    else if s[1] = '[' then
    begin // get block
      p := posstr(']', s, 2);
      if p = 0 then
        raise EJvjanScriptError.Create('unterminated block near ' + s);
      token := copy(s, 1, p);
      delete(s, 1, p);
      result := true;
    end
    else
    begin
      p := pos(' ', s);
      if p = 0 then
      begin
        token := s;
        result := true;
        s := '';
      end
      else
      begin
        token := copy(s, 1, p - 1);
        delete(s, 1, p);
        result := true
      end;
    end
  end;

begin
  Atoms.clearObjects;
  FSubsList.Clear;
  // reset return stack; needed in resolving flow statements
  rsp := 0;
  s := FScript;
  // include any include files, include files start with $$ and end with ;
  // when the parser detects and include file it will raise the oninclude event
  // include files can also include files (nested includes)
  deltaTicks := FScriptTimeOut * 1000;
  TimeOutticks := gettickcount + DeltaTicks;
  FIncludes.Clear; // clear the includes list
  repeat
    if gettickCount > timeOutTicks then
      raise EJvjanScriptError.Create('parser timed out after ' + inttostr(FScriptTimeout) + ' seconds; you may have circular includes');
    p := posstr('$$', s);
    if p > 0 then
    begin
      p2 := posstr(';', s, p);
      if p2 = 0 then
        raise EJvjanScriptError.Create('unterminated include near ' + copy(s, p, length(s)));
      incfile := copy(s, p + 2, p2 - p - 2) + '.jan';
      if posstr(' ', incfile, 1) > 0 then
        raise EJvjanScriptError.Create('illegal space character in the include file: ' + incfile);
      i := FIncludes.IndexOf(incfile);
      if i <> -1 then
      begin
        delete(s, p, p2 - p + 1);
      end
      else
      begin
        errStr := 'Can not find include file: ' + incfile;
        handled := false;
        incScript := '';
        if not assigned(oninclude) then
          raise EJvjanScriptError.Create('onInclude handler not assigned, can not handle include file: ' + copy(s, p, length(s)));
        oninclude(self, incfile, incScript, handled, errStr);
        if not handled then
          raise EJvjanScriptError.Create(errStr);
        delete(s, p, p2 - p + 1);
        insert(incScript, s, p);
        FIncludes.Append(incFile);
      end;
    end;
  until p = 0;
  s := trim(stringreplace(s, cr, ' ', [rfreplaceall]));
  // remove comments
  repeat
    p := pos('{', s);
    if p > 0 then
    begin
      p2 := posstr('}', s, p);
      if p2 = 0 then
        raise EJvjanScriptError.Create('missing "}" comment terminator near ' + s);
      delete(s, p, p2 - p + 1);
    end;
  until p = 0;
  if s = '' then exit;
  while gettoken do
  begin
    if token = 'cstr' then
      opush(dfoCstr)
    else if token = 'seldir' then
      opush(dfoseldir)
    else if token = 'dsobase' then
      opush(dfodsobase)
    else if token = 'dup' then
      opush(dfoDup)
    else if token = 'drop' then
      opush(dfoDrop)
    else if token = 'swap' then
      opush(dfoSwap)
    else if token = 'if' then
    begin
      p := pushatom(dfoIf);
      rpush(p);
    end
    else if token = 'endif' then
    begin
      p := pushatom(dfoEndIf);
      p2 := rpop;
      atom := TAtom(atoms[p2]);
      atom.value := p + 1;
    end
    else if token = 'else' then
    begin
      p := pushatom(dfoElse);
      p2 := rpop;
      rpush(p);
      atom := TAtom(atoms[p2]);
      atom.value := p + 1;
    end
    else if token = 'repeat' then
    begin
      p := pushatom(dforepeat);
      rpush(p);
    end
    else if token = 'until' then
    begin
      atomvalue := rpop;
      pushatom(dfoUntil);
    end
    else if token = 'now' then
      opush(dfonow)
    else if token = 'datestr' then
      opush(dfodatestr)
    else if token = 'timestr' then
      opush(dfotimestr)
    else if token = 'shellexecute' then
      opush(dfoshellexecute)
    else if token = ';' then
      opush(dfoEndSub)
    else if token = 'crlf' then
      opush(dfocrlf)
    else if token = '--' then
      opush(dfoNegate)
    else if token = '-' then
      opush(dfoSubtract)
    else if token = '+' then
      opush(dfoAdd)
    else if token = '*' then
      opush(dfoMultiply)
    else if token = '/' then
      opush(dfoDivide)
    else if token = '^' then
      opush(dfoPower)
    else if token = 'abs' then
      opush(dfoAbs)
    else if token = 'left' then
      opush(dfoleft)
    else if token = 'right' then
      opush(dforight)
    else if token = 'sqr' then
      opush(dfosqr)
    else if token = 'sqrt' then
      opush(dfosqrt)
    else if token = 'sin' then
      opush(dfosin)
    else if token = 'cos' then
      opush(dfocos)
    else if token = 'tan' then
      opush(dfotan)
    else if token = 'arcsin' then
      opush(dfoarcsin)
    else if token = 'arccos' then
      opush(dfoarccos)
    else if token = 'arctan' then
      opush(dfoarctan)
    else if token = 'arctan2' then
      opush(dfoarctan2)
    else if token = 'pi' then
      opush(dfopi)
    else if token = '<>' then
      opush(dfoNe)
    else if token = '>=' then
      opush(dfoGe)
    else if token = '>' then
      opush(dfoGt)
    else if token = '<=' then
      opush(dfoLe)
    else if token = '<' then
      opush(dfoLt)
    else if token = '=' then
      opush(dfoEq)
    else if token = 'or' then
      opush(dfoOr)
    else if token = 'and' then
      opush(dfoAnd)
    else if token = 'in' then
      opush(dfoIn)
    else if token = 'xor' then
      opush(dfoXor)
    else if token = 'not' then
      opush(dfoNot)
    else if token = 'like' then
      opush(dfoLike)
    else if token = 'unlike' then
      opush(dfoUnLike)
        // check for block
    else if token[1] = '[' then
    begin
      atomsymbol := token;
      atomvalue := copy(token, 2, length(token) - 2);
      pushatom(dfoSet);
    end
      // check for sub
    else if token[length(token)] = '=' then
    begin
      atomsymbol := copy(token, 1, length(token) - 1);
      p := pushatom(dfosub);
      FSubsList.AddObject(atomsymbol, Tobject(p + 1));
    end
      // check for xml object
    else if (token[1] = '?') and (length(token) > 1) then
    begin
      p := pos('.', token);
      if (p = 0) or (p < 3) or (p = length(token)) then
        raise EJvjanScriptError.Create('missing xml method specifier near ' + s);
      atomsymbol := copy(token, 2, p - 2);
      atomvalue := copy(token, p + 1, length(token));
      pushatom(dfoXML);
    end
      // check for data source object
    else if (token[1] = '_') and (length(token) > 1) then
    begin
      p := pos('.', token);
      if (p = 0) or (p < 3) or (p = length(token)) then
        raise EJvjanScriptError.Create('missing data source method specifier near ' + s);
      atomsymbol := copy(token, 2, p - 2);
      atomvalue := copy(token, p + 1, length(token));
      pushatom(dfoDSO);
    end
      // system
    else if (token[1] = ')') and (length(token) > 1) then
    begin
      p := pos('.', token);
      if (p = 0) or (p < 3) or (p = length(token)) then
        raise EJvjanScriptError.Create('missing system method specifier near ' + s);
      atomsymbol := copy(token, 2, p - 2);
      atomvalue := copy(token, p + 1, length(token));
      pushatom(dfoSystem);
    end
      // external variable
    else if (token[1] = '>') and (length(token) > 1) then
    begin
      p := pos('.', token);
      if (p = 0) or (p < 3) or (p = length(token)) then
        raise EJvjanScriptError.Create('missing external variable method specifier near ' + s);
      atomsymbol := copy(token, 2, p - 2);
      atomvalue := copy(token, p + 1, length(token));
      pushatom(dfoExtVar);
    end
      // check for internal variable
    else if (token[1] = ':') and (length(token) > 1) then
    begin
      p := pos('.', token);
      if (p = 0) or (p < 3) or (p = length(token)) then
        raise EJvjanScriptError.Create('missing internal variable method specifier near ' + s);
      atomsymbol := copy(token, 2, p - 2);
      atomvalue := copy(token, p + 1, length(token));
      pushatom(dfoIntVar);
    end
      // check for string
    else if token[1] = '"' then
    begin
      atomsymbol := token;
      atomvalue := copy(token, 2, length(token) - 2);
      pushatom(dfostring);
    end
      // check integer, float or date
    else
    begin
      try // integer
        vinteger := strtoint(token);
        atomsymbol := token;
        atomvalue := vinteger;
        pushatom(dfoInteger);
      except
        try // float
          vfloat := strtofloat(token);
          atomsymbol := token;
          atomvalue := vfloat;
          pushatom(dfofloat);
        except
          try // date
            vdate := strtodate(token);
            atomsymbol := token;
            atomvalue := vdate;
            pushatom(dfoDate);
          except // must be call to sub
            atomsymbol := token;
            p := FSubsList.IndexOf(atomsymbol);
            if p = -1 then
              raise EJvjanScriptError.Create('undefined word "' + atomsymbol + '" near ' + s);
            p := integer(FsubsList.objects[p]);
            atomvalue := p;
            pushatom(dfoCall);
          end;
        end;
      end;
    end;
  end; // while
end;

procedure TJvForthScript.doToken(aToken: TToken);
begin
  case aToken of
    dfonow: procnow;
    dfodatestr: procdatestr;
    dfotimestr: proctimestr;
    dfoshellexecute: procshellexecute;
    dfocrlf: proccrlf;
    dfoCStr: procCStr;
    dfoXML: procXML;
    dfoDSO: procDSO;
    dfoSeldir: procSelDir;
    dfoDSOBase: procDSOBase;
    dfoIntVar: procIntVar;
    dfoExtVar: procExtVar;
    dfoSystem: procSystem;
    //    dfoVarGet:procVarGet;
    //    dfoVarset:procVarSet;
    //    dfoSysGet:procSysGet;
    //    dfoSysSet:procSysSet;
    dfoSub: procSub;
    dfoEndSub: procEndSub;
    dfoCall: procCall;
    dfodrop: procdrop;
    dfodup: procdup;
    dfoswap: procswap;
    dfoIf: procif;
    dfoElse: procElse;
    dfoEndIf: procEndIf;
    dfoRepeat: procRepeat;
    dfoUntil: procUntil;
    dfonop: procNop;
    //    dfoassign: procassign;
    //    dfovariable: procVariable;
    dfointeger: procInteger;
    dfofloat: procFloat;
    dfoset: procSet;
    dfostring: procString;
    dfoboolean: procBoolean;
    dfoDate: procDate;
    dfoeq: procEq;
    dfone: procNe;
    dfogt: procGt;
    dfolt: procLt;
    dfoge: procGe;
    dfole: procLe;
    dfolike: procLike;
    dfounlike: procUnlike;
    dfonot: procNot;
    dfoand: procAnd;
    dfoxor: procXor;
    dfoor: procOr;
    dfoIn: procIn;
    dfoadd: procAdd;
    dfosubtract: procSubtract;
    dfomultiply: procMultiply;
    dfodivide: procDivide;
    dfoPower: procPower;
    dfoAbs: procAbs;
    dfopi: procpi;
    dfosin: procSin;
    dfocos: procCos;
    dfotan: procTan;
    dfoarcsin: procarcsin;
    dfoarccos: procarccos;
    dfoarctan: procarctan;
    dfoarctan2: procarctan2;
    dfonegate: procNegate;
    dfosqr: procSqr;
    dfosqrt: procSqrt;
    dfoleft: procLeft;
    dforight: procRight;
  end;
end;

function TJvForthScript.Execute: variant;
var
  c: integer;
  atom: TAtom;
  Token: TToken;
  TimeOutTicks: Cardinal;
  deltaTicks: cardinal;
begin
  result := null;
  osp := 0;
  vsp := 0;
  psp := 0;
  rsp := 0;
  c := atoms.Count;
  FVarsList.ClearObjects;
  FDSOList.ClearTables;
  FXMLList.ClearXMLS;
  FXMLSelect.Clear;
  FXMLSelectRecord := -1;
  if c = 0 then exit;
  pc := 0;
  deltaTicks := FScriptTimeOut * 1000;
  TimeOutticks := gettickcount + DeltaTicks;
  // evaluate all atoms
  while pc < c do
  begin
    if gettickCount > timeOutTicks then
      raise EJvjanScriptError.Create('Script timed out after ' + inttostr(FScriptTimeout) + ' seconds');
    atom := TAtom(atoms[pc]);
    inc(pc);
    CurrentValue := atom.value;
    CurrentSymbol := atom.symbol;
    token := atom.Token;
    case token of
      dfoInteger..dfoDate:
        begin
          vpush(CurrentValue)
        end;
    else
      begin
        doToken(token);
      end;
    end
  end;
  if vsp <= 0 then
    result := null
  else
    result := vpop;
end;

constructor TJvForthScript.Create(AOwner: TComponent);
begin
  inherited;
  atoms := TAtomList.Create;
  FIncludes := TStringList.create;
  FSubsList := Tstringlist.create;
  FVarsList := TvariantList.Create;
  FDSOList := TJvJanDSOList.Create;
  FXMLList := TJvJanXMLList.Create;
  FXMLSelect := TList.Create;
  FDSOBase := extractfilepath(paramstr(0));
  if FDSOBase[length(FDSOBase)] = '\' then
    delete(FDSOBase, length(FDSOBase), 1);
  vsp := 0;
  osp := 0;
  rsp := 0;
  FInDevice := 'dialog';
  FOutDevice := 'dialog';
  FScriptTimeOut := 30; // seconds
end;

destructor TJvForthScript.Destroy;
begin
  atoms.free;
  FIncludes.free;
  FSubsList.free;
  FVarsList.free;
  FDSOList.free;
  FXMLList.Free;
  FXMLSelect.free;
  inherited;
end;

procedure TJvForthScript.SetonGetVariable(const Value: TonGetVariable);
begin
  FonGetVariable := Value;
end;

procedure TJvForthScript.ClearAtoms;
var
  i, c: integer;
begin
  c := atoms.Count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    Tobject(atoms[i]).free;
  atoms.Clear;
end;

procedure TJvForthScript.SetonSetVariable(const Value: TonSetVariable);
begin
  FonSetVariable := Value;
end;

procedure TJvForthScript.procAdd;
var
  value: variant;
begin
  value := vpop;
  value := vpop + value;
  vpush(value);
end;

procedure TJvForthScript.procAnd;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop and value);
end;

procedure TJvForthScript.procassign;
var
  value: variant;
  handled: boolean;
  err: string;
begin
  value := vpop;
  vpush(value);
  handled := false;
  err := 'can not assign variable ' + CurrentSymbol;
  if assigned(onSetVariable) then
  begin
    onSetVariable(self, CurrentSymbol, value, handled, Err);
    if not handled then
      raise EJvjanScriptError.Create(err);
  end;
end;

procedure TJvForthScript.procBoolean;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.doproc;
var
  token: TToken;
begin
  if psp <= 0 then exit;
  dec(psp);
  token := pstack[psp];
  doToken(token);
end;

procedure TJvForthScript.procCos;
var
  value: variant;
begin
  value := vpop;
  vpush(cos(value));
end;

procedure TJvForthScript.procDate;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.procDivide;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop / value);
end;

procedure TJvForthScript.procEq;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop = value);
end;

procedure TJvForthScript.procFloat;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.procGe;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop >= value);
end;

procedure TJvForthScript.procGt;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop > value);
end;

procedure TJvForthScript.procIn;
var
  value: variant;
begin
  value := vpop;
  vpush(FuncIn(vpop, value));
end;

procedure TJvForthScript.procInteger;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.procLe;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop <= value);
end;

procedure TJvForthScript.procLeft;
var
  value, v2: variant;
  vali: integer;
  vals: string;
begin
  value := vpop;
  v2 := vpop;
  vali := value;
  vals := v2;
  value := copy(vals, 1, vali);
  vpush(value);
end;

procedure TJvForthScript.procLike;
var
  value: variant;
begin
  value := vartostr(vpop);
  vpush(pos(lowercase(value), lowercase(vartostr(vpop))) > 0);
end;

procedure TJvForthScript.procLt;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop < value);
end;

procedure TJvForthScript.procMultiply;
var
  value: variant;
begin
  value := vpop;
  value := vpop * value;
  vpush(value);
end;

procedure TJvForthScript.procNe;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop <> value);
end;

procedure TJvForthScript.procNegate;
var
  value: variant;
begin
  value := vpop;
  vpush(0 - value);
end;

procedure TJvForthScript.procNop;
begin
  //  just do nothing
end;

procedure TJvForthScript.procNot;
var
  value: variant;
begin
  value := vpop;
  vpush(not value);
end;

procedure TJvForthScript.procOr;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop or value);
end;

procedure TJvForthScript.procRight;
var
  value, v2: variant;
  vali: integer;
  vals: string;
begin
  value := vpop;
  v2 := vpop;
  vali := value;
  vals := v2;
  if vali <= length(vals) then
    value := copy(vals, length(vals) - vali + 1, vali)
  else
    value := vals;
  vpush(value);
end;

procedure TJvForthScript.procSet;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.procSin;
var
  value: variant;
begin
  value := vpop;
  vpush(sin(value));
end;

procedure TJvForthScript.procSqr;
var
  value: variant;
begin
  value := vpop;
  vpush(sqr(value));
end;

procedure TJvForthScript.procSqrt;
var
  value: variant;
begin
  value := vpop;
  vpush(sqrt(value));
end;

procedure TJvForthScript.procString;
begin
  Vpush(CurrentValue);
  doproc;
end;

procedure TJvForthScript.procSubtract;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop - value);
end;

procedure TJvForthScript.procUnlike;
var
  value: variant;
begin
  value := vartostr(vpop);
  vpush(pos(lowercase(value), lowercase(vartostr(vpop))) = 0);
end;

procedure TJvForthScript.procVariable;
var
  value: variant;
  handled: boolean;
  err: string;
begin
  handled := false;
  err := 'Variable ' + CurrentSymbol + ' not defined';
  if assigned(onGetVariable) then
    onGetVariable(self, CurrentSymbol, value, handled, Err);
  if not handled then
    raise EJvjanScriptError.Create(err)
  else
    Vpush(value);
end;

procedure TJvForthScript.procXor;
var
  value: variant;
begin
  value := vpop;
  vpush(vpop xor value);
end;

procedure TJvForthScript.procIf;
var
  v: variant;
begin
  v := vpop;
  if v then
    exit
  else
    pc := Currentvalue;
end;

procedure TJvForthScript.procElse;
begin
  pc := CurrentValue;
end;

procedure TJvForthScript.procDrop;
begin
  vpop;
end;

procedure TJvForthScript.procDup;
var
  v: variant;
begin
  v := vpop;
  vpush(v);
  vpush(v);
end;

procedure TJvForthScript.procSwap;
var
  v1, v2: variant;
begin
  v1 := vpop;
  v2 := vpop;
  vpush(v1);
  vpush(v2);
end;

// just a marker

procedure TJvForthScript.procEndif;
begin
  // do nothing
end;

// keep looping until vpop=true

procedure TJvForthScript.procUntil;
begin
  if not vpop then pc := CurrentValue;
end;

procedure TJvForthScript.procRepeat;
begin
  // do nothing
end;

function TJvForthScript.rpop: integer;
begin
  if rsp <= 0 then
    raise EJvjanScriptError.Create('return stack underflow')
  else
  begin
    dec(rsp);
    result := rstack[rsp];
  end;
end;

procedure TJvForthScript.rpush(aValue: Integer);
begin
  rstack[rsp] := avalue;
  if rsp < StackMax then
    inc(rsp)
  else
    raise EJvjanScriptError.Create('return stack overflow');
end;

procedure TJvForthScript.SetScriptTimeOut(const Value: integer);
begin
  FScriptTimeOut := Value;
end;

procedure TJvForthScript.procEndsub;
begin
  pc := rpop;
end;

// just skip till endSub

procedure TJvForthScript.procSub;
var
  c: integer;
  token: TToken;
begin
  while pc < c do
  begin
    token := TAtom(atoms[pc]).token;
    if token = dfoEndSub then
    begin
      inc(pc);
      exit;
    end;
    inc(pc);
  end;
end;

// call to a user sub, just look it up

procedure TJvForthScript.procCall;
var
  index: integer;
begin
  //  index:=FSubsList.IndexOf(CurrentSymbol);
  index := CurrentValue;
  if index <> -1 then
  begin
    rpush(pc);
    //    pc:=integer(FsubsList.objects[index]);
    pc := index;
    exit;
  end
  else
    raise EJvjanScriptError.Create('procedure ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarGet;
var
  v: variant;
begin
  v := FvarsList.GetVariable(Currentsymbol);
  if v <> null then
    vpush(v)
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarSet;
var
  v: variant;
begin
  v := vpop;
  FVarsList.SetVariable(CurrentSymbol, v);
end;

procedure TJvForthScript.procCStr;
var
  s: string;
begin
  s := vpop;
  vpush(s);
end;

procedure TJvForthScript.procSysGet;
var
  value: variant;
  handled: boolean;
  err, prompt: string;
begin
  prompt := vpop;
  handled := false;
  err := 'System ' + CurrentSymbol + ' not defined';
  if assigned(onGetSystem) then
    onGetSystem(self, CurrentSymbol, prompt, value, handled, Err);
  if not handled then
    raise EJvjanScriptError.Create(err)
  else
    Vpush(value);
end;

procedure TJvForthScript.procSysSet;
var
  value: variant;
  handled: boolean;
  err: string;
begin
  value := vpop;
  vpush(value);
  handled := false;
  err := 'can not assign System ' + CurrentSymbol;
  if assigned(onSetSystem) then
  begin
    onSetSystem(self, CurrentSymbol, value, handled, Err);
    if not handled then
      raise EJvjanScriptError.Create(err);
  end;
end;

procedure TJvForthScript.SetonGetSystem(const Value: TonGetSystem);
begin
  FonGetSystem := Value;
end;

procedure TJvForthScript.SetonSetSystem(const Value: TonSetSystem);
begin
  FonSetSystem := Value;
end;

function TJvForthScript.popValue: variant;
begin
  result := vpop;
end;

procedure TJvForthScript.pushValue(aValue: variant);
begin
  vpush(aValue);
end;

function TJvForthScript.canPopValue: boolean;
begin
  result := vsp > 0;
end;

function TJvForthScript.canPushValue: boolean;
begin
  result := vsp < StackMax;
end;

procedure TJvForthScript.procpi;
begin
  vpush(pi);
end;

procedure TJvForthScript.procDSO;
var
  aName, aMethod: string;
  table: TJvJanDSO;
  aField, aValue: string;
  akey: variant;
  c: integer;
begin
  aName := CurrentSymbol;
  aMethod := CurrentValue;
  table := FDSOList.Table(aName);
  if aMethod = 'set' then
  begin
    akey := vpop;
    aField := vpop;
    avalue := vpop;
    table.SetValue(akey, aField, aValue);
  end
  else if aMethod = 'get' then
  begin
    akey := vpop;
    aField := vpop;
    aValue := table.GetValue(akey, aField);
    vpush(avalue);
  end
  else if aMethod = 'load' then
  begin
    table.LoadFromFile(FDSOBase + '\' + aName + '.txt');
  end
  else if aMethod = 'save' then
  begin
    table.SaveToFile(FDSOBase + '\' + aName + '.txt');
  end
  else if aMethod = 'clear' then
  begin
    table.Clear;
  end
  else if aMethod = 'count' then
  begin
    c := table.Count;
    vpush(c);
  end;
end;

procedure TJvForthScript.procDSOBase;
var
  s: string;
begin
  s := vpop;
  FDSOBase := s;
end;

procedure TJvForthScript.procSelDir;
var
  Dir: string;
begin
  Dir := FDSOBase;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    FDSOBase := dir;
end;

procedure TJvForthScript.procExtVar;
var
  aName, aMethod: string;
begin
  aName := CurrentSymbol;
  aMethod := CurrentValue;
  if aMethod = 'set' then
    procAssign
  else if aMethod = 'get' then
    procVariable
  else
    raise EJvjanScriptError.Create('unrecognize external variable method ' + aname + '.' + amethod);
end;

procedure TJvForthScript.procIntVar;
var
  aName, aMethod: string;
begin
  aName := CurrentSymbol;
  aMethod := CurrentValue;
  if aMethod = 'set' then
    procVarSet
  else if aMethod = 'get' then
    procVarGet
  else if aMethod = '1+' then
    procVarInc
  else if aMethod = '[1+]' then
    procVarIncIndex
  else if aMethod = '1-' then
    procVarDec
  else if aMethod = '1-?0' then
    procVarDecTestZero
  else if aMethod = '+' then
    procVarAdd
  else if aMethod = '-' then
    procVarSub
  else if aMethod = '*' then
    procVarMul
  else if aMethod = '/' then
    procVarDiv
  else if aMethod = '--' then
    procVarNeg
  else if aMethod = 'load' then
    procVarLoad
  else if aMethod = 'save' then
    procVarSave
  else
    raise EJvjanScriptError.Create('unrecognize internal variable method ' + aname + '.' + amethod);
end;

procedure TJvForthScript.procSystem;
var
  aName, aMethod: string;
begin
  aName := CurrentSymbol;
  aMethod := CurrentValue;
  if aMethod = 'set' then
    procSysSet
  else if aMethod = 'get' then
    procSysGet
  else
    raise EJvjanScriptError.Create('unrecognize system method ' + aname + '.' + amethod);
end;

procedure TJvForthScript.procVarDec;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value - 1
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarInc;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value + 1
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarAdd;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value + vpop
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarDiv;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value / vpop
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarMul;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value * vpop
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarSub;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := vo.value - vpop
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarNeg;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
    vo.value := 0 - vo.value
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procPower;
var
  value: variant;
begin
  value := vpop;
  vpush(power(vpop, value));
end;

procedure TJvForthScript.procAbs;
var
  value: variant;
begin
  value := vpop;
  vpush(abs(value));
end;

procedure TJvForthScript.SetonInclude(const Value: TonInclude);
begin
  FonInclude := Value;
end;

procedure TJvForthScript.procTan;
var
  value: variant;
begin
  value := vpop;
  vpush(tan(value));
end;

procedure TJvForthScript.procarccos;
var
  value: variant;
begin
  value := vpop;
  vpush(arccos(value));
end;

procedure TJvForthScript.procarcsin;
var
  value: variant;
begin
  value := vpop;
  vpush(arcsin(value));
end;

procedure TJvForthScript.procarctan;
var
  value: variant;
begin
  value := vpop;
  vpush(arctan(value));
end;

procedure TJvForthScript.procarctan2;
var
  value: variant;
begin
  value := vpop;
  vpush(arctan2(vpop, value));
end;

procedure TJvForthScript.procVarLoad;
var
  vo: TVariantObject;
  ap, fn, s: string;
begin
  fn := vpop;
  ap := extractfilepath(paramstr(0));
  fn := stringreplace(fn, '%', ap, []);
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
  begin
    s := loadstring(fn);
    vo.value := s;
  end
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarSave;
var
  vo: TVariantObject;
  ap, fn, s: string;
begin
  fn := vpop;
  ap := extractfilepath(paramstr(0));
  fn := stringreplace(fn, '%', ap, []);
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
  begin
    s := vo.value;
    savestring(fn, s);
  end
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procXML;
var
  aName, aMethod: string;
  xmldso: TJvXMLTree;
  n: TJvXMLNode;
  a: TJvXMLAttribute;
  aPath, atName: string;
  aValue: variant;
  c, i, cc: integer;
  appldir: string;
  b: boolean;
begin
  n := nil;
  appldir := extractfilepath(paramstr(0));
  aName := CurrentSymbol;
  aMethod := CurrentValue;
  xmldso := FXMLList.xml(aName);
  if aMethod = 'set' then
  begin
    aPath := vpop;
    avalue := vpop;
    n := xmldso.ForceNamePathNode(aPath);
    n.Value := aValue;
  end
  else if aMethod = '@set' then
  begin
    aPath := vpop;
    atName := vpop;
    avalue := vpop;
    xmldso.ForceNamePathNodeAttribute(aPath, atName, avalue);
  end
  else if aMethod = 'get' then
  begin
    apath := vpop;
    n := xmldso.getNamePathNode(apath);
    if n = nil then
      aValue := ''
    else
      aValue := n.Value;
    vpush(avalue);
  end
  else if aMethod = 'count' then
  begin
    apath := vpop;
    n := xmldso.getNamePathNode(apath);
    aValue := 0;
    cc := 0;
    if n <> nil then
    begin
      // now count named node
      c := n.Nodes.Count;
      apath := vpop;
      if c > 0 then
      begin
        for i := 0 to c - 1 do
          if TJvXMLNode(n.nodes[i]).name = apath then inc(cc);
      end;
      aValue := cc;
    end;
    vpush(avalue);
  end
  else if aMethod = '@get' then
  begin
    apath := vpop;
    atname := vpop;
    a := xmldso.getNamePathNodeAttribute(apath, atname);
    if n = nil then
      aValue := ''
    else
      aValue := a.Value;
    vpush(avalue);
  end
  else if aMethod = 'load' then
  begin
    aPath := vpop;
    aPath := stringreplace(aPath, '%', appldir, []);
    if not fileexists(aPath) then
      raise EJvjanScriptError.Create('File ' + apath + ' does not exist');
    xmldso.LoadFromFile(apath);
  end
  else if aMethod = 'save' then
  begin
    apath := vpop;
    aPath := stringreplace(aPath, '%', appldir, []);
    try
      xmldso.SaveToFile(apath);
    except
      raise EJvjanScriptError.Create('Can not save to file ' + apath);
    end
  end
  else if aMethod = 'astext' then
  begin
    avalue := xmldso.asText;
    vpush(avalue);
  end
  else if aMethod = 'delete' then
  begin
    apath := vpop;
    xmldso.deleteNamePathNode(apath);
  end
  else if aMethod = '@delete' then
  begin
    apath := vpop;
    atname := vpop;
    xmldso.deleteNamePathNodeAttribute(apath, atName);
  end
  else if aMethod = 'select' then
  begin
    apath := vpop;
    apath := stringreplace(apath, '''', '"', [rfreplaceall]);
    FXMLSelect.Clear;
    FXMLSelectRecord := -1;
    xmldso.selectNodes(apath, FXMLSelect);
    vpush(FXMLSelect.count > 0);
  end
  else if aMethod = 'selectfirst' then
  begin
    b := FXMLSelect.Count <> 0;
    if b then
      FXMLSelectRecord := 0
    else
      FXMLSelectRecord := -1;
    avalue := b;
    vpush(aValue);
  end
  else if aMethod = 'selectnext' then
  begin
    b := FXMLSelect.Count <> 0;
    if b then
      inc(FXMLSelectRecord)
    else
      FXMLSelectRecord := -1;
    if FXMLSelectRecord >= FXMLSelect.count then
    begin
      b := false;
      FXMLSelectRecord := -1;
    end;
    avalue := b;
    vpush(aValue);
  end
  else if aMethod = 'selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvjanScriptError.Create('XML selection is empty');
    if FXMLSelectRecord = -1 then
      raise EJvjanScriptError.Create('no XML selection selected');
    if FXMLSelectRecord >= FXMLSelect.count then
      raise EJvjanScriptError.Create('XML selection out of range');
    n := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    aValue := n.value;
    vpush(aValue);
  end
  else if aMethod = '@selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvjanScriptError.Create('XML selection is empty');
    if FXMLSelectRecord = -1 then
      raise EJvjanScriptError.Create('no XML selection selected');
    if FXMLSelectRecord >= FXMLSelect.count then
      raise EJvjanScriptError.Create('XML selection out of range');
    n := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    atname := vpop;
    aValue := n.GetAttributeValue(atname);
    vpush(aValue);
  end
  else
    raise EJvjanScriptError.Create('invalid xml method specifier ' + aMethod);
end;

procedure TJvForthScript.procVarDecTestZero;
var
  v: variant;
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
  begin
    v := vo.value - 1;
    vo.value := v;
    vpush(v = 0);
  end
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.procVarIncIndex;
var
  vo: TVariantObject;
  s, sidx: string;
  pb, pe: integer;
  index: integer;
begin
  vo := FvarsList.GetObject(Currentsymbol);
  if vo <> nil then
  begin
    s := vo.value;
    pb := lastposchar('[', s);
    if pb = 0 then
      raise EJvjanScriptError.Create('Increment Index: "[" expected in ' + s);
    pe := lastposchar(']', s);
    if pe = 0 then
      raise EJvjanScriptError.Create('Increment Index: "]" expected in ' + s);
    sidx := copy(s, pb + 1, pe - pb - 1);
    try
      index := strtoint(sidx);
      inc(index);
      s := copy(s, 1, pb - 1) + '[' + inttostr(index) + ']';
      vo.value := s;
      vpush(s);
    except
      raise EJvjanScriptError.Create('Increment Index: expected integer between "[..]" in ' + s);
    end;
  end
  else
    raise EJvjanScriptError.Create('variable ' + CurrentSymbol + ' not defined');
end;

procedure TJvForthScript.proccrlf;
begin
  vpush(cr);
end;

procedure TJvForthScript.procshellexecute;
var
  afile: string;
  appldir: string;
begin
  appldir := extractfilepath(paramstr(0));
  afile := vpop;
  afile := stringreplace(afile, '%', appldir, []);
  launch(afile);
end;

procedure TJvForthScript.procdatestr;
var
  s: string;
begin
  s := formatdatetime('dd-mmm-yyyy', now);
  vpush(s);
end;

procedure TJvForthScript.proctimestr;
var
  s: string;
begin
  s := formatdatetime('hh:nn:ss', now);
  vpush(s);
end;

procedure TJvForthScript.procnow;
begin
  vpush(now);
end;

{ TAtom }

procedure TAtom.SetIsOperant(const Value: boolean);
begin
  FIsOperant := Value;
end;

procedure TAtom.SetToken(const Value: TToken);
begin
  FToken := Value;
end;

procedure TAtom.SetProc(const Value: TProcVar);
begin
  FProc := Value;
end;

procedure TAtom.Setsymbol(const Value: string);
begin
  Fsymbol := Value;
end;

procedure TAtom.Setvalue(const Value: variant);
begin
  Fvalue := Value;
end;

{ TAtomList }

procedure TAtomList.clearObjects;
var
  i, c: integer;
begin
  c := count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    TAtom(items[i]).free;
  clear;
end;

destructor TAtomList.Destroy;
begin
  ClearObjects;
  inherited;
end;

{ TVariantObject }

procedure TVariantObject.Setvalue(const Value: variant);
begin
  Fvalue := Value;
end;

{ TVariantList }

procedure TVariantList.ClearObjects;
var
  i, c: integer;
begin
  c := count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    TVariantObject(Objects[i]).free;
  clear;
end;

destructor TVariantList.Destroy;
begin
  ClearObjects;
  inherited;
end;

function TVariantList.GetObject(symbol: string): TvariantObject;
var
  index: integer;
begin
  result := nil;
  if count = 0 then exit;
  index := indexof(symbol);
  if index = -1 then exit;
  result := TVariantObject(objects[index]);
end;

function TVariantList.GetVariable(symbol: string): variant;
var
  index: integer;
begin
  result := null;
  if count = 0 then exit;
  index := indexof(symbol);
  if index = -1 then exit;
  result := TVariantObject(objects[index]).value;
end;

procedure TVariantList.SetVariable(symbol: string; Avalue: variant);
var
  index: integer;
  obj: TVariantObject;
begin
  index := indexof(symbol);
  if index = -1 then
  begin
    obj := TvariantObject.Create;
    obj.value := Avalue;
    addobject(symbol, obj);
  end
  else
  begin
    TvariantObject(objects[index]).value := Avalue;
  end;
end;

{ TJvJanDSOList }

procedure TJvJanDSOList.ClearTables;
var
  i, c: integer;
begin
  c := count;
  if c <> 0 then
    for i := 0 to c - 1 do
      TJvJanDSO(objects[i]).free;
  clear;
end;

destructor TJvJanDSOList.Destroy;
begin
  ClearTables;
  inherited;

end;

function TJvJanDSOList.Table(aName: string): TJvJanDSO;
var
  index: integer;
  dso: TJvJanDSO;
begin
  index := indexof(aName);
  if index = -1 then
  begin
    dso := TJvJanDSO.Create;
    addobject(aName, dso);
    result := dso;
  end
  else
    result := TJvJanDSO(objects[index]);
end;

{ TJvJanDSO }

function TJvJanDSO.GetValue(aKey: variant; aField: string): string;
var
  index: integer;
  key: string;
  strkey: boolean;
begin
  key := aKey;
  strkey := false;
  try
    index := strtoint(key)
  except
    strkey := true;
  end;
  if not strkey then
  begin
    if index >= count then
      raise EJvjanScriptError.Create('DSO index out of range ' + inttostr(index))
    else
      result := InternalGetValue(index, aField);
  end
  else
  begin
    index := indexofName(key);
    if index = -1 then
      raise EJvjanScriptError.Create('DSO unknown key ' + key);
    result := InternalGetValue(index, aField);
  end
end;

function TJvJanDSO.InternalGetValue(index: integer; aField: string): string;
var
  key, s: string;
  p: integer;
begin
  s := strings[index];
  p := pos('=', s);
  key := copy(s, 1, p - 1);
  s := copy(s, p + 1, length(s));
  result := GlobalGetValue(s, aField);
end;

procedure TJvJanDSO.InternalSetValue(index: integer; aField, aValue: string);
var
  key, s: string;
  p: integer;
begin
  s := strings[index];
  p := pos('=', s);
  key := copy(s, 1, p - 1);
  s := copy(s, p + 1, length(s));
  GlobalSetValue(s, aField, aValue);
  strings[index] := key + '=' + s;
end;

procedure TJvJanDSO.SetValue(aKey: variant; aField, aValue: string);
var
  index: integer;
  key: string;
  strkey: boolean;
begin
  key := akey;
  strkey := false;
  try
    index := strtoint(key)
  except
    strkey := true;
  end;
  if not strkey then
  begin
    if index >= count then
      raise EJvjanScriptError.Create('DSO index out of range ' + inttostr(index))
    else
      InternalSetValue(index, aField, aValue);
  end
  else
  begin
    index := indexofname(key);
    if index = -1 then
      index := add(key + '=');
    InternalSetValue(index, aField, aValue);
  end
end;

{ TJvJanXMLList }

procedure TJvJanXMLList.ClearXMLS;
var
  i, c: integer;
begin
  c := count;
  if c <> 0 then
    for i := 0 to c - 1 do
      TJvXMLTree(objects[i]).free;
  clear;
end;

destructor TJvJanXMLList.Destroy;
begin
  ClearXMLS;
  inherited;

end;

function TJvJanXMLList.xml(aName: string): TJvXMLTree;
var
  index: integer;
  xmldso: TJvXMLTree;
begin
  index := indexof(aName);
  if index = -1 then
  begin
    xmldso := TJvXMLTree.Create(aname, '', nil);
    addobject(aName, xmldso);
    result := xmldso;
  end
  else
    result := TJvXMLTree(objects[index]);
end;

end.
