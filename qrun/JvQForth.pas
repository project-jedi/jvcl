{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvForth.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}


{$WARN UNIT_PLATFORM OFF}


unit JvQForth;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, ShellAPI,
  {$ENDIF MSWINDOWS}  
  Types, QWindows, QForms, QDialogs, QFileCtrls,  
  Variants, 
  JvQXMLTree, JvQComponent, JvQStrings, JvQTypes;

const
  StackMax = 1000;

type
  EJvJanScriptError = class(EJVCLException);

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

  TProcVar = procedure of object;

  TOnGetVariable = procedure(Sender: TObject; Symbol: string; var Value: Variant; var Handled: Boolean; var ErrorStr: string) of object;
  TOnSetVariable = procedure(Sender: TObject; Symbol: string; Value: Variant; var Handled: Boolean; var ErrorStr: string) of object;
  TOnGetSystem = procedure(Sender: TObject; Symbol, Prompt: string; var Value: Variant; var Handled: Boolean; var ErrorStr: string) of object;
  TOnSetSystem = procedure(Sender: TObject; Symbol: string; Value: Variant; var Handled: Boolean; var ErrorStr: string) of object;
  TOnInclude = procedure(Sender: TObject; IncludeFile: string; var Value: string; var Handled: Boolean; var ErrorStr: string) of object;

  TJvJanDSO = class(TStringList)
  private
    function InternalGetValue(Index: Integer; AField: string): string;
    procedure InternalSetValue(Index: Integer; AField, AValue: string);
  public
    // when a key is not found it will be added
    procedure SetValue(AKey: Variant; AField, AValue: string);
    function GetValue(AKey: Variant; AField: string): string;
  end;

  TJvJanDSOList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearTables;
    function Table(AName: string): TJvJanDSO;
  end;

  TJvJanXMLList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearXMLS;
    function Xml(AName: string): TJvXMLTree;
  end;

  TVariantObject = class(TObject)
  private
    FValue: Variant;
    procedure SetValue(const Value: Variant);
  public
    property Value: Variant read FValue write SetValue;
  end;

  TVariantList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearObjects;
    procedure SetVariable(Symbol: string; AValue: Variant);
    function GetVariable(Symbol: string): Variant;
    function GetObject(Symbol: string): TVariantObject; reintroduce;
  end;

  TAtom = class(TObject)
  private
    FToken: TToken;
    FSymbol: string;
    FValue: Variant;
    FProc: TProcVar;
    FIsOperand: Boolean;
    procedure SetToken(const Value: TToken);
    procedure SetSymbol(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure SetProc(const Value: TProcVar);
    procedure SetIsOperand(const Value: Boolean);
  public
    property Token: TToken read FToken write SetToken;
    property Proc: TProcVar read FProc write SetProc;
    property Symbol: string read FSymbol write SetSymbol;
    property Value: Variant read FValue write SetValue;
    property IsOperand: Boolean read FIsOperand write SetIsOperand;
  end;

  TAtomList = class(TList)
  public
    destructor Destroy; override;
    procedure ClearObjects;
  end;

  TJvForthScript = class(TJvComponent)
  private
    FScript: string;
    FIncludes: TStringList;
    FInDevice: string;
    FOutDevice: string;
    FSubsList: TStringList;
    FVarsList: TVariantList;
    FDSOList: TJvJanDSOList;
    FXMLList: TJvJanXMLList;
    FXMLSelect: TList;
    FXMLSelectRecord: Integer;
    FDSOBase: string; // root directory for DSO tables
    FAtoms: TAtomList;
    // FRStack if the return stack for loop, sub etc.
    FRStack: array [0..StackMax] of Integer;
    FRSP: Integer;
    FVStack: array [0..StackMax] of Variant;
    FVSP: Integer;
    // ostack: array[0..StackMax] of TToken;
    // osp: Integer;
    FPStack: array [0..StackMax] of TToken;
    FPSP: Integer;
    FPC: Integer;
    FCurrentSymbol: string;
    FCurrentValue: Variant;
    FOnGetVariable: TOnGetVariable;
    FOnSetVariable: TOnSetVariable;
    FScriptTimeOut: Integer;
    FOnGetSystem: TOnGetSystem;
    FOnSetSystem: TOnSetSystem;
    FOnInclude: TOnInclude;
    // procedure ClearAtoms;
    procedure SetScript(const Value: string);
    procedure SetOnGetVariable(const Value: TOnGetVariable);
    procedure SetOnSetVariable(const Value: TOnSetVariable);
    // expresssion procedures

    // constants
    procedure ProcCrLf;
    // date and time
    procedure ProcNow;
    procedure ProcDateStr;
    procedure ProcTimeStr;
    // shell
    procedure ProcShellExecute;
    // xml variables
    procedure ProcXML;
    // data source variables
    procedure ProcDSO;
    procedure ProcSelDir;
    procedure ProcDSOBase;
    // external variables
    procedure ProcExtVar; // general dispatcher
    procedure ProcAssign;
    procedure ProcVariable;

    // internal variables
    procedure ProcIntVar; // general dispatcher
    procedure ProcVarGet;
    procedure ProcVarSet;
    procedure ProcVarInc;
    procedure ProcVarIncIndex;
    procedure ProcVarDec;
    procedure ProcVarDecTestZero;
    procedure ProcVarAdd;
    procedure ProcVarSub;
    procedure ProcVarMul;
    procedure ProcVarDiv;
    procedure ProcVarNeg;
    procedure ProcVarLoad;
    procedure ProcVarSave;
    // system io
    procedure ProcSystem; // general dispatcher
    procedure ProcSysGet;
    procedure ProcSysSet;
    // flow expressions
    procedure ProcIf;
    procedure ProcElse;
    procedure ProcEndif;
    procedure ProcUntil;
    procedure ProcRepeat;
    // end flow expressions

    // sub expressions
    procedure ProcSub;
    procedure ProcEndsub;
    procedure ProcCall;
    // conversion expressions
    procedure ProcCStr;
    procedure ProcNop;
    procedure ProcDup;
    procedure ProcDrop;
    procedure ProcSwap;
    procedure ProcInteger;
    procedure ProcFloat;
    procedure ProcSet;
    procedure ProcString;
    procedure ProcBoolean;
    procedure ProcDate;
    procedure ProcEq;
    procedure ProcNe;
    procedure ProcGt;
    procedure ProcLt;
    procedure ProcGe;
    procedure ProcLe;
    procedure ProcLike;
    procedure ProcUnlike;
    procedure ProcNot;
    procedure ProcAnd;
    procedure ProcXor;
    procedure ProcOr;
    procedure ProcIn;
    procedure ProcAdd;
    procedure ProcSubtract;
    procedure ProcMultiply;
    procedure ProcDivide;
    procedure ProcPower;
    procedure ProcAbs;
    // some gonio functions
    procedure Procpi;
    procedure ProcSin;
    procedure ProcCos;
    procedure ProcTan;
    procedure Procarcsin;
    procedure Procarccos;
    procedure Procarctan;
    procedure Procarctan2;

    procedure ProcNegate;
    procedure ProcSqr;
    procedure ProcSqrt;
    procedure ProcLeft;
    procedure ProcRight;
    function vpop: Variant;
    procedure vpush(AValue: Variant);
//    function opop: TToken;
//    procedure opush(AValue: TToken);
//    function ppop: TToken;
//    procedure ppush(AValue: TToken);
    function rpop: Integer;
    procedure rpush(AValue: Integer);
    procedure doproc;
    procedure doToken(aToken: TToken);
    procedure SetScriptTimeOut(const Value: Integer);
    procedure ParseScript;
    procedure SetonGetSystem(const Value: TOnGetSystem);
    procedure SetonSetSystem(const Value: TOnSetSystem);
    procedure SetonInclude(const Value: TOnInclude);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Variant;
    function popValue: Variant;
    function canPopValue: Boolean;
    procedure pushValue(AValue: Variant);
    function canPushValue: Boolean;
  published
    property Script: string read FScript write SetScript;
    property ScriptTimeOut: Integer read FScriptTimeOut write SetScriptTimeOut;
    property onGetVariable: TOnGetVariable read FOnGetVariable write SetOnGetVariable;
    property onSetVariable: TOnSetVariable read FOnSetVariable write SetOnSetVariable;
    property onSetSystem: TOnSetSystem read FOnSetSystem write SetonSetSystem;
    property onGetSystem: TOnGetSystem read FOnGetSystem write SetonGetSystem;
    property onInclude: TOnInclude read FOnInclude write SetonInclude;
  end;

// runs an external file or progam
procedure Launch(const AFile: string);

implementation

uses
  Math, 
  JvQConsts, JvQResources;

{ some utility functions }

procedure Launch(const AFile: string);
var
  Command, Params, WorkDir: string;
begin
  Command := AFile;
  Params := #0;
  WorkDir := #0;  
  ShellExecute(0, 'open', PChar(Command),
    PChar(Params), PChar(WorkDir), SW_SHOWNORMAL); 
end;

procedure GlobalSetValue(var aText: string; AName, AValue: string);
var
  p, p2, L: Integer;
begin
  l := Length(AName) + 2;
  if aText = '' then
  begin
    aText := AName + '="' + AValue + '"';
  end
  else
  begin
    p := PosText(AName + '="', aText);
    if p = 0 then
    begin
      aText := aText + ' ' + AName + '="' + AValue + '"';
    end
    else
    begin
      p2 := PosStr('"', aText, p + L);
      if p2 = 0 then
        Exit;
      Delete(aText, p + L, p2 - (p + L));
      Insert(AValue, aText, p + L);
    end;
  end;
end;

function GlobalGetValue(aText, AName: string): string;
var
  p, p2, L: Integer;
begin
  Result := '';
  L := Length(AName) + 2;
  p := PosText(AName + '="', aText);
  if p = 0 then
    Exit;
  p2 := PosStr('"', aText, p + L);
  if p2 = 0 then
    Exit;
  Result := Copy(atext, p + L, p2 - (p + L));
  Result := StringReplace(Result, '~~', sLineBreak, [rfreplaceall]);
end;

// some special expression functions

// returns the Index of Integer v in aList

function IndexOfInteger(aList: TStringList; v: Variant): Integer;
var
  c, i, Index, p: Integer;
  s, s1, s2: string;
begin
  Result := -1;
  i := v;
  c := AList.Count;
  if c = 0 then
    Exit;
  for Index := 0 to c - 1 do
  begin
    try
      s := aList[Index];
      p := Pos('..', s);
      if p = 0 then
      begin
        if strtoint(aList[Index]) = i then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin // have range
        s1 := trim(Copy(s, 1, p - 1));
        s2 := trim(Copy(s, p + 2, Length(s)));
        if (i >= strtoint(s1)) and (i <= strtoint(s2)) then
        begin
          Result := Index;
          Exit;
        end;
      end;
    except
      Exit;
    end;
  end;
end;

// returns the Index of float v (single or double)in aList

function IndexOfFloat(aList: TStringList; v: Variant): Integer;
var
  c, Index, p: Integer;
  f: extended;
  s, s1, s2: string;
begin
  Result := -1;
  f := v;
  c := AList.Count;
  if c = 0 then
    Exit;
  for Index := 0 to c - 1 do
  begin
    try
      s := aList[Index];
      p := Pos('..', s);
      if p = 0 then
      begin
        if strtofloat(s) = f then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin // have range
        s1 := trim(Copy(s, 1, p - 1));
        s2 := trim(Copy(s, p + 2, Length(s)));
        if (f >= strtofloat(s1)) and (f <= strtofloat(s2)) then
        begin
          Result := Index;
          Exit;
        end;
      end;
    except
      raise EJvJanScriptError.CreateResFmt(@RsEInvalidNumbers, [s]);
    end;
  end;
end;

// returns the Index of date v in aList

function IndexOfDate(aList: TStringList; v: Variant): Integer;
var
  c, Index, p: Integer;
  d: TDatetime;
  s, s1, s2: string;

begin
  Result := -1;
  d := v;
  c := AList.Count;
  if c = 0 then
    Exit;
  for Index := 0 to c - 1 do
  begin
    try
      s := aList[Index];
      p := Pos('..', s);
      if p = 0 then
      begin
        if strtodate(aList[Index]) = d then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin
        s1 := trim(Copy(s, 1, p - 1));
        s2 := trim(Copy(s, p + 2, Length(s)));
        if (d >= strtoDate(s1)) and (d <= strtoDate(s2)) then
        begin
          Result := Index;
          Exit;
        end;
      end;
    except
      Exit;
    end;
  end;
end;

// returns the Index of string v in aList

function IndexOfString(aList: TStringList; v: Variant): Integer;
var
  c, Index, p: Integer;
  sv: string;
  s, s1, s2: string;
begin
  Result := -1;
  sv := v;
  c := AList.Count;
  if c = 0 then
    Exit;
  for Index := 0 to c - 1 do
  begin
    try
      s := aList[Index];
      p := Pos('..', s);
      if p = 0 then
      begin
        if (aList[Index]) = sv then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin
        s1 := trim(Copy(s, 1, p - 1));
        s2 := trim(Copy(s, p + 2, Length(s)));
        if (sv >= s1) and (sv <= s2) then
        begin
          Result := Index;
          Exit;
        end;
      end;
    except
      Exit;
    end;
  end;
end;

// used by dfoIN
// tests if AValue is in aSet

function FuncIn(AValue: Variant; aSet: Variant): Boolean;
var
  List: TStringList;
  s: string;
  p: Integer;
  token: string;

  function GetToken: Boolean;
  begin
    Result := False;
    s := trimleft(s);
    if s = '' then
      Exit;
    p := 1;
    if s[1] = '"' then
    begin // get string
      p := posstr('"', s, 2);
      if p = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedStringNears, [s]);
      token := Copy(s, 2, p - 2);
      Delete(s, 1, p);
      Result := True;
    end
    else
    begin
      p := Pos(' ', s);
      if p = 0 then
      begin
        token := s;
        Result := True;
        s := '';
      end
      else
      begin
        token := Copy(s, 1, p - 1);
        Delete(s, 1, p);
        Result := True
      end;
    end
  end;

begin
  Result := False;
  s := aSet;
  if s = '' then
    Exit;
  List := TStringList.create;
  try
    while gettoken do
      List.append(token);
    //    c:=List.Count;
    case vartype(AValue) of
      varString:
        begin
          Result := IndexOfString(List, AValue) > -1;
        end;
      varInteger, varByte:
        begin
          Result := IndexOfInteger(List, AValue) > -1;
        end;
      varSingle, varDouble:
        begin
          Result := IndexOfFloat(List, AValue) > -1;
        end;
      varDate:
        begin
          Result := IndexOfDate(List, AValue) > -1;
        end;
    else
      raise EJvJanScriptError.CreateRes(@RsEUnrecognizedDataTypeInSetOperation);
    end;
  finally
    List.Free;
  end;
end;

//=== TJvForthScript =========================================================

constructor TJvForthScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAtoms := TAtomList.Create;
  FIncludes := TStringList.create;
  FSubsList := TStringList.create;
  FVarsList := TvariantList.Create;
  FDSOList := TJvJanDSOList.Create;
  FXMLList := TJvJanXMLList.Create;
  FXMLSelect := TList.Create;
  FDSOBase := ExtractFilePath(paramstr(0));
  if FDSOBase[Length(FDSOBase)] = '\' then
    Delete(FDSOBase, Length(FDSOBase), 1);
  FVSP := 0;
  // osp := 0;
  FRSP := 0;
  FInDevice := 'dialog';
  FOutDevice := 'dialog';
  FScriptTimeOut := 30; // seconds
end;

destructor TJvForthScript.Destroy;
begin
  FAtoms.Free;
  FIncludes.Free;
  FSubsList.Free;
  FVarsList.Free;
  FDSOList.Free;
  FXMLList.Free;
  FXMLSelect.Free;
  inherited Destroy;
end;

procedure TJvForthScript.vpush(AValue: Variant);
begin
  //FVStack.push(AValue);
  FVStack[FVSP] := AValue;
  if FVSP < StackMax then
    Inc(FVSP)
  else
    raise EJvJanScriptError.CreateRes(@RsEStackOverflow);
end;

(*
procedure TJvForthScript.opush(AValue: TToken);
begin
  ostack[osp] := AValue;
  if osp < StackMax then
    Inc(osp);
end;
*)
(*
function TJvForthScript.opop: TToken;
begin
  showmessage('opop');
  if osp <= 0 then
    Result := dfonop
  else
  begin
    Dec(osp);
    Result := ostack[osp];
  end;
end;
*)
(*
procedure TJvForthScript.ppush(AValue: TToken);
begin
  FPStack[FPSP] := AValue;
  if FPSP < StackMax then
    Inc(FPSP);
end;
*)
(*
function TJvForthScript.ppop: TToken;
begin
  if FPSP = 0 then
    Result := dfoError
  else
  begin
    Dec(FPSP);
    Result := FPStack[FPSP];
  end;
end;
*)
function TJvForthScript.vpop: Variant;
begin
  if FVSP = 0 then
    raise EJvJanScriptError.CreateRes(@RsEStackUnderflow)
  else
  begin
    Dec(FVSP);
    Result := FVStack[FVSP];
  end;
end;

procedure TJvForthScript.SetScript(const Value: string);
begin
  if Value <> FScript then
  begin
    FScript := Value;
    ParseScript;
  end;

end;

procedure TJvForthScript.ParseScript;
var
  s: string;
  i, p, p2: Integer;
  atom: TAtom;
  //  atomoperation: TToken;
  atomsymbol: string;
  atomValue: Variant;
  //  atomproc: TProcVar;
  token: string;
  vinteger: Integer;
  vfloat: double;
  vdate: TdateTime;
  // handling of includes:
  incfile: string;
  Handled: Boolean;
  incScript: string;
  errStr: string;
  TimeOutTicks: Cardinal;
  deltaTicks: Cardinal;

  function pushatom(aToken: TToken): Integer;
    //    var cc: Integer;
  begin
    atom := TAtom.Create;
    atom.Token := atoken;
    atom.Symbol := atomsymbol;
    atom.Value := atomValue;
    Result := FAtoms.Add(atom);
  end;

  procedure opush(aToken: TToken);
    //    var cc: Integer;
  begin
    atom := TAtom.Create;
    atom.Token := atoken;
    atom.Symbol := token;
    atom.Value := atomValue;
    FAtoms.Add(atom);
  end;

  procedure brcpush(proc: TProcVar);
    //    var cc: Integer;
  begin
    atom := TAtom.Create;
    atom.Proc := proc;
    atom.Symbol := atomsymbol;
    atom.Value := atomValue;
    atom.IsOperand := False;
    FAtoms.Add(atom);
  end;

  function GetToken: Boolean;
  begin
    Result := False;
    s := trimleft(s);
    if s = '' then
      Exit;
    p := 1;
    if s[1] = '"' then
    begin // get string
      p := posstr('"', s, 2);
      if p = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedStringNears, [s]);
      token := Copy(s, 1, p);
      Delete(s, 1, p);
      Result := True;
    end
    else
    if s[1] = '[' then
    begin // get block
      p := posstr(']', s, 2);
      if p = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedBlockNear, [s]);
      token := Copy(s, 1, p);
      Delete(s, 1, p);
      Result := True;
    end
    else
    begin
      p := Pos(' ', s);
      if p = 0 then
      begin
        token := s;
        Result := True;
        s := '';
      end
      else
      begin
        token := Copy(s, 1, p - 1);
        Delete(s, 1, p);
        Result := True
      end;
    end
  end;

begin
  FAtoms.ClearObjects;
  FSubsList.Clear;
  // reset return stack; needed in resolving flow statements
  FRSP := 0;
  s := FScript;
  // include any include files, include files start with $$ and end with ;
  // when the parser detects and include file it will raise the oninclude event
  // include files can also include files (nested includes)
  deltaTicks := FScriptTimeOut * 1000;
  TimeOutticks := GetTickCount + DeltaTicks;
  FIncludes.Clear; // Clear the includes List
  repeat
    if GetTickCount > timeOutTicks then
      raise EJvJanScriptError.CreateResFmt(@RsEParserTimedOutAfterdSecondsYouMayHa, [FScriptTimeout]);
    p := posstr('$$', s);
    if p > 0 then
    begin
      p2 := posstr(';', s, p);
      if p2 = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedIncludeNears, [Copy(s, p, Length(s))]);
      incfile := Copy(s, p + 2, p2 - p - 2) + '.jan';
      if posstr(' ', incfile, 1) > 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEIllegalSpaceCharacterInTheIncludeFi, [incfile]);
      i := FIncludes.IndexOf(incfile);
      if i <> -1 then
      begin
        Delete(s, p, p2 - p + 1);
      end
      else
      begin
        errStr := Format(RsECanNotFindIncludeFiles, [incfile]);
        Handled := False;
        incScript := '';
        if not Assigned(oninclude) then
          raise EJvJanScriptError.CreateResFmt(@RsEOnIncludeHandlerNotAssignedCanNotHa, [Copy(s, p, Length(s))]);
        oninclude(Self, incfile, incScript, Handled, errStr);
        if not Handled then
          raise EJvJanScriptError.Create(errStr);
        Delete(s, p, p2 - p + 1);
        Insert(incScript, s, p);
        FIncludes.Append(incFile);
      end;
    end;
  until p = 0;
  s := trim(StringReplace(s, sLineBreak, ' ', [rfreplaceall]));
  // remove comments
  repeat
    p := Pos('{', s);
    if p > 0 then
    begin
      p2 := posstr('}', s, p);
      if p2 = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingCommentTerminatorNears, [s]);
      Delete(s, p, p2 - p + 1);
    end;
  until p = 0;
  if s = '' then
    Exit;
  while gettoken do
  begin
    if token = 'cstr' then
      opush(dfoCstr)
    else
    if token = 'seldir' then
      opush(dfoseldir)
    else
    if token = 'dsobase' then
      opush(dfodsobase)
    else
    if token = 'dup' then
      opush(dfoDup)
    else
    if token = 'drop' then
      opush(dfoDrop)
    else
    if token = 'swap' then
      opush(dfoSwap)
    else
    if token = 'if' then
    begin
      p := pushatom(dfoIf);
      rpush(p);
    end
    else
    if token = 'endif' then
    begin
      p := pushatom(dfoEndIf);
      p2 := rpop;
      atom := TAtom(FAtoms[p2]);
      atom.Value := p + 1;
    end
    else
    if token = 'else' then
    begin
      p := pushatom(dfoElse);
      p2 := rpop;
      rpush(p);
      atom := TAtom(FAtoms[p2]);
      atom.Value := p + 1;
    end
    else
    if token = 'repeat' then
    begin
      p := pushatom(dforepeat);
      rpush(p);
    end
    else
    if token = 'until' then
    begin
      atomValue := rpop;
      pushatom(dfoUntil);
    end
    else
    if token = 'now' then
      opush(dfonow)
    else
    if token = 'datestr' then
      opush(dfodatestr)
    else
    if token = 'timestr' then
      opush(dfotimestr)
    else
    if token = 'shellexecute' then
      opush(dfoshellexecute)
    else
    if token = ';' then
      opush(dfoEndSub)
    else
    if token = 'crlf' then
      opush(dfocrlf)
    else
    if token = '--' then
      opush(dfoNegate)
    else
    if token = '-' then
      opush(dfoSubtract)
    else
    if token = '+' then
      opush(dfoAdd)
    else
    if token = '*' then
      opush(dfoMultiply)
    else
    if token = '/' then
      opush(dfoDivide)
    else
    if token = '^' then
      opush(dfoPower)
    else
    if token = 'abs' then
      opush(dfoAbs)
    else
    if token = 'left' then
      opush(dfoleft)
    else
    if token = 'right' then
      opush(dforight)
    else
    if token = 'sqr' then
      opush(dfosqr)
    else
    if token = 'sqrt' then
      opush(dfosqrt)
    else
    if token = 'sin' then
      opush(dfosin)
    else
    if token = 'cos' then
      opush(dfocos)
    else
    if token = 'tan' then
      opush(dfotan)
    else
    if token = 'arcsin' then
      opush(dfoarcsin)
    else
    if token = 'arccos' then
      opush(dfoarccos)
    else
    if token = 'arctan' then
      opush(dfoarctan)
    else
    if token = 'arctan2' then
      opush(dfoarctan2)
    else
    if token = 'pi' then
      opush(dfopi)
    else
    if token = '<>' then
      opush(dfoNe)
    else
    if token = '>=' then
      opush(dfoGe)
    else
    if token = '>' then
      opush(dfoGt)
    else
    if token = '<=' then
      opush(dfoLe)
    else
    if token = '<' then
      opush(dfoLt)
    else
    if token = '=' then
      opush(dfoEq)
    else
    if token = 'or' then
      opush(dfoOr)
    else
    if token = 'and' then
      opush(dfoAnd)
    else
    if token = 'in' then
      opush(dfoIn)
    else
    if token = 'xor' then
      opush(dfoXor)
    else
    if token = 'not' then
      opush(dfoNot)
    else
    if token = 'like' then
      opush(dfoLike)
    else
    if token = 'unlike' then
      opush(dfoUnLike)
    // check for block
    else
    if token[1] = '[' then
    begin
      atomsymbol := token;
      atomValue := Copy(token, 2, Length(token) - 2);
      pushatom(dfoSet);
    end
    // check for sub
    else
    if token[Length(token)] = '=' then
    begin
      atomsymbol := Copy(token, 1, Length(token) - 1);
      p := pushatom(dfosub);
      FSubsList.AddObject(atomsymbol, Tobject(p + 1));
    end
    // check for xml object
    else
    if (token[1] = '?') and (Length(token) > 1) then
    begin
      p := Pos('.', token);
      if (p = 0) or (p < 3) or (p = Length(token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingXmlMethodSpecifierNears, [s]);
      atomsymbol := Copy(token, 2, p - 2);
      atomValue := Copy(token, p + 1, Length(token));
      pushatom(dfoXML);
    end
    // check for data source object
    else
    if (token[1] = '_') and (Length(token) > 1) then
    begin
      p := Pos('.', token);
      if (p = 0) or (p < 3) or (p = Length(token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingDataSourceMethodSpecifierNea, [s]);
      atomsymbol := Copy(token, 2, p - 2);
      atomValue := Copy(token, p + 1, Length(token));
      pushatom(dfoDSO);
    end
    // system
    else
    if (token[1] = ')') and (Length(token) > 1) then
    begin
      p := Pos('.', token);
      if (p = 0) or (p < 3) or (p = Length(token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingSystemMethodSpecifierNears, [s]);
      atomsymbol := Copy(token, 2, p - 2);
      atomValue := Copy(token, p + 1, Length(token));
      pushatom(dfoSystem);
    end
    // external variable
    else
    if (token[1] = '>') and (Length(token) > 1) then
    begin
      p := Pos('.', token);
      if (p = 0) or (p < 3) or (p = Length(token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingExternalVariableMethodSpecif, [s]);
      atomsymbol := Copy(token, 2, p - 2);
      atomValue := Copy(token, p + 1, Length(token));
      pushatom(dfoExtVar);
    end
    // check for internal variable
    else
    if (token[1] = ':') and (Length(token) > 1) then
    begin
      p := Pos('.', token);
      if (p = 0) or (p < 3) or (p = Length(token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingInternalVariableMethodSpecif, [s]);
      atomsymbol := Copy(token, 2, p - 2);
      atomValue := Copy(token, p + 1, Length(token));
      pushatom(dfoIntVar);
    end
    // check for string
    else
    if token[1] = '"' then
    begin
      atomsymbol := token;
      atomValue := Copy(token, 2, Length(token) - 2);
      pushatom(dfostring);
    end
    // check Integer, float or date
    else
    begin
      try // Integer
        vinteger := strtoint(token);
        atomsymbol := token;
        atomValue := vinteger;
        pushatom(dfoInteger);
      except
        try // float
          vfloat := strtofloat(token);
          atomsymbol := token;
          atomValue := vfloat;
          pushatom(dfofloat);
        except
          try // date
            vdate := strtodate(token);
            atomsymbol := token;
            atomValue := vdate;
            pushatom(dfoDate);
          except // must be call to sub
            atomsymbol := token;
            p := FSubsList.IndexOf(atomsymbol);
            if p = -1 then
              raise EJvJanScriptError.CreateResFmt(@RsEUndefinedWordsNears, [atomsymbol, s]);
            p := Integer(FsubsList.Objects[p]);
            atomValue := p;
            pushatom(dfoCall);
          end;
        end;
      end;
    end;
  end;
end;

procedure TJvForthScript.doToken(aToken: TToken);
begin
  case aToken of
    dfonow: ProcNow;
    dfodatestr: ProcDateStr;
    dfotimestr: ProcTimeStr;
    dfoshellexecute: ProcShellExecute;
    dfocrlf: ProcCrLf;
    dfoCStr: procCStr;
    dfoXML: ProcXML;
    dfoDSO: procDSO;
    dfoSeldir: ProcSelDir;
    dfoDSOBase: ProcDSOBase;
    dfoIntVar: ProcIntVar;
    dfoExtVar: ProcExtVar;
    dfoSystem: procSystem;
    //    dfoVarGet: ProcVarGet;
    //    dfoVarset: ProcVarSet;
    //    dfoSysGet: procSysGet;
    //    dfoSysSet: procSysSet;
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
    //    dfoassign: ProcAssign;
    //    dfovariable: ProcVariable;
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

function TJvForthScript.Execute: Variant;
var
  c: Integer;
  atom: TAtom;
  Token: TToken;
  TimeOutTicks: Cardinal;
  deltaTicks: cardinal;
begin
  Result := null;
  // osp := 0;
  FVSP := 0;
  FPSP := 0;
  FRSP := 0;
  c := FAtoms.Count;
  FVarsList.ClearObjects;
  FDSOList.ClearTables;
  FXMLList.ClearXMLS;
  FXMLSelect.Clear;
  FXMLSelectRecord := -1;
  if c = 0 then
    Exit;
  FPC := 0;
  deltaTicks := FScriptTimeOut * 1000;
  TimeOutticks := GetTickCount + DeltaTicks;
  // evaluate all FAtoms
  while FPC < c do
  begin
    if GetTickCount > timeOutTicks then
      raise EJvJanScriptError.CreateResFmt(@RsEScriptTimedOutAfterdSeconds, [FScriptTimeout]);
    atom := TAtom(FAtoms[FPC]);
    Inc(FPC);
    FCurrentValue := atom.Value;
    FCurrentSymbol := atom.Symbol;
    token := atom.Token;
    case token of
      dfoInteger..dfoDate:
        begin
          vpush(FCurrentValue)
        end;
    else
      begin
        doToken(token);
      end;
    end
  end;
  if FVSP <= 0 then
    Result := null
  else
    Result := vpop;
end;

procedure TJvForthScript.SetOnGetVariable(const Value: TOnGetVariable);
begin
  FOnGetVariable := Value;
end;

(*)
procedure TJvForthScript.ClearAtoms;
var
  i, c: Integer;
begin
  c := FAtoms.Count;
  if c = 0 then
    Exit;
  for i := 0 to c - 1 do
    Tobject(FAtoms[i]).Free;
  FAtoms.Clear;
end;
(*)

procedure TJvForthScript.SetOnSetVariable(const Value: TOnSetVariable);
begin
  FOnSetVariable := Value;
end;

procedure TJvForthScript.procAdd;
var
  Value: Variant;
begin
  Value := vpop;
  Value := vpop + Value;
  vpush(Value);
end;

procedure TJvForthScript.procAnd;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop and Value);
end;

procedure TJvForthScript.ProcAssign;
var
  Value: Variant;
  Handled: Boolean;
  err: string;
begin
  Value := vpop;
  vpush(Value);
  Handled := False;
  err := Format(RsECanNotAssignVariables, [FCurrentSymbol]);
  if Assigned(onSetVariable) then
  begin
    onSetVariable(Self, FCurrentSymbol, Value, Handled, Err);
    if not Handled then
      raise EJvJanScriptError.Create(err);
  end;
end;

procedure TJvForthScript.procBoolean;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.doproc;
var
  token: TToken;
begin
  if FPSP <= 0 then
    Exit;
  Dec(FPSP);
  token := FPStack[FPSP];
  doToken(token);
end;

procedure TJvForthScript.procCos;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(cos(Value));
end;

procedure TJvForthScript.procDate;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.procDivide;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop / Value);
end;

procedure TJvForthScript.procEq;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop = Value);
end;

procedure TJvForthScript.procFloat;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.procGe;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop >= Value);
end;

procedure TJvForthScript.procGt;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop > Value);
end;

procedure TJvForthScript.procIn;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(FuncIn(vpop, Value));
end;

procedure TJvForthScript.procInteger;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.procLe;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop <= Value);
end;

procedure TJvForthScript.procLeft;
var
  Value, v2: Variant;
  vali: Integer;
  vals: string;
begin
  Value := vpop;
  v2 := vpop;
  vali := Value;
  vals := v2;
  Value := Copy(vals, 1, vali);
  vpush(Value);
end;

procedure TJvForthScript.procLike;
var
  Value: Variant;
begin
  Value := vartostr(vpop);
  vpush(Pos(LowerCase(Value), LowerCase(vartostr(vpop))) > 0);
end;

procedure TJvForthScript.procLt;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop < Value);
end;

procedure TJvForthScript.procMultiply;
var
  Value: Variant;
begin
  Value := vpop;
  Value := vpop * Value;
  vpush(Value);
end;

procedure TJvForthScript.procNe;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop <> Value);
end;

procedure TJvForthScript.procNegate;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(0 - Value);
end;

procedure TJvForthScript.procNop;
begin
  //  just do nothing
end;

procedure TJvForthScript.procNot;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(not Value);
end;

procedure TJvForthScript.procOr;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop or Value);
end;

procedure TJvForthScript.procRight;
var
  Value, v2: Variant;
  vali: Integer;
  vals: string;
begin
  Value := vpop;
  v2 := vpop;
  vali := Value;
  vals := v2;
  if vali <= Length(vals) then
    Value := Copy(vals, Length(vals) - vali + 1, vali)
  else
    Value := vals;
  vpush(Value);
end;

procedure TJvForthScript.procSet;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.procSin;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(Sin(Value));
end;

procedure TJvForthScript.procSqr;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(Sqr(Value));
end;

procedure TJvForthScript.procSqrt;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(Sqrt(Value));
end;

procedure TJvForthScript.procString;
begin
  Vpush(FCurrentValue);
  doproc;
end;

procedure TJvForthScript.procSubtract;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop - Value);
end;

procedure TJvForthScript.procUnlike;
var
  Value: Variant;
begin
  Value := vartostr(vpop);
  vpush(Pos(LowerCase(Value), LowerCase(vartostr(vpop))) = 0);
end;

procedure TJvForthScript.ProcVariable;
var
  Value: Variant;
  Handled: Boolean;
  err: string;
begin
  Handled := False;
  err := Format(RsEVariablesNotDefined, [FCurrentSymbol]);
  if Assigned(onGetVariable) then
    onGetVariable(Self, FCurrentSymbol, Value, Handled, Err);
  if not Handled then
    raise EJvJanScriptError.Create(err)
  else
    Vpush(Value);
end;

procedure TJvForthScript.procXor;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(vpop xor Value);
end;

procedure TJvForthScript.procIf;
var
  v: Variant;
begin
  v := vpop;
  if v then
    Exit
  else
    FPC := FCurrentValue;
end;

procedure TJvForthScript.procElse;
begin
  FPC := FCurrentValue;
end;

procedure TJvForthScript.procDrop;
begin
  vpop;
end;

procedure TJvForthScript.procDup;
var
  v: Variant;
begin
  v := vpop;
  vpush(v);
  vpush(v);
end;

procedure TJvForthScript.procSwap;
var
  v1, v2: Variant;
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

// keep looping until vpop=True

procedure TJvForthScript.procUntil;
begin
  if not vpop then
    FPC := FCurrentValue;
end;

procedure TJvForthScript.procRepeat;
begin
  // do nothing
end;

function TJvForthScript.rpop: Integer;
begin
  if FRSP <= 0 then
    raise EJvJanScriptError.CreateRes(@RsEReturnStackUnderflow)
  else
  begin
    Dec(FRSP);
    Result := FRStack[FRSP];
  end;
end;

procedure TJvForthScript.rpush(AValue: Integer);
begin
  FRStack[FRSP] := AValue;
  if FRSP < StackMax then
    Inc(FRSP)
  else
    raise EJvJanScriptError.CreateRes(@RsEReturnStackOverflow);
end;

procedure TJvForthScript.SetScriptTimeOut(const Value: Integer);
begin
  FScriptTimeOut := Value;
end;

procedure TJvForthScript.procEndsub;
begin
  FPC := rpop;
end;

// just skip till endSub

procedure TJvForthScript.procSub;
var
  c: Integer;
  token: TToken;
begin
  { TODO -oJVCL -cPOSSIBLEBUG : (p3) What should "c" really be here? }
  c := FAtoms.Count; //??
  while FPC < c do
  begin
    token := TAtom(FAtoms[FPC]).token;
    if token = dfoEndSub then
    begin
      Inc(FPC);
      Exit;
    end;
    Inc(FPC);
  end;
end;

// call to a user sub, just look it up

procedure TJvForthScript.procCall;
var
  Index: Integer;
begin
  //  Index:=FSubsList.IndexOf(FCurrentSymbol);
  Index := FCurrentValue;
  if Index <> -1 then
  begin
    rpush(FPC);
    //    FPC:=Integer(FsubsList.Objects[Index]);
    FPC := Index;
    Exit;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEProceduresNotDefined, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarGet;
var
  v: Variant;
begin
  v := FvarsList.GetVariable(FCurrentSymbol);
  if v <> null then
    vpush(v)
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSet;
var
  v: Variant;
begin
  v := vpop;
  FVarsList.SetVariable(FCurrentSymbol, v);
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
  Value: Variant;
  Handled: Boolean;
  err, prompt: string;
begin
  prompt := vpop;
  Handled := False;
  err := Format(RsESystemsNotDefined, [FCurrentSymbol]);
  if Assigned(onGetSystem) then
    onGetSystem(Self, FCurrentSymbol, prompt, Value, Handled, Err);
  if not Handled then
    raise EJvJanScriptError.Create(err)
  else
    Vpush(Value);
end;

procedure TJvForthScript.procSysSet;
var
  Value: Variant;
  Handled: Boolean;
  err: string;
begin
  Value := vpop;
  vpush(Value);
  Handled := False;
  err := Format(RsECanNotAssignSystems, [FCurrentSymbol]);
  if Assigned(onSetSystem) then
  begin
    onSetSystem(Self, FCurrentSymbol, Value, Handled, Err);
    if not Handled then
      raise EJvJanScriptError.Create(err);
  end;
end;

procedure TJvForthScript.SetonGetSystem(const Value: TOnGetSystem);
begin
  FOnGetSystem := Value;
end;

procedure TJvForthScript.SetonSetSystem(const Value: TOnSetSystem);
begin
  FOnSetSystem := Value;
end;

function TJvForthScript.popValue: Variant;
begin
  Result := vpop;
end;

procedure TJvForthScript.pushValue(AValue: Variant);
begin
  vpush(AValue);
end;

function TJvForthScript.canPopValue: Boolean;
begin
  Result := FVSP > 0;
end;

function TJvForthScript.canPushValue: Boolean;
begin
  Result := FVSP < StackMax;
end;

procedure TJvForthScript.procpi;
begin
  vpush(pi);
end;

procedure TJvForthScript.procDSO;
var
  AName, aMethod: string;
  table: TJvJanDSO;
  AField, AValue: string;
  AKey: Variant;
  c: Integer;
begin
  AName := FCurrentSymbol;
  aMethod := FCurrentValue;
  table := FDSOList.Table(AName);
  if aMethod = 'set' then
  begin
    AKey := vpop;
    AField := vpop;
    AValue := vpop;
    table.SetValue(AKey, AField, AValue);
  end
  else
  if aMethod = 'get' then
  begin
    AKey := vpop;
    AField := vpop;
    AValue := table.GetValue(AKey, AField);
    vpush(AValue);
  end
  else
  if aMethod = 'load' then
    table.LoadFromFile(FDSOBase + PathDelim + AName + '.txt')
  else
  if aMethod = 'save' then
    table.SaveToFile(FDSOBase + PathDelim + AName + '.txt')
  else
  if aMethod = 'Clear' then
    table.Clear
  else
  if aMethod = 'Count' then
  begin
    c := table.Count;
    vpush(c);
  end;
end;

procedure TJvForthScript.ProcDSOBase;
var
  s: string;
begin
  s := vpop;
  FDSOBase := s;
end;

procedure TJvForthScript.ProcSelDir;


var
  Dir: WideString;
begin
  Dir := FDSOBase;
  if SelectDirectory('Select Directory', PathDelim, Dir {$IFDEF LINUX}, True {$ENDIF}) then
    FDSOBase := Dir;
end;


procedure TJvForthScript.ProcExtVar;
var
  AName, aMethod: string;
begin
  AName := FCurrentSymbol;
  aMethod := FCurrentValue;
  if aMethod = 'set' then
    ProcAssign
  else
  if aMethod = 'get' then
    ProcVariable
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizeExternalVariableMethodss, [AName, amethod]);
end;

procedure TJvForthScript.ProcIntVar;
var
  AName, aMethod: string;
begin
  AName := FCurrentSymbol;
  aMethod := FCurrentValue;
  if aMethod = 'set' then
    ProcVarSet
  else
  if aMethod = 'get' then
    ProcVarGet
  else
  if aMethod = '1+' then
    ProcVarInc
  else
  if aMethod = '[1+]' then
    ProcVarIncIndex
  else
  if aMethod = '1-' then
    ProcVarDec
  else
  if aMethod = '1-?0' then
    ProcVarDecTestZero
  else
  if aMethod = '+' then
    ProcVarAdd
  else
  if aMethod = '-' then
    ProcVarSub
  else
  if aMethod = '*' then
    ProcVarMul
  else
  if aMethod = '/' then
    ProcVarDiv
  else
  if aMethod = '--' then
    ProcVarNeg
  else
  if aMethod = 'load' then
    ProcVarLoad
  else
  if aMethod = 'save' then
    ProcVarSave
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizeInternalVariableMethodss, [AName, amethod]);
end;

procedure TJvForthScript.procSystem;
var
  AName, aMethod: string;
begin
  AName := FCurrentSymbol;
  aMethod := FCurrentValue;
  if aMethod = 'set' then
    procSysSet
  else
  if aMethod = 'get' then
    procSysGet
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizeSystemMethodss, [AName, amethod]);
end;

procedure TJvForthScript.ProcVarDec;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value - 1
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarInc;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value + 1
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarAdd;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value + vpop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarDiv;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value / vpop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarMul;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value * vpop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSub;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := vo.Value - vpop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarNeg;
var
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
    vo.Value := 0 - vo.Value
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.procPower;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(power(vpop, Value));
end;

procedure TJvForthScript.procAbs;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(abs(Value));
end;

procedure TJvForthScript.SetonInclude(const Value: TOnInclude);
begin
  FOnInclude := Value;
end;

procedure TJvForthScript.procTan;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(tan(Value));
end;

procedure TJvForthScript.procarccos;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(arccos(Value));
end;

procedure TJvForthScript.procarcsin;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(arcsin(Value));
end;

procedure TJvForthScript.procarctan;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(arctan(Value));
end;

procedure TJvForthScript.procarctan2;
var
  Value: Variant;
begin
  Value := vpop;
  vpush(arctan2(vpop, Value));
end;

procedure TJvForthScript.ProcVarLoad;
var
  vo: TVariantObject;
  ap, fn, s: string;
begin
  fn := vpop;
  ap := ExtractFilePath(paramstr(0));
  fn := StringReplace(fn, '%', ap, []);
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
  begin
    s := loadstring(fn);
    vo.Value := s;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSave;
var
  vo: TVariantObject;
  ap, fn, s: string;
begin
  fn := vpop;
  ap := ExtractFilePath(paramstr(0));
  fn := StringReplace(fn, '%', ap, []);
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
  begin
    s := vo.Value;
    savestring(fn, s);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcXML;
var
  AName, aMethod: string;
  xmldso: TJvXMLTree;
  n: TJvXMLNode;
  a: TJvXMLAttribute;
  aPath, atName: string;
  AValue: Variant;
  c, i, cc: Integer;
  appldir: string;
  b: Boolean;
begin
  n := nil;
  appldir := ExtractFilePath(paramstr(0));
  AName := FCurrentSymbol;
  aMethod := FCurrentValue;
  xmldso := FXMLList.Xml(AName);
  if aMethod = 'set' then
  begin
    aPath := vpop;
    AValue := vpop;
    n := xmldso.ForceNamePathNode(aPath);
    n.Value := AValue;
  end
  else
  if aMethod = '@set' then
  begin
    aPath := vpop;
    atName := vpop;
    AValue := vpop;
    xmldso.ForceNamePathNodeAttribute(aPath, atName, AValue);
  end
  else
  if aMethod = 'get' then
  begin
    apath := vpop;
    n := xmldso.getNamePathNode(apath);
    if n = nil then
      AValue := ''
    else
      AValue := n.Value;
    vpush(AValue);
  end
  else
  if aMethod = 'Count' then
  begin
    apath := vpop;
    n := xmldso.getNamePathNode(apath);
    AValue := 0;
    cc := 0;
    if n <> nil then
    begin
      // now Count named node
      c := n.Nodes.Count;
      apath := vpop;
      if c > 0 then
      begin
        for i := 0 to c - 1 do
          if TJvXMLNode(n.nodes[i]).name = apath then
            Inc(cc);
      end;
      AValue := cc;
    end;
    vpush(AValue);
  end
  else
  if aMethod = '@get' then
  begin
    apath := vpop;
    atname := vpop;
    a := xmldso.getNamePathNodeAttribute(apath, atname);
    if n = nil then
      AValue := ''
    else
      AValue := a.Value;
    vpush(AValue);
  end
  else
  if aMethod = 'load' then
  begin
    aPath := vpop;
    aPath := StringReplace(aPath, '%', appldir, []);
    if not fileexists(aPath) then
      raise EJvJanScriptError.CreateResFmt(@RsEFilesDoesNotExist, [apath]);
    xmldso.LoadFromFile(apath);
  end
  else
  if aMethod = 'save' then
  begin
    apath := vpop;
    aPath := StringReplace(aPath, '%', appldir, []);
    try
      xmldso.SaveToFile(apath);
    except
      raise EJvJanScriptError.CreateResFmt(@RsECanNotSaveToFiles, [apath]);
    end
  end
  else
  if aMethod = 'astext' then
  begin
    AValue := xmldso.asText;
    vpush(AValue);
  end
  else
  if aMethod = 'Delete' then
  begin
    apath := vpop;
    xmldso.deleteNamePathNode(apath);
  end
  else
  if aMethod = '@Delete' then
  begin
    apath := vpop;
    atname := vpop;
    xmldso.deleteNamePathNodeAttribute(apath, atName);
  end
  else
  if aMethod = 'select' then
  begin
    apath := vpop;
    apath := StringReplace(apath, '''', '"', [rfreplaceall]);
    FXMLSelect.Clear;
    FXMLSelectRecord := -1;
    xmldso.selectNodes(apath, FXMLSelect);
    vpush(FXMLSelect.Count > 0);
  end
  else
  if aMethod = 'selectfirst' then
  begin
    b := FXMLSelect.Count <> 0;
    if b then
      FXMLSelectRecord := 0
    else
      FXMLSelectRecord := -1;
    AValue := b;
    vpush(AValue);
  end
  else
  if aMethod = 'selectnext' then
  begin
    b := FXMLSelect.Count <> 0;
    if b then
      Inc(FXMLSelectRecord)
    else
      FXMLSelectRecord := -1;
    if FXMLSelectRecord >= FXMLSelect.Count then
    begin
      b := False;
      FXMLSelectRecord := -1;
    end;
    AValue := b;
    vpush(AValue);
  end
  else
  if aMethod = 'selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionIsEmpty);
    if FXMLSelectRecord = -1 then
      raise EJvJanScriptError.CreateRes(@RsENoXMLSelectionSelected);
    if FXMLSelectRecord >= FXMLSelect.Count then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionOutOfRange);
    n := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    AValue := n.Value;
    vpush(AValue);
  end
  else
  if aMethod = '@selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionIsEmpty);
    if FXMLSelectRecord = -1 then
      raise EJvJanScriptError.CreateRes(@RsENoXMLSelectionSelected);
    if FXMLSelectRecord >= FXMLSelect.Count then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionOutOfRange);
    n := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    atname := vpop;
    AValue := n.GetAttributeValue(atname);
    vpush(AValue);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEInvalidXmlMethodSpecifiers, [aMethod]);
end;

procedure TJvForthScript.ProcVarDecTestZero;
var
  v: Variant;
  vo: TVariantObject;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
  begin
    v := vo.Value - 1;
    vo.Value := v;
    vpush(v = 0);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarIncIndex;
var
  vo: TVariantObject;
  s, sidx: string;
  pb, pe: Integer;
  Index: Integer;
begin
  vo := FvarsList.GetObject(FCurrentSymbol);
  if vo <> nil then
  begin
    s := vo.Value;
    pb := lastposchar('[', s);
    if pb = 0 then
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIns, [s]);
    pe := lastposchar(']', s);
    if pe = 0 then
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIns_, [s]);
    sidx := Copy(s, pb + 1, pe - pb - 1);
    try
      Index := strtoint(sidx);
      Inc(Index);
      s := Copy(s, 1, pb - 1) + '[' + inttostr(Index) + ']';
      vo.Value := s;
      vpush(s);
    except
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIntegerBetwee, [s]);
    end;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcCrLf;
begin
  vpush(sLineBreak);
end;

procedure TJvForthScript.ProcShellExecute;
var
  afile: string;
  appldir: string;
begin
  appldir := ExtractFilePath(paramstr(0));
  afile := vpop;
  afile := StringReplace(afile, '%', appldir, []);
  launch(afile);
end;

procedure TJvForthScript.ProcDateStr;
var
  s: string;
begin
  s := FormatDateTime('dd-mmm-yyyy', now);
  vpush(s);
end;

procedure TJvForthScript.ProcTimeStr;
var
  s: string;
begin
  s := FormatDateTime('hh:nn:ss', now);
  vpush(s);
end;

procedure TJvForthScript.ProcNow;
begin
  vpush(now);
end;

{ TAtom }

procedure TAtom.SetIsOperand(const Value: Boolean);
begin
  FIsOperand := Value;
end;

procedure TAtom.SetToken(const Value: TToken);
begin
  FToken := Value;
end;

procedure TAtom.SetProc(const Value: TProcVar);
begin
  FProc := Value;
end;

procedure TAtom.SetSymbol(const Value: string);
begin
  FSymbol := Value;
end;

procedure TAtom.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TAtomList }

procedure TAtomList.ClearObjects;
var
  i, c: Integer;
begin
  c := Count;
  if c = 0 then
    Exit;
  for i := 0 to c - 1 do
    TAtom(items[i]).Free;
  Clear;
end;

destructor TAtomList.Destroy;
begin
  ClearObjects;
  inherited;
end;

{ TVariantObject }

procedure TVariantObject.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TVariantList }

procedure TVariantList.ClearObjects;
var
  i, c: Integer;
begin
  c := Count;
  if c = 0 then
    Exit;
  for i := 0 to c - 1 do
    TVariantObject(Objects[i]).Free;
  Clear;
end;

destructor TVariantList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

function TVariantList.GetObject(Symbol: string): TVariantObject;
var
  Index: Integer;
begin
  Result := nil;
  if Count = 0 then
    Exit;
  Index := IndexOf(Symbol);
  if Index = -1 then
    Exit;
  Result := TVariantObject(Objects[Index]);
end;

function TVariantList.GetVariable(Symbol: string): Variant;
var
  Index: Integer;
begin
  Result := null;
  if Count = 0 then
    Exit;
  Index := IndexOf(Symbol);
  if Index = -1 then
    Exit;
  Result := TVariantObject(Objects[Index]).Value;
end;

procedure TVariantList.SetVariable(Symbol: string; AValue: Variant);
var
  Index: Integer;
  obj: TVariantObject;
begin
  Index := IndexOf(Symbol);
  if Index = -1 then
  begin
    obj := TVariantObject.Create;
    obj.Value := AValue;
    addobject(Symbol, obj);
  end
  else
  begin
    TVariantObject(Objects[Index]).Value := AValue;
  end;
end;

{ TJvJanDSOList }

procedure TJvJanDSOList.ClearTables;
var
  i, c: Integer;
begin
  c := Count;
  if c <> 0 then
    for i := 0 to c - 1 do
      TJvJanDSO(Objects[i]).Free;
  Clear;
end;

destructor TJvJanDSOList.Destroy;
begin
  ClearTables;
  inherited Destroy;
end;

function TJvJanDSOList.Table(AName: string): TJvJanDSO;
var
  Index: Integer;
  dso: TJvJanDSO;
begin
  Index := IndexOf(AName);
  if Index = -1 then
  begin
    dso := TJvJanDSO.Create;
    addobject(AName, dso);
    Result := dso;
  end
  else
    Result := TJvJanDSO(Objects[Index]);
end;

{ TJvJanDSO }

function TJvJanDSO.GetValue(AKey: Variant; AField: string): string;
var
  Index: Integer;
  key: string;
  strkey: Boolean;
begin
  key := AKey;
  strkey := False;
  Index := 0;
  try
    Index := StrToInt(key)
  except
    strkey := True;
  end;
  if not strkey then
  begin
    if Index >= Count then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOIndexOutOfRanged, [Index])
    else
      Result := InternalGetValue(Index, AField);
  end
  else
  begin
    Index := indexofName(key);
    if Index = -1 then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOUnknownKeys, [key]);
    Result := InternalGetValue(Index, AField);
  end
end;

function TJvJanDSO.InternalGetValue(Index: Integer; AField: string): string;
var
  key, s: string;
  p: Integer;
begin
  s := Strings[Index];
  p := Pos('=', s);
  key := Copy(s, 1, p - 1);
  s := Copy(s, p + 1, Length(s));
  Result := GlobalGetValue(s, AField);
end;

procedure TJvJanDSO.InternalSetValue(Index: Integer; AField, AValue: string);
var
  key, s: string;
  p: Integer;
begin
  s := Strings[Index];
  p := Pos('=', s);
  key := Copy(s, 1, p - 1);
  s := Copy(s, p + 1, Length(s));
  GlobalSetValue(s, AField, AValue);
  Strings[Index] := key + '=' + s;
end;

procedure TJvJanDSO.SetValue(AKey: Variant; AField, AValue: string);
var
  Index: Integer;
  key: string;
  strkey: Boolean;
begin
  key := AKey;
  strkey := False;
  Index := 0;
  try
    Index := strtoint(key)
  except
    strkey := True;
  end;
  if not strkey then
  begin
    if Index >= Count then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOIndexOutOfRanged, [Index])
    else
      InternalSetValue(Index, AField, AValue);
  end
  else
  begin
    Index := indexofname(key);
    if Index = -1 then
      Index := Add(key + '=');
    InternalSetValue(Index, AField, AValue);
  end
end;

{ TJvJanXMLList }

procedure TJvJanXMLList.ClearXMLS;
var
  i, c: Integer;
begin
  c := Count;
  if c <> 0 then
    for i := 0 to c - 1 do
      TJvXMLTree(Objects[i]).Free;
  Clear;
end;

destructor TJvJanXMLList.Destroy;
begin
  ClearXMLS;
  inherited Destroy;
end;

function TJvJanXMLList.Xml(AName: string): TJvXMLTree;
var
  Index: Integer;
  xmldso: TJvXMLTree;
begin
  Index := IndexOf(AName);
  if Index = -1 then
  begin
    xmldso := TJvXMLTree.Create(AName, '', nil);
    addobject(AName, xmldso);
    Result := xmldso;
  end
  else
    Result := TJvXMLTree(Objects[Index]);
end;

end.
