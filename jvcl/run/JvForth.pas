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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvForth;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF MSWINDOWS}
  Windows, Messages, Forms, Dialogs, FileCtrl, Variants,
  JvXmlTree, JvComponentBase, JvStrings, JvTypes;

const
  StackMax = 1000;

type
  EJvJanScriptError = class(EJVCLException);

  TToken = (dfoError, dfoNop,
    // flow actions
    dfoIf, dfoElse, dfoEndIf, dfoRepeat, dfoUntil,
    // sub routines
    dfoSub, dfoEndSub, dfoCall,
    // stack operations
    dfoDup, dfoDrop, dfoSwap,
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
    dfoCrLf,
    // some gonio functions
    dfoSin, dfoCos, dfoPi, dfoTan,
    dfoArcSin, dfoArcCos, dfoArcTan, dfoArcTan2,

    dfoNegate, dfoSqr, dfoSqrt,
    dfoLeft, dfoRight,
    // windows api
    dfoShellExecute,
    // date and time
    dfoNow, dfoTime, dfoDateStr, dfoTimeStr
   );

  TProcVar = procedure of object;

  TOnGetVariable = procedure(Sender: TObject; Symbol: string; var Value: Variant;
    var Handled: Boolean; var ErrorStr: string) of object;
  TOnSetVariable = procedure(Sender: TObject; Symbol: string; Value: Variant;
    var Handled: Boolean; var ErrorStr: string) of object;
  TOnGetSystem = procedure(Sender: TObject; Symbol, Prompt: string; var Value: Variant;
    var Handled: Boolean; var ErrorStr: string) of object;
  TOnSetSystem = procedure(Sender: TObject; Symbol: string; Value: Variant;
    var Handled: Boolean; var ErrorStr: string) of object;
  TOnInclude = procedure(Sender: TObject; IncludeFile: string; var Value: string;
    var Handled: Boolean; var ErrorStr: string) of object;

  TJvJanDSO = class(TStringList)
  private
    function InternalGetValue(Index: Integer; const AField: string): string;
    procedure InternalSetValue(Index: Integer; const AField, AValue: string);
  public
    // when a key is not found it will be added
    procedure SetValue(AKey: Variant; const AField, AValue: string);
    function GetValue(AKey: Variant; const AField: string): string;
  end;

  TJvJanDSOList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearTables;
    function Table(const AName: string): TJvJanDSO;
  end;

  TJvJanXMLList = class(TStringList)
  public
    destructor Destroy; override;
    procedure ClearXMLS;
    function Xml(const AName: string): TJvXMLTree;
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
    procedure SetVariable(const Symbol: string; AValue: Variant);
    function GetVariable(const Symbol: string): Variant;
    function GetObject(const Symbol: string): TVariantObject; reintroduce;
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

  TRStack = array [0..StackMax] of Integer;
  TVStack = array [0..StackMax] of Variant;
  TPStack = array [0..StackMax] of TToken;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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
    FRStack: TRStack;
    FRSP: Integer;
    FVStack: TVStack;
    FVSP: Integer;
    // ostack: array[0..StackMax] of TToken;
    // osp: Integer;
    FPStack: TPStack;
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
    procedure ProcArcSin;
    procedure ProcArcCos;
    procedure ProcArcTan;
    procedure ProcArcTan2;

    procedure ProcNegate;
    procedure ProcSqr;
    procedure ProcSqrt;
    procedure ProcLeft;
    procedure ProcRight;
    function VPop: Variant;
    procedure VPush(AValue: Variant);
//    function opop: TToken;
//    procedure opush(AValue: TToken);
//    function ppop: TToken;
//    procedure ppush(AValue: TToken);
    function RPop: Integer;
    procedure RPush(AValue: Integer);
    procedure DoProc;
    procedure DoToken(AToken: TToken);
    procedure SetScriptTimeOut(const Value: Integer);
    procedure ParseScript;
    procedure SetOnGetSystem(const Value: TOnGetSystem);
    procedure SetOnSetSystem(const Value: TOnSetSystem);
    procedure SetOnInclude(const Value: TOnInclude);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Variant;
    function PopValue: Variant;
    function CanPopValue: Boolean;
    procedure PushValue(AValue: Variant);
    function CanPushValue: Boolean;
  published
    property Script: string read FScript write SetScript;
    property ScriptTimeOut: Integer read FScriptTimeOut write SetScriptTimeOut;
    property OnGetVariable: TOnGetVariable read FOnGetVariable write SetOnGetVariable;
    property OnSetVariable: TOnSetVariable read FOnSetVariable write SetOnSetVariable;
    property OnSetSystem: TOnSetSystem read FOnSetSystem write SetOnSetSystem;
    property OnGetSystem: TOnGetSystem read FOnGetSystem write SetOnGetSystem;
    property OnInclude: TOnInclude read FOnInclude write SetOnInclude;
  end;

// runs an external file or progam
procedure Launch(const AFile: string);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  JvConsts, JvResources;

{ some utility functions }

procedure Launch(const AFile: string);
var
  Command, Params, WorkDir: string;
begin
  Command := AFile;
  Params := #0;
  WorkDir := #0;
  ShellExecute(Application.Handle, 'open', PChar(Command),
    PChar(Params), PChar(WorkDir), SW_SHOWNORMAL);
end;

procedure GlobalSetValue(var AText: string; const AName, AValue: string);
var
  P, P2, L: Integer;
begin
  L := Length(AName) + 2;
  if AText = '' then
  begin
    AText := AName + '="' + AValue + '"';
  end
  else
  begin
    P := PosText(AName + '="', AText);
    if P = 0 then
      AText := AText + ' ' + AName + '="' + AValue + '"'
    else
    begin
      P2 := PosStr('"', AText, P + L);
      if P2 = 0 then
        Exit;
      Delete(AText, P + L, P2 - (P + L));
      Insert(AValue, AText, P + L);
    end;
  end;
end;

function GlobalGetValue(const AText, AName: string): string;
var
  P, P2, L: Integer;
begin
  Result := '';
  L := Length(AName) + 2;
  P := PosText(AName + '="', AText);
  if P = 0 then
    Exit;
  P2 := PosStr('"', AText, P + L);
  if P2 = 0 then
    Exit;
  Result := Copy(AText, P + L, P2 - (P + L));
  Result := StringReplace(Result, '~~', sLineBreak, [rfReplaceAll]);
end;

// some special expression functions

// returns the Index of Integer v in aList

function IndexOfInteger(AList: TStringList; Value: Variant): Integer;
var
  C, I, Index, P: Integer;
  S, S1, S2: string;
begin
  Result := -1;
  I := Value;
  C := AList.Count;
  if C = 0 then
    Exit;
  for Index := 0 to C - 1 do
  begin
    try
      S := AList[Index];
      P := Pos('..', S);
      if P = 0 then
      begin
        if StrToInt(AList[Index]) = I then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin // have range
        S1 := Trim(Copy(S, 1, P - 1));
        S2 := Trim(Copy(S, P + 2, Length(S)));
        if (I >= StrToInt(S1)) and (I <= StrToInt(S2)) then
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

// returns the Index of float Value (single or double) in AList

function IndexOfFloat(AList: TStringList; Value: Variant): Integer;
var
  C, Index, P: Integer;
  F: Extended;
  S, S1, S2: string;
begin
  Result := -1;
  F := Value;
  C := AList.Count;
  if C = 0 then
    Exit;
  for Index := 0 to C - 1 do
  begin
    try
      S := AList[Index];
      P := Pos('..', S);
      if P = 0 then
      begin
        if StrToFloat(S) = F then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin // have range
        S1 := Trim(Copy(S, 1, P - 1));
        S2 := Trim(Copy(S, P + 2, Length(S)));
        if (F >= StrToFloat(S1)) and (F <= StrToFloat(S2)) then
        begin
          Result := Index;
          Exit;
        end;
      end;
    except
      raise EJvJanScriptError.CreateResFmt(@RsEInvalidNumbers, [S]);
    end;
  end;
end;

// returns the Index of date Value in AList

function IndexOfDate(AList: TStringList; Value: Variant): Integer;
var
  C, Index, P: Integer;
  D: TDateTime;
  S, S1, S2: string;
begin
  Result := -1;
  D := Value;
  C := AList.Count;
  if C = 0 then
    Exit;
  for Index := 0 to C - 1 do
  begin
    try
      S := AList[Index];
      P := Pos('..', S);
      if P = 0 then
      begin
        if StrToDate(AList[Index]) = D then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin
        S1 := Trim(Copy(S, 1, P - 1));
        S2 := Trim(Copy(S, P + 2, Length(S)));
        if (D >= StrToDate(S1)) and (D <= StrToDate(S2)) then
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

// returns the Index of string Value in AList

function IndexOfString(AList: TStringList; Value: Variant): Integer;
var
  C, Index, P: Integer;
  SV: string;
  S, S1, S2: string;
begin
  Result := -1;
  SV := Value;
  C := AList.Count;
  if C = 0 then
    Exit;
  for Index := 0 to C - 1 do
  begin
    try
      S := AList[Index];
      P := Pos('..', S);
      if P = 0 then
      begin
        if AList[Index] = SV then
        begin
          Result := Index;
          Exit;
        end;
      end
      else
      begin
        S1 := Trim(Copy(S, 1, P - 1));
        S2 := Trim(Copy(S, P + 2, Length(S)));
        if (SV >= S1) and (SV <= S2) then
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
// tests if AValue is in ASet

function FuncIn(AValue: Variant; ASet: Variant): Boolean;
var
  List: TStringList;
  S: string;
  P: Integer;
  Token: string;

  function GetToken: Boolean;
  begin
    Result := False;
    S := TrimLeft(S);
    if S = '' then
      Exit;
    P := 1;
    if S[1] = '"' then
    begin // get string
      P := PosStr('"', S, 2);
      if P = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedStringNears, [S]);
      Token := Copy(S, 2, P - 2);
      Delete(S, 1, P);
      Result := True;
    end
    else
    begin
      P := Pos(' ', S);
      if P = 0 then
      begin
        Token := S;
        Result := True;
        S := '';
      end
      else
      begin
        Token := Copy(S, 1, P - 1);
        Delete(S, 1, P);
        Result := True;
      end;
    end
  end;

begin
  Result := False;
  S := ASet;
  if S = '' then
    Exit;
  List := TStringList.Create;
  try
    while GetToken do
      List.Append(Token);
    //    c:=List.Count;
    case VarType(AValue) of
      varString:
        Result := IndexOfString(List, AValue) > -1;
      varInteger, varByte:
        Result := IndexOfInteger(List, AValue) > -1;
      varSingle, varDouble:
        Result := IndexOfFloat(List, AValue) > -1;
      varDate:
        Result := IndexOfDate(List, AValue) > -1;
    else
      raise EJvJanScriptError.CreateRes(@RsEUnrecognizedDataTypeInSetOperation);
    end;
  finally
    List.Free;
  end;
end;

//=== { TJvForthScript } =====================================================

constructor TJvForthScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAtoms := TAtomList.Create;
  FIncludes := TStringList.Create;
  FSubsList := TStringList.Create;
  FVarsList := TVariantList.Create;
  FDSOList := TJvJanDSOList.Create;
  FXMLList := TJvJanXMLList.Create;
  FXMLSelect := TList.Create;
  FDSOBase := ExtractFilePath(ParamStr(0));
  if FDSOBase[Length(FDSOBase)] = PathDelim then
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

procedure TJvForthScript.VPush(AValue: Variant);
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
function TJvForthScript.VPop: Variant;
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
  S: string;
  I, P, P2: Integer;
  Atom: TAtom;
  //  atomoperation: TToken;
  AtomSymbol: string;
  AtomValue: Variant;
  //  atomproc: TProcVar;
  Token: string;
  VInteger: Integer;
  VFloat: Double;
  VDate: TDateTime;
  // handling of includes:
  IncFile: string;
  Handled: Boolean;
  IncScript: string;
  ErrStr: string;
  TimeOutTicks: Cardinal;
  DeltaTicks: Cardinal;

  function PushAtom(AToken: TToken): Integer;
    //    var cc: Integer;
  begin
    Atom := TAtom.Create;
    Atom.Token := AToken;
    Atom.Symbol := AtomSymbol;
    Atom.Value := AtomValue;
    Result := FAtoms.Add(Atom);
  end;

  procedure OPush(AToken: TToken);
    //    var cc: Integer;
  begin
    Atom := TAtom.Create;
    Atom.Token := AToken;
    Atom.Symbol := Token;
    Atom.Value := AtomValue;
    FAtoms.Add(Atom);
  end;

  procedure BrcPush(Proc: TProcVar);
    //    var cc: Integer;
  begin
    Atom := TAtom.Create;
    Atom.Proc := Proc;
    Atom.Symbol := AtomSymbol;
    Atom.Value := AtomValue;
    Atom.IsOperand := False;
    FAtoms.Add(Atom);
  end;

  function GetToken: Boolean;
  begin
    Result := False;
    S :=TrimLeft(S);
    if S = '' then
      Exit;
    P := 1;
    if S[1] = '"' then
    begin // get string
      P := PosStr('"', S, 2);
      if P = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedStringNears, [S]);
      Token := Copy(S, 1, P);
      Delete(S, 1, P);
      Result := True;
    end
    else
    if S[1] = '[' then
    begin // get block
      P := PosStr(']', S, 2);
      if P = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedBlockNear, [S]);
      Token := Copy(S, 1, P);
      Delete(S, 1, P);
      Result := True;
    end
    else
    begin
      P := Pos(' ', S);
      if P = 0 then
      begin
        Token := S;
        Result := True;
        S := '';
      end
      else
      begin
        Token := Copy(S, 1, P - 1);
        Delete(S, 1, P);
        Result := True;
      end;
    end;
  end;

begin
  FAtoms.ClearObjects;
  FSubsList.Clear;
  // reset return stack; needed in resolving flow statements
  FRSP := 0;
  S := FScript;
  // include any include files, include files start with $$ and end with ;
  // when the parser detects and include file it will raise the oninclude event
  // include files can also include files (nested includes)
  DeltaTicks := FScriptTimeOut * 1000;
  TimeOutTicks := GetTickCount + DeltaTicks;
  FIncludes.Clear; // Clear the includes List
  repeat
    if GetTickCount > TimeOutTicks then
      raise EJvJanScriptError.CreateResFmt(@RsEParserTimedOutAfterdSecondsYouMayHa, [FScriptTimeout]);
    P := PosStr('$$', S);
    if P > 0 then
    begin
      P2 := PosStr(';', S, P);
      if P2 = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEUnterminatedIncludeNears, [Copy(S, P, Length(S))]);
      IncFile := Copy(S, P + 2, P2 - P - 2) + '.jan';
      if PosStr(' ', IncFile, 1) > 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEIllegalSpaceCharacterInTheIncludeFi, [IncFile]);
      I := FIncludes.IndexOf(IncFile);
      if I <> -1 then
        Delete(S, P, P2 - P + 1)
      else
      begin
        ErrStr := Format(RsECanNotFindIncludeFiles, [IncFile]);
        Handled := False;
        IncScript := '';
        if not Assigned(OnInclude) then
          raise EJvJanScriptError.CreateResFmt(@RsEOnIncludeHandlerNotAssignedCanNotHa, [Copy(S, P, Length(S))]);
        OnInclude(Self, IncFile, IncScript, Handled, ErrStr);
        if not Handled then
          raise EJvJanScriptError.Create(ErrStr);
        Delete(S, P, P2 - P + 1);
        Insert(IncScript, S, P);
        FIncludes.Append(IncFile);
      end;
    end;
  until P = 0;
  S := Trim(StringReplace(S, sLineBreak, ' ', [rfReplaceAll]));
  // remove comments
  repeat
    P := Pos('{', S);
    if P > 0 then
    begin
      P2 := PosStr('}', S, P);
      if P2 = 0 then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingCommentTerminatorNears, [S]);
      Delete(S, P, P2 - P + 1);
    end;
  until P = 0;
  if S = '' then
    Exit;
  while GetToken do
  begin
    if Token = 'cstr' then
      OPush(dfoCstr)
    else
    if Token = 'seldir' then
      OPush(dfoSelDir)
    else
    if Token = 'dsobase' then
      OPush(dfoDSOBase)
    else
    if Token = 'dup' then
      OPush(dfoDup)
    else
    if Token = 'drop' then
      OPush(dfoDrop)
    else
    if Token = 'swap' then
      OPush(dfoSwap)
    else
    if Token = 'if' then
    begin
      P := PushAtom(dfoIf);
      RPush(P);
    end
    else
    if Token = 'endif' then
    begin
      P := PushAtom(dfoEndIf);
      P2 := RPop;
      Atom := TAtom(FAtoms[P2]);
      Atom.Value := P + 1;
    end
    else
    if Token = 'else' then
    begin
      P := PushAtom(dfoElse);
      P2 := RPop;
      RPush(P);
      Atom := TAtom(FAtoms[P2]);
      Atom.Value := P + 1;
    end
    else
    if Token = 'repeat' then
    begin
      P := PushAtom(dfoRepeat);
      RPush(P);
    end
    else
    if Token = 'until' then
    begin
      AtomValue := RPop;
      PushAtom(dfoUntil);
    end
    else
    if Token = 'now' then
      OPush(dfoNow)
    else
    if Token = 'datestr' then
      OPush(dfoDateStr)
    else
    if Token = 'timestr' then
      OPush(dfoTimeStr)
    else
    if Token = 'shellexecute' then
      OPush(dfoShellExecute)
    else
    if Token = ';' then
      OPush(dfoEndSub)
    else
    if Token = 'crlf' then
      OPush(dfoCrLf)
    else
    if Token = '--' then
      OPush(dfoNegate)
    else
    if Token = '-' then
      OPush(dfoSubtract)
    else
    if Token = '+' then
      OPush(dfoAdd)
    else
    if Token = '*' then
      OPush(dfoMultiply)
    else
    if Token = '/' then
      OPush(dfoDivide)
    else
    if Token = '^' then
      OPush(dfoPower)
    else
    if Token = 'abs' then
      OPush(dfoAbs)
    else
    if Token = 'left' then
      OPush(dfoLeft)
    else
    if Token = 'right' then
      OPush(dfoRight)
    else
    if Token = 'sqr' then
      OPush(dfoSqr)
    else
    if Token = 'sqrt' then
      OPush(dfoSqrt)
    else
    if Token = 'sin' then
      OPush(dfoSin)
    else
    if Token = 'cos' then
      OPush(dfoCos)
    else
    if Token = 'tan' then
      OPush(dfoTan)
    else
    if Token = 'arcsin' then
      OPush(dfoArcSin)
    else
    if Token = 'arccos' then
      OPush(dfoArcCos)
    else
    if Token = 'arctan' then
      OPush(dfoArcTan)
    else
    if Token = 'arctan2' then
      OPush(dfoArcTan2)
    else
    if Token = 'pi' then
      OPush(dfoPi)
    else
    if Token = '<>' then
      OPush(dfoNe)
    else
    if Token = '>=' then
      OPush(dfoGe)
    else
    if Token = '>' then
      OPush(dfoGt)
    else
    if Token = '<=' then
      OPush(dfoLe)
    else
    if Token = '<' then
      OPush(dfoLt)
    else
    if Token = '=' then
      OPush(dfoEq)
    else
    if Token = 'or' then
      OPush(dfoOr)
    else
    if Token = 'and' then
      OPush(dfoAnd)
    else
    if Token = 'in' then
      OPush(dfoIn)
    else
    if Token = 'xor' then
      OPush(dfoXor)
    else
    if Token = 'not' then
      OPush(dfoNot)
    else
    if Token = 'like' then
      OPush(dfoLike)
    else
    if Token = 'unlike' then
      OPush(dfoUnlike)
    // check for block
    else
    if Token[1] = '[' then
    begin
      AtomSymbol := Token;
      AtomValue := Copy(Token, 2, Length(Token) - 2);
      PushAtom(dfoSet);
    end
    // check for sub
    else
    if Token[Length(Token)] = '=' then
    begin
      AtomSymbol := Copy(Token, 1, Length(Token) - 1);
      P := PushAtom(dfoSub);
      FSubsList.AddObject(AtomSymbol, TObject(P + 1));
    end
    // check for xml object
    else
    if (Token[1] = '?') and (Length(Token) > 1) then
    begin
      P := Pos('.', Token);
      if (P = 0) or (P < 3) or (P = Length(Token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingXmlMethodSpecifierNears, [S]);
      AtomSymbol := Copy(Token, 2, P - 2);
      AtomValue := Copy(Token, P + 1, Length(Token));
      PushAtom(dfoXML);
    end
    // check for data source object
    else
    if (Token[1] = '_') and (Length(Token) > 1) then
    begin
      P := Pos('.', Token);
      if (P = 0) or (P < 3) or (P = Length(Token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingDataSourceMethodSpecifierNea, [S]);
      AtomSymbol := Copy(Token, 2, P - 2);
      AtomValue := Copy(Token, P + 1, Length(Token));
      PushAtom(dfoDSO);
    end
    // system
    else
    if (Token[1] = ')') and (Length(Token) > 1) then
    begin
      P := Pos('.', Token);
      if (P = 0) or (P < 3) or (P = Length(Token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingSystemMethodSpecifierNears, [S]);
      AtomSymbol := Copy(Token, 2, P - 2);
      AtomValue := Copy(Token, P + 1, Length(Token));
      PushAtom(dfoSystem);
    end
    // external variable
    else
    if (Token[1] = '>') and (Length(Token) > 1) then
    begin
      P := Pos('.', Token);
      if (P = 0) or (P < 3) or (P = Length(Token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingExternalVariableMethodSpecif, [S]);
      AtomSymbol := Copy(Token, 2, P - 2);
      AtomValue := Copy(Token, P + 1, Length(Token));
      PushAtom(dfoExtVar);
    end
    // check for internal variable
    else
    if (Token[1] = ':') and (Length(Token) > 1) then
    begin
      P := Pos('.', Token);
      if (P = 0) or (P < 3) or (P = Length(Token)) then
        raise EJvJanScriptError.CreateResFmt(@RsEMissingInternalVariableMethodSpecif, [S]);
      AtomSymbol := Copy(Token, 2, P - 2);
      AtomValue := Copy(Token, P + 1, Length(Token));
      PushAtom(dfoIntVar);
    end
    // check for string
    else
    if Token[1] = '"' then
    begin
      AtomSymbol := Token;
      AtomValue := Copy(Token, 2, Length(Token) - 2);
      PushAtom(dfoString);
    end
    // check Integer, float or date
    else
    begin
      try // Integer
        VInteger := StrToInt(Token);
        AtomSymbol := Token;
        AtomValue := VInteger;
        PushAtom(dfoInteger);
      except
        try // float
          VFloat := StrToFloat(Token);
          AtomSymbol := Token;
          AtomValue := VFloat;
          PushAtom(dfoFloat);
        except
          try // date
            VDate := StrToDate(Token);
            AtomSymbol := Token;
            AtomValue := VDate;
            PushAtom(dfoDate);
          except // must be call to sub
            AtomSymbol := Token;
            P := FSubsList.IndexOf(AtomSymbol);
            if P = -1 then
              raise EJvJanScriptError.CreateResFmt(@RsEUndefinedWordsNears, [AtomSymbol, S]);
            P := Integer(FsubsList.Objects[P]);
            AtomValue := P;
            PushAtom(dfoCall);
          end;
        end;
      end;
    end;
  end;
end;

procedure TJvForthScript.DoToken(AToken: TToken);
begin
  case AToken of
    dfoNow:
      ProcNow;
    dfoDateStr:
      ProcDateStr;
    dfoTimeStr:
      ProcTimeStr;
    dfoShellExecute:
      ProcShellExecute;
    dfoCrLf:
      ProcCrLf;
    dfoCStr:
      ProcCStr;
    dfoXML:
      ProcXML;
    dfoDSO:
      ProcDSO;
    dfoSeldir:
      ProcSelDir;
    dfoDSOBase:
      ProcDSOBase;
    dfoIntVar:
      ProcIntVar;
    dfoExtVar:
      ProcExtVar;
    dfoSystem:
      ProcSystem;
    //    dfoVarGet: ProcVarGet;
    //    dfoVarset: ProcVarSet;
    //    dfoSysGet: ProcSysGet;
    //    dfoSysSet: ProcSysSet;
    dfoSub:
      ProcSub;
    dfoEndSub:
      ProcEndSub;
    dfoCall:
      ProcCall;
    dfoDrop:
      ProcDrop;
    dfoDup:
      ProcDup;
    dfoSwap:
      ProcSwap;
    dfoIf:
      ProcIf;
    dfoElse:
      ProcElse;
    dfoEndIf:
      ProcEndIf;
    dfoRepeat:
      ProcRepeat;
    dfoUntil:
      ProcUntil;
    dfoNop:
      ProcNop;
    //    dfoAssign: ProcAssign;
    //    dfoVariable: ProcVariable;
    dfoInteger:
      ProcInteger;
    dfoFloat:
      ProcFloat;
    dfoSet:
      ProcSet;
    dfoString:
      ProcString;
    dfoBoolean:
      ProcBoolean;
    dfoDate:
      ProcDate;
    dfoEq:
      ProcEq;
    dfoNe:
      ProcNe;
    dfoGt:
      ProcGt;
    dfoLt:
      ProcLt;
    dfoGe:
      ProcGe;
    dfoLe:
      ProcLe;
    dfoLike:
      ProcLike;
    dfoUnlike:
      ProcUnlike;
    dfoNot:
      ProcNot;
    dfoAnd:
      ProcAnd;
    dfoXor:
      ProcXor;
    dfoOr:
      ProcOr;
    dfoIn:
      ProcIn;
    dfoAdd:
      ProcAdd;
    dfoSubtract:
      ProcSubtract;
    dfoMultiply:
      ProcMultiply;
    dfoDivide:
      ProcDivide;
    dfoPower:
      ProcPower;
    dfoAbs:
      ProcAbs;
    dfoPi:
      ProcPi;
    dfoSin:
      ProcSin;
    dfoCos:
      ProcCos;
    dfoTan:
      ProcTan;
    dfoArcSin:
      ProcArcSin;
    dfoArcCos:
      ProcArcCos;
    dfoArcTan:
      ProcArcTan;
    dfoArcTan2:
      ProcArcTan2;
    dfoNegate:
      ProcNegate;
    dfoSqr:
      ProcSqr;
    dfoSqrt:
      ProcSqrt;
    dfoLeft:
      ProcLeft;
    dfoRight:
      ProcRight;
  end;
end;

function TJvForthScript.Execute: Variant;
var
  C: Integer;
  Atom: TAtom;
  Token: TToken;
  TimeOutTicks: Cardinal;
  DeltaTicks: Cardinal;
begin
  Result := Null;
  // osp := 0;
  FVSP := 0;
  FPSP := 0;
  FRSP := 0;
  C := FAtoms.Count;
  FVarsList.ClearObjects;
  FDSOList.ClearTables;
  FXMLList.ClearXMLS;
  FXMLSelect.Clear;
  FXMLSelectRecord := -1;
  if C = 0 then
    Exit;
  FPC := 0;
  DeltaTicks := FScriptTimeOut * 1000;
  TimeOutticks := GetTickCount + DeltaTicks;
  // evaluate all FAtoms
  while FPC < C do
  begin
    if GetTickCount > TimeOutTicks then
      raise EJvJanScriptError.CreateResFmt(@RsEScriptTimedOutAfterdSeconds, [FScriptTimeout]);
    Atom := TAtom(FAtoms[FPC]);
    Inc(FPC);
    FCurrentValue := Atom.Value;
    FCurrentSymbol := Atom.Symbol;
    Token := Atom.Token;
    case Token of
      dfoInteger..dfoDate:
        VPush(FCurrentValue);
    else
      DoToken(Token);
    end;
  end;
  if FVSP <= 0 then
    Result := Null
  else
    Result := VPop;
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

procedure TJvForthScript.ProcAdd;
var
  Value: Variant;
begin
  Value := VPop;
  Value := VPop + Value;
  VPush(Value);
end;

procedure TJvForthScript.ProcAnd;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop and Value);
end;

procedure TJvForthScript.ProcAssign;
var
  Value: Variant;
  Handled: Boolean;
  Err: string;
begin
  Value := VPop;
  VPush(Value);
  Handled := False;
  Err := Format(RsECanNotAssignVariables, [FCurrentSymbol]);
  if Assigned(OnSetVariable) then
  begin
    OnSetVariable(Self, FCurrentSymbol, Value, Handled, Err);
    if not Handled then
      raise EJvJanScriptError.Create(Err);
  end;
end;

procedure TJvForthScript.ProcBoolean;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.DoProc;
var
  Token: TToken;
begin
  if FPSP <= 0 then
    Exit;
  Dec(FPSP);
  Token := FPStack[FPSP];
  DoToken(Token);
end;

procedure TJvForthScript.ProcCos;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Cos(Value));
end;

procedure TJvForthScript.ProcDate;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.ProcDivide;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop / Value);
end;

procedure TJvForthScript.ProcEq;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop = Value);
end;

procedure TJvForthScript.ProcFloat;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.ProcGe;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop >= Value);
end;

procedure TJvForthScript.ProcGt;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop > Value);
end;

procedure TJvForthScript.ProcIn;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(FuncIn(VPop, Value));
end;

procedure TJvForthScript.ProcInteger;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.ProcLe;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop <= Value);
end;

procedure TJvForthScript.ProcLeft;
var
  Value, V2: Variant;
  Vali: Integer;
  Vals: string;
begin
  Value := VPop;
  V2 := VPop;
  Vali := Value;
  Vals := V2;
  Value := Copy(Vals, 1, Vali);
  VPush(Value);
end;

procedure TJvForthScript.ProcLike;
var
  Value: Variant;
begin
  Value := VarToStr(VPop);
  VPush(Pos(LowerCase(Value), LowerCase(VarToStr(VPop))) > 0);
end;

procedure TJvForthScript.ProcLt;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop < Value);
end;

procedure TJvForthScript.ProcMultiply;
var
  Value: Variant;
begin
  Value := VPop;
  Value := VPop * Value;
  VPush(Value);
end;

procedure TJvForthScript.ProcNe;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop <> Value);
end;

procedure TJvForthScript.ProcNegate;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(0 - Value);
end;

procedure TJvForthScript.ProcNop;
begin
  //  just do nothing
end;

procedure TJvForthScript.ProcNot;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(not Value);
end;

procedure TJvForthScript.ProcOr;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop or Value);
end;

procedure TJvForthScript.ProcRight;
var
  Value, V2: Variant;
  Vali: Integer;
  Vals: string;
begin
  Value := VPop;
  V2 := VPop;
  Vali := Value;
  Vals := V2;
  if Vali <= Length(Vals) then
    Value := Copy(Vals, Length(Vals) - Vali + 1, Vali)
  else
    Value := Vals;
  VPush(Value);
end;

procedure TJvForthScript.ProcSet;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.ProcSin;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Sin(Value));
end;

procedure TJvForthScript.ProcSqr;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Sqr(Value));
end;

procedure TJvForthScript.ProcSqrt;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Sqrt(Value));
end;

procedure TJvForthScript.ProcString;
begin
  VPush(FCurrentValue);
  DoProc;
end;

procedure TJvForthScript.ProcSubtract;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop - Value);
end;

procedure TJvForthScript.ProcUnlike;
var
  Value: Variant;
begin
  Value := VarToStr(VPop);
  VPush(Pos(LowerCase(Value), LowerCase(VarToStr(VPop))) = 0);
end;

procedure TJvForthScript.ProcVariable;
var
  Value: Variant;
  Handled: Boolean;
  Err: string;
begin
  Handled := False;
  Err := Format(RsEVariablesNotDefined, [FCurrentSymbol]);
  if Assigned(FOnGetVariable) then
    FOnGetVariable(Self, FCurrentSymbol, Value, Handled, Err);
  if not Handled then
    raise EJvJanScriptError.Create(Err)
  else
    VPush(Value);
end;

procedure TJvForthScript.ProcXor;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(VPop xor Value);
end;

procedure TJvForthScript.ProcIf;
var
  V: Variant;
begin
  V := VPop;
  if V then
    Exit
  else
    FPC := FCurrentValue;
end;

procedure TJvForthScript.ProcElse;
begin
  FPC := FCurrentValue;
end;

procedure TJvForthScript.ProcDrop;
begin
  VPop;
end;

procedure TJvForthScript.ProcDup;
var
  V: Variant;
begin
  V := VPop;
  VPush(V);
  VPush(V);
end;

procedure TJvForthScript.ProcSwap;
var
  V1, V2: Variant;
begin
  V1 := VPop;
  V2 := VPop;
  VPush(V1);
  VPush(V2);
end;

// just a marker

procedure TJvForthScript.ProcEndif;
begin
  // do nothing
end;

// keep looping until vpop=True

procedure TJvForthScript.ProcUntil;
begin
  if not VPop then
    FPC := FCurrentValue;
end;

procedure TJvForthScript.ProcRepeat;
begin
  // do nothing
end;

function TJvForthScript.RPop: Integer;
begin
  if FRSP <= 0 then
    raise EJvJanScriptError.CreateRes(@RsEReturnStackUnderflow)
  else
  begin
    Dec(FRSP);
    Result := FRStack[FRSP];
  end;
end;

procedure TJvForthScript.RPush(AValue: Integer);
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

procedure TJvForthScript.ProcEndsub;
begin
  FPC := RPop;
end;

// just skip till endSub

procedure TJvForthScript.ProcSub;
var
  C: Integer;
  Token: TToken;
begin
  { TODO -oJVCL -cPOSSIBLEBUG : (p3) What should "c" really be here? }
  C := FAtoms.Count; //??
  while FPC < C do
  begin
    Token := TAtom(FAtoms[FPC]).Token;
    if Token = dfoEndSub then
    begin
      Inc(FPC);
      Exit;
    end;
    Inc(FPC);
  end;
end;

// call to a user sub, just look it up

procedure TJvForthScript.ProcCall;
var
  Index: Integer;
begin
  //  Index:=FSubsList.IndexOf(FCurrentSymbol);
  Index := FCurrentValue;
  if Index <> -1 then
  begin
    RPush(FPC);
    //    FPC:=Integer(FsubsList.Objects[Index]);
    FPC := Index;
    Exit;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEProceduresNotDefined, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarGet;
var
  V: Variant;
begin
  V := FVarsList.GetVariable(FCurrentSymbol);
  if V <> null then
    VPush(V)
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSet;
var
  V: Variant;
begin
  V := VPop;
  FVarsList.SetVariable(FCurrentSymbol, V);
end;

procedure TJvForthScript.ProcCStr;
var
  S: string;
begin
  S := VPop;
  VPush(S);
end;

procedure TJvForthScript.ProcSysGet;
var
  Value: Variant;
  Handled: Boolean;
  Err, Prompt: string;
begin
  Prompt := VPop;
  Handled := False;
  Err := Format(RsESystemsNotDefined, [FCurrentSymbol]);
  if Assigned(OnGetSystem) then
    OnGetSystem(Self, FCurrentSymbol, Prompt, Value, Handled, Err);
  if not Handled then
    raise EJvJanScriptError.Create(Err)
  else
    VPush(Value);
end;

procedure TJvForthScript.ProcSysSet;
var
  Value: Variant;
  Handled: Boolean;
  Err: string;
begin
  Value := VPop;
  VPush(Value);
  Handled := False;
  Err := Format(RsECanNotAssignSystems, [FCurrentSymbol]);
  if Assigned(FOnSetSystem) then
  begin
    FOnSetSystem(Self, FCurrentSymbol, Value, Handled, Err);
    if not Handled then
      raise EJvJanScriptError.Create(Err);
  end;
end;

procedure TJvForthScript.SetOnGetSystem(const Value: TOnGetSystem);
begin
  FOnGetSystem := Value;
end;

procedure TJvForthScript.SetOnSetSystem(const Value: TOnSetSystem);
begin
  FOnSetSystem := Value;
end;

function TJvForthScript.PopValue: Variant;
begin
  Result := VPop;
end;

procedure TJvForthScript.PushValue(AValue: Variant);
begin
  VPush(AValue);
end;

function TJvForthScript.CanPopValue: Boolean;
begin
  Result := FVSP > 0;
end;

function TJvForthScript.CanPushValue: Boolean;
begin
  Result := FVSP < StackMax;
end;

procedure TJvForthScript.ProcPi;
begin
  VPush(Pi);
end;

procedure TJvForthScript.ProcDSO;
var
  AName, AMethod: string;
  Table: TJvJanDSO;
  AField, AValue: string;
  AKey: Variant;
  C: Integer;
begin
  AName := FCurrentSymbol;
  AMethod := FCurrentValue;
  Table := FDSOList.Table(AName);
  if AMethod = 'set' then
  begin
    AKey := VPop;
    AField := VPop;
    AValue := VPop;
    Table.SetValue(AKey, AField, AValue);
  end
  else
  if AMethod = 'get' then
  begin
    AKey := VPop;
    AField := VPop;
    AValue := Table.GetValue(AKey, AField);
    VPush(AValue);
  end
  else
  if AMethod = 'load' then
    Table.LoadFromFile(FDSOBase + PathDelim + AName + '.txt')
  else
  if AMethod = 'save' then
    Table.SaveToFile(FDSOBase + PathDelim + AName + '.txt')
  else
  if AMethod = 'Clear' then
    Table.Clear
  else
  if AMethod = 'Count' then
  begin
    C := Table.Count;
    VPush(C);
  end;
end;

procedure TJvForthScript.ProcDSOBase;
var
  S: string;
begin
  S := VPop;
  FDSOBase := S;
end;

procedure TJvForthScript.ProcSelDir;

var
  Dir: string;
begin
  Dir := FDSOBase;
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    FDSOBase := Dir;
end;



procedure TJvForthScript.ProcExtVar;
var
  AName, AMethod: string;
begin
  AName := FCurrentSymbol;
  AMethod := FCurrentValue;
  if AMethod = 'set' then
    ProcAssign
  else
  if AMethod = 'get' then
    ProcVariable
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizedExternalVariableMethodss, [AName, AMethod]);
end;

procedure TJvForthScript.ProcIntVar;
var
  AName, AMethod: string;
begin
  AName := FCurrentSymbol;
  AMethod := FCurrentValue;
  if AMethod = 'set' then
    ProcVarSet
  else
  if AMethod = 'get' then
    ProcVarGet
  else
  if AMethod = '1+' then
    ProcVarInc
  else
  if AMethod = '[1+]' then
    ProcVarIncIndex
  else
  if AMethod = '1-' then
    ProcVarDec
  else
  if AMethod = '1-?0' then
    ProcVarDecTestZero
  else
  if AMethod = '+' then
    ProcVarAdd
  else
  if AMethod = '-' then
    ProcVarSub
  else
  if AMethod = '*' then
    ProcVarMul
  else
  if AMethod = '/' then
    ProcVarDiv
  else
  if AMethod = '--' then
    ProcVarNeg
  else
  if AMethod = 'load' then
    ProcVarLoad
  else
  if AMethod = 'save' then
    ProcVarSave
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizedInternalVariableMethodss, [AName, AMethod]);
end;

procedure TJvForthScript.ProcSystem;
var
  AName, AMethod: string;
begin
  AName := FCurrentSymbol;
  AMethod := FCurrentValue;
  if AMethod = 'set' then
    ProcSysSet
  else
  if AMethod = 'get' then
    ProcSysGet
  else
    raise EJvJanScriptError.CreateResFmt(@RsEUnrecognizedSystemMethodss, [AName, AMethod]);
end;

procedure TJvForthScript.ProcVarDec;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value - 1
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarInc;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value + 1
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarAdd;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value + VPop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarDiv;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value / VPop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarMul;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value * VPop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSub;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := VO.Value - VPop
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarNeg;
var
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
    VO.Value := 0 - VO.Value
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcPower;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Power(VPop, Value));
end;

procedure TJvForthScript.ProcAbs;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Abs(Value));
end;

procedure TJvForthScript.SetOnInclude(const Value: TOnInclude);
begin
  FOnInclude := Value;
end;

procedure TJvForthScript.ProcTan;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(Tan(Value));
end;

procedure TJvForthScript.ProcArcCos;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(ArcCos(Value));
end;

procedure TJvForthScript.ProcArcSin;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(ArcSin(Value));
end;

procedure TJvForthScript.ProcArcTan;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(ArcTan(Value));
end;

procedure TJvForthScript.ProcArcTan2;
var
  Value: Variant;
begin
  Value := VPop;
  VPush(ArcTan2(VPop, Value));
end;

procedure TJvForthScript.ProcVarLoad;
var
  VO: TVariantObject;
  AP, FN, S: string;
begin
  FN := VPop;
  AP := ExtractFilePath(ParamStr(0));
  FN := StringReplace(FN, '%', AP, []);
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
  begin
    S := LoadString(FN);
    VO.Value := S;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarSave;
var
  VO: TVariantObject;
  AP, FN, S: string;
begin
  FN := VPop;
  AP := ExtractFilePath(ParamStr(0));
  FN := StringReplace(FN, '%', AP, []);
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
  begin
    S := VO.Value;
    SaveString(FN, S);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcXML;
var
  AName, AMethod: string;
  XmlDSO: TJvXMLTree;
  N: TJvXMLNode;
  A: TJvXMLAttribute;
  APath, AtName: string;
  AValue: Variant;
  C, I, C2: Integer;
  ApplDir: string;
  B: Boolean;
begin
  N := nil;
  ApplDir := ExtractFilePath(ParamStr(0));
  AName := FCurrentSymbol;
  AMethod := FCurrentValue;
  XmlDSO := FXMLList.Xml(AName);
  if AMethod = 'set' then
  begin
    APath := VPop;
    AValue := VPop;
    N := XmlDSO.ForceNamePathNode(APath);
    N.Value := AValue;
  end
  else
  if AMethod = '@set' then
  begin
    APath := VPop;
    AtName := VPop;
    AValue := VPop;
    XmlDSO.ForceNamePathNodeAttribute(APath, AtName, AValue);
  end
  else
  if AMethod = 'get' then
  begin
    APath := VPop;
    N := XmlDSO.GetNamePathNode(APath);
    if N = nil then
      AValue := ''
    else
      AValue := N.Value;
    VPush(AValue);
  end
  else
  if AMethod = 'Count' then
  begin
    APath := VPop;
    N := XmlDSO.GetNamePathNode(APath);
    AValue := 0;
    C2 := 0;
    if N <> nil then
    begin
      // now Count named node
      C := N.Nodes.Count;
      APath := VPop;
      if C > 0 then
      begin
        for I := 0 to C - 1 do
          if TJvXMLNode(N.Nodes[I]).Name = APath then
            Inc(C2);
      end;
      AValue := C2;
    end;
    VPush(AValue);
  end
  else
  if AMethod = '@get' then
  begin
    APath := VPop;
    AtName := VPop;
    A := XmlDSO.GetNamePathNodeAttribute(APath, AtName);
    if N = nil then
      AValue := ''
    else
      AValue := A.Value;
    VPush(AValue);
  end
  else
  if AMethod = 'load' then
  begin
    APath := VPop;
    APath := StringReplace(APath, '%', ApplDir, []);
    if not FileExists(APath) then
      raise EJvJanScriptError.CreateResFmt(@RsEFilesDoesNotExist, [APath]);
    XmlDSO.LoadFromFile(APath);
  end
  else
  if AMethod = 'save' then
  begin
    APath := VPop;
    APath := StringReplace(APath, '%', ApplDir, []);
    try
      XmlDSO.SaveToFile(APath);
    except
      raise EJvJanScriptError.CreateResFmt(@RsECanNotSaveToFiles, [APath]);
    end
  end
  else
  if AMethod = 'astext' then
  begin
    AValue := XmlDSO.AsText;
    VPush(AValue);
  end
  else
  if AMethod = 'Delete' then
  begin
    APath := VPop;
    XmlDSO.deleteNamePathNode(APath);
  end
  else
  if AMethod = '@Delete' then
  begin
    APath := VPop;
    AtName := VPop;
    XmlDSO.DeleteNamePathNodeAttribute(APath, AtName);
  end
  else
  if AMethod = 'select' then
  begin
    APath := VPop;
    APath := StringReplace(APath, '''', '"', [rfReplaceAll]);
    FXMLSelect.Clear;
    FXMLSelectRecord := -1;
    XmlDSO.SelectNodes(APath, FXMLSelect);
    VPush(FXMLSelect.Count > 0);
  end
  else
  if AMethod = 'selectfirst' then
  begin
    B := FXMLSelect.Count <> 0;
    if B then
      FXMLSelectRecord := 0
    else
      FXMLSelectRecord := -1;
    AValue := B;
    VPush(AValue);
  end
  else
  if AMethod = 'selectnext' then
  begin
    B := FXMLSelect.Count <> 0;
    if B then
      Inc(FXMLSelectRecord)
    else
      FXMLSelectRecord := -1;
    if FXMLSelectRecord >= FXMLSelect.Count then
    begin
      B := False;
      FXMLSelectRecord := -1;
    end;
    AValue := B;
    VPush(AValue);
  end
  else
  if AMethod = 'selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionIsEmpty);
    if FXMLSelectRecord = -1 then
      raise EJvJanScriptError.CreateRes(@RsENoXMLSelectionSelected);
    if FXMLSelectRecord >= FXMLSelect.Count then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionOutOfRange);
    N := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    AValue := N.Value;
    VPush(AValue);
  end
  else
  if AMethod = '@selectget' then
  begin
    if FXMLSelect.Count = 0 then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionIsEmpty);
    if FXMLSelectRecord = -1 then
      raise EJvJanScriptError.CreateRes(@RsENoXMLSelectionSelected);
    if FXMLSelectRecord >= FXMLSelect.Count then
      raise EJvJanScriptError.CreateRes(@RsEXMLSelectionOutOfRange);
    N := TJvXMLNode(FXMLSelect[FXMLSelectRecord]);
    AtName := VPop;
    AValue := N.GetAttributeValue(AtName);
    VPush(AValue);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEInvalidXmlMethodSpecifiers, [AMethod]);
end;

procedure TJvForthScript.ProcVarDecTestZero;
var
  V: Variant;
  VO: TVariantObject;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
  begin
    V := VO.Value - 1;
    VO.Value := V;
    VPush(V = 0);
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcVarIncIndex;
var
  VO: TVariantObject;
  S, SIdx: string;
  PB, PE: Integer;
  Index: Integer;
begin
  VO := FVarsList.GetObject(FCurrentSymbol);
  if VO <> nil then
  begin
    S := VO.Value;
    PB := LastPosChar('[', S);
    if PB = 0 then
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIns, [S]);
    PE := LastPosChar(']', S);
    if PE = 0 then
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIns_, [S]);
    SIdx := Copy(S, PB + 1, PE - PB - 1);
    try
      Index := StrToInt(SIdx);
      Inc(Index);
      S := Copy(S, 1, PB - 1) + '[' + IntToStr(Index) + ']';
      VO.Value := S;
      VPush(S);
    except
      raise EJvJanScriptError.CreateResFmt(@RsEIncrementIndexExpectedIntegerBetwee, [S]);
    end;
  end
  else
    raise EJvJanScriptError.CreateResFmt(@RsEVariablesNotDefined_, [FCurrentSymbol]);
end;

procedure TJvForthScript.ProcCrLf;
begin
  VPush(sLineBreak);
end;

procedure TJvForthScript.ProcShellExecute;
var
  AFile: string;
  ApplDir: string;
begin
  ApplDir := ExtractFilePath(ParamStr(0));
  AFile := VPop;
  AFile := StringReplace(AFile, '%', ApplDir, []);
  Launch(AFile);
end;

procedure TJvForthScript.ProcDateStr;
var
  S: string;
begin
  S := FormatDateTime('dd-mmm-yyyy', Now);
  VPush(S);
end;

procedure TJvForthScript.ProcTimeStr;
var
  S: string;
begin
  S := FormatDateTime('hh:nn:ss', Now);
  VPush(S);
end;

procedure TJvForthScript.ProcNow;
begin
  VPush(Now);
end;

//=== { TAtom } ==============================================================

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

//=== { TAtomList } ==========================================================

procedure TAtomList.ClearObjects;
var
  I, C: Integer;
begin
  C := Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
    TAtom(Items[I]).Free;
  Clear;
end;

destructor TAtomList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

//=== { TVariantObject } =====================================================

procedure TVariantObject.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

//=== { TVariantList } =======================================================

procedure TVariantList.ClearObjects;
var
  I, C: Integer;
begin
  C := Count;
  if C = 0 then
    Exit;
  for I := 0 to C - 1 do
    TVariantObject(Objects[I]).Free;
  Clear;
end;

destructor TVariantList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

function TVariantList.GetObject(const Symbol: string): TVariantObject;
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

function TVariantList.GetVariable(const Symbol: string): Variant;
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

procedure TVariantList.SetVariable(const Symbol: string; AValue: Variant);
var
  Index: Integer;
  Obj: TVariantObject;
begin
  Index := IndexOf(Symbol);
  if Index = -1 then
  begin
    Obj := TVariantObject.Create;
    Obj.Value := AValue;
    AddObject(Symbol, Obj);
  end
  else
  begin
    TVariantObject(Objects[Index]).Value := AValue;
  end;
end;

//=== { TJvJanDSOList } ======================================================

procedure TJvJanDSOList.ClearTables;
var
  I, C: Integer;
begin
  C := Count;
  if C <> 0 then
    for I := 0 to C - 1 do
      TJvJanDSO(Objects[I]).Free;
  Clear;
end;

destructor TJvJanDSOList.Destroy;
begin
  ClearTables;
  inherited Destroy;
end;

function TJvJanDSOList.Table(const AName: string): TJvJanDSO;
var
  Index: Integer;
  DSO: TJvJanDSO;
begin
  Index := IndexOf(AName);
  if Index = -1 then
  begin
    DSO := TJvJanDSO.Create;
    AddObject(AName, DSO);
    Result := DSO;
  end
  else
    Result := TJvJanDSO(Objects[Index]);
end;

//=== { TJvJanDSO } ==========================================================

function TJvJanDSO.GetValue(AKey: Variant; const AField: string): string;
var
  Index: Integer;
  Key: string;
  StrKey: Boolean;
begin
  Key := AKey;
  StrKey := False;
  Index := 0;
  try
    Index := StrToInt(Key)
  except
    StrKey := True;
  end;
  if not StrKey then
  begin
    if Index >= Count then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOIndexOutOfRanged, [Index])
    else
      Result := InternalGetValue(Index, AField);
  end
  else
  begin
    Index := IndexOfName(Key);
    if Index = -1 then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOUnknownKeys, [Key]);
    Result := InternalGetValue(Index, AField);
  end
end;

function TJvJanDSO.InternalGetValue(Index: Integer; const AField: string): string;
var
  Key, S: string;
  P: Integer;
begin
  S := Strings[Index];
  P := Pos('=', S);
  Key := Copy(S, 1, P - 1);
  S := Copy(S, P + 1, Length(S));
  Result := GlobalGetValue(S, AField);
end;

procedure TJvJanDSO.InternalSetValue(Index: Integer; const AField, AValue: string);
var
  Key, S: string;
  P: Integer;
begin
  S := Strings[Index];
  P := Pos('=', S);
  Key := Copy(S, 1, P - 1);
  S := Copy(S, P + 1, Length(S));
  GlobalSetValue(S, AField, AValue);
  Strings[Index] := Key + '=' + S;
end;

procedure TJvJanDSO.SetValue(AKey: Variant; const AField, AValue: string);
var
  Index: Integer;
  Key: string;
  StrKey: Boolean;
begin
  Key := AKey;
  StrKey := False;
  Index := 0;
  try
    Index := StrToInt(Key)
  except
    StrKey := True;
  end;
  if not StrKey then
  begin
    if Index >= Count then
      raise EJvJanScriptError.CreateResFmt(@RsEDSOIndexOutOfRanged, [Index])
    else
      InternalSetValue(Index, AField, AValue);
  end
  else
  begin
    Index := IndexOfName(Key);
    if Index = -1 then
      Index := Add(Key + '=');
    InternalSetValue(Index, AField, AValue);
  end
end;

//=== { TJvJanXMLList } ======================================================

procedure TJvJanXMLList.ClearXMLS;
var
  I, C: Integer;
begin
  C := Count;
  if C <> 0 then
    for I := 0 to C - 1 do
      TJvXMLTree(Objects[I]).Free;
  Clear;
end;

destructor TJvJanXMLList.Destroy;
begin
  ClearXMLS;
  inherited Destroy;
end;

function TJvJanXMLList.Xml(const AName: string): TJvXMLTree;
var
  Index: Integer;
  XmlDSO: TJvXMLTree;
begin
  Index := IndexOf(AName);
  if Index = -1 then
  begin
    XmlDSO := TJvXMLTree.Create(AName, '', nil);
    AddObject(AName, XmlDSO);
    Result := XmlDSO;
  end
  else
    Result := TJvXMLTree(Objects[Index]);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
