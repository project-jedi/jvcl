{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSAL.PAS, released on 2002-06-15.

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

unit JvSAL;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, 
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Messages, Graphics, Controls, Forms, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QWindows,
  {$ENDIF VisualCLX}
  JvSALHashList, JvStrings, JvComponent;

const
  StackLimit = 256;
  // message are processed every 250 milliseconds
  // use the stop procedure to stop a locked script
  TimeOut = 250;

type
  TOnGetUnitEvent = procedure(Sender: TObject; AUnit: string;
    var AValue: string; var Handled: Boolean) of object;

  TJvAtom = class(TObject)
  private
    FValue: Variant;
    FActor: TJvSALProc;
    procedure SetActor(const Value: TJvSALProc);
    procedure SetValue(const AValue: Variant);
  public
    property Value: Variant read FValue write SetValue;
    property Actor: TJvSALProc read FActor write SetActor;
  end;

  TJvSALProcAtom = class(TObject)
  private
    FParser: TJvSALProc;
    FActor: TJvSALProc;
    procedure SetActor(const Value: TJvSALProc);
    procedure SetParser(const Value: TJvSALProc);
  public
    property Actor: TJvSALProc read FActor write SetActor;
    property Parser: TJvSALProc read FParser write SetParser;
  end;

  TJvAtoms = class(TStringList)
  public
    procedure ClearAll;
    destructor Destroy; override;
  end;

  TJvSAL = class(TJvComponent)
  private
    FStop: Boolean;
    FCaption: string;
    FSP: Integer;
    FRSP: Integer;
    FBSP: Integer;
    FStack: array [0..StackLimit] of Variant;
    FBStack: array [0..StackLimit] of Boolean;
    FRStack: array [0..StackLimit] of Integer;
    FProcs: TJvSALHashList;
    FScript: string;
    FUnits: TStringList;
    FTicks: cardinal;
    FOnGetUnit: TOnGetUnitEvent;
    FVariableName: string;
    FVariable: TJvAtom;
    FSelection: Variant;
    FUseDirective: string;
    FBeginOfComment: string;
    FEndOfComment: string;
    FStringDelimiter: string;
    FPC: Integer;
    FAtoms: TJvAtoms;
    FPCProc: Integer;
    FToken: string;
    procedure SetScript(const Value: string);
    procedure SetGetUnit(const Value: TOnGetUnitEvent);
    procedure SetVariable(const Value: TJvAtom);
    procedure SetVariableName(const Value: string);
    procedure SetSelection(const Value: Variant);
    procedure SetUseDirective(const Value: string);
    procedure SetBeginOfComment(const Value: string);
    procedure SetEndOfComment(const Value: string);
    procedure SetStringDelimiter(const Value: string);
    procedure SetPC(const Value: Integer);
    procedure SetToken(const Value: string);
    procedure SetCaption(const Value: string);
  protected
    procedure ParseScript;
    // return FStack methods
    // SAL language
    procedure xBoSub;
    procedure xEoSub;
    procedure xValue;
    procedure xDefVariable;
    procedure xVariable;
    procedure xProc;
    procedure xNoParser;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure ClearProcedures;
    procedure AddProcedure(AName: string; AProcedure, AParser: TJvSALProc);
    function APO(Op: string; AProc: TJvSALProc): Integer;
    procedure Push(AValue: Variant);
    function Pop: Variant;
    procedure RPush(AValue: Integer);
    function RPop: Integer;
    procedure BoolPush(AValue: Boolean);
    function BoolPop: Boolean;
    procedure LoadFromFile(FileName: string);
    procedure Execute;
    procedure Stop;
    property PC: Integer read FPC write Setpc;
    property Atoms: TJvAtoms read FAtoms;
    property PCProc: Integer read FPCProc;
    property Token: string read FToken write SetToken;
    property Script: string read FScript write SetScript;
    property Caption: string read FCaption write SetCaption;
    property Variable: TJvAtom read FVariable write SetVariable;
    property VariableName: string read FVariableName write SetVariableName;
    property TheSelect: Variant read FSelection write SetSelection;
    property UseDirective: string read FUseDirective write SetUseDirective;
    property BeginOfComment: string read FBeginOfComment write SetBeginOfComment;
    property EndOfComment: string read FEndOfComment write SetEndOfComment;
    property StringDelim: string read FStringDelimiter write SetStringDelimiter;
  published
    property OnGetUnit: TOnGetUnitEvent read FOnGetUnit write SetGetUnit;
  end;

implementation

uses
  JvConsts, JvResources, JvTypes;

const
  // do not localize these strings
  cSAL = 'SAL';
  cUse = 'use::';
  cLiteral = 'literal';
  cProc = 'proc-';
  cEndProc = 'end-proc';
  cVar = 'var-';

//=== { TJvSAL } =============================================================

constructor TJvSAL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAtoms := TJvAtoms.Create;
  FProcs := TJvSALHashList.Create(ITinyHash, HashSecondaryOne, SameText);
  FUnits := TStringList.Create;
  FCaption := cSAL;
  FUseDirective := cUse;
  FBeginOfComment := '{';
  FEndOfComment := '}';
  FStringDelimiter := '"';
end;

destructor TJvSAL.Destroy;
begin
  FAtoms.Free;
  FProcs.Free;
  FUnits.Free;
  inherited Destroy;
end;

function TJvSAL.BoolPop: Boolean;
begin
  Dec(FBSP);
  if FBSP < 0 then
    raise EJVCLException.CreateRes(@RsEBooleanStackUnderflow);
  Result := FBStack[FBSP];
end;

procedure TJvSAL.BoolPush(AValue: Boolean);
begin
  FBStack[FBSP] := AValue;
  Inc(FBSP);
  if FBSP > StackLimit then
    raise EJVCLException.CreateRes(@RsEBooleanStackOverflow);
end;

procedure TJvSAL.Execute;
var
  A: TJvAtom;
  C: Integer;
begin
  PC := 0;
  FSP := 0;
  FRSP := 0;
  FBSP := 0;
  C := Atoms.Count;
  FStop := False;
  FTicks := GetTickCount;
  if C = 0 then
    Exit;
  repeat
    A := TJvAtom(Atoms.Objects[PC]);
    FPCProc := PC;
    Inc(FPC);
    A.Actor;
    if (GetTickCount - FTicks) > TimeOut then
    begin
      FTicks := GetTickCount;
      Application.ProcessMessages;
    end;
    if FStop then
      raise EJVCLException.CreateRes(@RsEProgramStopped);
  until PC >= C;
end;

procedure TJvSAL.ParseScript;
var
  S: string;
  //  iprocs: Integer;
  haveproc: Boolean;
  AActor: TJvSALProc;
  AParser: TJvSALProc;
  I, P, P2: Integer;
  fv: Double;
  A: TJvAtom;
  fn, TheUnit: string;
  Handled: Boolean;

  function CharFrom(From: Integer; AChar: Char; AText: string): Integer;
  var
    C: Integer;
  begin
    Result := 0;
    C := Length(AText);
    repeat
      if AText[From] = AChar then
      begin
        Result := From;
        Exit;
      end;
      Inc(From);
    until From > C;
  end;

begin
  PC := 1;
  S := FScript;
  FUnits.Clear;
  // process any includes
  repeat
    P := Pos(FUseDirective, S); // default use::
    if P > 0 then
    begin
      P2 := CharFrom(P, ' ', S);
      if P2 = 0 then
        raise EJVCLException.CreateResFmt(@RsEUnterminatedIncludeDirectiveNears, [Copy(S, P, 50)]);
      fn := Trim(Copy(S, P + Length(FUseDirective), P2 - P - Length(FUseDirective)));
      if not Assigned(FOnGetUnit) then
        raise EJVCLException.CreateRes(@RsEOngetUnitEventHandlerIsNotAssigned);
      Handled := False;
      fn := LowerCase(fn);
      if FUnits.IndexOf(fn) = -1 then
      begin
        OnGetUnit(Self, fn, TheUnit, Handled);
        if not Handled then
          raise EJVCLException.CreateResFmt(@RsECouldNotIncludeUnits, [fn]);
        TheUnit := StringReplace(TheUnit, Cr, ' ', [rfReplaceAll]);
        Delete(S, P, P2 - P);
        Insert(TheUnit, S, P);
        FUnits.Append(fn);
      end;
    end;
  until P = 0;

  while S <> '' do
  begin
    if Pos(FBeginOfComment, S) = 1 then
    begin // default= {
      P := Pos(FEndOfComment, S); // default= }
      if P = 0 then
        raise EJVCLException.CreateResFmt(@RsEUnterminatedCommentNears, [S]);
      Delete(S, 1, P + Length(FEndOfComment) - 1);
      S := Trim(S);
    end
    else
    if Pos(FStringDelimiter, S) = 1 then
    begin // default = "
      Delete(S, 1, Length(FStringDelimiter));
      P := Pos(FStringDelimiter, S);
      if P = 0 then
        raise EJVCLException.CreateResFmt(@RsEUnterminatedStringNears, [S]);
      Token := Copy(S, 1, P - 1);
      Delete(S, 1, P + Length(FStringDelimiter) - 1);
      S := Trim(S);
      A := TJvAtom.Create;
      A.Value := Token;
      A.Actor := xValue;
      Atoms.AddObject(cLiteral, A);
    end
    else
    begin
      P := Pos(' ', S);
      if P = 0 then
      begin
        Token := S;
        S := '';
      end
      else
      begin
        Token := Copy(S, 1, P - 1);
        Delete(S, 1, P);
        S := Trim(S);
      end;
      // take care of aliases
      if Token = '.' then
        Token := '+=';
      // check for user procs
      haveproc := FProcs.Hash(Token, AActor, AParser);
      try // float
        fv := StrToFloat(Token);
        A := TJvAtom.Create;
        A.Value := fv;
        A.Actor := xValue;
        Atoms.AddObject(cLiteral, A);
      except
        if Pos(cProc, Token) = 1 then
        begin // begin of procedure
          if Pos(cEndProc, S) = 0 then
            raise EJVCLException.CreateResFmt(@RsEUnterminatedProcedureNears, [S]);
          APO(Token, xBoSub);
        end
        else
        if Token = cEndProc then
          APO(Token, xEoSub)
        else
        if Copy(Token, Length(Token) - 1, 2) = '()' then
          APO(Token, xProc) // proc call
        else
        if Pos(cVar, Token) = 1 then
        begin // define variable
          if Atoms.IndexOf(Token) <> -1 then
            raise EJVCLException.CreateResFmt(@RsEVariablesAllreadyDefineds, [Token, S]);
          A := TJvAtom.Create;
          A.Actor := xDefVariable;
          Atoms.AddObject(Token, A);
        end
        else
        if Token[1] = '$' then
        begin // variable value
          // find address
          I := Atoms.IndexOf(cVar + Copy(Token, 2, MaxInt));
          if I = -1 then
            raise EJVCLException.CreateResFmt(@RsEVariablesIsNotYetDefineds, [Token, S]);
          A := TJvAtom.Create;
          A.Value := I;
          A.Actor := xVariable;
          Atoms.AddObject(Token, A);
        end
        else
        if haveproc then
        begin
          if Assigned(AParser) then
            AParser
          else
            APO(Token, AActor);
        end
        else
          raise EJVCLException.CreateResFmt(@RsEProceduresNears, [Token, S]);
      end
    end
  end;

  // now resolve procs()
  if Atoms.Count = 0 then
    Exit;
  for I := 0 to Atoms.Count - 1 do
  begin
    S := Atoms[I];
    if Copy(S, Length(S) - 1, 2) = '()' then
    begin
      S := cProc + Copy(S, 1, Length(S) - 2);
      P := Atoms.IndexOf(S);
      if P = -1 then
        raise EJVCLException.CreateResFmt(@RsEUndefinedProcedures, [S]);
      TJvAtom(Atoms.Objects[I]).Value := P;
    end;
  end;
end;

function TJvSAL.Pop: Variant;
begin
  Dec(FSP);
  if FSP < 0 then
    raise EJVCLException.CreateRes(@RsEStackUnderflow);
  Result := FStack[FSP];
end;

procedure TJvSAL.Push(AValue: Variant);
begin
  FStack[FSP] := AValue;
  Inc(FSP);
  if FSP > StackLimit then
    raise EJVCLException.CreateRes(@RsEStackOverflow);
end;

procedure TJvSAL.SetScript(const Value: string);
begin
  FScript := Trim(StringReplace(Value, Cr, ' ', [rfReplaceAll]));
  Atoms.ClearAll;
  ParseScript;
end;

procedure TJvSAL.xDefVariable;
var
  A: TJvAtom;
begin
  A := TJvAtom(Atoms.Objects[PCProc]);
  FVariableName := Atoms[PCProc];
  FVariableName := '$' + Copy(FVariableName, 5, MaxInt);
  FVariable := A;
end;

procedure TJvSAL.xValue;
begin
  Push(TJvAtom(Atoms.Objects[PCProc]).Value);
end;

procedure TJvSAL.xVariable;
var
  Index: Integer;
  A: TJvAtom;
begin
  A := TJvAtom(Atoms.Objects[PCProc]);
  VariableName := Atoms[PCProc];
  Index := A.Value;
  Variable := TJvAtom(Atoms.Objects[Index]);
end;

procedure TJvSAL.Stop;
begin
  FStop := True;
end;

procedure TJvSAL.LoadFromFile(FileName: string);
begin
  Script := Loadstring(FileName);
end;

procedure TJvSAL.ClearProcedures;
begin
  //  FProcs.ClearAll;
  FProcs.Clear;
end;

procedure TJvSAL.AddProcedure(AName: string; AProcedure, AParser: TJvSALProc);
//var
//  A: TJvSALProcAtom;
begin
  //  A:=TJvSALProcAtom.Create;
  //  A.Actor:=AProcedure;
  //  A.Parser:=AParser;
  //  FProcs.AddObject(AName,A);
  FProcs.AddString(AName, AProcedure, AParser);
end;

function TJvSAL.RPop: Integer;
begin
  Dec(FRSP);
  if FRSP < 0 then
    raise EJVCLException.CreateRes(@RsEReturnStackUnderflow);
  Result := FRStack[FRSP];
end;

procedure TJvSAL.RPush(AValue: Integer);
begin
  FRStack[FRSP] := AValue;
  Inc(FRSP);
  if FRSP > StackLimit then
    raise EJVCLException.CreateRes(@RsEReturnStackOverflow);
end;

// end of subroutine, marked with end-proc

procedure TJvSAL.xEoSub;
begin
  PC := RPop;
end;

// begin of subroutine, marked with [
// loop to ]

procedure TJvSAL.xBoSub;
var
  Op: string;
  C: Integer;
begin
  C := Atoms.Count;
  repeat
    Op := Atoms[PC];
    Inc(FPC);
    if Op = cEndProc then
      Exit;
  until PC >= C;
  raise EJVCLException.CreateRes(@RsECouldNotFindEndOfProcedure);
end;

procedure TJvSAL.SetGetUnit(const Value: TOnGetUnitEvent);
begin
  FOnGetUnit := Value;
end;

// function call

procedure TJvSAL.xProc;
var
  Index: Integer;
begin
  Index := TJvAtom(Atoms.Objects[PCProc]).Value;
  RPush(PC);
  PC := Index + 1;
end;

procedure TJvSAL.SetVariable(const Value: TJvAtom);
begin
  FVariable := Value;
end;

procedure TJvSAL.SetVariableName(const Value: string);
begin
  FVariableName := Value;
end;

procedure TJvSAL.SetSelection(const Value: Variant);
begin
  FSelection := Value;
end;

procedure TJvSAL.SetUseDirective(const Value: string);
begin
  FUseDirective := Value;
end;

procedure TJvSAL.SetBeginOfComment(const Value: string);
begin
  FBeginOfComment := Value;
end;

procedure TJvSAL.SetEndOfComment(const Value: string);
begin
  FEndOfComment := Value;
end;

procedure TJvSAL.SetStringDelimiter(const Value: string);
begin
  FStringDelimiter := Value;
end;

procedure TJvSAL.Setpc(const Value: Integer);
begin
  FPC := Value;
end;

function TJvSAL.APO(Op: string; AProc: TJvSALProc): Integer;
var
  A: TJvAtom;
begin
  A := TJvAtom.Create;
  A.Actor := AProc;
  Result := Atoms.AddObject(Op, A);
end;

procedure TJvSAL.SetToken(const Value: string);
begin
  FToken := Value;
end;

procedure TJvSAL.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJvSAL.xNoParser;
begin
  // do nothing
end;

//=== { TJvAtom } ============================================================

procedure TJvAtom.SetActor(const Value: TJvSALProc);
begin
  FActor := Value;
end;

procedure TJvAtom.SetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

//=== { TJvAtoms } ===========================================================

destructor TJvAtoms.Destroy;
begin
  ClearAll;
  inherited Destroy;
end;

procedure TJvAtoms.ClearAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TJvAtom(Objects[I]).Free;
  Clear;
end;

//=== { TJvSALProcAtom } =====================================================

procedure TJvSALProcAtom.SetActor(const Value: TJvSALProc);
begin
  FActor := Value;
end;

procedure TJvSALProcAtom.SetParser(const Value: TJvSALProc);
begin
  FParser := Value;
end;

end.

