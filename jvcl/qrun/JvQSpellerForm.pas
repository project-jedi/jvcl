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

The Original Code is: JvSpellerForm.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSpellerForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QWindows, QMessages, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls,
  JvQComponent;

type
  TJvSpeller = class;

  TJvSpellerForm = class(TJvForm)
    TextPanel: TPanel;
    LblContext: TLabel;
    TxtSpell: TEdit;
    ButtonPanel: TPanel;
    BtnSkip: TButton;
    BtnChange: TButton;
    BtnCancel: TButton;
    BtnAdd: TButton;
    BtnSkipAll: TButton;
  private
    FSpeller: TJvSpeller;
  end;

  TJvDicIndexArray = array [1..26] of Integer;

  TJvSpeller = class(TComponent)
  private
    FSourceText: string;
    FDict: string;
    FUserDic: string;
    FUserDicChanged: Boolean;
    FDicIndex: TJvDicIndexArray;
    FUserDicIndex: TJvDicIndexArray;
    FSpellerDialog: TJvSpellerForm;
    FWordBegin: Integer;
    FWordEnd: Integer;
    FDictionary: TFileName;
    FUserDictionary: TFileName;
    function WordBegin: Boolean;
    function WordEnd: Boolean;
    function ParseWord: string;
    procedure SpellNext;
    procedure Skip(Sender: TObject);
    procedure Add(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure IndexDictionary;
    procedure IndexUserDictionary;
    procedure SetDictionary(const Value: TFileName);
    procedure SetUserDictionary(const Value: TFileName);
    procedure CreateSpellerDialog(SpellWord: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadDictionary(AFile: string);
    procedure LoadUserDictionary(AFile: string);
    procedure Spell(var SourceText: string);
  published
    property Dictionary: TFileName read FDictionary write SetDictionary;
    property UserDictionary: TFileName read FUserDictionary write SetUserDictionary;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQConsts, JvQResources, JvQTypes;



{$R *.xfm}


function Q_PosStr(const FindString, SourceString: string; StartPos: Integer): Integer;
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

procedure SaveString(AFile, AText: string);
begin
  with TFileStream.Create(AFile, fmCreate) do
  try
    WriteBuffer(AText[1], Length(AText));
  finally
    Free;
  end;
end;

function LoadString(AFile: string): string;
begin
  with TFileStream.Create(AFile, fmOpenRead) do
  try
    SetLength(Result, Size);
    ReadBuffer(Result[1], Size);
  finally
    Free;
  end;
end;

//=== { TJvSpeller } =========================================================

constructor TJvSpeller.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  for I := 1 to 26 do
  begin
    FDicIndex[I] := 1;
    FUserDicIndex[I] := 1;
  end;
end;

procedure TJvSpeller.Add(Sender: TObject);
var
  S: string;
begin
  S := FSpellerDialog.TxtSpell.Text;
  if S = '' then
    Exit;
  FUserDic := FUserDic + LowerCase(S) + Cr;
  FUserDicChanged := True;
  with TStringList.Create do
    try
      Text := FUserDic;
      Sort;
      FUserDic := Text;
    finally
      Free;
    end;
  IndexUserDictionary;
  Skip(Sender);
end;

procedure TJvSpeller.Change(Sender: TObject);
var
  S: string;
begin
  S := FSpellerDialog.TxtSpell.Text;
  if S = '' then
    Exit;
  FSourceText := Copy(FSourceText, 1, FWordBegin - 1) + S +
    Copy(FSourceText, FWordEnd, Length(FSourceText));
  FWordEnd := FWordEnd + (Length(S) - (FWordEnd - FWordBegin));
  Skip(Sender);
end;

procedure TJvSpeller.IndexDictionary;
var
  I, P, StartPos: Integer;
begin
  FDicIndex[1] := 1;
  for I := 2 to 26 do
  begin
    if FDicIndex[I - 1] <> 1 then
      StartPos := FDicIndex[I - 1]
    else
      StartPos := 1;
    P := Q_PosStr(Cr + Chr(96 + I), FDict, StartPos);
    if P <> 0 then
      FDicIndex[I] := P
    else
      FDicIndex[I] := FDicIndex[I - 1];
  end;
end;

procedure TJvSpeller.IndexUserDictionary;
var
  I, P, StartPos: Integer;
begin
  FUserDicIndex[1] := 1;
  for I := 2 to 26 do
  begin
    if FUserDicIndex[I - 1] <> 1 then
      StartPos := FUserDicIndex[I - 1]
    else
      StartPos := 1;
    P := Q_PosStr(Cr + Chr(96 + I), FUserDic, StartPos);
    if P <> 0 then
      FUserDicIndex[I] := P
    else
      FUserDicIndex[I] := FUserDicIndex[I - 1];
  end;
end;

procedure TJvSpeller.LoadDictionary(AFile: string);
begin
  if FileExists(AFile) then
    FDict := LoadString(AFile)
  else
    FDict := '';
  IndexDictionary;
end;

procedure TJvSpeller.LoadUserDictionary(AFile: string);
begin
  UserDictionary := AFile;
  FUserDicChanged := False;
  if FileExists(AFile) then
    FUserDic := LoadString(AFile)
  else
    FUserDic := '';
  IndexUserDictionary;
end;

function TJvSpeller.ParseWord: string;
begin
  Result := '';
  if not WordBegin then
    Exit;
  if not WordEnd then
    Exit;
  Result := Copy(FSourceText, FWordBegin, FWordEnd - FWordBegin);
end;

procedure TJvSpeller.SetDictionary(const Value: TFileName);
begin
  if FDictionary <> Value then
  begin
    FDictionary := Value;
    LoadDictionary(FDictionary);
  end;
end;

procedure TJvSpeller.SetUserDictionary(const Value: TFileName);
begin
  if FUserDictionary <> Value then
  begin
    FUserDictionary := Value;
    LoadUserDictionary(FUserDictionary);
  end;
end;

procedure TJvSpeller.Skip(Sender: TObject);
begin
  FSpellerDialog.TxtSpell.Text := '';
  SpellNext;
end;

procedure TJvSpeller.CreateSpellerDialog(SpellWord: string);
begin
  FSpellerDialog := TJvSpellerForm.Create(Application);
  with FSpellerDialog do
  begin
    FSpeller := Self;
    BtnSkip.OnClick := Skip;
    BtnChange.OnClick := Change;
    BtnAdd.OnClick := Add;
    BtnAdd.Enabled := (UserDictionary <> '');
    TxtSpell.Text := SpellWord;
    LblContext.Caption := Copy(FSourceText, FWordBegin, 75);
  end;
end;

procedure TJvSpeller.Spell(var SourceText: string);
var
  Spw, S: string;
  StartPos, Index: Integer;
begin
  if FDict = '' then
    raise EJVCLException.CreateRes(@RsENoDictionaryLoaded);

  FSourceText := SourceText;
  FWordEnd := 1;
  repeat
    Spw := ParseWord;
    S := LowerCase(Spw);
    if Spw <> '' then
    begin
      Index := Ord(S[1]) - 96;
      if (Index > 0) and (Index < 27) then
        StartPos := FDicIndex[Index]
      else
        StartPos := 1;
      if Q_PosStr(S + Cr, FDict, StartPos) = 0 then
        if FUserDic <> '' then
        begin
          if (Index > 0) and (Index < 27) then
            StartPos := FUserDicIndex[Index]
          else
            StartPos := 1;
          if Q_PosStr(S + Cr, FUserDic, StartPos) = 0 then
          begin
            CreateSpellerDialog(Spw);
            try
              if FSpellerDialog.ShowModal = mrOk then
                SourceText := FSourceText;
              // (rom) the user dictionary has to be saved always!
              if FUserDicChanged then
                if FUserDic <> '' then
                  SaveString(UserDictionary, FUserDic);
            finally
              FSpellerDialog.Free;
            end;
            Exit;
          end
        end
        else
        begin
          CreateSpellerDialog(Spw);
          try
            if FSpellerDialog.ShowModal = mrOk then
              SourceText := FSourceText;
            // (rom) the user dictionary has to be saved always!
            if FUserDicChanged then
              if FUserDic <> '' then
                SaveString(UserDictionary, FUserDic);
          finally
            FSpellerDialog.Free;
          end;
          Exit;
        end;
    end;
  until Spw = '';
end;

procedure TJvSpeller.SpellNext;
var
  Spw, S: string;
  Index, StartPos: Integer;
begin
  repeat
    Spw := ParseWord;
    S := LowerCase(Spw);
    if Spw <> '' then
    begin
      Index := Ord(S[1]) - 96;
      if (Index > 0) and (Index < 27) then
        StartPos := FDicIndex[Index]
      else
        StartPos := 1;
      if Q_PosStr(S + Cr, FDict, StartPos) = 0 then
        if FUserDic <> '' then
        begin
          if (Index > 0) and (Index < 27) then
            StartPos := FUserDicIndex[Index]
          else
            StartPos := 1;
          if Q_PosStr(S + Cr, FUserDic, StartPos) = 0 then
          begin
            FSpellerDialog.TxtSpell.Text := Spw;
            FSpellerDialog.LblContext.Caption := Copy(FSourceText, FWordBegin, 75);
            Exit;
          end;
        end
        else
        begin
          FSpellerDialog.TxtSpell.Text := Spw;
          FSpellerDialog.LblContext.Caption := Copy(FSourceText, FWordBegin, 75);
          Exit;
        end;
    end;
  until Spw = '';
  FSpellerDialog.ModalResult := mrOk;
end;

function TJvSpeller.WordBegin: Boolean;
var
  L: Integer;
begin
  L := Length(FSourceText);
  FWordBegin := FWordEnd;
  while (FWordBegin <= L) and (not (FSourceText[FWordBegin] in ['a'..'z', 'A'..'Z'])) do
    Inc(FWordBegin);
  Result := (FWordBegin <= L);
end;

function TJvSpeller.WordEnd: Boolean;
var
  L: Integer;
begin
  FWordEnd := FWordBegin;
  L := Length(FSourceText);
  while (FWordEnd <= L) and (FSourceText[FWordEnd] in ['a'..'z', 'A'..'Z']) do
    Inc(FWordEnd);
  Result := (FWordEnd <= L);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

