{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindReplace.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
  Olivier Sannier
  Robert Marquardt

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Wrapper for the TFind / TReplace dialogs and a stand-alone full
  text search engine with support for all available dialog options:
  Search up/down, whole word only, case sensitive, replace all etc.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFindReplace;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Controls, Dialogs, StdCtrls,
  JvComponentBase, JvEditor;

type
  TJvReplaceProgressEvent = procedure(Sender: TObject; Position: Integer;
    var Terminate: Boolean) of object;
  TJvReplaceAllEvent = procedure(Sender: TObject; ReplaceCount: Integer) of object;

  // for custom property editor
  TJvEditControlName = TWinControl;

  // internal type to handle different component ancestry trees
  TJvFindReplaceEditKind = (etEmpty, etCustomEdit, etJvCustomEditor);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvFindReplace = class(TJvComponent)
  private
    FOnFind: TNotifyEvent;
    FOnReplace: TNotifyEvent;
    FOnReplacingAll: TNotifyEvent;
    FOnReplacedAll: TJvReplaceAllEvent;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnNotFound: TNotifyEvent;
    FOnProgress: TJvReplaceProgressEvent;
    FOwner: TComponent;
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FOptions: TFindOptions;
    FPosition: TPoint;
    FFast: Boolean;
    FHelpContext: THelpContext;
    FShowDialogs: Boolean;
    FFindText: string;
    FReplaceText: string;
    FNumberReplaced: Integer; // only used by Replace All
    FEditControl: TJvEditControlName;
    FEditKind: TJvFindReplaceEditKind;
    procedure SetEditControl(Value: TJvEditControlName);
    procedure SetPosition(Value: TPoint);
    procedure SetDialogTop(Value: Integer);
    procedure SetDialogLeft(Value: Integer);
    procedure SetOptions(Value: TFindOptions);
    procedure SetHelpContext(Value: THelpContext);
    procedure SetFindText(const Value: string);
    procedure SetReplaceText(const Value: string);
    procedure SetShowDialogs(Value: Boolean);
    function GetTop: Integer;
    function GetLeft: Integer;
    function ReplaceOne(Sender: TObject): Boolean;
    procedure UpdateDialogs;
    procedure UpdateProperties(Sender: TObject);
    procedure NeedDialogs;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetEditText: string; virtual;
    function GetEditSelText: string; virtual;
    function GetEditSelStart: Integer; virtual;
    function GetEditSelLength: Integer; virtual;
    function GetEditHandle: HWND; virtual;
    procedure TestEditAssigned; virtual;
    procedure SetEditText(const Text: string); virtual;
    procedure SetEditSelText(const Text: string); virtual;
    procedure SetEditSelStart(Start: Integer); virtual;
    procedure SetEditSelLength(Length: Integer); virtual;
    procedure SetEditFocus; virtual;
    procedure DoOnFind(Sender: TObject); virtual;
    procedure DoOnReplace(Sender: TObject); virtual;
    procedure DoOnShow(Sender: TObject); virtual;
    procedure DoOnClose(Sender: TObject); virtual;
    procedure DoFailed(Sender: TObject); virtual;
    procedure DoReplacingAll; virtual;
    procedure DoReplacedAll(Sender: TObject); virtual;
    procedure DoProgress(Position: Integer; var Terminate: Boolean); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Find; virtual;
    procedure FindAgain; virtual;
    procedure Replace; virtual;
    procedure ReplaceAll(const SearchText, ReplaceText: string); virtual;
    property Position: TPoint read FPosition write SetPosition;
    property Top: Integer read GetTop write SetDialogTop default -1;
    property Left: Integer read GetLeft write SetDialogLeft default -1;
  published
    property EditControl: TJvEditControlName read FEditControl write SetEditControl;
    property Fast: Boolean read FFast write FFast default False;
    property Options: TFindOptions read FOptions write SetOptions;
    property FindText: string read FFindText write SetFindText;
    property ReplaceText: string read FReplaceText write SetReplaceText;
    property ShowDialogs: Boolean read FShowDialogs write SetShowDialogs default True;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnReplacingAll: TNotifyEvent read FOnReplacingAll write FOnReplacingAll;
    property OnReplacedAll: TJvReplaceAllEvent read FOnReplacedAll write FOnReplacedAll;
    property OnNotFound: TNotifyEvent read FOnNotFound write FOnNotFound;
    property OnProgress: TJvReplaceProgressEvent read FOnProgress write FOnProgress;
  end;

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
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvConsts, JvResources, JvTypes;

{ utility }

function IsValidWholeWord(const S: string): Boolean;
begin
  Result := (Length(S) > 0) and not (CharInSet(S[1], IdentifierSymbols) or CharInSet(S[Length(S)], IdentifierSymbols));
end;

{ invert string }

function StrRev(const S: string): string;
var
  I, Len: Integer;
begin
  Len := Length(S);
  SetLength(Result, Len);
  for I := 1 to Len do
  begin
    Result[I] := S[Len];
    Dec(Len);
  end;
end;

{ Pascal adaption of a function originally in C }

function BoyerMoore(SubStr, S: PChar): Integer;
var
  CharJump, MatchJump, BackUp: array[0..255] of Integer;
  PatLen, TextLen, u, uA, uB, uText, uPat: Integer;
begin
  Result := 0;
  PatLen := StrLen(SubStr);
  TextLen := StrLen(S);

  FillChar(CharJump, 256 * SizeOf(Integer), 0);

  for u := 0 to PatLen do
    CharJump[Ord(SubStr[u])] := PatLen - u - 1;

  for u := 1 to PatLen - 1 do
    MatchJump[u] := 2 * PatLen - u;

  u := PatLen;
  uA := PatLen + 1;
  while u > 0 do
  begin
    BackUp[u] := uA;
    while (uA <= PatLen) and (SubStr[u - 1] <> SubStr[uA - 1]) do
    begin
      if MatchJump[uA] > PatLen - u then
        MatchJump[uA] := PatLen - u;
      uA := BackUp[uA];
    end;
    Dec(u);
    Dec(uA);
  end;

  for u := 1 to uA do
    if MatchJump[u] > PatLen + uA - u then
      MatchJump[u] := PatLen + uA - u;
  uB := BackUp[uA];

  while uA <= PatLen do
  begin
    while uA <= uB do
    begin
      if MatchJump[uA] > uB - uA + PatLen then
        MatchJump[uA] := uB - uA + PatLen;
      Inc(uA);
    end;
    uB := BackUp[uB];
  end;

  uPat := PatLen;
  uText := PatLen - 1;
  while (uText < TextLen) and (uPat <> 0) do
  begin
    if S[uText] = SubStr[uPat - 1] then
    begin
      Dec(uText);
      Dec(uPat);
    end
    else { mismatch - slide forward }
    begin
      uA := CharJump[Ord(S[uText])];
      uB := PatLen - uPat + 1;
      uText := uText + Max(uA, uB);
      uPat := PatLen;
    end;
  end;
  if uPat = 0 then
    Result := uText + 2;
end;

{ Find text, return a Longint }

function FindInText(const Text, Search: string; FromPos, Len: Integer;
  Fast, WholeWord, MatchCase: Boolean): Longint;
var
  Found, SearchLen, TextLen: Integer;
  S: string;
begin
  Result := -1; // assume failure

  // first character in string is at position 1
  if FromPos = 0 then
    FromPos := 1;

  Found := 1;
  while (Result = -1) and (Found > 0) do
  begin
    if Fast then
      Found := BoyerMoore(PChar(AnsiUpperCase(Search)),
        PChar(AnsiUpperCase(Copy(Text, FromPos, Len))))
    else
      Found := Pos(AnsiUpperCase(Search), AnsiUpperCase(Copy(Text, FromPos, Len)));
    if Found > 0 then
    begin
      Result := Found + FromPos - 1;
      SearchLen := Length(Search);
      TextLen := Length(Text);
      FromPos := Result + SearchLen;
      // is match-case required and does it?
      if MatchCase and (AnsiCompareStr(Search, Copy(Text, Result, SearchLen)) <> 0) then
        Result := -1
          // is whole-word-only required and is it?
      else
      if WholeWord and (SearchLen < TextLen) then
      begin
        // check for extremes...
        S := Copy(Text, Result - 1, SearchLen + 2);
        // check for match at beginning or end of string
        if Result = 1 then
          S := Copy(' ' + S, 1, SearchLen + 2);
        if Result - 1 + SearchLen + 1 > TextLen then
          S := Copy(S + ' ', Length(S) - SearchLen-2, SearchLen + 2);
        if not IsValidWholeWord(S) then
          result := -1;
      end;
    end;
  end;
end;

{ invert and search }

function FindInTextRev(const Text, Search: string; FromPos, Len: Integer;
  Fast, WholeWord, MatchCase: Boolean): Longint;
begin
  Result := FindInText(StrRev(Text), StrRev(Search), FromPos, Len, Fast,
    WholeWord, MatchCase);
  if Result > -1 then
    Result := Length(Text) - (Result - 1) - (Length(Search) - 1);
end;

//=== { TJvFindReplace } =====================================================

constructor TJvFindReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FHelpContext := 0;
  FShowDialogs := True;
  FPosition := Point(-1, -1);
end;

procedure TJvFindReplace.Loaded;
begin
  inherited Loaded;
  UpdateDialogs;
end;

procedure TJvFindReplace.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEditControl) then
    FEditControl := nil;
end;

procedure TJvFindReplace.Find;
begin
  TestEditAssigned;
  UpdateDialogs;
  if FShowDialogs then
    FFindDialog.Execute
  else
    DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.FindAgain;
begin
  TestEditAssigned;
  UpdateDialogs;
  DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.Replace;
begin
  TestEditAssigned;
  UpdateDialogs;

  if FShowDialogs then
    FReplaceDialog.Execute
  else
    DoOnReplace(FReplaceDialog);
end;

procedure TJvFindReplace.ReplaceAll(const SearchText, ReplaceText: string);
var
  Txt: string;
  FoundPos: Longint;
  SLen, RLen, TLen: Integer;
  Terminate: Boolean;
  WholeWord, MatchCase: Boolean;
begin
  TestEditAssigned;
  Terminate := False;
  UpdateDialogs;
  WholeWord := frWholeWord in FOptions;
  MatchCase := frMatchCase in FOptions;
  Txt := GetEditText;
  SLen := Length(SearchText);
  RLen := Length(ReplaceText);
  TLen := Length(Txt);
  FoundPos := FindInText(Txt, SearchText, GetEditSelStart + GetEditSelLength,
    TLen, FFast, WholeWord, MatchCase);

  if FoundPos > -1 then
  begin
    DoReplacingAll;
    FNumberReplaced := 0;
    while FoundPos > -1 do
    begin
      Inc(FNumberReplaced);

      Delete(Txt, FoundPos, SLen);
      Insert(ReplaceText, Txt, FoundPos);
      FoundPos := FindInText(Txt, SearchText, FoundPos + RLen + 1, TLen + (RLen - SLen), FFast, WholeWord, MatchCase);

      DoProgress(FoundPos, Terminate);
      if Terminate then
        Exit;
    end;
    SetEditText(Txt);
    DoReplacedAll(FReplaceDialog);
  end
  else
    DoFailed(FReplaceDialog);
end;

function TJvFindReplace.ReplaceOne(Sender: TObject): Boolean;
var
  Equal: Integer;
  S, R: string;
begin
  Result := False;

  if FShowDialogs then
  begin
    S := TReplaceDialog(Sender).FindText;
    R := TReplaceDialog(Sender).ReplaceText;
  end
  else
  begin
    S := FFindText;
    R := FReplaceText;
  end;

  if frMatchCase in TFindDialog(Sender).Options then
    Equal := AnsiCompareStr(GetEditSelText, S)
  else
    Equal := AnsiCompareText(GetEditSelText, S);

  if Equal = 0 then
  begin
    Result := True;
    SetEditSelText(R);
    SetEditFocus;
  end;
end;

procedure TJvFindReplace.NeedDialogs;
begin
  if not Assigned(FFindDialog) then
  begin
    FFindDialog := TFindDialog.Create(Self);
    FFindDialog.FindText := FFindText;
    FFindDialog.OnFind := DoOnFind;
    FFindDialog.Position := FPosition;
  end;
  if not Assigned(FReplaceDialog) then
  begin
    FReplaceDialog := TReplaceDialog.Create(Self);
    FReplaceDialog.FindText := FFindText;
    FReplaceDialog.ReplaceText := FReplaceText;
    FReplaceDialog.OnFind := DoOnFind;
    FReplaceDialog.OnReplace := DoOnReplace;
    FReplaceDialog.Position := FPosition;
  end;
end;

procedure TJvFindReplace.UpdateDialogs;
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    NeedDialogs;

    FFindDialog.Position := FPosition;
    FFindDialog.Options := FOptions;
    FFindDialog.HelpContext := FHelpContext;
    FFindDialog.FindText := FFindText;

    FReplaceDialog.Position := FPosition;
    FReplaceDialog.Options := FOptions;
    FReplaceDialog.HelpContext := FHelpContext;
    FReplaceDialog.FindText := FFindText;
    FReplaceDialog.ReplaceText := FReplaceText;
  end;
end;

procedure TJvFindReplace.UpdateProperties(Sender: TObject);
begin
  if Sender is TFindDialog then
  begin
    FPosition := TFindDialog(Sender).Position;
    FOptions := TFindDialog(Sender).Options;
    FHelpContext := TFindDialog(Sender).HelpContext;
    FFindText := TFindDialog(Sender).FindText;
  end;
  if Sender is TReplaceDialog then
    FReplaceText := TReplaceDialog(Sender).ReplaceText;
end;

procedure TJvFindReplace.DoOnFind(Sender: TObject);
var
  FoundPos: Longint;
  Offset: Integer;
  WholeWord, MatchCase: Boolean;
begin
  /// update the local properties with the current values from the dialog
  /// in case the user has changed the options (or the find/replace text)
  UpdateProperties(Sender);
  WholeWord := frWholeWord in FOptions;
  MatchCase := frMatchCase in FOptions;

  if not (frDown in FOptions) then
  begin
    Offset := GetEditSelStart - 1;
    if Offset <= 0 then
      Offset := 1;
    FoundPos := FindInTextRev(GetEditText, FFindText,
      Length(GetEditText) - Offset, Length(GetEditText), FFast,
      WholeWord, MatchCase)
  end else
  begin
    Offset := GetEditSelStart + GetEditSelLength + 1;
    FoundPos := FindInText(GetEditText, FFindText, Offset,
      Length(GetEditText), FFast, WholeWord, MatchCase);
  end;

  if FoundPos > -1 then
  begin
    SetEditFocus;
    SetEditSelStart(FoundPos - 1);
    SetEditSelLength(Length(FFindText));
    if GetEditHandle <> 0 then
      SendMessage(GetEditHandle, EM_SCROLLCARET, 0, 0);
    if Assigned(FOnFind) then
      FOnFind(Self);
  end
  else
    DoFailed(Sender);
end;

procedure TJvFindReplace.DoOnReplace(Sender: TObject);
begin
  UpdateProperties(Sender);

  if frReplaceAll in FOptions then
  begin
    ReplaceAll(FFindText, FReplaceText);
    if Assigned(FOnReplace) then
      FOnReplace(Self);
  end
  else
  begin
    if GetEditSelLength < 1 then
      DoOnFind(Sender);
    if GetEditSelLength < 1 then
      Exit;
    ReplaceOne(Sender);
    if Assigned(FOnReplace) then
      FOnReplace(Self);
    DoOnFind(Sender);
  end;
end;

procedure TJvFindReplace.DoOnShow(Sender: TObject);
begin
  TestEditAssigned;
  UpdateDialogs;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TJvFindReplace.DoOnClose(Sender: TObject);
begin
  TestEditAssigned;
  UpdateProperties(Sender);
  UpdateDialogs;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvFindReplace.DoFailed(Sender: TObject);
var
  FCaption: string;
begin
  TestEditAssigned;
  UpdateProperties(Sender);
  if Assigned(FOnNotFound) then
    FOnNotFound(Self);
  if not FShowDialogs then
    Exit;

  if Sender = FReplaceDialog then
    FCaption := RsReplaceCaption
  else
    FCaption := RsFindCaption;

  MessageBox(
    TFindDialog(Sender).Handle,
    PChar(Format(RsNotFound, [FFindText])),
    PChar(FCaption), MB_OK or MB_ICONINFORMATION);
end;

procedure TJvFindReplace.DoReplacingAll;
begin
  if Assigned(FOnReplacingAll) then
    FOnReplacingAll(Self);
end;

procedure TJvFindReplace.DoReplacedAll(Sender: TObject);
begin
  UpdateProperties(Sender);
  if FShowDialogs then
  begin
    MessageBox(
      TFindDialog(Sender).Handle,
      PChar(Format(RsXOccurrencesReplaced, [FNumberReplaced, FFindText])),
      PChar(RsReplaceCaption), MB_OK or MB_ICONINFORMATION);
  end;

  if Assigned(FOnReplacedAll) then
    FOnReplacedAll(Self, FNumberReplaced);
end;

procedure TJvFindReplace.DoProgress(Position: Integer; var Terminate: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Terminate);
end;

procedure TJvFindReplace.SetPosition(Value: TPoint);
begin
  FPosition := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetDialogTop(Value: Integer);
begin
  FPosition.Y := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetDialogLeft(Value: Integer);
begin
  FPosition.X := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetOptions(Value: TFindOptions);
begin
  FOptions := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetFindText(const Value: string);
begin
  FFindText := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetShowDialogs(Value: Boolean);
begin
  if FShowDialogs <> Value then
    FShowDialogs := Value;
  if not Value then
  begin
    NeedDialogs;
    FFindDialog.CloseDialog;
    FReplaceDialog.CloseDialog;
  end;
end;

procedure TJvFindReplace.SetReplaceText(const Value: string);
begin
  FReplaceText := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetHelpContext(Value: THelpContext);
begin
  FHelpContext := Value;
  UpdateDialogs;
end;

function TJvFindReplace.GetTop: Integer;
begin
  Result := FPosition.Y;
end;

function TJvFindReplace.GetLeft: Integer;
begin
  Result := FPosition.X;
end;

procedure TJvFindReplace.SetEditControl(Value: TJvEditControlName);
begin
  if FEditControl <> nil then
    FEditControl.RemoveFreeNotification(Self);
  FEditControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  if Value is TCustomEdit then
    FEditKind := etCustomEdit
  else
  if Value is TJvCustomEditor then
    FEditKind := etJvCustomEditor
  else
    FEditKind := etEmpty;
end;

procedure TJvFindReplace.TestEditAssigned;
begin
  if not Assigned(FEditControl) then
    raise EJVCLException.CreateRes(@RsENoEditAssigned);
end;

function TJvFindReplace.GetEditText: string;
begin
  case FEditKind of
    etCustomEdit:
      Result := TCustomEdit(FEditControl).Text;
    etJvCustomEditor:
      Result := TJvCustomEditor(FEditControl).Lines.Text;
  else
    Result := '';
  end;
end;

function TJvFindReplace.GetEditSelText: string;
begin
  case FEditKind of
    etCustomEdit:
      Result := TCustomEdit(FEditControl).SelText;
    etJvCustomEditor:
      Result := TJvCustomEditor(FEditControl).SelText;
  else
    Result := '';
  end;
end;

function TJvFindReplace.GetEditSelStart: Integer;
begin
  case FEditKind of
    etCustomEdit:
      Result := TCustomEdit(FEditControl).SelStart;
    etJvCustomEditor:
      Result := TJvCustomEditor(FEditControl).SelStart;
  else
    Result := 0;
  end;
end;

function TJvFindReplace.GetEditSelLength: Integer;
begin
  case FEditKind of
    etCustomEdit:
      Result := TCustomEdit(FEditControl).SelLength;
    etJvCustomEditor:
      Result := TJvCustomEditor(FEditControl).SelLength;
  else
    Result := 0;
  end;
end;

function TJvFindReplace.GetEditHandle: HWND;
begin
  case FEditKind of
    etCustomEdit:
      Result := TCustomEdit(FEditControl).Handle;
    etJvCustomEditor:
      Result := TJvCustomEditor(FEditControl).Handle;
  else
    Result := HWND(0);
  end;
end;

procedure TJvFindReplace.SetEditText(const Text: string);
begin
  case FEditKind of
    etCustomEdit:
      TCustomEdit(FEditControl).Text := Text;
    etJvCustomEditor:
      TJvCustomEditor(FEditControl).Lines.Text := Text;
  end;
end;

procedure TJvFindReplace.SetEditSelText(const Text: string);
begin
  case FEditKind of
    etCustomEdit:
      TCustomEdit(FEditControl).SelText := Text;
    etJvCustomEditor:
      TJvCustomEditor(FEditControl).SelText := Text;
  end;
end;

procedure TJvFindReplace.SetEditSelStart(Start: Integer);
begin
  case FEditKind of
    etCustomEdit:
      TCustomEdit(FEditControl).SelStart := Start;
    etJvCustomEditor:
      TJvCustomEditor(FEditControl).SelStart := Start;
  end;
end;

procedure TJvFindReplace.SetEditSelLength(Length: Integer);
begin
  case FEditKind of
    etCustomEdit:
      TCustomEdit(FEditControl).SelLength := Length;
    etJvCustomEditor:
      TJvCustomEditor(FEditControl).SelLength := Length;
  end;
end;

procedure TJvFindReplace.SetEditFocus;
begin
  case FEditKind of
    etCustomEdit:
      TCustomEdit(FEditControl).SetFocus;
    etJvCustomEditor:
      TJvCustomEditor(FEditControl).SetFocus;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
