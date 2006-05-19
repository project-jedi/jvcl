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

The Original Code is: JvFindReplace.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Wrapper for the TFind / TReplace dialogs and a stand-alone full
  text search engine with support for all available dialog options:
  Search up/down, whole word only, case sensitive, replace all etc.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQFindReplace;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, QWindows, QMessages, QControls, QDialogs, QStdCtrls,
  JvQComponent;

type
  TJvReplaceProgressEvent = procedure(Sender: TObject; Position: Integer;
    var Terminate: Boolean) of object;
  TJvReplaceAllEvent = procedure(Sender: TObject; ReplaceCount: Integer) of object;

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
    FEditControl: TCustomMemo;
    FOwner: TComponent;
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FOptions: TFindOptions;
    FPosition: TPoint;
    FFast: Boolean;
    FHelpContext: THelpContext;
    FShowDialogs: Boolean;
    FFindText: widestring;
    FReplaceText: widestring;
    FNumberReplaced: Integer; // only used by Replace All
    procedure SetPosition(Value: TPoint);
    procedure SetDialogTop(Value: Integer);
    procedure SetDialogLeft(Value: Integer);
    procedure SetOptions(Value: TFindOptions);
    procedure SetEditControl(Value: TCustomMemo);
    procedure SetHelpContext(Value: THelpContext);
    procedure SetFindText(const Value: widestring);
    procedure SetReplaceText(const Value: widestring);
    procedure SetShowDialogs(Value: Boolean);
    function GetTop: Integer;
    function GetLeft: Integer;
    function ReplaceOne(Sender: TObject): Boolean;
    procedure UpdateDialogs;
    procedure UpdateProperties(sender: TObject);
    procedure NeedDialogs;
    procedure TestEditAssigned;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    property Fast: Boolean read FFast write FFast default False;
    property Options: TFindOptions read FOptions write SetOptions;
    property EditControl: TCustomMemo read FEditControl write SetEditControl;
    property FindText: widestring read FFindText write SetFindText;
    property ReplaceText: widestring read FReplaceText write SetReplaceText;
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
  QForms, Math,
  JvQConsts, JvQResources, JvQTypes;

{ utility }

type
  TDialogAccessProtected = class(TDialog);

procedure Center(Dlg: TDialog);
begin
  with TDialogAccessProtected(Dlg) do
  if Screen.ActiveForm <> nil then
  begin
    Position := Point(Screen.ActiveForm.Left + (Screen.ActiveForm.Width - Width) div 2,
                      Screen.ActiveForm.Top + (Screen.ActiveForm.Height - Height) div 2);
  end
  else if Application.MainForm <> nil then
  begin
    Position := Point(Application.MainForm.Left + (Application.MainForm.Width - Width) div 2,
                Application.MainForm.Top + (Application.MainForm.Height - Height) div 2);
  end
  else
  begin
    Position := Point((Screen.Width - Width) div 2,
                      (Screen.Height - Height) div 2);
  end;
end;



function IsValidWholeWord(const S: string): Boolean;
begin
  Result := (Length(S) > 0) and not ((S[1] in IdentifierSymbols) or (S[Length(S)] in IdentifierSymbols));
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

{ Find text, return a longint }

function FindInText(const Text, Search: string; FromPos, Len: Integer; Fast,
  WholeWord, MatchCase: Boolean): longint;
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
  Fast, WholeWord, MatchCase: Boolean): longint;
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

procedure TJvFindReplace.TestEditAssigned;
begin
  if not Assigned(FEditControl) then
    raise EJVCLException.CreateRes(@RsENoEditAssigned);
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
  FoundPos: longint;
  SLen, RLen, TLen: Integer;
  Terminate: Boolean;
  WholeWord, MatchCase: Boolean;
begin
  TestEditAssigned;
  Terminate := False;
  UpdateDialogs;
  WholeWord := frWholeWord in FOptions;
  MatchCase := frMatchCase in FOptions;
  Txt := FEditControl.Text;
  SLen := Length(SearchText);
  RLen := Length(ReplaceText);
  TLen := Length(Txt);
  FoundPos := FindInText(Txt, SearchText, EditControl.SelStart + EditControl.SelLength,
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
    FEditControl.Text := Txt;
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
    Equal := AnsiCompareStr(FEditControl.SelText, S)
  else
    Equal := AnsiCompareText(FEditControl.SelText, S);

  if Equal = 0 then
  begin
    Result := True;
    FEditControl.SelText := R;
    FEditControl.SetFocus;
  end;
end;

procedure TJvFindReplace.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEditControl) then
    FEditControl := nil;
end;

procedure TJvFindReplace.NeedDialogs;
begin
  if not Assigned(FFindDialog) then
  begin
    FFindDialog := TFindDialog.Create(Self);
    FFindDialog.FindText := FFindText;
    FFindDialog.OnFind := DoOnFind;
  end;
  if not Assigned(FReplaceDialog) then
  begin
    FReplaceDialog := TReplaceDialog.Create(Self);
    FReplaceDialog.FindText := FFindText;
    FReplaceDialog.ReplaceText := FReplaceText;
    FReplaceDialog.OnFind := DoOnFind;
    FReplaceDialog.OnReplace := DoOnReplace;
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

procedure TJvFindReplace.UpdateProperties(sender: TObject);
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
  FoundPos: longint;
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
    Offset := FEditControl.SelStart;
    if Offset = 0 then
      Offset := 1;
    FoundPos := FindInTextRev(FEditControl.Text, FFindText,
      Length(FEditControl.Text) - Offset, Length(FEditControl.Text), FFast,
      WholeWord, MatchCase)
  end else
  begin
    Offset := FEditControl.SelStart + FEditControl.SelLength;
    if Offset = 0 then
      Offset := 1;
    FoundPos := FindInText(FEditControl.Text, FFindText, Offset,
      Length(FEditControl.Text), FFast, WholeWord, MatchCase);
  end;

  if FoundPos > -1 then
  begin
    FEditControl.SetFocus;
    FEditControl.SelStart := FoundPos - 1;
    FEditControl.SelLength := Length(FFindText);
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
    if FEditControl.SelLength < 1 then
      DoOnFind(Sender);
    if FEditControl.SelLength < 1 then
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
    TFindDialog(Sender).Form.Handle, 
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
      TFindDialog(Sender).Form.Handle, 
      PChar(Format(RsXOccurencesReplaced, [FNumberReplaced, FFindText])),
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

procedure TJvFindReplace.SetEditControl(Value: TCustomMemo);
begin
  FEditControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvFindReplace.SetFindText(const Value: widestring);
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
    if FFindDialog.Form <> nil
    then
      FFindDialog.Form.Close;
    if FReplaceDialog.Form <> nil
    then
      FReplaceDialog.Form.Close;
  end;
end;

procedure TJvFindReplace.SetReplaceText(const Value: widestring);
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

procedure TJvFindReplace.Loaded;
begin
  inherited Loaded;
  UpdateDialogs;
  Center(FFindDialog);
  Center(FReplaceDialog);
  FPosition := FFindDialog.Position;

end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

