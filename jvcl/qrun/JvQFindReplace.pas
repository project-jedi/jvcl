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
    FEditControl: TCustomEdit;
    FOwner: TComponent;
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FOptions: TFindOptions;
    FPosition: TPoint;
    FLeft: Integer;
    FTop: Integer;
    FFast: Boolean;
    FHelpContext: THelpContext;
    FShowDialogs: Boolean;
    FKeepText: Boolean;
    FFindText: string;
    FReplaceText: string;
    FNumberReplaced: Integer; // only used by Replace All
    procedure SetPosition(Value: TPoint);
    procedure SetDialogTop(Value: Integer);
    procedure SetDialogLeft(Value: Integer);
    procedure SetOptions(Value: TFindOptions);
    procedure SetEditControl(Value: TCustomEdit);
    procedure SetHelpContext(Value: THelpContext);
    procedure SetFindText(const Value: string);
    procedure SetReplaceText(const Value: string);
    procedure SetShowDialogs(Value: Boolean);
    function GetPosition: TPoint;
    function GetTop: Integer;
    function GetLeft: Integer;
    function GetOptions: TFindOptions;
    function GetHelpContext: THelpContext;
    function GetFindText: string;
    function GetReplaceText: string;
    function ReplaceOne(Sender: TObject): Boolean;
    procedure UpdateDialogs;
    procedure NeedDialogs;
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
  public
    constructor Create(AOwner: TComponent); override;
    procedure Find; virtual;
    procedure FindAgain; virtual;
    procedure Replace; virtual;
    procedure ReplaceAll(const SearchText, ReplaceText: string); virtual;
    property Position: TPoint read GetPosition write SetPosition;
    property Top: Integer read GetTop write SetDialogTop default -1;
    property Left: Integer read GetLeft write SetDialogLeft default -1;
  published
    property Fast: Boolean read FFast write FFast default False;
    property Options: TFindOptions read GetOptions write SetOptions;
    property EditControl: TCustomEdit read FEditControl write SetEditControl;
    property FindText: string read GetFindText write SetFindText;
    property KeepText: Boolean read FKeepText write FKeepText default False;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ShowDialogs: Boolean read FShowDialogs write SetShowDialogs default True;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext default 0;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnReplacingAll: TNotifyEvent read FOnReplacingAll write FOnReplacingAll;
    property OnReplacedAll: TJvReplaceAllEvent read FOnReplacedAll write FOnReplacedAll;
    property OnNotFound: TNotifyEvent read FOnNotFound write FOnNotFound;
    property OnProgress: TJvReplaceProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math,
  JvQConsts, JvQResources, JvQTypes;

type
  TFoundText = record
    StartAt: Longint;
    EndAt: Longint;
    isWhole: Boolean;
    isSameCase: Boolean;
  end;

procedure Error;
begin
  EJVCLException.CreateRes(@RsENoEditAssigned);
end;

{ utility }

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
  CharJump, MatchJump, BackUp: array [0..255] of Integer;
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

{ Find text, return a TFoundText }

function FindInText(const Text, Search: string; FromPos, ToPos: Integer; Fast: Boolean): TFoundText;
var
  Found: Integer;
  S: string;
begin
  Result.StartAt := -1; // assume failure

  if Fast then
    Found := BoyerMoore(PChar(AnsiUpperCase(Search)), PChar(AnsiUpperCase(Copy(Text, FromPos + 1, ToPos))))
  else
    Found := Pos(AnsiUpperCase(Search), AnsiUpperCase(Copy(Text, FromPos + 1, ToPos)));
  if Found > 0 then
  begin
    Result.StartAt := Found + FromPos - 1;
    Result.EndAt := Length(Search);
//    S := Copy(Text, Result.StartAt - 1, Result.EndAt + 3);
    S := Copy(Text, Result.StartAt - 1, Result.EndAt + 2);
    Result.isWhole := IsValidWholeWord(S);
//    S := Copy(S, 3, Length(S) - 3);
//    Result.isSameCase := (AnsiCompareStr(Search, S) = 0);
    S := Copy(S, 1, Length(S) - 1);
    Result.isSameCase := (AnsiCompareStr(trim(Search), trim(S)) = 0);
  end;
end;

{ invert and search }

function FindInTextRev(const Text, Search: string; FromPos, ToPos: Integer; Fast: Boolean): TFoundText;
begin
  Result := FindInText(StrRev(Text), StrRev(Search), FromPos, ToPos, Fast);
  if Result.StartAt > -1 then
    Result.StartAt := Length(Text) - Result.StartAt - Result.EndAt;
end;

constructor TJvFindReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FHelpContext := 0;
  FKeepText := False;
  FShowDialogs := True;
  FPosition := Point(-1, -1);
end;

procedure TJvFindReplace.Find;
begin
  if not Assigned(FEditControl) then
    Error;
  UpdateDialogs;
  if FShowDialogs then
    FFindDialog.Execute
  else
    DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.FindAgain;
begin
  if not Assigned(FEditControl) then
    Error;
  UpdateDialogs;
  DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.Replace;
begin
  if not Assigned(FEditControl) then
    Error;
  UpdateDialogs;

  if FShowDialogs then
    FReplaceDialog.Execute
  else
    DoOnReplace(FReplaceDialog);
end;

procedure TJvFindReplace.ReplaceAll(const SearchText, ReplaceText: string);
var
  Txt: string;
  FoundPos: TFoundText;
  TmpOptions: TFindOptions;
  SLen, RLen, TLen: Integer;
  Terminate: Boolean;
begin
  if not Assigned(FEditControl) then
    Error;
  Terminate := False;
  TmpOptions := FReplaceDialog.Options;
  Txt := FEditControl.Text;
  SLen := Length(SearchText);
  RLen := Length(ReplaceText);
  TLen := Length(Txt);
  FoundPos := FindInText(Txt, SearchText, 0, TLen, True);

  if FoundPos.StartAt > -1 then
  begin
    DoReplacingAll;
    FNumberReplaced := 0;
    while FoundPos.StartAt > -1 do
    begin
      Inc(FNumberReplaced);
      if (frWholeWord in TmpOptions) and not FoundPos.isWhole then
      begin
        FoundPos := FindInText(Txt, SearchText, FoundPos.StartAt + RLen + 1, TLen + (RLen - SLen), True);
        Continue;
      end;
      if (frMatchCase in TmpOptions) and not FoundPos.isSameCase then
      begin
        FoundPos := FindInText(Txt, SearchText, FoundPos.StartAt + RLen + 1, TLen + (RLen - SLen), True);
        Continue;
      end;
      Delete(Txt, FoundPos.StartAt + 1, SLen);
      Insert(ReplaceText, Txt, FoundPos.StartAt + 1);
      FoundPos := FindInText(Txt, SearchText, FoundPos.StartAt + RLen + 1, TLen + (RLen - SLen), True);
      if FoundPos.StartAt mod 60 = 0 then
      begin
        DoProgress(FoundPos.StartAt, Terminate);
        if Terminate then
          Exit;
      end;
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
  if (Operation = opRemove) and (AComponent = FEditControl) then
    FEditControl := nil;
end;

procedure TJvFindReplace.NeedDialogs;
begin
  if not Assigned(FFindDialog) then
  begin
    FFindDialog := TFindDialog.Create(Self);
    FFindDialog.OnFind := DoOnFind;
  end;
  if not Assigned(FReplaceDialog) then
  begin
    FReplaceDialog := TReplaceDialog.Create(Self);
    FReplaceDialog.OnFind := DoOnFind;
    FReplaceDialog.OnReplace := DoOnReplace;
  end;
end;

procedure TJvFindReplace.UpdateDialogs;
begin
  NeedDialogs;

  FFindDialog.Position := GetPosition;  
  FFindDialog.Position := Point(GetTop, GetLeft); 
  FFindDialog.Options := FOptions;
  FFindDialog.HelpContext := GetHelpContext;
  FFindDialog.FindText := GetFindText;
  FReplaceDialog.Position := GetPosition;
  
  FReplaceDialog.Position := Point(GetTop, GetLeft); 
  FReplaceDialog.Options := FOptions;
  FReplaceDialog.HelpContext := GetHelpContext;
  FReplaceDialog.FindText := GetFindText;
  FReplaceDialog.ReplaceText := GetReplaceText;
end;

procedure TJvFindReplace.DoOnFind(Sender: TObject);
var
  FoundPos: TFoundText;
  S: string;
  Offset: Integer;
begin
  if FShowDialogs then
    S := TFindDialog(Sender).FindText
  else
    S := FFindText;

  if FKeepText then
  begin
    FFindText := TFindDialog(Sender).FindText;
    FReplaceText := TReplaceDialog(Sender).ReplaceText;
  end;
  if (FEditControl.SelStart = 0) and (FEditControl.SelLength < 1) then
    Offset := 0
  else
    Offset := 1;
  if not (frDown in TFindDialog(Sender).Options) then
    FoundPos := FindInTextRev(FEditControl.Text, S, Length(FEditControl.Text) - FEditControl.SelStart + Offset,
      Length(FEditControl.Text), FFast)
  else
    FoundPos := FindInText(FEditControl.Text, S, FEditControl.SelStart + Offset, Length(FEditControl.Text), FFast);

  if FoundPos.StartAt > -1 then
  begin
    if (frWholeWord in TFindDialog(Sender).Options) and not FoundPos.isWhole then
      DoFailed(Sender)
    else
    if (frMatchCase in TFindDialog(Sender).Options) and not FoundPos.isSameCase then
      DoFailed(Sender)
    else
    begin
      FEditControl.SetFocus;
      FEditControl.SelStart := FoundPos.StartAt;
      FEditControl.SelLength := FoundPos.EndAt; 
      if Assigned(FOnFind) then
        FOnFind(Self);
    end
  end
  else
    DoFailed(Sender);
end;

procedure TJvFindReplace.DoOnReplace(Sender: TObject);
begin
  if FEditControl.SelLength < 1 then
    DoOnFind(Sender);
  if FEditControl.SelLength < 1 then
    Exit;

  if frReplaceAll in TFindDialog(Sender).Options then
  begin
    SetFindText(FReplaceDialog.FindText);
    SetReplaceText(FReplaceDialog.ReplaceText);
    ReplaceAll(FFindText, FReplaceText);
    if Assigned(FOnReplace) then
      FOnReplace(Self);
  end
  else
  begin
    ReplaceOne(Sender);
    if Assigned(FOnReplace) then
      FOnReplace(Self);
    DoOnFind(Sender);
  end;
end;

procedure TJvFindReplace.DoOnShow(Sender: TObject);
begin
  if not Assigned(FEditControl) then
    Error;
  UpdateDialogs;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TJvFindReplace.DoOnClose(Sender: TObject);
begin
  if not Assigned(FEditControl) then
    Error;
  UpdateDialogs;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvFindReplace.DoFailed(Sender: TObject);
var
  FCaption: string;
begin
  if not Assigned(FEditControl) then
    Error;
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
    PChar(Format(RsNotFound, [TFindDialog(Sender).FindText])),
    PChar(FCaption), MB_OK or MB_ICONINFORMATION);
end;

procedure TJvFindReplace.DoReplacingAll;
begin
  if Assigned(FOnReplacingAll) then
    FOnReplacingAll(Self);
end;

procedure TJvFindReplace.DoReplacedAll(Sender: TObject);
begin
  if FShowDialogs then
  begin
    MessageBox(  
      TFindDialog(Sender).Form.Handle, 
      PChar(Format(RsXOccurencesReplaced, [FNumberReplaced, TFindDialog(Sender).FindText])),
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
  FTop := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetDialogLeft(Value: Integer);
begin
  FLeft := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetOptions(Value: TFindOptions);
begin
  FOptions := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetEditControl(Value: TCustomEdit);
begin
  FEditControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvFindReplace.SetFindText(const Value: string);
begin
  FFindText := Value;
  if Assigned(FFindDialog) and Assigned(FReplaceDialog) then
  begin
    FFindDialog.FindText := Value;
    FReplaceDialog.FindText := Value;
  end;
end;

procedure TJvFindReplace.SetShowDialogs(Value: Boolean);
begin
  if FShowDialogs <> Value then
    FShowDialogs := Value;
  if not Value then
  begin
    NeedDialogs;  
    FFindDialog.Form.Close;
    FReplaceDialog.Form.Close; 
  end;
end;

procedure TJvFindReplace.SetReplaceText(const Value: string);
begin
  FReplaceText := Value;
  if Assigned(FReplaceDialog) then
    FReplaceDialog.ReplaceText := Value;
end;

procedure TJvFindReplace.SetHelpContext(Value: THelpContext);
begin
  FHelpContext := Value;
  UpdateDialogs;
end;

function TJvFindReplace.GetPosition: TPoint;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition
  else
    Result := FFindDialog.Position;
end;

function TJvFindReplace.GetTop: Integer;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition.Y
  else  
    Result := FFindDialog.Position.Y; 
end;

function TJvFindReplace.GetLeft: Integer;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition.X
  else  
    Result := FFindDialog.Position.X; 
end;

function TJvFindReplace.GetOptions: TFindOptions;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FOptions
  else
    Result := FFindDialog.Options;
end;

function TJvFindReplace.GetHelpContext: THelpContext;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FHelpContext
  else
    Result := FFindDialog.HelpContext;
end;

function TJvFindReplace.GetFindText: string;
begin
  if not FShowDialogs or (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FFindText
  else
    Result := FFindDialog.FindText;
end;

function TJvFindReplace.GetReplaceText: string;
begin
  if not FShowDialogs or (csDesigning in ComponentState) or not Assigned(FReplaceDialog) then
    Result := FReplaceText
  else
    Result := FReplaceDialog.ReplaceText;
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

