{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindReplace.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Wrapper for the TFind / TReplace dialogs and a stand-alone full
     text search engine with support for all available dialog options:
     Search up/down, whole word only, case sensitive, replace all etc. }

unit JvFindReplace;

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Controls, Dialogs, StdCtrls, JvComponent;

type
  TJvReplaceProgressEvent = procedure(Sender: TObject; Position: integer; var Terminate: boolean) of object;
  TJvFindReplace = class(TJvComponent)
  private
    { Private declarations }
    FOnFind: TNotifyEvent;
    FOnReplace: TNotifyEvent;
    FOnReplacingAll: TNotifyEvent;
    FOnReplacedAll: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnNotFound: TNotifyEvent;
    FOnProgress: TJvReplaceProgressEvent;
    FEdit: TCustomEdit;
    FOwner: TComponent;
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FOptions: TFindOptions;
    FPosition: TPoint;
    FLeft: integer;
    FTop: integer;
    FCtl3D: boolean;
    FFast: boolean;
    FHelpContext: THelpContext;
    FShowDialogs: boolean;
    FKeepText: boolean;
    FFindText: string;
    FReplaceText: string;
    procedure SetPosition(Value: TPoint);
    procedure SetDialogTop(Value: integer);
    procedure SetDialogLeft(Value: integer);
    procedure SetOptions(Value: TFindOptions);
    procedure SetEditControl(Value: TCustomEdit);
    procedure SetCtl3D(Value: boolean);
    procedure SetHelpContext(Value: THelpContext);
    procedure SetFindText(Value: string);
    procedure SetReplaceText(Value: string);
    procedure SetShowDialogs(Value: boolean);
    function GetPosition: TPoint;
    function GetTop: integer;
    function GetLeft: integer;
    function GetOptions: TFindOptions;
    function GetCtl3D: boolean;
    function GetHelpContext: THelpContext;
    function GetFindText: string;
    function GetReplaceText: string;
    function ReplaceOne(Sender: TObject): boolean;
    procedure UpdateDialogs;
    procedure NeedDialogs;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoOnFind(Sender: TObject); virtual;
    procedure DoOnReplace(Sender: TObject); virtual;
    procedure DoOnShow(Sender: TObject); virtual;
    procedure DoOnClose(Sender: TObject); virtual;
    procedure DoFailed(Sender: TObject); virtual;
    procedure DoReplacingAll; virtual;
    procedure DoReplacedAll; virtual;
    procedure DoProgress(Position: integer; var Terminate: boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Find; virtual;
    procedure FindAgain; virtual;
    procedure Replace; virtual;
    procedure ReplaceAll(const S, R: string); virtual;
    property Position: TPoint read GetPosition write SetPosition;
    property Top: integer read GetTop write SetDialogTop default -1;
    property Left: integer read GetLeft write SetDialogLeft default -1;
  published
    { Published declarations }
    property Fast: boolean read FFast write FFast default False;
    property Options: TFindOptions read GetOptions write SetOptions;
    property EditControl: TCustomEdit read FEdit write SetEditControl;
    property FindText: string read GetFindText write SetFindText;
    property KeepText: boolean read FKeepText write FKeepText default False;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ShowDialogs: boolean read FShowDialogs write SetShowDialogs default True;
    property Ctl3D: boolean read GetCtl3D write SetCtl3D default True;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext default 0;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
    property OnReplacingAll: TNotifyEvent read FOnReplacingAll write FOnReplacingAll;
    property OnReplacedAll: TNotifyEvent read FOnReplacedAll write FOnReplacedAll;
    property OnNotFound: TNotifyEvent read FOnNotFound write FOnNotFound;
    property OnProgress: TJvReplaceProgressEvent read FOnProgress write FOnProgress;
  end;

resourcestring
  SNotFound = 'Search string ''%s'' not found';
  SReplaceCap = 'Replace';
  SFindCap = 'Find';
  SNoEditAssigned = 'No edit control assigned!';

implementation

type
  TFoundText = record
    StartAt: longint;
    EndAt: longint;
    isWhole: boolean;
    isSameCase: boolean;
  end;

procedure Error;
begin
  Exception.Create(SNoEditAssigned);
end;

{ utility }

function IsValidWholeWord(S: string): boolean;
const AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
begin
  Result := not ((S[1] in AlphaNum) or (S[Length(S)] in AlphaNum));
end;

{ invert string }

function StrRev(S: string): string;
var atEnd, atStart: integer;
begin
  Result := S;
  atEnd := Length(S);
  atStart := 1;

  while (atEnd >= 1) do
  begin
    S[atStart] := Char(integer(S[atStart]) xor integer(Result[atEnd]));
    Result[atEnd] := Char(integer(Result[atEnd]) xor integer(S[atStart]));
    //      S[atStart] := Char(integer(S[atStart]) xor integer(Result[atEnd])); // reset to normal
    Inc(atStart);
    Dec(atEnd);
  end;
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

{ Pascal adaption of a function originally in C }

function BoyerMoore(SubStr, S: PChar): integer;
var
  CharJump, MatchJump, BackUp: array[0..255] of integer;
  PatLen, TextLen, u, uA, uB, uText, uPat: integer;
begin
  Result := 0;
  PatLen := StrLen(SubStr);
  TextLen := StrLen(S);

  FillChar(CharJump, 256 * sizeof(integer), 0);

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

function FindInText(Text, Search: string; FromPos, ToPos: integer; Fast: boolean): TFoundText;
var Found: integer; S: string;
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
    S := Copy(Text, Result.StartAt - 1, Result.EndAt + 3);
    Result.isWhole := IsValidWholeWord(S);
    S := Copy(S, 3, Length(S) - 3);
    Result.isSameCase := (AnsiCompareStr(Search, S) = 0);
  end;
end;

{ invert and search }

function FindInTextRev(Text, Search: string; FromPos, ToPos: integer; Fast: boolean): TFoundText;
begin
  Result := FindInText(StrRev(Text), StrRev(Search), FromPos, ToPos, Fast);
  if Result.StartAt > -1 then
    Result.StartAt := Length(Text) - Result.StartAt - Result.EndAt;
end;

{ public methods }

constructor TJvFindReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FHelpContext := 0;
  FKeepText := False;
  FCtl3D := True;
  FShowDialogs := True;
  FPosition := Point(-1, -1);
end;

destructor TJvFindReplace.Destroy;
begin
  inherited Destroy;
end;

procedure TJvFindReplace.Find;
begin
  if not Assigned(FEdit) then
    Error;
  UpdateDialogs;
  if FShowDialogs then
    FFindDialog.Execute
  else
    DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.FindAgain;
begin
  if not Assigned(FEdit) then
    Error;
  UpdateDialogs;
  DoOnFind(FFindDialog);
end;

procedure TJvFindReplace.Replace;
begin
  if not Assigned(FEdit) then
    Error;
  UpdateDialogs;

  if FShowDialogs then
    FReplaceDialog.Execute
  else
    DoOnReplace(FReplaceDialog);
end;

procedure TJvFindReplace.ReplaceAll;
var Txt: string;
  FoundPos: TFoundText;
  tmpOptions: TFindOptions;
  SLen, RLen, TLen: integer;
  Terminate: boolean;
begin
  if not Assigned(FEdit) then
    Error;
  Terminate := false;
  tmpOptions := FReplaceDialog.Options;
  Txt := FEdit.Text;
  SLen := Length(S);
  RLen := Length(FReplaceText);
  TLen := Length(Txt);
  FoundPos := FindInText(Txt, FFindText, 0, TLen, true);
  DoReplacingAll;
  while FoundPos.StartAt > -1 do
  begin
    if (frWholeWord in tmpOptions) and not FoundPos.isWhole then
      Continue;
    if (frMatchCase in tmpOptions) and not FoundPos.isSameCase then
      Continue;
    Delete(Txt, FoundPos.StartAt + 1, SLen);
    Insert(FReplaceText, Txt, FoundPos.StartAt + 1);
    FoundPos := FindInText(Txt, FFindText, FoundPos.StartAt + RLen + 1, TLen + (RLen - SLen), true);
    if FoundPos.StartAt mod 60 = 0 then
    begin
      DoProgress(FoundPos.StartAt, Terminate);
      if Terminate then
        Exit;
    end;
  end;
  DoReplacedAll;
  FEdit.Text := Txt;
  DoFailed(FReplaceDialog);
end;

function TJvFindReplace.ReplaceOne(Sender: TObject): boolean;
var Equal: integer; S, R: string;
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
    Equal := AnsiCompareStr(FEdit.SelText, S)
  else
    Equal := AnsiCompareText(FEdit.SelText, S);

  if Equal = 0 then
  begin
    Result := True;
    FEdit.SelText := R;
    FEdit.SetFocus;
  end;
end;

{ protected }

procedure TJvFindReplace.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FEdit) then
    FEdit := nil;
end;

{ private }

procedure TJvFindReplace.NeedDialogs;
begin
  if not Assigned(FFindDialog) then
  begin
    FFindDialog := TFindDialog.Create(self);
    FFindDialog.OnFind := DoOnFind;
  end;
  if not Assigned(FReplaceDialog) then
  begin
    FReplaceDialog := TReplaceDialog.Create(self);
    FReplaceDialog.OnFind := DoOnFind;
    FReplaceDialog.OnReplace := DoOnReplace;
  end;
end;

procedure TJvFindReplace.UpdateDialogs;
begin
  NeedDialogs;

  FFindDialog.Position := GetPosition;
  FFindDialog.Top := GetTop;
  FFindDialog.Left := GetLeft;
  FFindDialog.Options := FOptions;
  FFindDialog.Ctl3D := GetCtl3D;
  FFindDialog.HelpContext := GetHelpContext;
  FFindDialog.FindText := GetFindText;
  FReplaceDialog.Position := GetPosition;

  FReplaceDialog.Top := GetTop;
  FReplaceDialog.Left := GetLeft;
  FReplaceDialog.Options := FOptions;
  FReplaceDialog.Ctl3D := GetCtl3D;
  FReplaceDialog.HelpContext := GetHelpContext;
  FReplaceDialog.FindText := GetFindText;
  FReplaceDialog.ReplaceText := GetReplaceText;
end;

{ event handlers }

procedure TJvFindReplace.DoOnFind(Sender: TObject);
var FoundPos: TFoundText; S: string; Offset: integer;
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
  if (FEdit.SelStart = 0) and (FEdit.SelLength < 1) then
    Offset := 0
  else
    Offset := 1;
  if not (frDown in TFindDialog(Sender).Options) then
    FoundPos := FindInTextRev(FEdit.Text, S, Length(FEdit.Text) - FEdit.SelStart + Offset, Length(FEdit.Text), FFast)
  else
    FoundPos := FindInText(FEdit.Text, S, FEdit.SelStart + Offset, Length(FEdit.Text), FFast);

  if FoundPos.StartAt > -1 then
  begin
    if (frWholeWord in TFindDialog(Sender).Options) and not FoundPos.isWhole then
      DoFailed(Sender)
    else if (frMatchCase in TFindDialog(Sender).Options) and not FoundPos.isSameCase then
      DoFailed(Sender)
    else
    begin
      FEdit.SetFocus;
      FEdit.SelStart := FoundPos.StartAt;
      FEdit.SelLength := FoundPos.EndAt;
      SendMessage(FEdit.Handle, EM_SCROLLCARET, 0, 0);
      if Assigned(FOnFind) then
        FOnFind(self);
    end
  end
  else
    DoFailed(Sender);
end;

procedure TJvFindReplace.DoOnReplace(Sender: TObject);
begin
  if FEdit.SelLength < 1 then
    DoOnFind(Sender);
  if FEdit.SelLength < 1 then
    Exit;

  if (frReplaceAll in TFindDialog(Sender).Options) then
  begin
    SetFindText(FReplaceDialog.FindText);
    SetReplaceText(FReplaceDialog.ReplaceText);
    ReplaceAll(FFindText, FReplaceText);
    {    while ReplaceOne(Sender) do
          DoOnFind(Sender);}
  end
  else
    ReplaceOne(Sender);
  if Assigned(FOnReplace) then
    FOnReplace(self);
end;

procedure TJvFindReplace.DoOnShow(Sender: TObject);
begin
  if not Assigned(FEdit) then
    Error;
  UpdateDialogs;
  if Assigned(FOnShow) then
    FOnShow(self);
end;

procedure TJvFindReplace.DoOnClose(Sender: TObject);
begin
  if not Assigned(FEdit) then
    Error;
  UpdateDialogs;
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure TJvFindReplace.DoFailed(Sender: TObject);
var FCaption: string;
begin
  if not Assigned(FEdit) then
    Error;
  if Assigned(FOnNotFound) then
    FOnNotFound(self);
  if not FShowDialogs then
    Exit;

  if Sender = FReplaceDialog then
    FCaption := SReplaceCap
  else
    FCaption := SFindCap;

  MessageBox(TFindDialog(Sender).Handle, PChar(Format(SNotFound, [TFindDialog(Sender).FindText])),
    PChar(FCaption), MB_OK or MB_ICONINFORMATION);
end;

procedure TJvFindReplace.DoReplacingAll;
begin
  if Assigned(FonReplacingAll) then
    FOnReplacingAll(self);
end;

procedure TJvFindReplace.DoReplacedAll;
begin
  if Assigned(FonReplacedAll) then
    FOnReplacedAll(Self);
end;

procedure TJvFindReplace.DoProgress(Position: integer; var Terminate: boolean);
begin
  if Assigned(FonProgress) then
    FOnProgress(self, Position, Terminate);
end;

{ property access methods }

{ set methods }

procedure TJvFindReplace.SetPosition(Value: TPoint);
begin
  FPosition := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetDialogTop(Value: integer);
begin
  FTop := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetDialogLeft(Value: integer);
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
  FEdit := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvFindReplace.SetCtl3D(Value: boolean);
begin
  FCtl3D := Value;
  UpdateDialogs;
end;

procedure TJvFindReplace.SetFindText(Value: string);
begin
  FFindText := Value;
  if Assigned(FFindDialog) and Assigned(FReplaceDialog) then
  begin
    FFindDialog.FindText := Value;
    FReplaceDialog.FindText := Value;
  end;
end;

procedure TJvFindReplace.SetShowDialogs(Value: boolean);
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

procedure TJvFindReplace.SetReplaceText(Value: string);
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

{ get methods }

function TJvFindReplace.GetPosition: TPoint;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition
  else
    Result := FFindDialog.Position;
end;

function TJvFindReplace.GetTop: integer;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition.Y
  else
    Result := FFindDialog.Top;
end;

function TJvFindReplace.GetLeft: integer;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FPosition.X
  else
    Result := FFindDialog.Left;

end;

function TJvFindReplace.GetOptions: TFindOptions;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FOptions
  else
    Result := FFindDialog.Options;

end;

function TJvFindReplace.GetCtl3D: boolean;
begin
  if (csDesigning in ComponentState) or not Assigned(FFindDialog) then
    Result := FCtl3D
  else
    Result := FFindDialog.Ctl3D;
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

end.

