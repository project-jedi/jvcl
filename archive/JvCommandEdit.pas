{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCmdEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{A TEdit that displays and reacts to command input. }

unit JvCommandEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus,
  JvEdit, JvComponent, JvTypes;

type
  TJvCmdEvent = procedure(Sender: TObject; Cmd: Integer; Params: TStrings; var Handled: Boolean) of object;

  TJvCustomCommandEdit = class(TJvCustomEdit)
  private
    FShift: TShiftState;
    FKey: Word;
    FSuppress: Boolean;
    FCurrCmd: string;
    FCurrCmdIndex: Integer;
    FPrompt: string;
    FOnCommand: TJvCmdEvent;
    FExecuteKey: TShortCut;
    FHistory: TStrings;
    FCommands: TStrings;
    FArrowKeys: Boolean;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure SetCommands(const Value: TStrings);
    procedure SetCurrCmd(const Value: string);
    procedure SetExecuteKey(const Value: TShortCut);
    procedure SetHistory(const Value: TStrings);
    procedure SetPrompt(const Value: string);
    procedure CheckLimits(var Key: Word);
    procedure ResetHistory;
//    procedure UpdateState;
    function GetCurrCmd: string;
    procedure SetArrowKeys(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoCommand(Cmd: Integer; Params: TStrings): Boolean; virtual;

    property CurrCmd: string read GetCurrCmd write SetCurrCmd;
    property Prompt: string read FPrompt write SetPrompt;
    property History: TStrings read FHistory write SetHistory;
    property Commands: TStrings read FCommands write SetCommands;
    property ExecuteKey: TShortCut read FExecuteKey write SetExecuteKey default VK_RETURN;
    property OnCommand: TJvCmdEvent read FOnCommand write FOnCommand;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GotoCmd(Index: Integer; RelToCurr: Boolean): Boolean;
    function Execute: Boolean;
    function ExecuteCommand(const Cmd: string): Boolean;
  end;

  TJvCommandEdit = class(TJvCustomCommandEdit)
  published
    property CurrCmd;
    property ClipboardCommands;
    property Commands;
    property ExecuteKey;
    property History;
    property Prompt;
    property OnCommand;

    property ArrowKeys;
    property Align;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

// uses
//  utilsString;

constructor TJvCustomCommandEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHistory := TStringList.Create;
  FCommands := TStringList.Create;
  FArrowKeys := True;
  FExecuteKey := VK_RETURN;
  ShortCutToKey(FExecuteKey, FKey, FShift);
  FCurrCmd := '';
  Prompt := ':';
end;

destructor TJvCustomCommandEdit.Destroy;
begin
  FHistory.Free;
  FCommands.Free;
  inherited Destroy;
end;

{
procedure TJvCustomCommandEdit.UpdateState;
begin
  Text := FPrompt;
  FSuppress := False;
  if CanFocus and HasParent then
    SelStart := length(FPrompt);
end;
}

procedure TJvCustomCommandEdit.CheckLimits(var Key: Word);
var
  P: TPoint;
begin
  if SelStart < Length(FPrompt) then
  begin
    Key := 0;
    P.X := SelStart + SelLength;
    P.Y := Length(FPrompt);
    SendMessage(Handle, EM_SETSEL, P.X, P.Y);
    SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TJvCustomCommandEdit.ResetHistory;
begin
  FHistory.Clear;
  FCurrCmdIndex := 0;
end;

{$IFOPT J-}
{$DEFINE RESETJ}
{$ENDIF}
{$J+}

function strtok(Search, Delim: string): string;
const
  I: Integer = 1;
  Len: Integer = 0;
  PrvStr: string = '';
begin
  Result := '';
  if Search <> '' then
  begin
    I := 1;
    PrvStr := Search;
    Len := Length(PrvStr);
  end;
  if PrvStr = '' then
    Exit;
  while (I <= Len) and (Pos(PrvStr[I], Delim) > 0) do
    Inc(I);
  while (I <= Len) and (Pos(PrvStr[I], Delim) = 0) do
  begin
    Result := Result + PrvStr[I];
    Inc(I);
  end;
end;

{$IFDEF RESETJ}
{$UNDEF RESETJ}
{$J-}
{$ENDIF}

function TJvCustomCommandEdit.Execute: Boolean;
var
  Params: TStringList;
  Cmd: string;
begin
  FCurrCmd := Copy(Text, Length(Prompt) + 1, MaxInt);
  Cmd := FCurrCmd;
  Cmd := strtok(Cmd, ' ');
  Params := TStringList.Create;
  try
    while Cmd <> '' do
    begin
      Params.Add(Cmd);
      Cmd := strtok('', ' ');
    end;
    if Params.Count > 0 then
    begin
      Cmd := Params[0];
      Params.Delete(0);
    end
    else
      Cmd := '';
    Result := DoCommand(FCommands.IndexOf(Cmd), Params);
    if Result then
    begin
      FHistory.Add(CurrCmd);
      FCurrCmdIndex := FHistory.Count - 1;
    end;
  finally
    Params.Free;
  end;
//  UpdateState;
end;

function TJvCustomCommandEdit.GotoCmd(Index: Integer;
  RelToCurr: Boolean): Boolean;
begin
  Result := False;
  if RelToCurr then
    Inc(FCurrCmdIndex, Index)
  else
    FCurrCmdIndex := Index;
  if FCurrCmdIndex < 0 then
    FCurrCmdIndex := 0;
  if FCurrCmdIndex >= FHistory.Count then
    FCurrCmdIndex := FHistory.Count - 1;
  Index := FCurrCmdIndex;
  if (Index > -1) and (Index < FHistory.Count) then
  begin
    CurrCmd := History[Index];
    Result := True;
    SelStart := Length(Text) + 1;
  end
  else
    Beep;
end;

procedure TJvCustomCommandEdit.KeyPress(var Key: Char);
begin
  if FSuppress {and (Key = Char(FKey))} then
    Key := #0
  else
  begin
//    CheckLimits(Char(Key));
    ReadOnly := (SelStart < Length(Prompt)) or ((SelStart <= Length(Prompt)) and (Key = #8));
  end;
  inherited KeyPress(Key);
  FSuppress := False;
end;

procedure TJvCustomCommandEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  CheckLimits(Key);
  ReadOnly := (SelStart < Length(Prompt)) or ((SelStart <= Length(Prompt)) and (Key = VK_BACK));
  if (FArrowKeys and (Shift = []) and (Key in [VK_UP, VK_DOWN, VK_F3, VK_ESCAPE])) or
    ((Key = FKey) and (FShift = Shift)) or ((Key = VK_LEFT) and (SelStart + SelLength <= Length(Prompt))) then
  begin
//    FSuppress := True;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomCommandEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  K2: Word;
begin
  CheckLimits(Key);
  K2 := Key;
  Key := 0;
  ReadOnly := (SelStart + SelLength < Length(Prompt)) or ((SelStart + SelLength <= Length(Prompt)) and (Key = VK_BACK));
  if (K2 = FKey) and (FShift = Shift) then
    Execute
  else
  if FArrowKeys and (Shift = []) then
  begin
    FSuppress := True;
    case K2 of
      VK_UP:
        GotoCmd(-1, True);
      VK_DOWN:
        GotoCmd(1, True);
      VK_F3:
        GotoCmd(FHistory.Count - 1, False);
      VK_ESCAPE:
        CurrCmd := '';
    else
      begin
        Key := K2;
        FSuppress := False;
        inherited KeyUp(Key, Shift);
      end
    end
  end
  else
  begin
    FSuppress := False;
    Key := K2;
    inherited KeyUp(Key, Shift);
  end;
end;

procedure TJvCustomCommandEdit.SetCommands(const Value: TStrings);
begin
  FCommands.Assign(Value);
  ResetHistory;
end;

procedure TJvCustomCommandEdit.SetCurrCmd(const Value: string);
begin
  FCurrCmd := Value;
  Text := FPrompt + FCurrCmd;
  if CanFocus and HasParent then
    SelStart := Length(Prompt) + 1;
end;

procedure TJvCustomCommandEdit.SetExecuteKey(const Value: TShortCut);
begin
  if FExecuteKey <> Value then
  begin
    FExecuteKey := Value;
    ShortCutToKey(FExecuteKey, FKey, FShift);
  end;
end;

procedure TJvCustomCommandEdit.SetHistory(const Value: TStrings);
begin
  FHistory.Assign(Value);
end;

procedure TJvCustomCommandEdit.SetPrompt(const Value: string);
begin
  FPrompt := Value;
  Text := FPrompt + CurrCmd;
end;

procedure TJvCustomCommandEdit.WMCopy(var Msg: TWMCopy);
var
  Key: Word;
begin
  if caCopy in ClipboardCommands then
  begin
    Key := 0;
    CheckLimits(Key);
    inherited;
  end;
end;

procedure TJvCustomCommandEdit.WMCut(var Msg: TWMCut);
var
  Key: Word;
begin
  if caCut in ClipboardCommands then
  begin
    Key := 0;
    CheckLimits(Key);
    inherited;
  end;
end;

procedure TJvCustomCommandEdit.WMPaste(var Msg: TWMPaste);
var
  Key: Word;
begin
  if caPaste in ClipboardCommands then
  begin
    Key := 0;
    CheckLimits(Key);
    inherited;
  end;
end;

function TJvCustomCommandEdit.DoCommand(Cmd: Integer; Params: TStrings): Boolean;
begin
  Result := True;
//  FSuppress := True;
  if Assigned(FOnCommand) then
    FOnCommand(Self, Cmd, Params, Result);
end;

procedure TJvCustomCommandEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Key: Word;
begin
  inherited MouseDown(Button, Shift, X, y);
  CheckLimits(Key);
end;

procedure TJvCustomCommandEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Key: Word;
begin
  inherited MouseUp(Button, Shift, X, Y);
  CheckLimits(Key);
end;

function TJvCustomCommandEdit.GetCurrCmd: string;
begin
  Result := Copy(Text, Length(FPrompt) + 1, MaxInt);
end;

procedure TJvCustomCommandEdit.SetArrowKeys(const Value: Boolean);
begin
  if FArrowKeys <> Value then
    FArrowKeys := Value;
end;

function TJvCustomCommandEdit.ExecuteCommand(const Cmd: string): Boolean;
begin
  CurrCmd := Cmd;
  Result := Execute;
end;

end.

