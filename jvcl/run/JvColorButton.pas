{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A color selection button that mimicks the one on the 'Display Properties' page in Win95/NT4

Known Issues:
    If the OtherCaption is set to an empty string, the default '&Other..' magically appears.
    Solution: Set OtherCaption to ' ' instead
-----------------------------------------------------------------------------}
// $Id$

unit JvColorButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  JvConsts, JvColorBox, JvComponent;

type
  TJvColorButtonPaletteShowing = procedure(var CanShowPalette: Boolean) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvColorButton = class(TJvCustomDropButton)
  private
    FColorForm: TJvForm;
    FIsDown: Boolean;
    FOtherCaption: string;
    FOnChange: TNotifyEvent;
    FCustomColors: TStringList;
    FEdgeWidth: Integer;
    FColor: TColor;
    FButtonShowsPalette: Boolean;
    FOnPaletteShowing: TJvColorButtonPaletteShowing;
  protected
    FOptions: TColorDialogOptions;
    procedure SetOptions(Value: TColorDialogOptions);
    function GetCustomColors: TStrings;
    procedure SetEdgeWidth(Value: Integer);
    procedure SetCustomColors(Value: TStrings);
    procedure SetOtherCaption(Value: string);
    procedure SetColor(const Value: TColor);
  protected
    procedure CMPopupCloseUp(var Msg: TMessage); message CM_POPUPCLOSEUP;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure ShowColorPopup(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure PopupCloseUp; dynamic;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure DoPaletteShowing(var CanShowPalette: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ArrowWidth;
    property OtherCaption: string read FOtherCaption write SetOtherCaption;
    property EdgeWidth: Integer read FEdgeWidth write SetEdgeWidth default 4;
    property ButtonShowsPalette: Boolean read FButtonShowsPalette write FButtonShowsPalette default True;
    property Options: TColorDialogOptions read FOptions write SetOptions;
    property CustomColors: TStrings read GetCustomColors write SetCustomColors;
    property Color: TColor read FColor write SetColor default clBlack;
    property Height default 21;
    property Width default 42;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPaletteShowing: TJvColorButtonPaletteShowing read FOnPaletteShowing write FOnPaletteShowing;

    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
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
  SysUtils, ExtCtrls,
  JvColorForm, JvResources;

constructor TJvColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FOptions := [];
  FCustomColors := TStringList.Create;
  Color := clBlack;
  FEdgeWidth := 4;
  Width := 42;
  Height := 21;
  FButtonShowsPalette := True;
  FColorForm := TJvColorForm.CreateNew(Self);
  TJvColorForm(FColorForm).SetButton(Self);
  FOtherCaption := RsOtherCaption;
  FColorForm.Visible := False;
end;

destructor TJvColorButton.Destroy;
begin
  FCustomColors.Free;
  FreeAndNil(FColorForm);
  inherited Destroy;
end;

procedure TJvColorButton.DoPaletteShowing(var CanShowPalette: Boolean);
begin
  if Assigned(OnPaletteShowing) then
    OnPaletteShowing(CanShowPalette);
end;

procedure TJvColorButton.FocusKilled(NextWnd: THandle);
var
  Sender: TWinControl;
  Focused: Boolean;
begin
  inherited FocusKilled(NextWnd);
  Focused := Screen.ActiveControl <> Self;
  if not Focused then
  begin
    Sender := FindControl(NextWnd);
    if (Sender <> Self) and (Sender <> FColorForm) and
      Assigned(FColorForm) and not FColorForm.ContainsControl(Sender) then
    begin
      { MSDN : While processing this message (WM_KILLFOCUS), do not make any
               function calls that display or activate a window.
      }
      PostMessage(Handle, CM_POPUPCLOSEUP, 0, 0);
    end;
  end;
end;

procedure TJvColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CanShowPalette: Boolean;
begin
  SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
  if FButtonShowsPalette or (X > Width - ArrowWidth) then
  begin
    CanShowPalette := True;
    DoPaletteShowing(CanShowPalette);
    if CanShowPalette then
    begin
      ShowColorPopup(Button, Shift, X, Y);
      FIsDown := True;
    end;
  end;
  Repaint;
end;

procedure TJvColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FIsDown := False;
  Repaint;
end;

procedure TJvColorButton.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FColorForm) and
     Assigned(FColorForm) and not FColorForm.ContainsControl(Msg.Sender) then
    PopupCloseUp;
end;

procedure TJvColorButton.CMPopupCloseUp(var Msg: TMessage);
begin
  PopupCloseUp;
end;

procedure TJvColorButton.PopupCloseUp;
begin
  FColorForm.Hide;
end;

procedure TJvColorButton.ShowColorPopup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button <> mbLeft) or not Enabled or not Assigned(FColorForm) then
    Exit;
  with TJvColorForm(FColorForm) do
  begin
    ColorDialog.Options := FOptions;
    OtherBtn.Caption := FOtherCaption;
    ColorDialog.Color := FColor;
    ColorDialog.CustomColors.Assign(FCustomColors);
    if ArrowWidth = 0 then
    begin
      if ColorDialog.Execute then
      begin
        FColor := ColorDialog.Color;
        FCustomColors.Assign(ColorDialog.CustomColors);
      end;
      MouseUp(mbLeft, [], X, Y);
    end
    else
    if not FColorForm.Visible then
      FColorForm.ShowNoActivate(True)
    else
      FColorForm.Hide;
    //    ColorSquare21.Color := Self.Color;
  end;
end;

procedure TJvColorButton.Paint;
var
  Rec: TRect;
begin
  inherited Paint;
  { draw the colorsquare }
  Rec := ClientRect;
  Rec.Right := Rec.Right - ArrowWidth;
  InflateRect(Rec, -FEdgeWidth, -FEdgeWidth);
  if FIsDown then
    OffsetRect(Rec, 1, 1);

  if Enabled then
  begin
    Frame3D(Canvas, Rec, cl3DDkShadow, cl3DDkShadow, 1);
    Canvas.Brush.Style := bsSolid;
  end
  else
  begin
    Frame3D(Canvas, Rec, clBtnShadow, clBtnHighLight, 1);
    Canvas.Brush.Style := bsBDiagonal;
  end;
  Canvas.Brush.Color := FColor;
  Canvas.FillRect(Rec);
end;

procedure TJvColorButton.SetEdgeWidth(Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Repaint;
  end;
end;


procedure TJvColorButton.SetOptions(Value: TColorDialogOptions);
begin
  if FOptions <> Value then
    FOptions := Value;
end;


function TJvColorButton.GetCustomColors: TStrings;
begin
  Result := FCustomColors;
end;

procedure TJvColorButton.SetCustomColors(Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;

procedure TJvColorButton.SetOtherCaption(Value: string);
begin
  FOtherCaption := Value;
  Repaint;
end;

procedure TJvColorButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN, VK_SPACE:
      // (rom) accept Shift key pressed at max
      if Shift * KeyboardShiftStates <= [ssShift] then
        MouseDown(mbLeft, [], 0, 0);
    VK_ESCAPE:
      // (rom) only accept without Shift, Alt or Ctrl down
      if Shift * KeyboardShiftStates = [] then
      begin
        FColorForm.Hide;
        Key := 0;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvColorButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN, VK_SPACE:
      // (rom) accept Shift key pressed at max
      if Shift * KeyboardShiftStates <= [ssShift] then
        MouseUp(mbLeft, [], 0, 0);
  end;
  inherited KeyUp(Key, Shift);
end;

procedure TJvColorButton.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
