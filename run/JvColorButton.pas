{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBtn.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
    If the OtherCaption is set to an empty string, the default '&Other..' magically appears.
    Solution: Set OtherCaption to ' ' instead
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{A color selection button that mimicks the one on the 'Display Properties' page in Win95/NT4 }

unit JvColorBtn;

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, Graphics, Controls,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Menus,
  JvColorBox;

type
  TJvColorButton = class(TJvCustomDropButton)
  private
    FColorForm: TForm;
    FIsDown: Boolean;
    FColor: TColor;
    FOtherCaption: string;
    FOnChange: TNotifyEvent;
    FOptions: TColorDialogOptions;
    FCustomColors: TStrings;
    FEdgeWidth: Integer;
    procedure SetEdgeWidth(Value: Integer);
    procedure SetOptions(Value: TColorDialogOptions);
    procedure SetCustomColors(Value: TStrings);
    procedure SetColor(Value: TColor);
    procedure SetOtherCaption(Value: string);
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ArrowWidth;
    property OtherCaption: string read FOtherCaption write SetOtherCaption;
    property EdgeWidth: Integer read FEdgeWidth write SetEdgeWidth default 4;
    property Options: TColorDialogOptions read FOptions write SetOptions;
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
    property Color: TColor read FColor write SetColor default clBlack;
    property Enabled;
    property Hint;
    property Height default 21;
    property ShowHint;
    property Width default 42;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
  end;

implementation

uses
  JvColorForm;

resourcestring
  SOtherCaption = '&Other...';

constructor TJvColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  FOptions := [];
  FCustomColors := TStringList.Create;
  FColor := clBlack;
  FEdgeWidth := 4;
  Width := 42;
  Height := 21;
  FColorForm := TJvClrFrm.Create(nil);
  TJvClrFrm(FColorForm).SetButton(Self);
  FOtherCaption := SOtherCaption;
  FColorForm.Visible := False;
end;

destructor TJvColorButton.Destroy;
begin
  FCustomColors.Free;
  if FColorForm <> nil then
  begin
    FColorForm.Free;
    FColorForm := nil;
  end;
  inherited Destroy;
end;

procedure TJvColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled or not Assigned(FColorForm) then
    Exit;
  with TJvClrFrm(FColorForm) do
  begin
    CD.Options := FOptions;
    OtherBtn.Caption := FOtherCaption;
    CD.CustomColors.Assign(FCustomColors);
    if ArrowWidth = 0 then
    begin
      if CD.Execute then
        FColor := CD.Color;
      MouseUp(mbLeft, [], X, Y);
    end
    else
    if not FColorForm.Visible then
      FColorForm.Show
    else
      FColorForm.Hide;
    //    ColorSquare21.Color := Self.Color;
  end;
  if ArrowWidth <> 0 then
    FIsDown := True
  else
    FIsDown := False;
  Repaint;
end;

procedure TJvColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FIsDown := False;
  Repaint;
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
    Frame3D(Canvas, Rec, cl3DDkShadow, cl3DDkShadow, 1)
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

procedure TJvColorButton.SetCustomColors(Value: TStrings);
begin
  FCustomColors.Assign(Value);
end;

procedure TJvColorButton.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
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
      if not ((ssAlt in Shift) or (ssCtrl in Shift)) then
        MouseDown(mbLeft, [], 0, 0);
    VK_ESCAPE:
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
      if not ((ssAlt in Shift) or (ssCtrl in Shift)) then
        MouseUp(mbLeft, [], 0, 0);
  end;
  inherited KeyUp(Key, Shift);
end;

procedure TJvColorButton.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
end;

procedure TJvColorButton.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
end;

end.

