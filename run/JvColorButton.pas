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
located at http://jvcl.sourceforge.net

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
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  JvColorBox;

type
  TJvColorButton = class(TJvCustomDropButton)
  private
    FColorForm: TForm;
    FIsDown: Boolean;
    FOtherCaption: string;
    FOnChange: TNotifyEvent;
    FCustomColors: TStringList;
    FEdgeWidth: Integer;
    FColor: TColor;
    {$IFDEF VCL}
    FOptions: TColorDialogOptions;
    procedure SetOptions(Value: TColorDialogOptions);
    {$ENDIF VCL}
    function GetCustomColors: TStrings;
    procedure SetEdgeWidth(Value: Integer);
    procedure SetCustomColors(Value: TStrings);
    procedure SetOtherCaption(Value: string);
    procedure SetColor(const Value: TColor);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure ShowColorPopup(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ArrowWidth;
    property OtherCaption: string read FOtherCaption write SetOtherCaption;
    property EdgeWidth: Integer read FEdgeWidth write SetEdgeWidth default 4;
    {$IFDEF VCL}
    property Options: TColorDialogOptions read FOptions write SetOptions;
    {$ENDIF VCL}
    property CustomColors: TStrings read GetCustomColors write SetCustomColors;
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
  SysUtils, ExtCtrls,
  JvConsts, JvColorForm, JvResources;

constructor TJvColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  {$IFDEF VCL}
  FOptions := [];
  {$ENDIF VCL}
  FCustomColors := TStringList.Create;
  Color := clBlack;
  FEdgeWidth := 4;
  Width := 42;
  Height := 21;
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

procedure TJvColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  ShowColorPopup(Button, Shift, X, Y);
  FIsDown := ArrowWidth <> 0;
  Repaint;
end;

procedure TJvColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FIsDown := False;
  Repaint;
end;

procedure TJvColorButton.ShowColorPopup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button <> mbLeft) or not Enabled or not Assigned(FColorForm) then
    Exit;
  with TJvColorForm(FColorForm) do
  begin
    {$IFDEF VCL}
    ColorDialog.Options := FOptions;
    {$ENDIF VCL}
    OtherBtn.Caption := FOtherCaption;
    ColorDialog.CustomColors.Assign(FCustomColors);
    if ArrowWidth = 0 then
    begin
      if ColorDialog.Execute then
        FColor := ColorDialog.Color;
      MouseUp(mbLeft, [], X, Y);
    end
    else
    if not FColorForm.Visible then
      FColorForm.Show
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

{$IFDEF VCL}
procedure TJvColorButton.SetOptions(Value: TColorDialogOptions);
begin
  if FOptions <> Value then
    FOptions := Value;
end;
{$ENDIF VCL}

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

end.

