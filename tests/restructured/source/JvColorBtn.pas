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
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{A color selection button that mimicks the one on the 'Display Properties' page in Win95/NT4 }

unit JvColorBtn;
{
  Bugs:
    If the OtherCaption is set to an empty string, the default '&Other..' magically appears.
    Solution: Set OtherCaption to ' ' instead
}

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, Graphics, Controls,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Menus, JvColorBox;

type
  TJvColorButton = class(TJvCustomDropButton)
  private
    aClrFrm: TForm;
    FIsDown: boolean;
    FColor: TColor;
    FOtherCaption: string;
    FOnChange: TNotifyEvent;
    FOpts: TColorDialogOptions;
    FCustCol: TStrings;
    FEdgeWidth: integer;
    procedure SetEdgeWidth(Value: integer);
    procedure SetOpts(Value: TColorDialogOptions);
    procedure SetCustCol(Value: TStrings);
    procedure SetColor(Value: TColor);
    procedure SetOtherCaption(Value: string);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
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
    property EdgeWidth: integer read FEdgeWidth write SetEdgeWidth default 4;
    property Options: TColorDialogOptions read FOpts write SetOpts;
    property CustomColors: TStrings read FCustCol write SetCustCol;
    property Color: TColor read FColor write SetColor default clBlack;
    property Enabled;
    property Hint;
    property ShowHint;
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

  { TJvColorButton }

constructor TJvColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := false;
  FOpts := [];
  FCustCol := TStringList.Create;
  FColor := clBlack;
  FEdgeWidth := 4;
  Width := 42;
  Height := 21;
  aClrFrm := TJvClrFrm.Create(nil);
  TJvClrFrm(aClrFrm).SetButton(self);
  FOtherCaption := SOtherCaption;
  aClrFrm.Visible := False;
end;

destructor TJvColorButton.Destroy;
begin
  FCustCol.Free;
  if aClrFrm <> nil then
  begin
    aClrFrm.Free;
    aClrFrm := nil;
  end;
  inherited Destroy;
end;

procedure TJvColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled or not Assigned(aClrFrm) then
    Exit;
  with TJvClrFrm(aClrFrm) do
  begin
    CD.Options := FOpts;
    OtherBtn.Caption := FOtherCaption;
    CD.CustomColors.Assign(FCustCol);
    if ArrowWidth = 0 then
    begin
      if CD.Execute then
        FColor := CD.Color;
      MouseUp(mbLeft, [], X, Y);
    end
    else if not aClrFrm.Visible then
      aClrFrm.Show
    else
      aClrFrm.Hide;
    //    ColorSquare21.Color := self.Color;
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
var Rec: TRect;
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

procedure TJvColorButton.SetEdgeWidth(Value: integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Repaint;
  end;
end;

procedure TJvColorButton.SetOpts(Value: TColorDialogOptions);
begin
  if FOpts <> Value then
    FOpts := Value;
end;

procedure TJvColorButton.SetCustCol(Value: TStrings);
begin
  FCustCol.Assign(Value);
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
        aClrFrm.Hide;
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

procedure TJvColorButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

procedure TJvColorButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
end;

end.

