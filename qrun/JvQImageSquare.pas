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

The Original Code is: JvImageWindow.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQImageSquare;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QImgList, QForms, QWindows, Types, 
  JvQComponent;

type
  TJvImageSquare = class(TJvGraphicControl)
  private
    FHiColor: TColor;
    FTmpColor: TColor;
    FBackColor: TColor;
    FBorderStyle: TBorderStyle;
    FImageList: TCustomImageList;
    FIndex: Integer;
    FDown: Boolean;
    FShowClick: Boolean;
    FImageChangeLink: TChangeLink;
    procedure SetHiColor(Value: TColor);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetIndex(Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ColorChanged; override;
    procedure PaintFrame; virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color default clWindow;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property HiColor: TColor read FHiColor write SetHiColor default clActiveCaption;
    property Images: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: Integer read FIndex write SetIndex default 0;
    property ShowClick: Boolean read FShowClick write FShowClick default False;
    property Width default 36;
    property Height default 36;

    property Align;
    property Anchors;
    property Action;
    property Text;
    property Visible;
    property Enabled; 
    property DragMode;
    property PopupMenu;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

implementation

uses  
  QExtCtrls, 
  JvQThemes, JvQResources;

//=== { TJvImageSquare } =====================================================

constructor TJvImageSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHiColor := clActiveCaption;
  Color := clWindow;
  FTmpColor := clWindow;
  FBackColor := clWindow;
  FIndex := 0;
  FDown := False;
  FShowClick := False;
  Width := 36;
  Height := 36;
  FBorderStyle := bsSingle;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TJvImageSquare.Destroy;
begin
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageSquare.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure TJvImageSquare.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FImageList) and (Operation = opRemove) then
    FImageList := nil;
end;

procedure TJvImageSquare.PaintFrame;
var
  R: TRect;
begin
  R := GetClientRect;
  if FDown and FShowClick then
  begin
    Frame3D(Canvas, R, cl3DDkShadow, cl3DDkShadow, 1);
    Frame3D(Canvas, R, clBtnHighLight, clBtnHighLight, 1);
    Frame3D(Canvas, R, cl3DDkShadow, cl3DDkShadow, 1);
  end
  else 
  if FBorderStyle = bsSingle then
  begin
    Frame3D(Canvas, R, clBtnFace, clBtnFace, 1);
    Frame3D(Canvas, R, clBtnShadow, clBtnHighLight, 1);
    Frame3D(Canvas, R, cl3DDkShadow, clBtnFace, 1);
  end
  else
    Frame3D(Canvas, R, FHiColor, FHiColor, 3);
end;

procedure TJvImageSquare.Paint;
var
  R: TRect;
  DX, DY: Integer;
begin
  R := Rect(0, 0, Width, Height);

  if FBorderStyle = bsSingle then
  begin
    PaintFrame;
    InflateRect(R, -3, -3);
  end;

  { fill in the rest }
  with Canvas do
  begin
    Brush.Color := FTmpColor;
    Brush.Style := bsSolid;
    FillRect(R);
  end;

  if Assigned(FImageList) then
  begin
    { draw in middle }
    DX := (Width - FImageList.Width) div 2;
    DY := (Height - FImageList.Height) div 2;  
    FImageList.Draw(Canvas, DX, DY, FIndex); 
  end;
end;

procedure TJvImageSquare.SetHiColor(Value: TColor);
begin
  if FHiColor <> Value then
  begin
    FHiColor := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetIndex(Value: Integer);
begin
  if FIndex <> Value then
  begin
    FIndex := Value;
    Repaint;
  end;
end;

procedure TJvImageSquare.SetImageList(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  if Images <> nil then
    FImageList.RegisterChanges(FImageChangeLink);
  Repaint;
end;

procedure TJvImageSquare.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FDown := False;
  if FShowClick then
    PaintFrame;
end;

procedure TJvImageSquare.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FDown := True;
  if FShowClick then
    PaintFrame;
end;

procedure TJvImageSquare.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseEnter(Control);
  if ColorToRGB(FTmpColor) <> ColorToRGB(FHiColor) then
  begin
    FTmpColor := FHiColor;
    Repaint;
  end;
end;

procedure TJvImageSquare.MouseLeave(Control: TControl);
begin
  FDown := False;
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  if ColorToRGB(FTmpColor) <> ColorToRGB(FBackColor) then
  begin
    FTmpColor := FBackColor;
    Repaint;
  end;
end;

procedure TJvImageSquare.ColorChanged;
begin
  inherited ColorChanged;
  FBackColor := Color;
  FTmpColor := Color;
  Repaint;
end;

end.

