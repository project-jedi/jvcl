{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGraphicButton.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgGraphicButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  JvgTypes, JvgUtils, JvComponent;
  
type
  TJvgButtonState = (bsActive, bsPassive, bsPushed);

  TJvgGraphicButton = class(TJvGraphicControl)
  private
    FAutoSize: Boolean;
    FGlyphActive: TPicture;
    FGlyphPassive: TPicture;
    FGlyphPushed: TPicture;
    FState: TJvgButtonState;
    procedure SetGlyphActive(Value: TPicture);
    procedure SetGlyphPassive(Value: TPicture);
    procedure SetGlyphPushed(Value: TPicture);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Canvas;
  published
    property Enabled;
    property Height default 105;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 105;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property GlyphActive: TPicture read FGlyphActive write SetGlyphActive;
    property GlyphPassive: TPicture read FGlyphPassive write SetGlyphPassive;
    property GlyphPushed: TPicture read FGlyphPushed write SetGlyphPushed;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses
  JvThemes;

constructor TJvgGraphicButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  Width := 105;
  Height := 105;
  IncludeThemeStyle(Self, [csParentBackground]);

  FGlyphActive := TPicture.Create;
  FGlyphPassive := TPicture.Create;
  FGlyphPushed := TPicture.Create;
  //...defaults
  FAutoSize := False;
  FState := bsPassive;
end;

destructor TJvgGraphicButton.Destroy;
begin
  FGlyphActive.Free;
  FGlyphPassive.Free;
  FGlyphPushed.Free;
  inherited Destroy;
end;

procedure TJvgGraphicButton.Paint;
var
  Glyph: TPicture;
begin
  case FState of
    bsActive:
      if Assigned(FGlyphActive) then
        Glyph := FGlyphActive
      else
        Glyph := FGlyphPassive;
    bsPassive:
      Glyph := FGlyphPassive;
  else {bsPushed}
    begin
      if Assigned(FGlyphPushed) then
        Glyph := FGlyphPushed
      else
        Glyph := FGlyphActive;
      if not Assigned(Glyph) then
        Glyph := FGlyphPassive;
    end;
  end;
  if Assigned(Glyph) then
    BitBlt(Canvas.Handle, 0, 0, Glyph.Width, Glyph.Height,
      Glyph.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

end;

procedure TJvgGraphicButton.SetGlyphActive(Value: TPicture);
begin
  FGlyphActive.Assign(Value);
  Invalidate;
end;

procedure TJvgGraphicButton.SetGlyphPassive(Value: TPicture);
begin
  FGlyphPassive.Assign(Value);
  Invalidate;
end;

procedure TJvgGraphicButton.SetGlyphPushed(Value: TPicture);
begin
  FGlyphPushed.Assign(Value);
  Invalidate;
end;

procedure TJvgGraphicButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseEnter(Control);
  FState := bsActive;
  Repaint;
end;

procedure TJvgGraphicButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  FState := bsPassive;
  Repaint;
end;

procedure TJvgGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (not Enabled) or (FState = bsPassive) then
    Exit;
  FState := bsPushed;
  Invalidate;
end;

procedure TJvgGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
//  if FState = bsPushed then
//    Click; // already called
  if FState = bsPushed then
    FState := bsActive
  else
    FState := bsPassive;
  Invalidate;
end;

end.

