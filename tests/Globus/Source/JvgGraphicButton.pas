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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgGraphicButton;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  JVComponent,
  Graphics,
  controls,
  JvgTypes,
  JvgUtils;

type

  TGButtonState = (bsActive, bsPassive, bsPushed);

  TJvgGraphicButton = class(TJvGraphicControl)
  private
    FAutoSize: boolean;
    FGlyphActive: TBitmap;
    FGlyphPassive: TBitmap;
    FGlyphPushed: TBitmap;
    State: TGButtonState;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure SetGlyphActive(Value: TBitmap);
    procedure SetGlyphPassive(Value: TBitmap);
    procedure SetGlyphPushed(Value: TBitmap);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    //    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    procedure Paint; override;
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property GlyphActive: TBitmap read FGlyphActive write SetGlyphActive;
    property GlyphPassive: TBitmap read FGlyphPassive write SetGlyphPassive;
    property GlyphPushed: TBitmap read FGlyphPushed write SetGlyphPushed;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write
      FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write
      FOnMouseLeave;
  end;

implementation

//*****************************************_____________LowLevel METHODS
//________________________________________________________

constructor TJvgGraphicButton.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
    {inherited } Width := 105;
  {inherited } Height := 105;

  FGlyphActive := TBitmap.create;
  FGlyphPassive := TBitmap.create;
  FGlyphPushed := TBitmap.create;
  //...defaults
  FAutoSize := false;
  State := bsPassive;
end;
//________________________________________________________

destructor TJvgGraphicButton.Destroy;
begin
  FGlyphActive.Free;
  FGlyphPassive.Free;
  FGlyphPushed.Free;
  inherited;
end;
//________________________________________________________

procedure TJvgGraphicButton.Paint;
var
  Glyph: TBitmap;
begin
  case State of
    bsActive: if Assigned(FGlyphActive) then
        Glyph := FGlyphActive
      else
        Glyph := FGlyphPassive;
    bsPassive: Glyph := FGlyphPassive;
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
      Glyph.Canvas.Handle, 0, 0, SRCCOPY);
  if (csDesigning in ComponentState) and (tag <> 9999) then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, width, height);
    end;

end;
//________________________________________________________

procedure TJvgGraphicButton.SetGlyphActive(Value: TBitmap);
begin
  FGlyphActive.Assign(Value);
  Repaint;
end;

procedure TJvgGraphicButton.SetGlyphPassive(Value: TBitmap);
begin
  FGlyphPassive.Assign(Value);
  Repaint;
end;

procedure TJvgGraphicButton.SetGlyphPushed(Value: TBitmap);
begin
  FGlyphPushed.Assign(Value);
  Repaint;
end;

//________________________________________________________

procedure TJvgGraphicButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  State := bsActive;
  Paint;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TJvgGraphicButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  State := bsPassive;
  Paint;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

procedure TJvgGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or (not Enabled) or (State = bsPassive) then
    exit;
  State := bsPushed;
  Paint;
end;

procedure TJvgGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (State = bsPushed) and Assigned(OnClick) then
    OnClick(self);
  if State = bsPushed then
    State := bsActive
  else
    State := bsPassive;
  Paint;
end;

end.
