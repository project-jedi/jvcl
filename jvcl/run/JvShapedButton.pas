{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShapedButton.PAS, released on 2002-11-12.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvShapedButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  JvThemes, JvExControls, JvExStdCtrls;

type
  TJvButtonShapes = (jvSLeftArrow, jvRightArrow, jvSRound, jvSHex, jvSOctagon, jvSPar,
    jvSDiamond, jvSTriangleUp, jvSTriangleDown, jvSTriangleLeft,
    jvSTriangleRight, jvSPentagon, JvSRevPentagon, jvSRing);

  TJvShapedButton = class(TJvExButton, IJvDenySubClassing)
  private
    bm: TBitmap;
    IsFocused: Boolean;
    IsHot: Boolean;
    FCanvas: TCanvas;
    FHotColor: TColor;
    FFlat: Boolean;
    FFlatBorderColor: TColor;
    FButtonShape: TJvButtonShapes;
    xp, yp: Integer;
    FFlatArrow: Boolean;
    FAntiAlias: Boolean;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure SetHotColor(const Value: TColor);
    procedure SetFlat(const Value: Boolean);
    procedure SetFlatBorderColor(const Value: TColor);
    procedure SetButtonShape(const Value: TJvButtonShapes);
    procedure CNDrawItemOctagon(var Msg: TWMDrawItem);
    procedure CNDrawItemTriangleDown(var Msg: TWMDrawItem);
    procedure CNDrawItemTriangleLeft(var Msg: TWMDrawItem);
    procedure CNDrawItemTriangleRight(var Msg: TWMDrawItem);
    procedure CNDrawItemTriangleUp(var Msg: TWMDrawItem);
    procedure CNDrawItemPar(var Msg: TWMDrawItem);
    procedure calcpentagon(AWidth, Aheight: Integer);
    procedure SetFlatArrow(const Value: Boolean);
    procedure CNDrawItemLeftArrow(var Msg: TWMDrawItem);
    procedure CNDrawItemRightArrow(var Msg: TWMDrawItem);
    procedure CNDrawItemRing(var Msg: TWMDrawItem);
    procedure CNDrawItemRound(var Msg: TWMDrawItem);
    procedure CNDrawItemPentagon(var Msg: TWMDrawItem);
    procedure CNDrawItemRevPentagon(var Msg: TWMDrawItem);
    procedure CNDrawItemHex(var Msg: TWMDrawItem);
    procedure CNDrawItemDiamond(var Msg: TWMDrawItem);
    procedure SetButton(ALeft, ATop, AWidth, AHeight: Integer);
    procedure DoAntiAlias(Bmp:TBitmap);
    procedure SetAntiAlias(const Value: Boolean);
  protected
    procedure SetRegionOctagon(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionTriangleDown(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionTriangleUp(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionTriangleLeft(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionTriangleRight(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionPar(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionLeftArrow(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionRightArrow(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionRound(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionHex(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionDiamond(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionPentagon(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionRevPentagon(ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRegionRing(ALeft, ATop, AWidth, AHeight: Integer);

    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure MouseLeave(Control: TControl); override;
    procedure MouseEnter(Control: TControl); override;
    procedure FontChanged; override;
    procedure EnabledChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ButtonShape: TJvButtonShapes read FButtonShape write SetButtonShape;
    property Color;
    property AntiAlias: Boolean read FAntiAlias write SetAntiAlias default False;
    property HotColor: TColor read FHotColor write SetHotColor;
    property Flat: Boolean read FFlat write SetFlat;
    property FlatBorderColor: TColor read FFlatBorderColor write SetFlatBorderColor;
    property FlatArrow: Boolean read FFlatArrow write SetFlatArrow;
    property Width default 65;
    property Height default 65;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
  end;

implementation

uses
  Math,
  JvJCLUtils;

constructor TJvShapedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAntiAlias := false;
  bm := Tbitmap.create;
  SetBounds(Left, Top, 65, 65);
  FCanvas := TCanvas.Create;
  FHotColor := clBlue;
  FFlatborderColor := clWhite;
  FButtonShape := jvSTriangleUp; //TODO: Change to Left Arrow
end;

destructor TJvShapedButton.Destroy;
begin
  inherited Destroy;
  bm.Free;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvShapedButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or bs_OwnerDraw;
end;

procedure TJvShapedButton.CreateWnd;
begin
  inherited CreateWnd;
  SetButton(Left, Top, Width, Height);
end;

procedure TJvShapedButton.SetButton(ALeft, ATop,AWidth, AHeight: Integer);
begin
  if HandleAllocated then
  begin
    case FButtonShape of
      jvSLeftArrow: SetRegionLeftArrow(aLeft, aTop, aWidth, aHeight);
      jvRightArrow: SetRegionRightArrow(aLeft, aTop, aWidth, aHeight);
      jvSRound: SetRegionRound(aLeft, aTop, aWidth, aHeight);
      jvSHex: SetRegionHex(aLeft, aTop, aWidth, aHeight);
      jvSOctagon: SetRegionOctagon(aLeft, aTop, aWidth, aHeight);
      jvSPar: SetRegionPar(aLeft, aTop, aWidth, aHeight);
      jvSDiamond: SetRegionDiamond(aLeft, aTop, aWidth, aHeight);
      jvSTriangleUp: SetRegionTriangleUp(aLeft, aTop, aWidth, aHeight);
      jvSTriangleDown: SetRegionTriangleDown(aLeft, aTop, aWidth, aHeight);
      jvSTriangleLeft: SetRegionTriangleLeft(aLeft, aTop, aWidth, aHeight);
      jvSTriangleRight: SetRegionTriangleRight(aLeft, aTop, aWidth, aHeight);
      jvSPentagon: SetRegionPentagon(aLeft, aTop, aWidth, aHeight);
      JvSRevPentagon: SetRegionRevPentagon(aLeft, aTop, aWidth, aHeight);
      jvSRing: SetRegionRing(aLeft, aTop, aWidth, aHeight);
    end;
  end;
end;

procedure TJvShapedButton.SetBounds(ALeft, ATop,
  AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetButton(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvShapedButton.CNDrawItem(var Msg: TWMDrawItem);
begin
  case FButtonShape of
    jvSLeftArrow: CNDrawItemLeftArrow(Msg);
    jvRightArrow: CNDrawItemRightArrow(Msg);
    jvSRound: CNDrawItemRound(Msg);
    jvSHex: CNDrawItemHex(Msg);
    jvSOctagon: CNDrawItemOctagon(Msg);
    jvSPar: CNDrawItemPar(Msg);
    jvSDiamond: CNDrawItemDiamond(Msg);
    jvSTriangleUp: CNDrawItemTriangleUp(Msg);
    jvSTriangleDown: CNDrawItemTriangleDown(Msg);
    jvSTriangleLeft: CNDrawItemTriangleLeft(Msg);
    jvSTriangleRight: CNDrawItemTriangleRight(Msg);
    jvSPentagon: CNDrawItemPentagon(Msg);
    JvSRevPentagon: CNDrawItemRevPentagon(Msg);
    jvSRing: CNDrawItemRing(Msg);
  end;
end;

procedure TJvShapedButton.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

procedure TJvShapedButton.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TJvShapedButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

procedure TJvShapedButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Invalidate;
  end;
end;

procedure TJvShapedButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not IsHot then
  begin
    IsHot := True;
    Invalidate;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvShapedButton.MouseLeave(Control: TControl);
begin
  if IsHot then
  begin
    IsHot := False;
    Invalidate;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvShapedButton.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

procedure TJvShapedButton.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  invalidate;
end;

procedure TJvShapedButton.SetFlatBorderColor(const Value: TColor);
begin
  FFlatBorderColor := Value;
end;

procedure TJvShapedButton.SetButtonShape(const Value: TJvButtonShapes);
begin
  if Value <> FButtonShape then
  begin
    FButtonShape := Value;
    if HandleAllocated then
    begin
      RecreateWnd;
      Invalidate;
    end;
  end;
end;

procedure TJvShapedButton.SetRegionOctagon(ALeft, ATop, AWidth, AHeight: Integer);
var
  x4, y4: Integer;
  hRegion: THandle;
  poly: array[0..7] of TPoint;
begin
  x4 := Width div 4;
  y4 := Aheight div 4;
  poly[0] := point(x4, 0);
  poly[1] := point(Awidth - x4, 0);
  poly[2] := point(Awidth, y4);
  poly[3] := point(Awidth, Aheight - y4);
  poly[4] := point(AWidth - x4, Aheight);
  poly[5] := point(x4, Aheight);
  poly[6] := point(0, Aheight - y4);
  poly[7] := point(0, y4);
  hRegion := CreatePolygonRgn(poly, 8, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.CNDrawItemOctagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..8] of TPoint;
  polyBR: array[0..4] of TPoint;
  polyTL: array[0..4] of TPoint;
  x4, y4, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x4 := w div 4;
    y4 := h div 4;
    poly[0] := point(Rect.left + x4, Rect.top);
    poly[1] := point(Rect.right - x4, Rect.top);
    poly[2] := point(Rect.right, Rect.top + y4);
    poly[3] := point(Rect.right, Rect.bottom - y4);
    poly[4] := point(Rect.Right - x4, Rect.bottom);
    poly[5] := point(Rect.left + x4, Rect.bottom);
    poly[6] := point(Rect.left, Rect.bottom - y4);
    poly[7] := point(Rect.left, y4);
    poly[8] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyBR[4] := poly[5];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[5];
      polyTL[1] := poly[6];
      polyTL[2] := poly[7];
      polyTL[3] := poly[0];
      polyTL[4] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyBR[4] := poly[5];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyBR[4] := poly[5];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[5];
      polyTL[1] := poly[6];
      polyTL[2] := poly[7];
      polyTL[3] := poly[0];
      polyTL[4] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyBR[4] := poly[5];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.SetRegionTriangleDown(ALeft, ATop, AWidth, AHeight: Integer);
var
  x2: Integer;
  hRegion: THandle;
  poly: array[0..2] of TPoint;
begin
  x2 := Width div 2;
  //  y2:=Aheight div 2;
  poly[0] := point(0, 0);
  poly[1] := point(Awidth, 0);
  poly[2] := point(x2, Aheight);
  hRegion := CreatePolygonRgn(poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleLeft(ALeft, ATop, AWidth, AHeight: Integer);
var
  y2: Integer;
  hRegion: THandle;
  poly: array[0..2] of TPoint;
begin
  //  x2:=Width div 2;
  y2 := Aheight div 2;
  poly[0] := point(0, y2);
  poly[1] := point(Awidth, 0);
  poly[2] := point(AWidth, Aheight);
  hRegion := CreatePolygonRgn(poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleRight(ALeft, ATop, AWidth, AHeight: Integer);
var
  y2: Integer;
  hRegion: THandle;
  poly: array[0..2] of TPoint;
begin
  //  x2:=Width div 2;
  y2 := Aheight div 2;
  poly[0] := point(0, 0);
  poly[1] := point(Awidth, y2);
  poly[2] := point(0, Aheight);
  hRegion := CreatePolygonRgn(poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleUp(ALeft, ATop, AWidth, AHeight: Integer);
var
  x2: Integer;
  hRegion: THandle;
  poly: array[0..2] of TPoint;
begin
  x2 := Width div 2;
  //  y2:=Aheight div 2;
  poly[0] := point(x2, 0);
  poly[1] := point(Awidth, Aheight);
  poly[2] := point(0, Aheight);
  hRegion := CreatePolygonRgn(poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.CNDrawItemTriangleRight(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..3] of TPoint;
  polyBR: array[0..2] of TPoint;
  polyTL: array[0..1] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left, Rect.top);
    poly[1] := point(Rect.right, Rect.top + y2);
    poly[2] := point(Rect.left, Rect.bottom);
    poly[3] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleUp(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..3] of TPoint;
  polyBR: array[0..2] of TPoint;
  polyTL: array[0..1] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left + x2, Rect.top);
    poly[1] := point(Rect.right, Rect.bottom);
    poly[2] := point(Rect.left, Rect.bottom);
    poly[3] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleLeft(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..3] of TPoint;
  polyBR: array[0..1] of TPoint;
  polyTL: array[0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left, Rect.top + y2);
    poly[1] := point(Rect.right, Rect.top);
    poly[2] := point(Rect.right, Rect.bottom);
    poly[3] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleDown(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..3] of TPoint;
  polyBR: array[0..1] of TPoint;
  polyTL: array[0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left, Rect.top);
    poly[1] := point(Rect.right, Rect.top);
    poly[2] := point(Rect.left + x2, Rect.bottom);
    poly[3] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[2];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemPar(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..4] of TPoint;
  polyBR: array[0..2] of TPoint;
  polyTL: array[0..2] of TPoint;
  x4, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x4 := w div 4;
    y2 := h div 2;
    poly[0] := point(Rect.left + x4, Rect.top);
    poly[1] := point(Rect.right, Rect.top);
    poly[2] := point(Rect.right - x4, Rect.bottom);
    poly[3] := point(Rect.left, Rect.bottom);
    poly[4] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[3];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[3];
      polyTL[1] := poly[0];
      polyTL[2] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.SetRegionPar(ALeft, ATop, AWidth, AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..3] of TPoint;
  x4: Integer;
begin
  x4 := Width div 4;
  //  y2:=Aheight div 2;
  poly[0] := point(x4, 0);
  poly[1] := point(Awidth, 0);
  poly[2] := point(Awidth - x4, Aheight);
  poly[3] := point(0, Aheight);
  hRegion := CreatePolygonRgn(poly, 4, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionDiamond(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..3] of TPoint;
  x2, y2: Integer;
begin
  x2 := AWidth div 2;
  y2 := AHeight div 2;
  poly[0] := point(x2, 0);
  poly[1] := point(AWidth, y2);
  poly[2] := point(x2, AHeight);
  poly[3] := point(0, y2);
  hRegion := CreatePolygonRgn(poly, 4, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionHex(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..5] of TPoint;
  x4, y2: Integer;
begin
  x4 := Width div 4;
  y2 := Aheight div 2;
  poly[0] := point(x4, 0);
  poly[1] := point(Awidth - x4, 0);
  poly[2] := point(Awidth, y2);
  poly[3] := point(Awidth - x4, Aheight);
  poly[4] := point(x4, Aheight);
  poly[5] := point(0, y2);
  hRegion := CreatePolygonRgn(poly, 6, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionLeftArrow(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..5] of TPoint;
  x8, y2: Integer;
begin
  if FFlatArrow then
    x8 := Width div 16
  else
    x8 := Width div 8;
  y2 := Aheight div 2;
  poly[0] := point(0, 0);
  poly[1] := point(Awidth - x8, 0);
  poly[2] := point(Awidth, y2);
  poly[3] := point(Awidth - x8, Aheight);
  poly[4] := point(0, Aheight);
  poly[5] := point(x8, y2);
  hRegion := CreatePolygonRgn(poly, 6, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionPentagon(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..4] of TPoint;
  x2: Integer;
begin
  x2 := Awidth div 2;
  calcpentagon(Awidth, aheight);
  poly[0] := point(x2, 0);
  poly[1] := point(Awidth, yp);
  poly[2] := point(Awidth - xp, Aheight);
  poly[3] := point(xp, Aheight);
  poly[4] := point(0, yp);
  hRegion := CreatePolygonRgn(poly, 5, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionRevPentagon(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..4] of TPoint;
  x2: Integer;
begin
  x2 := Awidth div 2;
  calcpentagon(Awidth, aheight);
  poly[0] := point(xp, 0);
  poly[1] := point(Awidth - xp, 0);
  poly[2] := point(Awidth, Aheight - yp);
  poly[3] := point(x2, Aheight);
  poly[4] := point(0, Aheight - yp);

  hRegion := CreatePolygonRgn(poly, 5, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionRightArrow(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
  poly: array[0..5] of TPoint;
  x8, y2: Integer;
begin
  if FFlatArrow then
    x8 := Width div 16
  else
    x8 := Width div 8;
  y2 := Aheight div 2;
  poly[0] := point(x8, 0);
  poly[1] := point(Awidth, 0);
  poly[2] := point(Awidth - x8, y2);
  poly[3] := point(Awidth, Aheight);
  poly[4] := point(x8, Aheight);
  poly[5] := point(0, y2);
  hRegion := CreatePolygonRgn(poly, 6, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionRing(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  rgn1, rgn2, rgn3: Hrgn;
  x4, y4: Integer;
begin
  x4 := AWidth div 4 ;
  y4 := AHeight div 4;
  rgn1 := CreateEllipticRgn(0, 0, AWidth+1, AHeight+1);
  rgn2 := CreateEllipticRgn(x4, y4, AWidth - x4, AHeight - x4);
  rgn3 := 0; // Remove Warning
  Combinergn(rgn3, rgn1, rgn2, RGN_XOR);
  SetWindowRgn(Handle, rgn3, True);
end;

procedure TJvShapedButton.SetRegionRound(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: THandle;
begin
  hRegion := CreateEllipticRgn(0, 0, AWidth, AHeight);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.calcpentagon(AWidth, Aheight: Integer);
var
  x2, y2, r: Integer;
  a: extended;
begin
  a := pi / 2 - (2 * pi / 5);
  x2 := Awidth div 2;
  y2 := Aheight div 2;
  r := round(x2 / cos(a));
  yp := y2 - round(r * sin(a));
  a := pi - (4 * pi / 5);
  xp := round(x2 - r * sin(a));
end;

procedure TJvShapedButton.SetFlatArrow(const Value: Boolean);
begin
  if value <> FFlatArrow then
  begin
    FFlatArrow := Value;
    setbounds(Left, top, Width, height);
    invalidate;
  end;
end;

procedure TJvShapedButton.CNDrawItemLeftArrow(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..6] of TPoint;
  polyBR: array[0..3] of TPoint;
  polyTL: array[0..3] of TPoint;
  x8, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    if FFlatArrow then
      x8 := w div 16
    else
      x8 := w div 8;
    y2 := h div 2;
    poly[0] := point(Rect.left, Rect.top);
    poly[1] := point(Rect.right - x8, Rect.top);
    poly[2] := point(Rect.right, y2);
    poly[3] := point(Rect.right - x8, Rect.bottom);
    poly[4] := point(0, Rect.bottom);
    poly[5] := point(x8, y2);
    poly[6] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRightArrow(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..6] of TPoint;
  polyBR: array[0..3] of TPoint;
  polyTL: array[0..3] of TPoint;
  x8, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    if FFlatArrow then
      x8 := w div 16
    else
      x8 := w div 8;
    y2 := h div 2;
    poly[0] := point(Rect.left + x8, Rect.top);
    poly[1] := point(Rect.right, Rect.top);
    poly[2] := point(Rect.right - x8, y2);
    poly[3] := point(Rect.right, Rect.bottom);
    poly[4] := point(Rect.left + x8, Rect.bottom);
    poly[5] := point(Rect.left, y2);
    poly[6] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRing(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  R, Ri: TRect;
  x4, y4: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  x4 := (Width div 4) - 1;
  y4 := (height div 4) - 1;
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  R := ClientRect;
  Ri := Rect(R.left + x4, R.top + y4, R.right - x4, R.bottom - y4);
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;

  bm.PixelFormat := pf24bit;
  bm.Width := Width;
  bm.height := height;

  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Self.Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    Dec(R.Right);
    Dec(R.Bottom);
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default
    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
      begin
        Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        Ellipse(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom);
      end;
      // reduce the area for further operations
      InflateRect(R, -1, -1);
      InflateRect(Ri, 1, 1);
    end;

    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
      begin
        Ellipse(R.Left, R.Top, R.Right, R.Bottom);
        Ellipse(Ri.Left, Ri.Top, Ri.right, Ri.Bottom);
      end;
      // white border (bottom-right)
      Pen.Color := clwhite;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end
      Pen.Color := clbtnshadow;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end

      // gray border (top-left)
      Pen.Color := clbtnshadow;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end
      Pen.Color := clbtnhighlight;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end

      // gray border (top-left, internal)
      Pen.Color := clBtnShadow;
      InflateRect(R, -1, -1);
      InflateRect(Ri, 1, 1);
      //      Arc (Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
      //        Rect.Right, Rect.Top, // start
      //        Rect.Left, Rect.Bottom); // end
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end
      Pen.Color := clbtnhighlight;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end

      // white border (top-left)
      Pen.Color := clWhite;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end
      Pen.Color := clbtnshadow;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end

      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(R, -1, -1);
      InflateRect(Ri, 1, 1);
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end
      Pen.Color := clBtnHighlight;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end

    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
{    InflateRect (Rect, - Width div 5, - Height div 5);
    if OdsDown then
    begin
      Inc (Rect.Left, 2);
      Inc (Rect.Top, 2);
    end;
    Font := Self.Font;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color:= clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect (Rect);}
  end; // with FCanvas and if DrawEntire
  bm.Transparent := True;
  bm.TransparentColor := Self.Color;
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRound(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;

  bm.Width := Width;
  bm.height := height;
  bm.PixelFormat := pf24bit;

  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default
    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        Ellipse(Rect.Left, Rect.Top,
          Rect.Right, Rect.Bottom);
      // reduce the area for further operations
      InflateRect(Rect, -1, -1);
    end;

    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Ellipse(Rect.Left, Rect.Top,
          Rect.Right, Rect.Bottom);
      // white border (bottom-right)
      Pen.Color := clwhite;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
      // gray border (top-left)
      Pen.Color := clbtnshadow;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Right, Rect.Top, // start
        Rect.Left, Rect.Bottom); // end
      // gray border (top-left, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      //      Arc (Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
      //        Rect.Right, Rect.Top, // start
      //        Rect.Left, Rect.Bottom); // end
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
      // white border (top-left)
      Pen.Color := clWhite;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Right, Rect.Top, // start
        Rect.Left, Rect.Bottom); // end
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with FCanvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemPentagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..5] of TPoint;
  polyBR: array[0..3] of TPoint;
  polyTL: array[0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left + x2, Rect.top);
    poly[1] := point(Rect.right, Rect.top + yp);
    poly[2] := point(Rect.right - xp, Rect.bottom);
    poly[3] := point(Rect.left + xp, Rect.bottom);
    poly[4] := point(Rect.Left, Rect.top + yp);
    poly[5] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;

  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyBR[3] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[3];
      polyTL[1] := poly[4];
      polyTL[2] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyBR[3] := poly[3];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyBR[3] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[3];
      polyTL[1] := poly[4];
      polyTL[2] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyBR[3] := poly[3];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRevPentagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..5] of TPoint;
  polyBR: array[0..2] of TPoint;
  polyTL: array[0..3] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left + xp, Rect.top);
    poly[1] := point(Rect.right - xp, Rect.top);
    poly[2] := point(Rect.right, Rect.bottom - yp);
    poly[3] := point(Rect.left + x2, Rect.bottom);
    poly[4] := point(Rect.Left, Rect.bottom - yp);
    poly[5] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[3];
      polyTL[1] := poly[4];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[3];
      polyTL[1] := poly[4];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemHex(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..6] of TPoint;
  polyBR: array[0..3] of TPoint;
  polyTL: array[0..3] of TPoint;
  x4, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x4 := w div 4;
    y2 := h div 2;
    poly[0] := point(Rect.left + x4, Rect.top);
    poly[1] := point(Rect.right - x4, Rect.top);
    poly[2] := point(Rect.right, y2);
    poly[3] := point(Rect.right - x4, Rect.bottom);
    poly[4] := point(Rect.left + x4, Rect.bottom);
    poly[5] := point(Rect.left, y2);
    poly[6] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[4];
      polyTL[1] := poly[5];
      polyTL[2] := poly[0];
      polyTL[3] := poly[1];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[1];
      polyBR[1] := poly[2];
      polyBR[2] := poly[3];
      polyBR[3] := poly[4];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemDiamond(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  poly: array[0..4] of TPoint;
  polyBR: array[0..2] of TPoint;
  polyTL: array[0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure setpoly;
  begin
    w := Rect.right - Rect.left + 1;
    h := Rect.bottom - Rect.top + 1;
    x2 := w div 2;
    y2 := h div 2;
    poly[0] := point(Rect.left + x2, Rect.top);
    poly[1] := point(Rect.right, Rect.top + y2);
    poly[2] := point(Rect.left + x2, Rect.bottom);
    poly[3] := point(Rect.left, Rect.top + y2);
    poly[4] := poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  setpoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = oda_Focus
  end;
  bm.Width := Width;
  bm.height := height;
  with bm.Canvas do
  begin
    pen.Width := 2;
    Brush.Color := Color;
    if not ActionFocus then
    begin
      // fill with current Color
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;
    // do not fill any more
    Brush.Style := bsClear;
    // draw border if default

{    if Default or OdsFocus then
    begin
      Pen.Color := clWindowFrame;
      if not ActionFocus then
        polyLine(poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not IsHot) and (not (csdesigning in componentstate)) then
    begin
      Pen.Color := FFlatbordercolor;
      polyLine(poly);
    end
    else if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        polyline(poly);
      // gray border (bottom-right)
      Pen.Color := clwhite;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clwindowframe;
      polyTL[0] := poly[2];
      polyTL[1] := poly[3];
      polyTL[2] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end
    else if not ActionFocus then
    begin
      // gray border (bottom-right)
      Pen.Color := clWindowFrame;
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
      // white border (top-left)
      Pen.Color := clWhite;
      polyTL[0] := poly[2];
      polyTL[1] := poly[3];
      polyTL[2] := poly[0];
      polyline(polyTL);
      // gray border (bottom-right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      setpoly;
      polyBR[0] := poly[0];
      polyBR[1] := poly[1];
      polyBR[2] := poly[2];
      polyline(polyBR);
    end;
    // smooth edges
    DoAntiAlias(bm);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if (IsHot and (not OdsDown)) then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(bm.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if IsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end; // with bm.Canvas and if DrawEntire
  FCanvas.Draw(0, 0, bm);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.DoAntiAlias(Bmp: TBitmap);
begin
  if AntiAlias then
    JvJCLUtils.AntiAlias(Bmp);
end;

procedure TJvShapedButton.SetAntiAlias(const Value: Boolean);
begin
  if FAntiAlias <> Value then
  begin
    FAntiAlias := Value;
    Invalidate;
  end;
end;

function TJvShapedButton.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  DrawThemedBackground(Self, Canvas.Handle, ClientRect, Parent.Brush.Handle, False);
  Result := True;
end;

end.
