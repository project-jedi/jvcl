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
    FBmp: TBitmap;
    FIsFocused: Boolean;
    FIsHot: Boolean;
    FCanvas: TCanvas;
    FHotColor: TColor;
    FFlat: Boolean;
    FFlatBorderColor: TColor;
    FButtonShape: TJvButtonShapes;
    FXP: Integer;
    FYP: Integer;
    FFlatArrow: Boolean;
    FAntiAlias: Boolean;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
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
    procedure CalcPentagon(AWidth, AHeight: Integer);
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
    procedure DoAntiAlias(Bmp: TBitmap);
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
  FAntiAlias := False;
  FBmp := TBitmap.Create;
  SetBounds(Left, Top, 65, 65);
  FCanvas := TCanvas.Create;
  FHotColor := clBlue;
  FFlatBorderColor := clWhite;
  FButtonShape := jvSTriangleUp; //TODO: Change to Left Arrow
end;

destructor TJvShapedButton.Destroy;
begin
  inherited Destroy;
  FBmp.Free;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvShapedButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
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
      jvSLeftArrow:
        SetRegionLeftArrow(ALeft, ATop, AWidth, AHeight);
      jvRightArrow:
        SetRegionRightArrow(ALeft, ATop, AWidth, AHeight);
      jvSRound:
        SetRegionRound(ALeft, ATop, AWidth, AHeight);
      jvSHex:
        SetRegionHex(ALeft, ATop, AWidth, AHeight);
      jvSOctagon:
        SetRegionOctagon(ALeft, ATop, AWidth, AHeight);
      jvSPar:
        SetRegionPar(ALeft, ATop, AWidth, AHeight);
      jvSDiamond:
        SetRegionDiamond(ALeft, ATop, AWidth, AHeight);
      jvSTriangleUp:
        SetRegionTriangleUp(ALeft, ATop, AWidth, AHeight);
      jvSTriangleDown:
        SetRegionTriangleDown(ALeft, ATop, AWidth, AHeight);
      jvSTriangleLeft:
        SetRegionTriangleLeft(ALeft, ATop, AWidth, AHeight);
      jvSTriangleRight:
        SetRegionTriangleRight(ALeft, ATop, AWidth, AHeight);
      jvSPentagon:
        SetRegionPentagon(ALeft, ATop, AWidth, AHeight);
      JvSRevPentagon:
        SetRegionRevPentagon(ALeft, ATop, AWidth, AHeight);
      jvSRing:
        SetRegionRing(ALeft, ATop, AWidth, AHeight);
    end;
  end;
end;

procedure TJvShapedButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetButton(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvShapedButton.CNDrawItem(var Msg: TWMDrawItem);
begin
  case FButtonShape of
    jvSLeftArrow:
      CNDrawItemLeftArrow(Msg);
    jvRightArrow:
      CNDrawItemRightArrow(Msg);
    jvSRound:
      CNDrawItemRound(Msg);
    jvSHex:
      CNDrawItemHex(Msg);
    jvSOctagon:
      CNDrawItemOctagon(Msg);
    jvSPar:
      CNDrawItemPar(Msg);
    jvSDiamond:
      CNDrawItemDiamond(Msg);
    jvSTriangleUp:
      CNDrawItemTriangleUp(Msg);
    jvSTriangleDown:
      CNDrawItemTriangleDown(Msg);
    jvSTriangleLeft:
      CNDrawItemTriangleLeft(Msg);
    jvSTriangleRight:
      CNDrawItemTriangleRight(Msg);
    jvSPentagon:
      CNDrawItemPentagon(Msg);
    JvSRevPentagon:
      CNDrawItemRevPentagon(Msg);
    jvSRing:
      CNDrawItemRing(Msg);
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

procedure TJvShapedButton.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Msg.Keys, Longint(Msg.Pos));
end;

procedure TJvShapedButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Invalidate;
  end;
end;

procedure TJvShapedButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FIsHot then
  begin
    FIsHot := True;
    Invalidate;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvShapedButton.MouseLeave(Control: TControl);
begin
  if FIsHot then
  begin
    FIsHot := False;
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
  Invalidate;
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
  hRegion: HRGN;
  Poly: array [0..7] of TPoint;
begin
  x4 := Width div 4;
  y4 := AHeight div 4;
  Poly[0] := Point(x4, 0);
  Poly[1] := Point(AWidth - x4, 0);
  Poly[2] := Point(AWidth, y4);
  Poly[3] := Point(AWidth, AHeight - y4);
  Poly[4] := Point(AWidth - x4, AHeight);
  Poly[5] := Point(x4, AHeight);
  Poly[6] := Point(0, AHeight - y4);
  Poly[7] := Point(0, y4);
  hRegion := CreatePolygonRgn(Poly, 8, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.CNDrawItemOctagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..8] of TPoint;
  PolyBR: array [0..4] of TPoint;
  PolyTL: array [0..4] of TPoint;
  x4, y4, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x4 := w div 4;
    y4 := h div 4;
    Poly[0] := Point(Rect.Left + x4, Rect.Top);
    Poly[1] := Point(Rect.Right - x4, Rect.Top);
    Poly[2] := Point(Rect.Right, Rect.Top + y4);
    Poly[3] := Point(Rect.Right, Rect.Bottom - y4);
    Poly[4] := Point(Rect.Right - x4, Rect.Bottom);
    Poly[5] := Point(Rect.Left + x4, Rect.Bottom);
    Poly[6] := Point(Rect.Left, Rect.Bottom - y4);
    Poly[7] := Point(Rect.Left, y4);
    Poly[8] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      PolyBR[4] := Poly[5];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[5];
      PolyTL[1] := Poly[6];
      PolyTL[2] := Poly[7];
      PolyTL[3] := Poly[0];
      PolyTL[4] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      PolyBR[4] := Poly[5];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      PolyBR[4] := Poly[5];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[5];
      PolyTL[1] := Poly[6];
      PolyTL[2] := Poly[7];
      PolyTL[3] := Poly[0];
      PolyTL[4] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      PolyBR[4] := Poly[5];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.SetRegionTriangleDown(ALeft, ATop, AWidth, AHeight: Integer);
var
  x2: Integer;
  hRegion: HRGN;
  Poly: array [0..2] of TPoint;
begin
  x2 := Width div 2;
  //  y2:=AHeight div 2;
  Poly[0] := Point(0, 0);
  Poly[1] := Point(AWidth, 0);
  Poly[2] := Point(x2, AHeight);
  hRegion := CreatePolygonRgn(Poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleLeft(ALeft, ATop, AWidth, AHeight: Integer);
var
  y2: Integer;
  hRegion: HRGN;
  Poly: array [0..2] of TPoint;
begin
  //  x2:=Width div 2;
  y2 := AHeight div 2;
  Poly[0] := Point(0, y2);
  Poly[1] := Point(AWidth, 0);
  Poly[2] := Point(AWidth, AHeight);
  hRegion := CreatePolygonRgn(Poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleRight(ALeft, ATop, AWidth, AHeight: Integer);
var
  y2: Integer;
  hRegion: HRGN;
  Poly: array [0..2] of TPoint;
begin
  //  x2:=Width div 2;
  y2 := AHeight div 2;
  Poly[0] := Point(0, 0);
  Poly[1] := Point(AWidth, y2);
  Poly[2] := Point(0, AHeight);
  hRegion := CreatePolygonRgn(Poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionTriangleUp(ALeft, ATop, AWidth, AHeight: Integer);
var
  x2: Integer;
  hRegion: HRGN;
  Poly: array [0..2] of TPoint;
begin
  x2 := Width div 2;
  //  y2:=AHeight div 2;
  Poly[0] := Point(x2, 0);
  Poly[1] := Point(AWidth, AHeight);
  Poly[2] := Point(0, AHeight);
  hRegion := CreatePolygonRgn(Poly, 3, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.CNDrawItemTriangleRight(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..3] of TPoint;
  PolyBR: array [0..2] of TPoint;
  PolyTL: array [0..1] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top + y2);
    Poly[2] := Point(Rect.Left, Rect.Bottom);
    Poly[3] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleUp(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..3] of TPoint;
  PolyBR: array [0..2] of TPoint;
  PolyTL: array [0..1] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x2, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Bottom);
    Poly[2] := Point(Rect.Left, Rect.Bottom);
    Poly[3] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleLeft(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..3] of TPoint;
  PolyBR: array [0..1] of TPoint;
  PolyTL: array [0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left, Rect.Top + y2);
    Poly[1] := Point(Rect.Right, Rect.Top);
    Poly[2] := Point(Rect.Right, Rect.Bottom);
    Poly[3] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemTriangleDown(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..3] of TPoint;
  PolyBR: array [0..1] of TPoint;
  PolyTL: array [0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top);
    Poly[2] := Point(Rect.Left + x2, Rect.Bottom);
    Poly[3] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemPar(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..4] of TPoint;
  PolyBR: array [0..2] of TPoint;
  PolyTL: array [0..2] of TPoint;
  x4, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x4 := w div 4;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x4, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top);
    Poly[2] := Point(Rect.Right - x4, Rect.Bottom);
    Poly[3] := Point(Rect.Left, Rect.Bottom);
    Poly[4] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[0];
      PolyTL[2] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.SetRegionPar(ALeft, ATop, AWidth, AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..3] of TPoint;
  x4: Integer;
begin
  x4 := Width div 4;
  //  y2:=AHeight div 2;
  Poly[0] := Point(x4, 0);
  Poly[1] := Point(AWidth, 0);
  Poly[2] := Point(AWidth - x4, AHeight);
  Poly[3] := Point(0, AHeight);
  hRegion := CreatePolygonRgn(Poly, 4, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionDiamond(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..3] of TPoint;
  x2, y2: Integer;
begin
  x2 := AWidth div 2;
  y2 := AHeight div 2;
  Poly[0] := Point(x2, 0);
  Poly[1] := Point(AWidth, y2);
  Poly[2] := Point(x2, AHeight);
  Poly[3] := Point(0, y2);
  hRegion := CreatePolygonRgn(Poly, 4, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionHex(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..5] of TPoint;
  x4, y2: Integer;
begin
  x4 := Width div 4;
  y2 := AHeight div 2;
  Poly[0] := Point(x4, 0);
  Poly[1] := Point(AWidth - x4, 0);
  Poly[2] := Point(AWidth, y2);
  Poly[3] := Point(AWidth - x4, AHeight);
  Poly[4] := Point(x4, AHeight);
  Poly[5] := Point(0, y2);
  hRegion := CreatePolygonRgn(Poly, 6, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionLeftArrow(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..5] of TPoint;
  x8, y2: Integer;
begin
  if FFlatArrow then
    x8 := Width div 16
  else
    x8 := Width div 8;
  y2 := AHeight div 2;
  Poly[0] := Point(0, 0);
  Poly[1] := Point(AWidth - x8, 0);
  Poly[2] := Point(AWidth, y2);
  Poly[3] := Point(AWidth - x8, AHeight);
  Poly[4] := Point(0, AHeight);
  Poly[5] := Point(x8, y2);
  hRegion := CreatePolygonRgn(Poly, 6, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionPentagon(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..4] of TPoint;
  x2: Integer;
begin
  x2 := AWidth div 2;
  CalcPentagon(AWidth, AHeight);
  Poly[0] := Point(x2, 0);
  Poly[1] := Point(AWidth, FYP);
  Poly[2] := Point(AWidth - FXP, AHeight);
  Poly[3] := Point(FXP, AHeight);
  Poly[4] := Point(0, FYP);
  hRegion := CreatePolygonRgn(Poly, 5, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionRevPentagon(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..4] of TPoint;
  x2: Integer;
begin
  x2 := AWidth div 2;
  CalcPentagon(AWidth, AHeight);
  Poly[0] := Point(FXP, 0);
  Poly[1] := Point(AWidth - FXP, 0);
  Poly[2] := Point(AWidth, AHeight - FYP);
  Poly[3] := Point(x2, AHeight);
  Poly[4] := Point(0, AHeight - FYP);

  hRegion := CreatePolygonRgn(Poly, 5, WINDING);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.SetRegionRightArrow(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  hRegion: HRGN;
  Poly: array [0..5] of TPoint;
  x8, y2: Integer;
begin
  if FFlatArrow then
    x8 := Width div 16
  else
    x8 := Width div 8;
  y2 := AHeight div 2;
  Poly[0] := Point(x8, 0);
  Poly[1] := Point(AWidth, 0);
  Poly[2] := Point(AWidth - x8, y2);
  Poly[3] := Point(AWidth, AHeight);
  Poly[4] := Point(x8, AHeight);
  Poly[5] := Point(0, y2);
  hRegion := CreatePolygonRgn(Poly, 6, WINDING);
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
  hRegion: HRGN;
begin
  hRegion := CreateEllipticRgn(0, 0, AWidth, AHeight);
  SetWindowRgn(Handle, hRegion, True);
end;

procedure TJvShapedButton.CalcPentagon(AWidth, AHeight: Integer);
var
  x2, y2, r: Integer;
  a: Extended;
begin
  a := Pi / 2 - (2 * Pi / 5);
  x2 := AWidth div 2;
  y2 := AHeight div 2;
  r := Round(x2 / Cos(a));
  FYP := y2 - Round(r * Sin(a));
  a := Pi - (4 * pi / 5);
  FXP := Round(x2 - r * Sin(a));
end;

procedure TJvShapedButton.SetFlatArrow(const Value: Boolean);
begin
  if Value <> FFlatArrow then
  begin
    FFlatArrow := Value;
    SetBounds(Left, Top, Width, Height);
    Invalidate;
  end;
end;

procedure TJvShapedButton.CNDrawItemLeftArrow(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..6] of TPoint;
  PolyBR: array [0..3] of TPoint;
  PolyTL: array [0..3] of TPoint;
  x8, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    if FFlatArrow then
      x8 := w div 16
    else
      x8 := w div 8;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left, Rect.Top);
    Poly[1] := Point(Rect.Right - x8, Rect.Top);
    Poly[2] := Point(Rect.Right, y2);
    Poly[3] := Point(Rect.Right - x8, Rect.Bottom);
    Poly[4] := Point(0, Rect.Bottom);
    Poly[5] := Point(x8, y2);
    Poly[6] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRightArrow(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..6] of TPoint;
  PolyBR: array [0..3] of TPoint;
  PolyTL: array [0..3] of TPoint;
  x8, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    if FFlatArrow then
      x8 := w div 16
    else
      x8 := w div 8;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x8, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top);
    Poly[2] := Point(Rect.Right - x8, y2);
    Poly[3] := Point(Rect.Right, Rect.Bottom);
    Poly[4] := Point(Rect.Left + x8, Rect.Bottom);
    Poly[5] := Point(Rect.Left, y2);
    Poly[6] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
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
  y4 := (Height div 4) - 1;
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  R := ClientRect;
  Ri := Rect(R.Left + x4, R.Top + y4, R.Right - x4, R.Bottom - y4);
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;

  FBmp.PixelFormat := pf24bit;
  FBmp.Width := Width;
  FBmp.Height := Height;

  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Ellipse(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom);
      end;
      // white border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end
      Pen.Color := clbtnshadow;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end

      // gray border (Top-Left)
      Pen.Color := clbtnshadow;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end
      Pen.Color := clbtnhighlight;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end

      // gray border (Top-Left, internal)
      Pen.Color := clBtnShadow;
      InflateRect(R, -1, -1);
      InflateRect(Ri, 1, 1);
      //      Arc (Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
      //        Rect.Right, Rect.Top, // start
      //        Rect.Left, Rect.Bottom); // end
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end
      Pen.Color := clbtnhighlight;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Left, R.Bottom, // start
        R.Right, R.Top); // end

      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      Arc(R.Left, R.Top, R.Right, R.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end
      Pen.Color := clbtnshadow;
      Arc(Ri.Left, Ri.Top, Ri.Right, Ri.Bottom, // ellipse
        R.Right, R.Top, // start
        R.Left, R.Bottom); // end

      // gray border (Bottom-Right, internal)
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
    DoAntiAlias(FBmp);
    // draw the caption
{    InflateRect (Rect, - Width div 5, - Height div 5);
    if OdsDown then
    begin
      Inc (Rect.Left, 2);
      Inc (Rect.Top, 2);
    end;
    Font := Self.Font;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color:= clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect (Rect);}
  end;
  FBmp.Transparent := True;
  FBmp.TransparentColor := Self.Color;
  FCanvas.Draw(0, 0, FBmp);
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
    ActionFocus := ItemAction = ODA_FOCUS;
  end;

  FBmp.Width := Width;
  FBmp.Height := Height;
  FBmp.PixelFormat := pf24bit;

  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      // white border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
      // gray border (Top-Left)
      Pen.Color := clbtnshadow;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Right, Rect.Top, // start
        Rect.Left, Rect.Bottom); // end
      // gray border (Top-Left, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      //      Arc (Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
      //        Rect.Right, Rect.Top, // start
      //        Rect.Left, Rect.Bottom); // end
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Right, Rect.Top, // start
        Rect.Left, Rect.Bottom); // end
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, // ellipse
        Rect.Left, Rect.Bottom, // start
        Rect.Right, Rect.Top); // end
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemPentagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..5] of TPoint;
  PolyBR: array [0..3] of TPoint;
  PolyTL: array [0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x2, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top + FYP);
    Poly[2] := Point(Rect.Right - FXP, Rect.Bottom);
    Poly[3] := Point(Rect.Left + FXP, Rect.Bottom);
    Poly[4] := Point(Rect.Left, Rect.Top + FYP);
    Poly[5] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;

  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      PolyBR[3] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[4];
      PolyTL[2] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      PolyBR[3] := Poly[3];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      PolyBR[3] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[4];
      PolyTL[2] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      PolyBR[3] := Poly[3];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemRevPentagon(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..5] of TPoint;
  PolyBR: array [0..2] of TPoint;
  PolyTL: array [0..3] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + FXP, Rect.Top);
    Poly[1] := Point(Rect.Right - FXP, Rect.Top);
    Poly[2] := Point(Rect.Right, Rect.Bottom - FYP);
    Poly[3] := Point(Rect.Left + x2, Rect.Bottom);
    Poly[4] := Point(Rect.Left, Rect.Bottom - FYP);
    Poly[5] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[4];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[3];
      PolyTL[1] := Poly[4];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemHex(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..6] of TPoint;
  PolyBR: array [0..3] of TPoint;
  PolyTL: array [0..3] of TPoint;
  x4, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x4 := w div 4;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x4, Rect.Top);
    Poly[1] := Point(Rect.Right - x4, Rect.Top);
    Poly[2] := Point(Rect.Right, y2);
    Poly[3] := Point(Rect.Right - x4, Rect.Bottom);
    Poly[4] := Point(Rect.Left + x4, Rect.Bottom);
    Poly[5] := Point(Rect.Left, y2);
    Poly[6] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[4];
      PolyTL[1] := Poly[5];
      PolyTL[2] := Poly[0];
      PolyTL[3] := Poly[1];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[1];
      PolyBR[1] := Poly[2];
      PolyBR[2] := Poly[3];
      PolyBR[3] := Poly[4];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
  FCanvas.Handle := 0;
  Msg.Result := 1; // message handled
end;

procedure TJvShapedButton.CNDrawItemDiamond(var Msg: TWMDrawItem);
var
  OdsDown, OdsFocus, ActionFocus: Boolean;
  Rect: TRect;
  Poly: array [0..4] of TPoint;
  PolyBR: array [0..2] of TPoint;
  PolyTL: array [0..2] of TPoint;
  x2, y2, w, h: Integer;

  procedure SetPoly;
  begin
    w := Rect.Right - Rect.Left + 1;
    h := Rect.Bottom - Rect.Top + 1;
    x2 := w div 2;
    y2 := h div 2;
    Poly[0] := Point(Rect.Left + x2, Rect.Top);
    Poly[1] := Point(Rect.Right, Rect.Top + y2);
    Poly[2] := Point(Rect.Left + x2, Rect.Bottom);
    Poly[3] := Point(Rect.Left, Rect.Top + y2);
    Poly[4] := Poly[0];
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  // initialize
  FCanvas.Handle := Msg.DrawItemStruct^.hDC;
  Rect := ClientRect;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  SetPoly;
  with Msg.DrawItemStruct^ do
  begin
    OdsDown := itemState and ODS_SELECTED <> 0;
    OdsFocus := itemState and ODS_FOCUS <> 0;
    ActionFocus := ItemAction = ODA_FOCUS;
  end;
  FBmp.Width := Width;
  FBmp.Height := Height;
  with FBmp.Canvas do
  begin
    Pen.Width := 2;
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
        Polyline(Poly);
      // reduce the area for further operations
      InflateRect (Rect, -1, -1);
    end;}
    // test code:
    //InflateRect (Rect, -1, -1);

    if FFlat and (not OdsDown) and (not FIsHot) and (not (csDesigning in ComponentState)) then
    begin
      Pen.Color := FFlatBorderColor;
      Polyline(Poly);
    end
    else
    if OdsDown then
    begin
      // draw gray border all around
      Pen.Color := clBtnShadow;
      if not ActionFocus then
        Polyline(Poly);
      // gray border (Bottom-Right)
      Pen.Color := clBtnHighlight;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clWindowFrame;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[3];
      PolyTL[2] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end
    else
    if not ActionFocus then
    begin
      // gray border (Bottom-Right)
      Pen.Color := clWindowFrame;
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
      // white border (Top-Left)
      Pen.Color := clBtnHighlight;
      PolyTL[0] := Poly[2];
      PolyTL[1] := Poly[3];
      PolyTL[2] := Poly[0];
      Polyline(PolyTL);
      // gray border (Bottom-Right, internal)
      Pen.Color := clBtnShadow;
      InflateRect(Rect, -1, -1);
      SetPoly;
      PolyBR[0] := Poly[0];
      PolyBR[1] := Poly[1];
      PolyBR[2] := Poly[2];
      Polyline(PolyBR);
    end;
    // smooth edges
    DoAntiAlias(FBmp);
    // draw the caption
    InflateRect(Rect, -Width div 5, -Height div 5);
    if OdsDown then
    begin
      Inc(Rect.Left, 2);
      Inc(Rect.Top, 2);
    end;
    Font := Self.Font;
    if FIsHot and not OdsDown then
      Font.Color := FHotColor;
    if not ActionFocus then
      DrawText(FBmp.Canvas, Caption, -1,
        Rect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);

    // draw the focus Rect around the text
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Brush.Color := clWhite;
    if FIsFocused or OdsFocus or ActionFocus then
      DrawFocusRect(Rect);
  end;
  FCanvas.Draw(0, 0, FBmp);
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
