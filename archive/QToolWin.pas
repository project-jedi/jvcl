{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QToolWin.pas, released on 2004-01-12

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): André Snepvangers. (Viewport implementation)

Last Modified: 2004-11-29

Known Issues:
----------------------------------------------------------------------------}
//

unit QToolWin;

interface

uses
  SysUtils, Classes, Types, Qt, QGraphics, QControls, QExtCtrls, QWindows;

type
{ TToolWindow }

  TEdgeBorder = (ebLeft, ebTop, ebRight, ebBottom);
  TEdgeBorders = set of TEdgeBorder;

  TToolWindow = class(TWidgetControl)
  private
    FEdgeBorders: TEdgeBorders;
    FEdgeInner: TEdgeStyle;
    FEdgeOuter: TEdgeStyle;
    FBorderWidth: TBorderWidth;
    FCanvas: TControlCanvas;
    FViewPortHandle: QWidgetH;
    FViewportHook: QWidget_HookH;
    procedure SetEdgeBorders(Value: TEdgeBorders);
    procedure SetEdgeInner(Value: TEdgeStyle);
    procedure SetEdgeOuter(Value: TEdgeStyle);
    procedure SetBorderWidth(const Value: TBorderWidth);
    function GetCanvas: TCanvas;
  protected
    procedure PaletteChanged(Sender: TObject); override;
    procedure BoundsChanged; override;
    procedure Paint; virtual;
    procedure NCPaint; virtual;
    procedure CreateWidget; override;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    procedure HookEvents; override;
    procedure WidgetDestroyed; override;
    function GetAlignedPaintDevice: QPaintDeviceH; override;
    function GetPaintDevice: QPaintDeviceH; override;
    function GetChildHandle: QWidgetH; override;
    function ViewPortHandle: QWidgetH;
    function ViewPortRect: TRect; override;
    procedure UpdateControl; virtual;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollBy(DeltaX, DeltaY: Integer); override;

    property EdgeBorders: TEdgeBorders read FEdgeBorders write SetEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle read FEdgeInner write SetEdgeInner default esRaised;
    property EdgeOuter: TEdgeStyle read FEdgeOuter write SetEdgeOuter default esLowered;
    property Canvas: TCanvas read GetCanvas;
  end;

implementation

{ TToolWindow }

constructor TToolWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := ControlStyle + [csAcceptsControls];
  FEdgeInner := esRaised;
  FEdgeOuter := esLowered;
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

procedure TToolWindow.CreateWidget;
begin
  inherited CreateWidget;
  FViewportHandle := QWidget_create(Handle, nil, WidgetFlags);
  QClxObjectMap_add(FViewportHandle, Integer(Self));
end;

procedure TToolWindow.HookEvents;
var
  Method: TMethod;
begin
  FViewportHook := QWidget_hook_create(ViewportHandle);
  TEventFilterMethod(Method) := MainEventFilter;
  Qt_hook_hook_events(FViewportHook, Method);
  inherited HookEvents;
end;


procedure TToolWindow.WidgetDestroyed;
begin
  QClxObjectMap_remove(FViewportHandle);
  QWidget_hook_destroy(FViewportHook);
  FViewportHandle := nil;
  inherited WidgetDestroyed;
end;

procedure TToolWindow.UpdateControl;
var
  R, R2: TRect;
begin
  Invalidate;
  R := ClientRect;
  R2 := R;
  OffsetRect(R2, -Top, -Left);
  QWidget_setGeometry(FViewPortHandle, @R2);
  AlignControls(Self, R);
end;

procedure TToolWindow.BoundsChanged;
var
  R2: TRect;
begin
  inherited BoundsChanged;
  R2 := ClientRect;
  QWidget_setGeometry(FViewPortHandle, @R2);
end;

procedure TToolWindow.SetEdgeBorders(Value: TEdgeBorders);
begin
  if Value <> FEdgeBorders then
  begin
    FEdgeBorders := Value;
    UpdateControl;
  end;
end;

procedure TToolWindow.ControlsListChanging(Control: TControl;
  Inserting: Boolean);
var
  WasVisible: Boolean;
begin
  inherited ControlsListChanging(Control, Inserting);
  if Control is TWidgetControl then
    if Inserting then
    begin
      HandleNeeded;
      WasVisible := Control.Visible;
      TWidgetControl(Control).ParentWidget := ChildHandle;
      Control.Visible := WasVisible;
    end
end;

procedure TToolWindow.SetEdgeInner(Value: TEdgeStyle);
begin
  if Value <> FEdgeInner then
  begin
    FEdgeInner := Value;
    UpdateControl;
  end;
end;

procedure TToolWindow.SetEdgeOuter(Value: TEdgeStyle);
begin
  if Value <> FEdgeOuter then
  begin
    FEdgeOuter := Value;
    UpdateControl;
  end;
end;

function TToolWindow.GetAlignedPaintDevice: QPaintDeviceH;
begin
  Result := QWidget_to_QPaintDevice(FViewportHandle);
end;

function TToolWindow.GetPaintDevice: QPaintDeviceH;
begin
  Result := QWidget_to_QPaintDevice(FViewportHandle);
end;

function TToolWindow.GetChildHandle: QWidgetH;
begin
  Result := FViewportHandle;
end;

(*
function TToolWindow.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  Inc(Result.X, BorderWidth);
  Inc(Result.Y, BorderWidth);
end;
*)

function TToolWindow.GetClientOrigin: TPoint;
var
  P: TPoint;
begin
  Result := inherited GetClientOrigin;
  P := ViewportRect.TopLeft;
  Result.X := Result.X + P.X;
  Result.Y := Result.Y + P.Y;
end;

function TToolWindow.ViewportRect: TRect;
var
  Offset: TPoint;
begin
  QWidget_geometry(ViewportHandle, @Result);
  Offset := ClientRect.TopLeft;
  OffsetRect(Result, Offset.X, Offset.Y);
end;

function TToolWindow.ViewportHandle: QWidgetH;
begin
  if not (csDestroying in ComponentState) then
    HandleNeeded;
  Result := FViewportHandle;
end;

function TToolWindow.GetClientRect: TRect;
var
  EdgeSize: Integer;
begin
  Result := inherited GetClientRect;
  InflateRect(Result, -BorderWidth, -BorderWidth);
  EdgeSize := 0;
  if EdgeInner <> esNone then
    Inc(EdgeSize, 1);
  if EdgeOuter <> esNone then
    Inc(EdgeSize, 1);
  if ebLeft in FEdgeBorders then
    Inc(Result.Left, EdgeSize);
  if ebTop in FEdgeBorders then
    Inc(Result.Top, EdgeSize);
  if ebRight in FEdgeBorders then
    Dec(Result.Right, EdgeSize);
  if ebBottom in FEdgeBorders then
    Dec(Result.Bottom, EdgeSize);
end;

function TToolWindow.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TToolWindow.Paint;
begin

end;

procedure TToolWindow.NCPaint;
const
  InnerStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
var
  R: TRect;
  DC: QPainterH;
begin
  R := BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  DC := GetDC(FHandle);
  DrawEdge(DC, R, InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],
    Byte(FEdgeBorders) or BF_ADJUST);
  ReleaseDC(nil, DC);
end;

procedure TToolWindow.PaletteChanged(Sender: TObject);
begin
  inherited;
  QWidget_setPalette(ViewportHandle, (Sender as TPalette).Handle, True);
end;

procedure TToolWindow.ScrollBy(DeltaX, DeltaY: Integer);
var
  IsVisible: Boolean;
begin
  IsVisible := Visible and HandleAllocated;
  if IsVisible then
    QWidget_scroll(ViewPortHandle, DeltaX, DeltaY);
  ScrollControls(DeltaX, DeltaY, IsVisible);
  Realign;
end;

function TToolWindow.EventFilter(Sender: QObjectH;
  Event: QEventH): Boolean;
begin
  case QEvent_type(Event) of
    QEventType_Paint:
      if not (csWidgetPainting in ControlState) then
      begin
        Result := inherited EventFilter(Sender, Event);
        NCPaint;
        TControlCanvas(Canvas).StartPaint;
        try
          Paint;
        finally
          TControlCanvas(Canvas).StopPaint;
        end;
        Exit;
      end;
  end;
  Result := inherited EventFilter(Sender, Event);
end;

procedure TToolWindow.SetBorderWidth(const Value: TBorderWidth);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    UpdateControl;
  end;
end;

end.
