{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQDoubleBuffering.pas, released on 2004-09-21

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att users.sourceforge.net]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDoubleBuffering;

interface

uses
  Classes, SysUtils,
  Qt, QWindows, QGraphics, QControls, QForms,
  JvQEventFilter;

type
  TJvDoubleBuffering = class(TJvEventFilter)
//  private
//    FHook: QApplication_hookH;
//    FPaintObject: QObjectH;
//    FIgnoreList: TList;
  protected
    procedure SetParent(const Value: TWinControl); override;
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean; cdecl;
//    procedure Loaded; override;
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
  end;

implementation

type
  THackedWidgetControl = class(TWidgetControl);

function SetPixmapMask(Pixmap: QPixmapH; RegionForMask: QRegionH): boolean;
var
  Bmp: QBitmapH;
  Painter: QPainterH;
  Canvas: TCanvas;
  w, h: integer;
begin
  Result := false;
  w := QPixmap_width(Pixmap);
  h := QPixmap_height(Pixmap);
  Bmp := QBitmap_Create(w, h, True, QPixmapOptimization_DefaultOptim);
  try
    Painter := QPainter_create(Bmp);
    try
      Canvas := TCanvas.Create;
      try
        Canvas.Start(False);
        Canvas.Handle := Painter;
        with Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clMask;
          FillRect(Rect(0, 0, w, h));
          QPainter_setClipRegion(Painter, RegionForMask);
          QPainter_setClipping(Painter, True);
          Brush.Color := clDontMask;
          FillRect(Rect(0, 0, w, h));
        end;
        Canvas.Stop;
      finally
        Canvas.Free;
      end;
      QPixmap_setMask(Pixmap, bmp);
      Result := True;
    finally
      QPainter_destroy(Painter);
    end;
  finally
    QBitmap_destroy(Bmp);
  end;
end;
{
constructor TJvDoubleBuffered.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaintObject := nil;
  FIgnoreList := TList.Create;
end;

destructor TJvDoubleBuffered.Destroy;
begin
  if Assigned(FHook) then
  begin
    QApplication_hook_destroy(FHook);
  end;
  FIgnoreList.Free;
  inherited Destroy;
end;


procedure TJvDoubleBuffered.Loaded;
var
  Method: TMethod;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHook := QApplication_hook_create(Application.Handle);
    TEventFilterMethod(Method) := EventFilter;
    Qt_hook_hook_events(FHook, Method);
  end;
end;
}

procedure TJvDoubleBuffering.SetParent(const Value: TWinControl);
var
  WasActive: Boolean;
  Method: TMethod;
begin
  if Value <> Parent then
  begin
    WasActive := Active;
    Active := false;
    if Assigned(FHook) then
      QObject_hook_destroy(FHook);
    FHook := nil;
    inherited SetParent(Value);
    if Assigned(Parent) then
    begin
      FHook := QObject_hook_create(Parent.Handle);
      TEventFilterMethod(Method) := EventFilter;
      Qt_hook_hook_events(FHook, Method);
    end;
    Active := WasActive;
  end;
  QWidget_setBackGroundMode(Parent.Handle , QWidgetBackgroundMode_NoBackground );
end;


function TJvDoubleBuffering.EventFilter(Receiver: QObjectH; Event: QEventH): Boolean;
var
  PixMap: QPixmapH;
  R: TRect;
  PaintRegion: QRegionH;
  Instance: TWidgetControl;
//  tl, br: TPoint;
begin
  Result := False;
  if QEvent_type(Event) = QEventType_Paint then
  begin
    Instance := FindControl(QWidgetH(Receiver));
    if not Assigned(Instance) then
    begin
      Exit;
    end;
    if (csDestroying in Instance.ComponentState ) or
       Application.Terminated then
    begin
      Result := true;
      Exit;
    end;
    if (csPaintCopy in Instance.ControlState) then
      Exit;
//    FPaintObject := Receiver;
    PaintRegion := QRegion_create(QPaintEvent_region(QPaintEventH(Event)));
    QRegion_boundingRect(PaintRegion, @R);
    Pixmap := QPixmap_create(R.Right - R.Left, R.Bottom - R.Top, -1, QPixmapOptimization_DefaultOptim);
    try
      Instance.ControlState := Instance.ControlState + [csPaintCopy];
      QPixmap_grabWidget(PixMap, QWidgetH(Receiver), R.Left, R.Top,
            R.Right - R.Left, R.Bottom - R.Top);
      Qt.BitBlt(QWidget_to_QPaintDevice(QWidgetH(Receiver)), R.Left, R.Top, PixMap,
            0, 0, R.Right - R.Left, R.Bottom - R.Top, RasterOp_CopyROP,
            False);
      Result := True;
    finally
      Instance.ControlState := Instance.ControlState - [csPaintCopy];
      QRegion_destroy(PaintRegion);
      QPixMap_destroy(PixMap);
    end;
  end
end;

end.
