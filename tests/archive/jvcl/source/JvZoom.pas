{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvZoom.PAS, released on 2001-02-28.
2002-12-08 : added crosshair options and OnContentsChanged event (Antoine Potten)

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com], Antoine Potten [jvcl@antp.be]

Last Modified: 2002-12-08

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvZoom;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, JVCLVer;

type
  TJvZoom = class(TCustomControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FTimer: TTimer;
    FActive: Boolean;
    FZoomLevel: Integer;
    FDelay: Cardinal;
    FDesktopCanvas: TCanvas;
    FLastPoint: TPoint;
    FCrosshair: Boolean;
    FCrosshairColor: TColor;
    FCrosshairSize: Integer;
    FOnContentsChanged: TNotifyEvent;
    FCacheOnDeactivate: Boolean;
    FCacheBitmap: TBitmap;
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetCacheOnDeactivate(const Value: Boolean);
    procedure PaintMe(Sender: TObject);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure Paint; override;
    procedure PaintZoom;
    procedure Loaded; override;
    procedure Cache;
    procedure FlushCache;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Active: Boolean read FActive write SetActive default True;
    property ZoomLevel: Integer read FZoomLevel write FZoomLevel default 100;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property Crosshair: Boolean read FCrosshair write FCrosshair default False;
    property CrosshairColor: TColor read FCrosshairColor write FCrosshairColor default clBlack;
    property CrosshairSize: Integer read FCrosshairSize write FCrosshairSize default 20;
    property CacheOnDeactivate: Boolean read FCacheOnDeactivate write SetCacheOnDeactivate default True;
    property OnContentsChanged: TNotifyEvent read FOnContentsChanged write FOnContentsChanged;
    property OnMouseDown;
    property OnClick;
    property OnDblClick;
    property OnMouseUp;
    property OnResize;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

implementation

constructor TJvZoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 100;
  Width := 100;
  FDelay := 100;
  FZoomLevel := 100;
  FCrosshairSize := 20;
  FCrosshairColor := clBlack;
  FCacheOnDeactivate := True;
  FActive := True;

  //Create the canvas to retrieve informations
  FDesktopCanvas := TCanvas.Create;
  FDesktopCanvas.Handle := GetDC(HWND_DESKTOP);

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := PaintMe;
  FTimer.Interval := 100;
end;

destructor TJvZoom.Destroy;
begin
  ReleaseDC(HWND_DESKTOP, FDesktopCanvas.Handle);
  FDesktopCanvas.Free;
  inherited Destroy;
end;

procedure TJvZoom.Cache;
begin
  if not Assigned(FCacheBitmap) then
    FCacheBitmap := TBitmap.Create;
  FCacheBitmap.Width := Width;
  FCacheBitmap.Height := Height;
  FCacheBitmap.Canvas.CopyRect(ClientRect, Canvas, ClientRect);
end;

procedure TJvZoom.FlushCache;
begin
  FreeAndNil(FCacheBitmap);
end;

procedure TJvZoom.Loaded;
begin
  inherited Loaded;
  FTimer.Enabled := FActive;
end;

procedure TJvZoom.Paint;
begin
  if Active then
    PaintZoom
  else
  begin
    if Assigned(FCacheBitmap) then
      Canvas.Draw(0, 0, FCacheBitmap)
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
  end;
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TJvZoom.PaintMe(Sender: TObject);
begin
  { Reading Canvas.Handle will implicitly set the canvas handle to the
    control's device context
    Calling PaintWindow will lock the canvas and call Paint
  }
  PaintWindow(Canvas.Handle);
end;

procedure TJvZoom.PaintZoom;
var
  P: TPoint;
  X, Y, Dx, Dy: Integer;
  SourceRect: TRect;
begin
  GetCursorPos(P);

  //Only draw if on a different position
  if (P.X = FLastPoint.X) and (P.Y = FLastPoint.Y) then
    Exit;

  //Analyse the point
  FLastPoint := P;

  //Create the area to Copy
  X := (Width div 2) * FZoomLevel div 100;
  Y := (Height div 2) * FZoomLevel div 100;

  Dx := 0;
  Dy := 0;

  if P.X < X then
  begin
    Dx := (P.X - X - 1) * 100 div FZoomLevel;
    P.X := X
  end
  else
  if P.X + X > Screen.Width then
  begin
    Dx := (X - (Screen.Width - P.X) + 1) * 100 div FZoomLevel;
    P.X := Screen.Width - X;
  end;
  if P.Y < Y then
  begin
    Dy := (P.Y - Y - 1) * 100 div FZoomLevel;
    P.Y := Y
  end
  else
  if P.Y + Y > Screen.Height then
  begin
    Dy := (Y - (Screen.Height - P.Y) + 1) * 100 div FZoomLevel;
    P.Y := Screen.Height - Y;
  end;

  SourceRect.Left := P.X - X;
  SourceRect.Top := P.Y - Y;
  SourceRect.Right := P.X + X;
  SourceRect.Bottom := P.Y + Y;

  //Draw the area around the mouse
  Canvas.CopyRect(Rect(0, 0, Width, Height), FDesktopCanvas, SourceRect);

  if FCrosshair then
    with Canvas do
    begin
      Pen.Color := FCrosshairColor;
      Pen.Style := psSolid;
      MoveTo(Width div 2 + Dx, Height div 2 - FCrosshairSize div 2 + Dy);
      LineTo(Width div 2 + Dx, Height div 2 + FCrosshairSize div 2 + Dy);
      MoveTo(Width div 2 - FCrosshairSize div 2 + Dx, Height div 2 + Dy);
      LineTo(Width div 2 + FCrosshairSize div 2 + Dx, Height div 2 + Dy);
    end;

  if Assigned(FOnContentsChanged) then
    FOnContentsChanged(Self);
end;

procedure TJvZoom.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;

  FActive := Value;

  if not (csReading in ComponentState) then
    FTimer.Enabled := FActive;

  if not FActive then
  begin
    if FCacheOnDeactivate then
      Cache
    else
      Invalidate;
  end;
end;

procedure TJvZoom.SetCacheOnDeactivate(const Value: Boolean);
begin
  if Value = FCacheOnDeactivate then
    Exit;

  FCacheOnDeactivate := Value;

  if not FCacheOnDeactivate then
  begin
    FlushCache;
    if not Active then
      Invalidate;
  end;
end;

procedure TJvZoom.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := Value;
end;

procedure TJvZoom.WMSize(var Msg: TWMSize);
begin
  //On resize, refresh it
  inherited;
  { Forget the old point; thus force repaint }
  FLastPoint := Point(High(LongInt), High(LongInt));
  PaintMe(Self);
end;

end.

