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

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com], Antoine Potten [jvcl att antp dott be]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvZoom;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  JvComponent;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvZoom = class(TJvCustomControl)
  private
    FTimer: TTimer;
    FActive: Boolean;
    FZoomLevel: Integer;
    FDelay: Cardinal;
    FLastPoint: TPoint;
    FCrossHair: Boolean;
    FCrosshairColor: TColor;
    FCrosshairSize: Integer;
    FOnContentsChanged: TNotifyEvent;
    FCacheOnDeactivate: Boolean;
    FCacheBitmap: TBitmap;
    FCrossHairPicture: TPicture;
    procedure SetActive(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetZoomLevel(const Value: Integer);
    procedure SetCacheOnDeactivate(const Value: Boolean);
    procedure SetCrossHairPicture(const Value: TPicture);
    function GetZoomPercentage: Integer;
    procedure SetZoomPercentage(const Value: Integer);
    procedure PaintMe(Sender: TObject);
    procedure SetCrossHair(const Value: Boolean);
  protected
    procedure Resize; override;
    procedure Paint; override;
    procedure PaintZoom;
    procedure Loaded; override;
    procedure Cache;
    procedure FlushCache;
    procedure DoContentsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ForceUpdate;
    procedure ZoomInAt(X, Y: Integer);
  published
    property Anchors;
    property Align;
    property Constraints;
    property Color;
    property Enabled;
    property Visible;
    property Active: Boolean read FActive write SetActive default True;
    property ZoomLevel: Integer read FZoomLevel write SetZoomLevel default 100;
    property ZoomPercentage: Integer read GetZoomPercentage write SetZoomPercentage stored False;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property Crosshair: Boolean read FCrossHair write SetCrossHair default False;
    property CrossHairPicture: TPicture read FCrossHairPicture write SetCrossHairPicture;
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJVCLUtils;

constructor TJvZoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrossHairPicture := TPicture.Create;
  FCrossHairPicture.OnChange := PaintMe;
  Height := 100;
  Width := 100;
  FDelay := 100;
  FZoomLevel := 100;
  FCrosshairSize := 20;
  FCrosshairColor := clBlack;
  FCacheOnDeactivate := True;
  FActive := True;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := PaintMe;
  FTimer.Interval := 100;
end;

destructor TJvZoom.Destroy;
begin
  FCacheBitmap.Free;
  FCacheBitmap := nil;
  FCrossHairPicture.OnChange := nil;
  FCrossHairPicture.Free;
  { Timer is automatically freed }
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
  DesktopCanvas: TJvDesktopCanvas;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    //Only draw if on a different position
    if (P.X = FLastPoint.X) and (P.Y = FLastPoint.Y) then
      Exit;
  end
  else
    P := FLastPoint;

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
    P.X := X;
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
    P.Y := Y;
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
  DesktopCanvas := TJvDesktopCanvas.Create;
  Canvas.CopyRect(Rect(0, 0, Width, Height), DesktopCanvas, SourceRect);
  DesktopCanvas.Free;

  if FCrossHair then
  begin
    if (FCrossHairPicture.Graphic <> nil) and not FCrossHairPicture.Graphic.Empty then
    begin
      FCrossHairPicture.Graphic.Transparent := True;
      Canvas.Draw((Width - FCrossHairPicture.Graphic.Width) div 2 + Dx,
        (Height - FCrossHairPicture.Graphic.Height) div 2 + Dy,FCrossHairPicture.Graphic);
    end
    else
    with Canvas do
    begin
      Pen.Color := FCrosshairColor;
      Pen.Style := psSolid;
      MoveTo(Width div 2 + Dx, Height div 2 - FCrosshairSize div 2 + Dy);
      LineTo(Width div 2 + Dx, Height div 2 + FCrosshairSize div 2 + Dy);
      MoveTo(Width div 2 - FCrosshairSize div 2 + Dx, Height div 2 + Dy);
      LineTo(Width div 2 + FCrosshairSize div 2 + Dx, Height div 2 + Dy);
    end;
  end;
  if Enabled then
    DoContentsChanged;
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
  end
  else
  if not Enabled then
    FLastPoint := Point(MaxLongint, MaxLongint);
  Invalidate;
end;

procedure TJvZoom.SetCacheOnDeactivate(const Value: Boolean);
begin
  if Value <> FCacheOnDeactivate then
  begin
    FCacheOnDeactivate := Value;

    if not Value then
    begin
      FlushCache;
      if not Active then
        Invalidate;
    end;
  end;
end;

procedure TJvZoom.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := Value;
end;

procedure TJvZoom.SetZoomLevel(const Value: Integer);
begin
  if (FZoomLevel <> Value) and (Value > 0) then
  begin
    FZoomLevel := Value;
    { Forget the old point; thus force repaint }
    if Enabled then
      FLastPoint := Point(MaxLongint, MaxLongint);
    Invalidate;
  end;
end;

procedure TJvZoom.SetCrossHair(const Value: Boolean);
begin
  if FCrossHair <> Value then
  begin
    FCrossHair := Value;
    { Forget the old point; thus force repaint }
    ForceUpdate;
  end;
end;

procedure TJvZoom.Resize;
begin
  //On resize, refresh it
  inherited Resize;
  { Forget the old point; thus force repaint }
  if Enabled then
    FLastPoint := Point(MaxLongint, MaxLongint);
  PaintMe(Self);
end;

function TJvZoom.GetZoomPercentage: Integer;
begin
  if ZoomLevel <> 0 then
    Result := Trunc((100.0 / ZoomLevel) * 100.0)
  else
    Result := 0;
end;

procedure TJvZoom.SetZoomPercentage(const Value: Integer);
begin
  if Value <> 0 then
    ZoomLevel := Trunc((100.0 / Value) * 100.0);
end;

procedure TJvZoom.SetCrossHairPicture(const Value: TPicture);
begin
  FCrossHairPicture.Assign(Value);
end;

procedure TJvZoom.ZoomInAt(X, Y: Integer);
begin
  if Enabled then
    SetCursorPos(X,Y)
  else
  begin
    if (FLastPoint.X <> X) or (FLastPoint.Y <> Y) then
    begin
      FLastPoint.X := X;
      FLastPoint.Y := Y;
      DoContentsChanged;
    end;
  end;
  Invalidate;
end;

procedure TJvZoom.ForceUpdate;
begin
  if Enabled then
    FLastPoint := Point(MaxLongint, MaxLongint);
  Invalidate;
end;

procedure TJvZoom.DoContentsChanged;
begin
  if Assigned(FOnContentsChanged) then
    FOnContentsChanged(Self);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
