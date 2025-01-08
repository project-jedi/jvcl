{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransled.PAS, released on 2002-12-23.

The Initial Developer of the Original Code is Thomas Hensle (http://www.thensle.de)
Portions created by Thomas Hensle are Copyright (C) 2002 Thomas Hensle.
Portions created by XXXX Corp. are Copyright (C) 2002, 2003 XXXX Corp.
All Rights Reserved.

Contributor(s):
  Thomas Huber (Thomas_D_huber att t-online dott de)
  peter3 (load new image only when needed, center image in control, draw border at designtime)
  marcelb (merging of JvTransLED and JvBlinkingLED)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvLED;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Controls, Forms, Graphics, ExtCtrls, Classes,
  JvComponent;

type
  TJvCustomLED = class(TJvGraphicControl)
  private
    FImgPict: TBitmap;
    FImgMask: TBitmap;
    FTimer: TTimer;
    FColorOn: TColor;
    FColorOff: TColor;
    FActive: Boolean;
    FStatus: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetInterval(Value: Cardinal);
    function GetInterval: Cardinal;
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
    procedure SetStatus(Value: Boolean);
    function GetStatus: Boolean;
    procedure DoBlink(Sender: TObject);
  protected
    procedure ColorChanged; override;
    procedure Paint; override;
    property Active: Boolean read GetActive write SetActive default False;
    property Color default clLime;
    property ColorOn: TColor read FColorOn write SetColorOn default clLime;
    property ColorOff: TColor read FColorOff write SetColorOff default clRed;
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    property Status: Boolean read GetStatus write SetStatus default True;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvLED = class(TJvCustomLED)
  published
    property Active;
    property Align;
    property Anchors;
    property AutoSize;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property ColorOn;
    property ColorOff;
    property Constraints;
    property DragMode;
    property Height default 17;
    property Interval;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Status;
    property Visible;
    property Width default 17;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
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
  SysUtils;

{$R JvLED.res}

const
  cMaskLEDName = 'JvCustomLEDMASK';
  cGreenLEDName = 'JvCustomLEDGREEN';

//=== { TJvCustomLED } =======================================================

constructor TJvCustomLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImgPict := TBitmap.Create;
  FImgMask := TBitmap.Create;
  FImgMask.LoadFromResourceName(HInstance, cMaskLEDName);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoBlink;
  FTimer.Interval := 1000;
  Color := clLime;
  Width := 17;
  Height := 17;
  ColorOn := clLime;
  ColorOff := clRed;
  Active := False;
  Status := True;
end;

destructor TJvCustomLED.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.OnTimer := nil;
  FImgPict.Free;
  FImgMask.Free;
  inherited Destroy;
end;

procedure TJvCustomLED.Paint;
var
  DestRect, SrcRect: TRect;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
  SrcRect := Rect(0, 0, FImgPict.Width, FImgPict.Height);
  DestRect := SrcRect;
  OffsetRect(DestRect, (ClientWidth - FImgPict.Width) div 2, (ClientHeight - FImgPict.Height) div 2);
  Canvas.CopyMode := cmSrcAnd;
  with Canvas do
  begin
    CopyRect(DestRect, FImgMask.Canvas, SrcRect);
    CopyMode := cmSrcPaint;
    CopyRect(DestRect, FImgPict.Canvas, SrcRect);
  end;
end;

procedure TJvCustomLED.SetColorOn(Value: TColor);
begin
  FColorOn := Value;
  if Status then
    Color := Value;
end;

procedure TJvCustomLED.SetColorOff(Value: TColor);
begin
  FColorOff := Value;
  if not Status then
    Color := Value;
end;

function TJvCustomLED.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvCustomLED.SetInterval(Value: Cardinal);
begin
  if Value <> FTimer.Interval then
    FTimer.Interval := Value;
end;

function TJvCustomLED.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TJvCustomLED.SetActive(Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := Value;
end;

procedure TJvCustomLED.SetStatus(Value: Boolean);
begin
  if Value <> FStatus then
  begin
    FStatus := Value;
    if FStatus then
      Color := ColorOn
    else
      Color := ColorOff;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TJvCustomLED.GetStatus: Boolean;
begin
  Result := FStatus;
end;

procedure TJvCustomLED.DoBlink(Sender: TObject);
begin
  if not IsIconic(Application.Handle) then
    Status := not Status;
end;

procedure TJvCustomLED.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AutoSize and (Align in [alNone, alCustom]) then
    inherited SetBounds(ALeft, ATop, FImgPict.Width, FImgPict.Height)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;



procedure TJvCustomLED.ColorChanged;
var
  X, Y: Integer;
begin
  inherited ColorChanged;
  { Work around a TBitmap.ReadDIB() bug where FreeContext() is not called. This
    missing call leads to a GDI-Handle leak when the application is minimized
    because then FreeBitmapContexts() is not called. }
  FImgPict.Assign(nil);
  FImgPict.LoadFromResourceName(HInstance, cGreenLEDName);
  FImgPict.PixelFormat := pf24bit;
  for X := 0 to FImgPict.Width - 1 do
    for Y := 0 to FImgPict.Height - 1 do
      if FImgPict.Canvas.Pixels[X, Y] = clLime then
        FImgPict.Canvas.Pixels[X, Y] := Color;
  Repaint;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
