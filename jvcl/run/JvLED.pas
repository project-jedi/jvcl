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
  Thomas Huber (Thomas_D_huber@t-online.de)
  peter3 (load new image only when needed, center image in control, draw border at designtime)
  marcelb (merging of JvTransLED and JvBlinkingLED)

Last Modified: 2002-12-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvLED;

interface

uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Classes, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows, QControls, Classes, QGraphics, Types,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvCustomLED = class(TJvGraphicControl)
  private
    FImgPict: TBitmap;
    FImgMask: TBitmap;
    FThread: TThread;
    FColorOn: TColor;
    FColorOff: TColor;
    FStatus: Boolean;
    FOnChange: TNotifyEvent;
    FInterval: Cardinal;
    {$IFDEF VisualCLX}
    FAutoSize: Boolean;
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF VisualCLX}
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetInterval(Value: Cardinal);
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
    procedure SetStatus(Value: Boolean);
    function GetStatus: Boolean;
    procedure DoBlink(Sender: TObject; BlinkOn: Boolean);
  protected
    procedure ColorChanged; override;
    procedure Paint; override;
    property Active: Boolean read GetActive write SetActive default False;
    property Color default clLime;
    property ColorOn: TColor read FColorOn write SetColorOn default clLime;
    property ColorOff: TColor read FColorOff write SetColorOff default clRed;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property Status: Boolean read GetStatus write SetStatus default True;
    {$IFDEF VisualCLX}
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TJvLED = class(TJvCustomLED)
  published
    property Active;
    property Align;
    property Anchors;
    property AutoSize;
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
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

implementation

uses
  SysUtils;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvLED.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvLED.res}
{$ENDIF LINUX}

const
  cMaskLEDName = 'JVTR_MASK_LED';
  cGreenLEDName = 'JVTR_GREEN_LED';

type
  TBlinkEvent = procedure(Sender: TObject; BlinkOn: boolean) of object;
  TBlinkThread = class(TThread)
  private
    FOnBlink: TBlinkEvent;
    FBlinkOn: Boolean;
    FInterval: Cardinal;
    procedure DoBlink;
  public
    constructor Create(Interval: Cardinal);
    procedure Execute; override;
    property Interval: Cardinal read FInterval;
    property OnBlink: TBlinkEvent read FOnBlink write FOnBlink;
  end;

//=== TJvCustomLED ===========================================================

constructor TJvCustomLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImgPict := TBitmap.Create;
  FImgMask := TBitmap.Create;
  FImgMask.LoadFromResourceName(HInstance, cMaskLEDName);
  Color := clLime;
  Width := 17;
  Height := 17;
  FInterval := 1000;
  ColorOn := clLime;
  ColorOff := clRed;
  Active := False;
  Status := True;
  {$IFDEF VisualCLX}
  FAutoSize := True;
  {$ENDIF VisualCLX}
end;

destructor TJvCustomLED.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  FreeAndNil(FThread);
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
  Canvas.CopyRect(DestRect, FImgMask.Canvas, SrcRect);
  Canvas.CopyMode := cmSrcPaint;
  Canvas.CopyRect(DestRect, FImgPict.Canvas, SrcRect);
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

procedure TJvCustomLED.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FThread <> nil then
    begin
      FreeAndNil(FThread);
      FThread := TBlinkThread.Create(FInterval);
      TBlinkThread(FThread).OnBlink := DoBlink;
      if FInterval > 0 then
        FThread.Resume;
    end;
  end;
end;

procedure TJvCustomLED.SetActive(Value: Boolean);
begin
  if Value then
  begin
    if FThread = nil then
      FThread := TBlinkThread.Create(Interval);
    TBlinkThread(FThread).OnBlink := DoBlink;
    if Interval > 0 then
      FThread.Resume;
  end
  else
  if FThread <> nil then
    FThread.Suspend;
end;

function TJvCustomLED.GetActive: Boolean;
begin
  Result := (FThread <> nil) and (FInterval > 0) and not FThread.Suspended;
end;

procedure TJvCustomLED.SetStatus(Value: Boolean);
begin
  FStatus := Value;
  if Status then
    Color := ColorOn
  else
    Color := ColorOff;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvCustomLED.GetStatus: Boolean;
begin
  Result := FStatus;
end;

procedure TJvCustomLED.DoBlink(Sender: TObject; BlinkOn: Boolean);
begin
  Status := BlinkOn;
end;

procedure TJvCustomLED.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  {$IFDEF COMPILER6_UP}
  if AutoSize and (Align in [alNone, alCustom]) then
  {$ELSE}
  if AutoSize and (Align = alNone) then
  {$ENDIF COMPILER6_UP}
    inherited SetBounds(ALeft, ATop, FImgPict.Width, FImgPict.Height)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

{$IFDEF VisualCLX}
procedure TJvCustomLED.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    if FAutoSize then
      SetBounds(Left, Top, Width, Height);
  end;
end;
{$ENDIF VisualCLX}

//=== TBlinkThread ===========================================================

constructor TBlinkThread.Create(Interval: Cardinal);
begin
  inherited Create(True);
  FInterval := Interval;
end;

procedure TBlinkThread.DoBlink;
begin
  if Assigned(FOnBlink) then
    FOnBlink(Self, FBlinkOn);
  FBlinkOn := not FBlinkOn;
end;

procedure TBlinkThread.Execute;
begin
  FBlinkOn := False;
  while not Terminated and not Suspended do
  begin
    Synchronize(DoBlink);
    Sleep(FInterval);
  end;
end;

procedure TJvCustomLED.ColorChanged;
var
  X, Y: Integer;
begin
  inherited ColorChanged;
  FImgPict.LoadFromResourceName(HInstance, cGreenLEDName);
  FImgPict.PixelFormat := pf24bit;
  for X := 0 to FImgPict.Width - 1 do
    for Y := 0 to FImgPict.Height - 1 do
      if FImgPict.Canvas.Pixels[X, Y] = clLime then
        FImgPict.Canvas.Pixels[X, Y] := Color;
  Repaint;
end;

end.

