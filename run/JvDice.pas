{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDice.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDice;

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls,
  JvTimer, JvComponent, JvExControls;

type
  TJvDiceValue = 1..6;

  TJvDice = class(TJvCustomControl) // , IJvDenySubClassing
  private
    FActive: Boolean;
    FBitmap: array [TJvDiceValue] of TBitmap;
    FInterval: Cardinal;
    FAutoStopInterval: Cardinal;
    FOnChange: TNotifyEvent;
    FRotate: Boolean;
    FShowFocus: Boolean;
    FTimer: TJvTimer;
    FTickCount: Longint;
    FValue: TJvDiceValue;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    {$IFDEF VisualCLX}
    FAutoSize: Boolean;
    {$ENDIF VisualCLX}
    procedure SetInterval(Value: Cardinal);
    procedure SetRotate(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure SetValue(Value: TJvDiceValue);
    procedure TimerFires(Sender: TObject);
    procedure NewRandomValue;
  protected
    procedure DoFocusChanged(Control: TWinControl); override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure SetAutoSize(Value: Boolean); {$IFDEF VCL} override; {$ENDIF}
    {$IFDEF VCL}
    function GetPalette: HPALETTE; override;
    {$ENDIF VCL}
    procedure AdjustSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Change; dynamic;
    procedure DoStart; dynamic;
    procedure DoStop; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Throw;
  published
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
    property Align;
    {$IFDEF VCL}
    property AutoSize default True;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    {$ENDIF VisualCLX}
    property AutoStopInterval: Cardinal read FAutoStopInterval write FAutoStopInterval default 0;
    property Color;
    property Cursor;
    property DragMode;
    property Enabled;
    property Interval: Cardinal read FInterval write SetInterval default 60;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Rotate: Boolean read FRotate write SetRotate;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus;
    property ShowHint;
    property Anchors;
    property Constraints;
    property TabOrder;
    property TabStop;
    property Value: TJvDiceValue read FValue write SetValue default Low(TJvDiceValue);
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnContextPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
  SysUtils, ImgList,
  JvThemes;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDice.Res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDice.Res}
{$ENDIF LINUX}

constructor TJvDice.Create(AOwner: TComponent);
var
  I: TJvDiceValue;
begin
  inherited Create(AOwner);
  Randomize;
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
  FInterval := 60;
  FValue := Low(TJvDiceValue);
  for I := Low(TJvDiceValue) to High(TJvDiceValue) do
  begin
    FBitmap[I] := TBitmap.Create;
    {$IFDEF VCL}
    FBitmap[I].Handle := LoadBitmap(HInstance, PChar(Format('JV_DICE%d', [Ord(I)])));
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FBitmap[I].LoadFromResourceName(HInstance, Format('JV_DICE%d', [Ord(I)]));
    {$ENDIF VisualCLX}
  end;
  {$IFDEF VCL}
  AutoSize := True;
  {$ENDIF VCL}
  Width := FBitmap[Value].Width + 2;
  Height := FBitmap[Value].Height + 2;
end;

destructor TJvDice.Destroy;
var
  I: TJvDiceValue;
begin
  FOnChange := nil;
  for I := Low(TJvDiceValue) to High(TJvDiceValue) do
    FBitmap[I].Free;
  inherited Destroy;
end;

procedure TJvDice.Throw;
begin
  Value := TJvDiceValue(Random(6) + 1);
end;

procedure TJvDice.NewRandomValue;
var
  Val: Byte;
begin
  repeat
    Val := Random(6) + 1;
  until Val <> Byte(Value);
  Value := TJvDiceValue(Val);
end;

{$IFDEF VCL}
function TJvDice.GetPalette: HPALETTE;
begin
  Result := FBitmap[Value].Palette;
end;
{$ENDIF VCL}

procedure TJvDice.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TJvDice.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

procedure TJvDice.DoFocusChanged(Control: TWinControl);
var
  Active: Boolean;
begin
  Active := (Control = Self);
  if Active <> FActive then
  begin
    FActive := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited DoFocusChanged(Control);
end;

function TJvDice.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := True; // Paint clears the background
end;

procedure TJvDice.AdjustSize;
var
  MinSide: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    if AutoSize and (FBitmap[Value].Width > 0) and
      (FBitmap[Value].Height > 0) then
      SetBounds(Left, Top, FBitmap[Value].Width + 2, FBitmap[Value].Height + 2)
    else
    begin
      { Adjust aspect ratio if control size changed }
      MinSide := Width;
      if Height < Width then
        MinSide := Height;
      SetBounds(Left, Top, MinSide, MinSide);
    end;
  end;
end;

procedure TJvDice.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and TabStop and CanFocus then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvDice.Paint;
var
  ARect: TRect;

  procedure DrawBitmap;
  var
    TmpImage: TBitmap;
    IWidth, IHeight: Integer;
    IRect: TRect;
    ImgList: TImageList;
  begin
    IWidth := FBitmap[Value].Width;
    IHeight := FBitmap[Value].Height;
    if (IWidth = 0) and (IHeight = 0) then
      Exit;

    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage := TBitmap.Create;
    ImgList := TImageList.CreateSize(IWidth, IHeight);
    try
      ImgList.AddMasked(FBitmap[Value], FBitmap[Value].TransparentColor);
      TmpImage.Width := IWidth;
      TmpImage.Height := IHeight;
      TmpImage.Canvas.CopyRect(ClientRect, Canvas, ClientRect);
      ImgList.Draw(TmpImage.Canvas, 0, 0, 0);
      InflateRect(ARect, -1, -1);
      Canvas.StretchDraw(ARect, TmpImage);
    finally
      TmpImage.Free;
      ImgList.Free;
    end;
  end;

begin
  Canvas.Brush.Color := Parent.Brush.Color;
  DrawThemedBackground(Self, Canvas, ClientRect);
  ARect := ClientRect;
  DrawBitmap;
  if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(ARect);
end;

procedure TJvDice.TimerFires(Sender: TObject);
var
  Now: Longint;
begin
  NewRandomValue;
  if not FRotate then
  begin
    FTimer.Free;
    FTimer := nil;
    DoStop;
  end
  else
  if AutoStopInterval > 0 then
  begin
    Now := GetTickCount;
    if (Now - FTickCount >= Integer(AutoStopInterval)) or (Now < FTickCount) then
      Rotate := False;
  end;
end;

procedure TJvDice.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDice.SetValue(Value: TJvDiceValue);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Invalidate;
    Change;
  end;
end;

procedure TJvDice.SetAutoSize(Value: Boolean);
begin
  {$IFDEF VCL}
  inherited SetAutoSize(Value);
  {$ENDIF VCL}
  AdjustSize;
  Invalidate;
end;

procedure TJvDice.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if FTimer <> nil then
      FTimer.Interval := FInterval;
  end;
end;

procedure TJvDice.SetRotate(Value: Boolean);
begin
  if FRotate <> Value then
  begin
    if Value then
    begin
      if FTimer = nil then
        FTimer := TJvTimer.Create(Self);
      try
        with FTimer do
        begin
          OnTimer := TimerFires;
          Interval := FInterval;
          Enabled := True;
        end;
        FRotate := Value;
        FTickCount := GetTickCount;
        DoStart;
      except
        FTimer.Free;
        FTimer := nil;
        raise;
      end;
    end
    else
      FRotate := Value;
  end;
end;

procedure TJvDice.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if not (csDesigning in ComponentState) then
      Invalidate;
  end;
end;

end.

