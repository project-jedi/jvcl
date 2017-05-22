{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimPID.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSimPID;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Graphics, Controls,
  JvComponent;

type
  TJvSymFunc = (sfPid, sfAdd, sfCompare, sfRamp, sfMul);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSimPID = class(TJvGraphicControl)
  private
    FMV: Extended;
    FMVColor: TColor;
    FSP: Extended;
    FSPColor: TColor;
    FCV: Extended;
    FCVColor: TColor;
    FKD: Extended;
    FKP: Extended;
    FKI: Extended;
    FI: Extended;
    FD: Extended;
    FDirect: Boolean;
    FManual: Boolean;
    FSource: TJvSimPID;
    FActive: Boolean;
    FSymFunc: TJvSymFunc;
    procedure SetMV(Value: Extended);
    procedure SetMVColor(Value: TColor);
    procedure SetSP(const Value: Extended);
    procedure SetSPColor(const Value: TColor);
    procedure SetCV(const Value: Extended);
    procedure SetCVColor(const Value: TColor);
    procedure SetKD(const Value: Extended);
    procedure SetKI(const Value: Extended);
    procedure SetKP(const Value: Extended);
    procedure CalcOut;
    procedure SetDirect(const Value: Boolean);
    procedure SetManual(const Value: Boolean);
    procedure SetSource(const Value: TJvSimPID);
    procedure SetActive(const Value: Boolean);
    procedure SetSymFunc(const Value: TJvSymFunc);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    property SymFunc: TJvSymFunc read FSymFunc write SetSymFunc;
    property Source: TJvSimPID read FSource write SetSource;
    property MV: Extended read FMV write SetMV;
    property MVColor: TColor read FMVColor write SetMVColor default clRed;
    property SP: Extended read FSP write SetSP;
    property SPColor: TColor read FSPColor write SetSPColor default clLime;
    property CV: Extended read FCV write SetCV;
    property CVColor: TColor read FCVColor write SetCVColor default clYellow;
    property KP: Extended read FKP write SetKP;
    property KI: Extended read FKI write SetKI;
    property KD: Extended read FKD write SetKD;
    property Direct: Boolean read FDirect write SetDirect default False;
    property Manual: Boolean read FManual write SetManual default False;
    property Active: Boolean read FActive write SetActive default False;

    property Align;
    property Anchors;
    property Color default clWhite;
    property Height default 100;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Width default 20;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER9_UP}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
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
  JvJVCLUtils;


constructor TJvSimPID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  MVColor := clRed;
  SPColor := clLime;
  CVColor := clYellow;
  Direct := False;
  Manual := False;
  Active := False;
  FMV := 50;
  FSP := 50;
  FCV := 50;
  FKP := 0.5;
  FKI := 0;
  FKD := 0;
  Width := 20;
  Height := 100;
end;

procedure TJvSimPID.SetMV(Value: Extended);
var
  MVOld: Extended;
begin
  MVOld := FMV;
  if Value <> FMV then
  begin
    if Value > 100 then
      MV := 100
    else
    if Value < 0 then
      MV := 0
    else
      FMV := Value;
  end;
  FI := FI + KI * (FMV - FSP);
  if FI > 50 then
    FI := 50;
  if FI < -50 then
    FI := -50;
  FD := KD * (FMV - MVOld);
  if FD > 50 then
    FD := 50;
  if FD < -50 then
    FD := -50;
  CalcOut;
end;

procedure TJvSimPID.SetMVColor(Value: TColor);
begin
  if Value <> FMVColor then
  begin
    FMVColor := Value;
    Invalidate;
  end;
end;

procedure TJvSimPID.Paint;

  procedure DrawValue(Left, Right: Integer; Value: Extended; AColor: TColor);
  var
    DrawRect: TRect;
  begin
    DrawRect.Left := Left;
    DrawRect.Right := Right;
    DrawRect.Top := DrawRect.Top + Round((100 - Value) *
      (DrawRect.Bottom - DrawRect.Top) / 100);
    DrawRect.Bottom := DrawRect.Bottom;
    Canvas.Brush.Color := AColor;
    Canvas.FillRect(DrawRect);
    Canvas.Brush.Color := Color;
    DrawRect.Bottom := DrawRect.Top;
    DrawRect.Top := DrawRect.Top;
    Canvas.FillRect(DrawRect);
  end;

var
  bw: Integer;
  DrawRect: TRect;
begin
  DrawRect := ClientRect;
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);
  InflateRect(DrawRect, -1, -1);

  bw := (DrawRect.Right - DrawRect.Left) div 3;
  // first draw the Measured Value
  DrawValue(DrawRect.Left + bw, DrawRect.Right - bw, SP, SPColor);
  // and now the SetPoint
  DrawValue(DrawRect.Left, DrawRect.Left + bw, MV, MVColor);
  // draw the Corrective Value (CV)
  DrawValue(DrawRect.Right - bw, DrawRect.Right, CV, CVColor);
end;

procedure TJvSimPID.SetSP(const Value: Extended);
begin
  if Value <> FSP then
  begin
    if Value > 100 then
      FSP := 100
    else
    if Value < 0 then
      FSP := 0
    else
      FSP := Value;
    CalcOut;
  end;
end;

procedure TJvSimPID.SetSPColor(const Value: TColor);
begin
  if Value <> FSPColor then
  begin
    FSPColor := Value;
    Invalidate;
  end;
end;

procedure TJvSimPID.SetCV(const Value: Extended);
begin
  if Value <> FCV then
  begin
    if Value > 100 then
      FCV := 100
    else
    if Value < 0 then
      FCV := 0
    else
      FCV := Value;
  end;
  Invalidate;
end;

procedure TJvSimPID.SetCVColor(const Value: TColor);
begin
  if Value <> FCVColor then
  begin
    FCVColor := Value;
    Invalidate;
  end;
end;

procedure TJvSimPID.SetKD(const Value: Extended);
begin
  FKD := Value;
end;

procedure TJvSimPID.SetKI(const Value: Extended);
begin
  FKI := Value;
  if FKI = 0 then
    FI := 0;
end;

procedure TJvSimPID.SetKP(const Value: Extended);
begin
  FKP := Value;
end;

procedure TJvSimPID.CalcOut;
var
  Output: Extended;
begin
  if not Manual then
  begin
    if Direct then
      Output := 50 + KP * (MV - SP) + FI + FD
    else
      Output := 50 - (KP * (MV - SP) + FI + FD);
    SetCV(Output);
  end;
end;

procedure TJvSimPID.SetDirect(const Value: Boolean);
begin
  FDirect := Value;
end;

procedure TJvSimPID.SetManual(const Value: Boolean);
begin
  FManual := Value;
end;

procedure TJvSimPID.SetSource(const Value: TJvSimPID);
begin
  ReplaceComponentReference(Self, Value, TComponent(FSource));
end;

procedure TJvSimPID.Execute;
var
  Value: Extended;
begin
  if Active then
    if Assigned(FSource) then
    begin
      Value := Source.CV;
      SetMV(Value);
    end;
end;

procedure TJvSimPID.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TJvSimPID.SetSymFunc(const Value: TJvSymFunc);
begin
  FSymFunc := Value;
end;

procedure TJvSimPID.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Source) and (Operation = opRemove) then
    Source := nil;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.