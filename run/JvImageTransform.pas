{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageTransform.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImageTransform;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Graphics, Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows, QGraphics, QControls, QExtCtrls, Types,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvTransformationKind = (ttWipeLeft, ttWipeRight, ttWipeUp, ttWipeDown,
    ttTurnLeft, ttTurnRight, ttTurnUp, ttTurnDown,
    ttWipeDownRight, ttWipeDownLeft, ttWipeUpRight, ttWipeUpLeft);

  TJvImageTransform = class(TJvGraphicControl)
  private
    FPicture1: TPicture;
    FPicture2: TPicture;
    FTimer: TTimer;
    FInterval: Integer;
    FImageShown: Byte;
    FSteps: Integer;
    FType: TJvTransformationKind;
    StepNum: Integer;
    FOnFinished: TNotifyEvent;
    {$IFDEF VisualCLX}
    FAutoSize: Boolean;
    {$ENDIF VisualCLX}
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture1(Value: TPicture);
    procedure SetPicture2(Value: TPicture);
    procedure SetImageShown(Value: Byte);
    procedure SetInterval(Value: Integer);
    procedure SetType(Value: TJvTransformationKind);
  protected
    procedure SetAutoSize(Value: Boolean); {$IFDEF VCL} override; {$ENDIF}
    {$IFDEF VCL}
    function GetPalette: HPALETTE; override;
    {$ENDIF VCL}
    procedure Paint; override;
    procedure TimerTick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF VCL}
    property AutoSize;
    property DragCursor;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    {$ENDIF VisualCLX}
    property DragMode;
    property Enabled;
    property ImageShown: Byte read FImageShown write SetImageShown default 1;
    property Interval: Integer read FInterval write SetInterval default 1;
    property ParentShowHint;
    property Picture1: TPicture read FPicture1 write SetPicture1;
    property Picture2: TPicture read FPicture2 write SetPicture2;
    property PopupMenu;
    property ShowHint;
    property Steps: Integer read FSteps write FSteps default 10;
    property TransformType: TJvTransformationKind read FType write SetType default ttWipeLeft;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
    procedure Transform;
  end;

implementation

constructor TJvImageTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageShown := 1;
  FPicture1 := TPicture.Create;
  FPicture1.OnChange := PictureChanged;
  FPicture2 := TPicture.Create;
  FPicture2.OnChange := PictureChanged;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := False;
  FInterval := 1;
  FType := ttWipeLeft;
  FSteps := 10;
  Height := 105;
  Width := 105;
end;

destructor TJvImageTransform.Destroy;
begin
  FPicture1.Free;
  FPicture2.Free;
  FTimer.Free;
  inherited Destroy;
end;

{$IFDEF VCL}
function TJvImageTransform.GetPalette: HPALETTE;
begin
  if FPicture1.Graphic is TBitmap then
    Result := TBitmap(FPicture1.Graphic).Palette
  else
    Result := 0;
end;
{$ENDIF VCL}

procedure TJvImageTransform.SetAutoSize(Value: Boolean);
begin
  {$IFDEF VCL}
  inherited SetAutoSize(Value);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FAutoSize := Value;
  {$ENDIF VisualCLX}
  PictureChanged(Self);
end;

procedure TJvImageTransform.SetPicture1(Value: TPicture);
begin
  FPicture1.Assign(Value);
end;

procedure TJvImageTransform.SetPicture2(Value: TPicture);
begin
  FPicture2.Assign(Value);
end;

procedure TJvImageTransform.SetImageShown(Value: Byte);
begin
  if Value in [1, 2] then
  begin
    FImageShown := Value;
    Invalidate;
  end;
end;

procedure TJvImageTransform.SetInterval(Value: Integer);
begin
  FInterval := Value;
  if Value > 1000 then
    FInterval := 1000;
  if Value < 1 then
    FInterval := 1;
  {Reset the timer interval}
  if FTimer <> nil then
    FTimer.Interval := FInterval;
end;

procedure TJvImageTransform.SetType(Value: TJvTransformationKind);
begin
  FType := Value;
end;

procedure TJvImageTransform.PictureChanged(Sender: TObject);
begin
  if AutoSize and (Picture1.Width > 0) and (Picture1.Height > 0) then
    SetBounds(Left, Top, Picture1.Width, Picture1.Height);
  if (Picture1.Graphic is TBitmap) and (Picture1.Width = Width) and (Picture1.Height = Height) then
    ControlStyle := ControlStyle + [csOpaque]
  else
    ControlStyle := ControlStyle - [csOpaque];
  Invalidate;
end;

procedure TJvImageTransform.Transform;
begin
  StepNum := 0;
  {Turn on the timer}
  if FTimer <> nil then
  begin
    FTimer.Interval := 1;
    FTimer.Enabled := True;
  end;
end;

procedure TJvImageTransform.TimerTick;
begin
  if FTimer <> nil then
    FTimer.Interval := FInterval;
  Inc(StepNum);
  Repaint;
  if FTimer <> nil then
    if FTimer.Enabled then
      if StepNum >= Steps then
      begin
        FTimer.Enabled := False;
        if ImageShown = 1 then
          ImageShown := 2
        else
          ImageShown := 1;
        if Assigned(FOnFinished) then
          FOnFinished(Self);
      end;
end;

procedure TJvImageTransform.Paint;
var
  PctDone: Real;
  PctLeft: Real;
  DestRect: TRect;
  ShowCurrentImage: Boolean;
  Other: TGraphic;
begin
  with inherited Canvas do
  begin
    if csDesigning in ComponentState then
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
    ShowCurrentImage := False;
    if FTimer <> nil then
      if not FTimer.Enabled then
        ShowCurrentImage := True;
    if StepNum < 1 then
      ShowCurrentImage := True;
    if ShowCurrentImage then
    begin
      if ImageShown = 1 then
        Draw(0, 0, Picture1.Graphic)
      else
        Draw(0, 0, Picture2.Graphic);
      Exit;
    end;
    if FSteps > 0 then
      PctDone := (StepNum / FSteps)
    else
      PctDone := 0.0;
    PctLeft := 1 - PctDone;
    // (rom) simplified with variable Other
    if ImageShown = 1 then
      Other := Picture2.Graphic
    else
      Other := Picture1.Graphic;
    if PctDone > 0.0 then
      case TransformType of
        ttWipeLeft:
          Draw(Round(Picture1.Width * PctLeft), 0, Other);
        ttWipeRight:
          Draw(-Round(Picture1.Width * PctLeft), 0, Other);
        ttWipeUp:
          Draw(0, Round(Picture1.Height * PctLeft), Other);
        ttWipeDown:
          Draw(0, -Round(Picture1.Height * PctLeft), Other);
        ttTurnLeft:
          begin
            with Picture1 do
              DestRect := Rect(Round(Width * PctLeft), 0,
                Round(Width * PctLeft) +
                Round(Width * PctDone), Height);
            StretchDraw(DestRect, Other);
          end;
        ttTurnRight:
          begin
            with Picture1 do
              DestRect := Rect(0, 0, Round(Width * PctDone), Height);
            StretchDraw(DestRect, Other);
          end;
        ttTurnUp:
          begin
            with Picture1 do
              DestRect := Rect(0, Round(Height * PctLeft),
                Width, Round(Height * PctLeft) +
                Round(Height * PctDone));
            StretchDraw(DestRect, Other);
          end;
        ttTurnDown:
          begin
            with Picture1 do
              DestRect := Rect(0, 0, Width, Round(Height * PctDone));
            StretchDraw(DestRect, Other);
          end;
        ttWipeDownRight:
          Draw(-Round(Picture1.Width * PctLeft), -Round(Picture1.Height * PctLeft), Other);
        ttWipeDownLeft:
          Draw(Round(Picture1.Width * PctLeft), -Round(Picture1.Height * PctLeft), Other);
        ttWipeUpRight:
          Draw(-Round(Picture1.Width * PctLeft), Round(Picture1.Height * PctLeft), Other);
        ttWipeUpLeft:
          Draw(Round(Picture1.Width * PctLeft), Round(Picture1.Height * PctLeft), Other);
      end;
  end;
end;

end.

