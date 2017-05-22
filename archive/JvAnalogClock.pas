{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnalogClock.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is Frenk Vrtariè [VFrenk1@volja.net]
Portions created by Frenk Vrtariè are Copyright (C) 2002 Frenk Vrtariè.
All Rights Reserved.

Contributor(s):
Uljaki Sándor [ujlaki.sandor@drotposta.hu]
ccrows

Last Modified: 2002-12-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAnalogClock;

interface

uses
  {$IFDEF VCL}
  Windows, Graphics, Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QExtCtrls,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JvTypes, JvJCLUtils, JvComponent, JvThemes;

type
  TJvNotifyTime = procedure(Sender: TObject; Hour, Min, Sec: Integer) of object;
  TJvHourStyle = (hsLine, hsCircle, hsNumber, hsNumberInCircle);
  TJvHourMarks = (hmNone, hmFour, hmAll);

  // Bianconi
  TJvAnalogClock = class; // forward declaration

  TJvAlarmInfo = class(TPersistent)
  private
    FOwner: TJvAnalogClock;
    FDate: TDateTime;
    FEnabled: Boolean;
    FColorOff: TColor;
    FColorOn: TColor;
    FVisible: Boolean;
    FTrigger: TJvTriggerKind;
    FWidthMin: Integer;
    FWidthHour: Integer;
    procedure SetAlarmEnabled(AValue: Boolean);
    function GetAlarmDate: TDateTime;
    procedure SetAlarmDate(AValue: TDateTime);
    procedure SetColorOn(AValue: TColor);
    procedure SetColorOff(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidthMin(AValue: Integer);
    procedure SetWidthHour(AValue: Integer);
  public
    constructor Create(AOwner: TJvAnalogClock);
    destructor Destroy; override;
  published
    property Date: TDateTime read GetAlarmDate write SetAlarmDate;
    property Enabled: Boolean read FEnabled write SetAlarmEnabled;
    property ColorOn: TColor read FColorOn write SetColorOn;
    property ColorOff: TColor read FColorOff write SetColorOff;
    property Visible: Boolean read FVisible write SetVisible;
    property Trigger: TJvTriggerKind read FTrigger write FTrigger;
    property WidthMin: Integer read FWidthMin write SetWidthMin;
    property WidthHour: Integer read FWidthHour write SetWidthHour;
  end;
  // End of Bianconi

  // (rom) renamed all Hr name parts to proper Hour

  TJvAnalogClock = class(TJvCustomPanel)
  private
    FHourStyle: TJvHourStyle;
    FMinuteStyle: TJvHourStyle;
    FHourMarks: TJvHourMarks;
    FHourSize: Integer;
    FMinuteSize: Integer;
    FMinuteFontSize: Integer;

    FOldSecond: Word;

    FSeconds: Boolean;
    FActive: Boolean;
    FSpider: Boolean;
    FSecJump: Boolean;
    FTime: TDateTime;
    FOffset: Integer;
    FShowDate: Boolean;

    FMinMarks: Boolean;
    FColorHour: TColor;
    FColorHourIn: TColor;
    FColorMin: TColor;
    FColorMinIn: TColor;
    FColorHandHour: TColor;
    FColorHandMin: TColor;
    FColorHandSec: TColor;

    FWidthHandMin: Byte;
    FWidthHandHour: Byte;
    FWidthHandSec: Byte;
    FWidthHour: Byte;
    FWidthMin: Byte;

    FCenterSize: Byte;
    FCenterColor: TColor;

    FSecOver: Boolean;

    FHalfSecond: Integer;
    FClockHour: Integer;
    FClockMin: Integer;
    FClockSec: Integer;
    FCenterX: Integer;
    FCenterY: Integer;
    FCurMarkX: Integer;
    FCurMarkY: Integer;
    FCurMark: Integer;
    FCurMark23: Integer;

    FOldHour: Integer;
    FOldMin: Integer;
    FOldSec: Integer;
    FDrawRect: TRect;
    FOldDate: string;
    DateBottom: Boolean;

    FTimer: TTimer;

    FOnChangeSec: TJvNotifyTime;
    FOnChangeMin: TJvNotifyTime;
    FOnChangeHour: TJvNotifyTime;
    // Bianconi
    FOnAlarm: TNotifyEvent;
    FAlarm: TJvAlarmInfo;
    // End of Bianconi

    //    pfMinFont :TFont;
    procedure SetActive(Value: Boolean);
    procedure SetShowDate(Value: Boolean);
    procedure SetSecJump(Value: Boolean);
    procedure SetSpider(Value: Boolean);
    procedure SetMinMarks(Value: Boolean);
    procedure SetHourStyle(Value: TJvHourStyle);
    procedure SetMinuteStyle(Value: TJvHourStyle);
    procedure SetHourMarks(Value: TJvHourMarks);
    procedure SetHourSize(Value: Integer);
    procedure SetMinSize(Value: Integer);
    procedure SetMinFontSize(Value: Integer);
    procedure SetTime(Value: TDateTime);
    procedure SetOffset(Value: Integer);
    procedure SetColorHour(Value: TColor);
    procedure SetColorHourIn(Value: TColor);
    procedure SetColorMin(Value: TColor);
    procedure SetColorMinIn(Value: TColor);

    procedure SetColorHandHour(Value: TColor);
    procedure SetColorHandMin(Value: TColor);
    procedure SetColorHandSec(Value: TColor);

    procedure SetWidthHandMin(Value: Byte);
    procedure SetWidthHandHour(Value: Byte);
    procedure SetWidthHandSec(Value: Byte);
    procedure SetWidthHour(Value: Byte);
    procedure SetWidthMin(Value: Byte);

    procedure InternalPaint;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure ActTimer(Sender: TObject);

    procedure DoAlarm;
    procedure DoChangeSec(AHour, AMin, ASec: Integer);
    procedure DoChangeMin(AHour, AMin, ASec: Integer);
    procedure DoChangeHour(AHour, AMin, ASec: Integer);

    procedure CMDenySubClassing(var Msg: TCMFocusChanged); message CM_DENYSUBCLASSING;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowDate: Boolean read FShowDate write SetShowDate default False;
    property Active: Boolean read FActive write SetActive default True;
    property Time: TDateTime read FTime write SetTime;
    property TimeOffset: Integer read FOffset write SetOffset default 0;
    property SpiderClock: Boolean read FSpider write SetSpider default False;
    property SecJump: Boolean read FSecJump write SetSecJump default False;
    property Seconds: Boolean read FSeconds write FSeconds default True;
    property MinMarks: Boolean read FMinMarks write SetMinMarks default True;
    property HourStyle: TJvHourStyle read FHourStyle write SetHourStyle default hsLine;
    property MinuteStyle: TJvHourStyle read FMinuteStyle write SetMinuteStyle default hsLine;
    property HourMarks: TJvHourMarks read FHourMarks write SetHourMarks default hmAll;
    property HourSize: Integer read FHourSize write SetHourSize default 12;
    property MinuteSize: Integer read FMinuteSize write SetMinSize default 7;
    property MinuteFontSize: Integer read FMinuteFontSize write SetMinFontSize default 7;
    property ColorHour: TColor read FColorHour write SetColorHour default clBlack;
    property ColorHourIn: TColor read FColorHourIn write SetColorHourIn default clBlack;
    property ColorMin: TColor read FColorMin write SetColorMin default clBlack;
    property ColorMinIn: TColor read FColorMinIn write SetColorMinIn default clBlack;
    property ColorHandHour: TColor read FColorHandHour write SetColorHandHour default clBlack;
    property ColorHandMin: TColor read FColorHandMin write SetColorHandMin default clBlack;
    property ColorHandSec: TColor read FColorHandSec write SetColorHandSec default clBlack;
    property WidthHandSec: Byte read FWidthHandSec write SetWidthHandSec default 1;
    property WidthHandMin: Byte read FWidthHandMin write SetWidthHandMin default 3;
    property WidthHandHour: Byte read FWidthHandHour write SetWidthHandHour default 5;
    property WidthHour: Byte read FWidthHour write SetWidthHour default 2;
    property WidthMin: Byte read FWidthMin write SetWidthMin default 1;
    // Bianconi
    property Alarm: TJvAlarmInfo read FAlarm write FAlarm;
    // End of Bianconi
    //    property MinFont :TFont read pfMinFont write pfMinFont;
    property CenterSize: Byte read FCenterSize write FCenterSize default 5;
    property CenterCol: TColor read FCenterColor write FCenterColor default clBlack;
    property Align;
    property BevelWidth;
    property BevelInner default bvRaised;
    property BevelOuter default bvLowered;
    property Color default clBtnFace;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Height default 137;
    property ParentColor;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width default 137;
    property OnChangeSec: TJvNotifyTime read FOnChangeSec write FOnChangeSec;
    property OnChangeMin: TJvNotifyTime read FOnChangeMin write FOnChangeMin;
    property OnChangeHour: TJvNotifyTime read FOnChangeHour write FOnChangeHour;
    // Bianconi
    property OnAlarm: TNotifyEvent read FOnAlarm write FOnAlarm;
    // End of Bianconi
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

//=== TJvAlarmInfo ===========================================================

// Bianconi

constructor TJvAlarmInfo.Create(AOwner: TJvAnalogClock);
begin
  inherited Create;
  FOwner := AOwner;
  FDate := Now;
  FEnabled := False;
  FColorOn := clTeal;
  FColorOff := clGray;
  FTrigger := tkOneShot;
  FWidthHour := FOwner.WidthHandHour;
  FWidthMin := FOwner.WidthHandMin;
end;

destructor TJvAlarmInfo.Destroy;
begin
  FOwner := nil;
  FEnabled := False;
  inherited Destroy;
end;

procedure TJvAlarmInfo.SetAlarmEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    FOwner.Invalidate;
  end;
end;

function TJvAlarmInfo.GetAlarmDate: TDateTime;
begin
  Result := FDate;
end;

procedure TJvAlarmInfo.SetAlarmDate(AValue: TDateTime);
begin
  FDate := AValue;
  FOwner.Invalidate;
end;

procedure TJvAlarmInfo.SetColorOn(AValue: TColor);
begin
  if FColorOn <> AValue then
  begin
    FColorOn := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetColorOff(AValue: TColor);
begin
  if FColorOff <> AValue then
  begin
    FColorOff := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetWidthMin(AValue: Integer);
begin
  if FWidthMin <> AValue then
  begin
    FWidthMin := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetWidthHour(AValue: Integer);
begin
  if FWidthHour <> AValue then
  begin
    FWidthHour := AValue;
    FOwner.Invalidate;
  end;
end;

// End of Bianconi

//=== TJvAnalogClock =========================================================

constructor TJvAnalogClock.Create(AOwner: TComponent);
var
  H, M, S, Ms: Word;
begin
  inherited Create(AOwner);
  BevelInner := bvRaised;
  BevelOuter := bvLowered;
  FHourSize := 12;
  FMinuteSize := 7;
  FMinuteFontSize := 7;

  FSpider := False;
  FSecJump := False;
  FActive := True;

  Color := clBtnFace;
  Width := 137;
  Height := 137;
  Caption := ' ';
  FSeconds := True;
  FMinMarks := True;
  FHalfSecond := 50;

  FHourStyle := hsLine;
  FMinuteStyle := hsLine;
  FHourMarks := hmAll;

  FColorHour := clBlack;
  FColorHourIn := clBlack;
  FColorMin := clBlack;
  FColorMinIn := clBlack;
  FColorHandHour := clBlack;
  FColorHandMin := clBlack;
  FColorHandSec := clBlack;

  FWidthHandSec := 1;
  FWidthHandMin := 3;
  FWidthHandHour := 5;
  FWidthHour := 2;
  FWidthMin := 1;

  FCenterColor := clBlack;
  FCenterSize := 5;

  // Bianconi
  FAlarm := TJvAlarmInfo.Create(Self);
  // End of Bianconi

  DecodeTime(Now, H, M, S, Ms);
  FOldHour := H;
  FOldMin := M;
  FOldSec := S;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := FActive;
  FTimer.Interval := 500;
  FTimer.OnTimer := ActTimer;
end;

destructor TJvAnalogClock.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FAlarm.Enabled := False;
  FAlarm.Free;
  //  pfMinFont.Free;
  inherited Destroy;
end;

procedure TJvAnalogClock.Loaded;
begin
  inherited Loaded;
  InternalPaint;
end;

procedure TJvAnalogClock.Resize;
begin
  inherited Resize;
  InternalPaint;
end;

procedure TJvAnalogClock.Paint;
begin
  inherited Paint;
  InternalPaint;
end;

procedure TJvAnalogClock.SetTime(Value: TDateTime);
begin
  FTime := Value;
  FHalfSecond := 50;
  ActTimer(Self);
end;

procedure TJvAnalogClock.SetOffset(Value: Integer);
begin
  FOffset := Value;
  FHalfSecond := 50;
  if not FActive then
    ActTimer(Self);
end;

procedure TJvAnalogClock.SetActive(Value: Boolean);
begin
  FActive := Value;

  //  if FEnabled and (not FTimer.Enabled) then
  FTimer.Enabled := FActive;

  FHalfSecond := 50;
  if not FActive then
  begin
    FTime := Now;
    ActTimer(Self);
  end;
end;

procedure TJvAnalogClock.SetMinMarks(Value: Boolean);
begin
  FMinMarks := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetHourStyle(Value: TJvHourStyle);
begin
  FHourStyle := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetMinuteStyle(Value: TJvHourStyle);
begin
  FMinuteStyle := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetHourMarks(Value: TJvHourMarks);
begin
  FHourMarks := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetHourSize(Value: Integer);
begin
  FHourSize := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetMinSize(Value: Integer);
begin
  FMinuteSize := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetMinFontSize(Value: Integer);
begin
  FMinuteFontSize := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHour(Value: TColor);
begin
  FColorHour := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHourIn(Value: TColor);
begin
  FColorHourIn := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorMinIn(Value: TColor);
begin
  FColorMinIn := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorMin(Value: TColor);
begin
  FColorMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHandHour(Value: TColor);
begin
  FColorHandHour := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHandMin(Value: TColor);
begin
  FColorHandMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHandSec(Value: TColor);
begin
  FColorHandSec := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthHandSec(Value: Byte);
begin
  FWidthHandSec := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthHandMin(Value: Byte);
begin
  FWidthHandMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthHandHour(Value: Byte);
begin
  FWidthHandHour := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthHour(Value: Byte);
begin
  FWidthHour := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthMin(Value: Byte);
begin
  FWidthMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetSecJump(Value: Boolean);
begin
  FSecJump := Value;
  //if FSecJump then FTimer.Interval := 500 else FTimer.Interval := 100;
end;

procedure TJvAnalogClock.SetSpider(Value: Boolean);
begin
  FSpider := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetShowDate(Value: Boolean);
begin
  FShowDate := Value;
  Invalidate;
end;

// Original code - Bianconi
//procedure TJvAnalogClock.DoAlarm;
//  if Assigned(FOnSameTime) and not (csDestroying in ComponentState) then
//    FOnSameTime(Self);
//end;
// New code

procedure TJvAnalogClock.DoAlarm;
begin
  if Assigned(FOnAlarm) and not (csDestroying in ComponentState) then
    FOnAlarm(Self);
end;

procedure TJvAnalogClock.DoChangeSec(AHour, AMin, ASec: Integer);
begin
  if Assigned(FOnChangeSec) and not (csDestroying in ComponentState) then
    FOnChangeSec(Self, AHour, AMin, ASec);
end;

procedure TJvAnalogClock.DoChangeMin(AHour, AMin, ASec: Integer);
begin
  if Assigned(FOnChangeMin) and not (csDestroying in ComponentState) then
    FOnChangeMin(Self, AHour, AMin, ASec);
end;

procedure TJvAnalogClock.DoChangeHour(AHour, AMin, ASec: Integer);
begin
  if Assigned(FOnChangeHour) and not (csDestroying in ComponentState) then
    FOnChangeHour(Self, AHour, AMin, ASec);
end;

procedure TJvAnalogClock.CMDenySubClassing(var Msg: TCMFocusChanged);
begin
  Msg.Result := 1;
end;

procedure TJvAnalogClock.InternalPaint;
var
  AFactor: Real;
  I: Integer;
  ACurrMinute: Integer; //??
  NPkT, AMinuteX, AMinuteY: Integer;
  ARect: TRect;
  AMinuteStr: ShortString;
  ADateX, ADateY: Integer;
  ADateStr: string;
  AColor: TColor;
begin
  // Bianconi
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  // End of Bianconi

  FCenterX := Width div 2;
  FCenterY := Height div 2;
  //if FShowDate then FCenterY := (Height - pFDate.Height) div 2;
   // if FShowDate then
   //   FCenterY := (Height - (Font.Height + 4)) div 2;

  FCurMarkX := FCenterX - (1 + HourSize);
  if BevelInner <> bvNone then
    FCurMarkX := FCurMarkX - BevelWidth;
  if BevelOuter <> bvNone then
    FCurMarkX := FCurMarkX - BevelWidth;

  FCurMarkY := FCenterY - (1 + HourSize);
  if BevelInner <> bvNone then
    FCurMarkY := FCurMarkY - BevelWidth;
  if BevelOuter <> bvNone then
    FCurMarkY := FCurMarkY - BevelWidth;

  FCurMark := FCurMarkY;
  if FCurMarkX < FCurMarkY then
    FCurMark := FCurMarkX;
  FCurMark := FCurMark - FWidthHour;
  FCurMark23 := FCurMark div 3;

  ADateStr := DateToStr(SysUtils.Date);
  ADateX := FCenterX - ((Canvas.TextWidth(ADateStr)) div 2);
  ADateY := FCenterY div 2;
  if BevelInner <> bvNone then
    ADateY := ADateY - BevelWidth;
  if BevelOuter <> bvNone then
    ADateY := ADateY - BevelWidth;
  FDrawRect := Rect(ADateX, ADateY, ADateX + Canvas.TextWidth(ADateStr), ADateY + Canvas.TextHeight(ADateStr));

  //if FShowDate then
 // begin
 //  Canvas.Brush.Style := bsClear;
  // Canvas.TextRect(FDrawRect, FDrawRect.Left, FDrawRect.Top, ADateStr);

 // end;

  AFactor := Pi / 3000;

  if FMinMarks then
  begin
    if MinuteStyle = hsLine then
    begin
      Canvas.Pen.Color := FColorMin;
      Canvas.Pen.Width := FWidthMin;
      for I := 0 to 59 do
      begin
        if (HourStyle = hsNumberInCircle) or
          ((I mod 5) > 0) or (HourMarks = hmNone) or
          ((HourMarks = hmFour) and (((I div 5) mod 3) > 0)) then
        begin
          ACurrMinute := I * 100;
          if FSpider then
            NPkT := Round(Sqrt(Abs((FCurMarkY) * Cos(ACurrMinute * AFactor)) * Abs((FCurMarkY) *
              Cos(ACurrMinute * AFactor)) + Abs((FCurMarkX) * Sin(ACurrMinute * AFactor)) * Abs((FCurMarkX) *
              Sin(ACurrMinute * AFactor))))
          else
          if FCurMarkY < FCurMarkX then
            NPkT := FCurMarkY
          else
            NPkT := FCurMarkX;
          Canvas.MoveTo(FCenterX + Round((FCurMark + 1) * Sin(ACurrMinute * AFactor)), FCenterY -
            Round((FCurMark + 1) * Cos(ACurrMinute * AFactor)));
          AMinuteX := FCenterX + Round((NPkT + MinuteSize) * Sin(ACurrMinute * AFactor));
          if AMinuteX > FCenterX + FCurMarkX + MinuteSize then
            AMinuteX := FCenterX + FCurMarkX + MinuteSize;
          if AMinuteX < FCenterX - FCurMarkX - MinuteSize then
            AMinuteX := FCenterX - FCurMarkX - MinuteSize;
          AMinuteY := FCenterY - Round((NPkT + MinuteSize) * Cos(ACurrMinute * AFactor));
          if AMinuteY > FCenterY + FCurMarkY + MinuteSize then
            AMinuteY := FCenterY + FCurMarkY + MinuteSize;
          if AMinuteY < FCenterY - FCurMarkY - MinuteSize then
            AMinuteY := FCenterY - FCurMarkY - MinuteSize;
          Canvas.LineTo(AMinuteX, AMinuteY);
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := FColorMin;
      Canvas.Pen.Width := FWidthMin;
      if (MinuteStyle = hsNumber) or (MinuteStyle = hsNumberInCircle) then
      begin
        Canvas.Font := Font;
        Canvas.Font.Size := MinuteFontSize;
      end;
      for I := 0 to 59 do
      begin
        if ((I mod 5) > 0) or (HourMarks = hmNone) or ((HourMarks = hmFour) and
          (((I div 5) mod 3) > 0)) then
          //      if ((I mod 5) > 0) then
        begin
          ACurrMinute := I * 100;
          if FCurMarkY < FCurMarkX then
            NPkT := FCurMarkY
          else
            NPkT := FCurMarkX;

          AMinuteX := FCenterX + 1 + Round((NPkT + (MinuteSize div 2)) * Sin(ACurrMinute * AFactor));
          AMinuteY := FCenterY + 1 - Round((NPkT + (MinuteSize div 2)) * Cos(ACurrMinute * AFactor));
          ARect := Rect(AMinuteX - (MinuteSize div 2), AMinuteY - (MinuteSize div 2), AMinuteX +
            (MinuteSize div 2), AMinuteY + (MinuteSize div 2));
          AMinuteStr := IntToStr(I);
          if (MinuteStyle = hsCircle) or (MinuteStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Color := FColorMinIn;
            Canvas.Brush.Style := bsSolid;
            Canvas.Ellipse(ARect);
          end;
          if (MinuteStyle = hsNumber) or (MinuteStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Style := bsClear;
            Canvas.TextRect(ARect, ARect.Right - ((ARect.Right - ARect.Left) div 2) -
              (Canvas.TextWidth(AMinuteStr) div 2) - 1, ARect.Bottom - ((ARect.Bottom - ARect.Top)
              div 2) - (Canvas.TextHeight(AMinuteStr) div 2) - 1, AMinuteStr);
          end;
        end;
      end;
    end;
  end;

  if HourMarks <> hmNone then
  begin
    if HourStyle = hsLine then
    begin
      Canvas.Pen.Color := FColorHour;
      Canvas.Pen.Width := FWidthHour;
      for I := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((I mod 3) = 0)) then
        begin
          ACurrMinute := ((I) * 100) * 30 div 6;
          if FSpider then
            NPkT := Round(Sqrt(Abs((FCurMarkY) * Cos(ACurrMinute * AFactor)) * Abs((FCurMarkY) *
              Cos(ACurrMinute * AFactor)) + Abs((FCurMarkX) * Sin(ACurrMinute * AFactor)) * Abs((FCurMarkX) *
              Sin(ACurrMinute * AFactor))))
          else
          if FCurMarkY < FCurMarkX then
            NPkT := FCurMarkY
          else
            NPkT := FCurMarkX;
          Canvas.MoveTo(FCenterX + Round((FCurMark + 1) * Sin(ACurrMinute * AFactor)), FCenterY -
            Round((FCurMark + 1) * Cos(ACurrMinute * AFactor)));
          AMinuteX := FCenterX + Round((NPkT + HourSize) * Sin(ACurrMinute * AFactor));
          if AMinuteX > FCenterX + FCurMarkX + HourSize then
            AMinuteX := FCenterX + FCurMarkX + HourSize;
          if AMinuteX < FCenterX - FCurMarkX - HourSize then
            AMinuteX := FCenterX - FCurMarkX - HourSize;
          AMinuteY := FCenterY - Round((NPkT + HourSize) * Cos(ACurrMinute * AFactor));
          if AMinuteY > FCenterY + FCurMarkY + HourSize then
            AMinuteY := FCenterY + FCurMarkY + HourSize;
          if AMinuteY < FCenterY - FCurMarkY - HourSize then
            AMinuteY := FCenterY - FCurMarkY - HourSize;
          Canvas.LineTo(AMinuteX, AMinuteY);
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := FColorHour;
      Canvas.Pen.Width := FWidthHour;
      if (HourStyle = hsNumber) or (HourStyle = hsNumberInCircle) then
        Canvas.Font := Font;
      for I := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((I mod 3) = 0)) then
        begin
          ACurrMinute := ((I) * 100) * 30 div 6;
          if FCurMarkY < FCurMarkX then
            NPkT := FCurMarkY
          else
            NPkT := FCurMarkX;
          AMinuteX := FCenterX + 1 + Round((NPkT + (HourSize div 2)) * Sin(ACurrMinute * AFactor));
          AMinuteY := FCenterY + 1 - Round((NPkT + (HourSize div 2)) * Cos(ACurrMinute * AFactor));
          ARect := Rect(AMinuteX - (HourSize div 2), AMinuteY - (HourSize div 2), AMinuteX +
            (HourSize div 2), AMinuteY + (HourSize div 2));
          AMinuteStr := IntToStr(I);
          if (HourStyle = hsCircle) or (HourStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Color := FColorHourIn;
            Canvas.Brush.Style := bsSolid;
            Canvas.Ellipse(ARect);
          end;
          if (HourStyle = hsNumber) or (HourStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Style := bsClear;
            Canvas.TextRect(ARect, ARect.Right - ((ARect.Right - ARect.Left) div 2) -
              (Canvas.TextWidth(AMinuteStr) div 2) - 1, ARect.Bottom - ((ARect.Bottom - ARect.Top) div 2) -
              (Canvas.TextHeight(AMinuteStr) div 2) - 1, AMinuteStr);
          end;
        end;
      end;
    end;
  end;

  if not FActive then
  begin
    FHalfSecond := 50;
    ActTimer(Self);
  end;
end;

procedure TJvAnalogClock.ActTimer(Sender: TObject);
var
  H, M, S, Ms: Word;
  H1, M1, S1, Ms1: Word;
  AFactor: Real;
  I: Integer;
  ADateTime: TDateTime;
  ADateStr: string;
  ADateBottom: Boolean;
  ASin, ACos: Extended;
begin
  if not FActive then
    FTimer.Enabled := FActive;

  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  if FActive then
    ADateTime := Now
  else
    ADateTime := FTime;

  ADateTime := ADateTime + (FOffset / (60 * 24));
  DecodeTime(ADateTime, H, M, S, Ms);
  Inc(FHalfSecond);
  if FSecJump then
  begin
    if S = FOldSecond then
      if FActive then
        Exit;
    FOldSecond := S;
    Ms := 0;
  end;

  if FShowDate then
  begin
    ADateStr := DateToStr(ADateTime);
    ADateBottom := ((H mod 12) < 2) or ((H mod 12) > 7);
    if (ADateBottom <> DateBottom) or (ADateStr <> FOldDate) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.TextRect(FDrawRect, FDrawRect.Left, FDrawRect.Top, '');
      Canvas.TextRect(Rect(FDrawRect.Left, FDrawRect.Top * 3, FDrawRect.Right,
        FDrawRect.Top * 2 + FDrawRect.Bottom), FDrawRect.Left, FDrawRect.Top * 3, '');
    end;
    DateBottom := ADateBottom;
    FOldDate := ADateStr;
    Canvas.Brush.Style := bsClear;
    if DateBottom then
      Canvas.TextRect(Rect(FDrawRect.Left, FDrawRect.Top * 3, FDrawRect.Right,
        FDrawRect.Top * 2 + FDrawRect.Bottom), FDrawRect.Left, FDrawRect.Top * 3, ADateStr)
    else
      Canvas.TextRect(FDrawRect, FDrawRect.Left, FDrawRect.Top, ADateStr);
  end;

  //event handler by Ujlaki Sándor e-mail: ujlaki.sandor@drotposta.hu
  if FActive and (s <> FOldSec) then //every seconds
  begin
    FOldSec := S;
    DoChangeSec(H, M, S);
    DecodeTime(FTime, H1, M1, S1, Ms1);

    // Bianconi
    //    if (s1 = s) and (m1 = m) and (h1 = h) then
    //      DoAlarm;
    // End of Bianconi

    if M <> FOldMin then
    begin
      FOldMin := M;
      DoChangeMin(H, M, S);
      if h <> FOldHour then
      begin
        FOldHour := H;
        DoChangeHour(H, M, S);
      end;
    end;
  end;

  I := (((S) * 1000) + Ms) div 10;
  AFactor := Pi / 3000;
  if FSeconds then
    Inc(FHalfSecond);

  //Delete sec hand
  if FSeconds or (FClockSec > 0) then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := FWidthHandSec;
    ASin := Sin(FClockSec * AFactor);
    ACos := Cos(FClockSec * AFactor);

    Canvas.MoveTo(FCenterX + Round(4 * ASin), FCenterY - Round(4 * ACos));
    Canvas.LineTo(FCenterX + Round(FCurMark * ASin), FCenterY - Round(FCurMark * ACos));

    Canvas.MoveTo(FCenterX - Round(4 * ASin), FCenterY + Round(4 * ACos));
    Canvas.LineTo(FCenterX - Round(15 * ASin), FCenterY + Round(15 * ACos));
  end;

  //Delete hand
  if FHalfSecond > 50 then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := FWidthHandHour;
    ASin := Sin(FClockMin * AFactor);
    ACos := Cos(FClockMin * AFactor);

    //Urni kazalec
    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(3 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FCurMark23) * ASin), FCenterY - Round((FCurMark - FCurMark23) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin), FCenterY + Round(3 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin), FCenterY + Round(10 * ACos));

    //Minutni kazalec
    Canvas.Pen.Width := FWidthHandMin;
    ASin := Sin(FClockHour * AFactor);
    ACos := Cos(FClockHour * AFactor);

    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(3 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FWidthHandMin) * ASin),
      FCenterY - Round((FCurMark - FWidthHandMin) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin), FCenterY + Round(3 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin), FCenterY + Round(10 * ACos));

    FClockHour := ((M * 60 + S) * 10) div 6;
    FClockMin := ((H * 60 + M) * 25) div 3;
  end;

  // Draw Alarm mark
  if FAlarm.Visible then
  begin
    if FAlarm.Enabled and FActive then
      Canvas.Pen.Color := FAlarm.ColorOn
    else
      Canvas.Pen.Color := FAlarm.ColorOff;

    DecodeTime(FAlarm.Date, H1, M1, S1, Ms1);
    H1 := (((H1 * 60) + M1) * 25) div 3;
    M1 := (((M1 * 60) + S1) * 10) div 6;
    //    S1 := ((S1 * 1000) + ms1) div 10;

    // Don't draw Second
    //    Canvas.Pen.Width := FWidthHandSec;
    //    Canvas.MoveTo( FCenterX + Round(4 * Sin(S1 * AFactor)),
    //                   FCenterY - Round(4 * Cos(S1 * AFactor)));
    //    Canvas.LineTo( FCenterX + Round(FCurMark * Sin(S1 * AFactor)),
    //                   FCenterY - Round(FCurMark * Cos(S1 * AFactor)));

    //    Canvas.MoveTo( FCenterX - Round(4 * Sin(S1 * AFactor)),
    //                   FCenterY + Round(4 * Cos(S1 * AFactor)));
    //    Canvas.LineTo( FCenterX - Round(15 * Sin(S1 * AFactor)),
    //                   FCenterY + Round(15 * Cos(S1 * AFactor)));

        // Draw Alarm Minute Indicator
    Canvas.Pen.Width := FAlarm.WidthMin;
    ASin := Sin(M1 * AFactor);
    ACos := Cos(M1 * AFactor);
    Canvas.MoveTo(FCenterX + Round(3 * ASin),
      FCenterY - Round(3 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FWidthHandMin) * ASin),
      FCenterY - Round((FCurMark - FWidthHandMin) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin),
      FCenterY + Round(3 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin),
      FCenterY + Round(10 * ACos));

    // Draw Alarm Hour indicator
    Canvas.Pen.Width := FAlarm.WidthHour;
    ASin := Sin(H1 * AFactor);
    ACos := Cos(H1 * AFactor);

    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(03 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FCurMark23) * ASin),
      FCenterY - Round((FCurMark - FCurMark23) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin),
      FCenterY + Round(03 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin),
      FCenterY + Round(10 * ACos));
  end;

  //Draw hand
  if (FHalfSecond > 50) or FSecOver then
  begin
    FSecOver := False;

    //Minutni kazalec
    ASin := Sin(FClockHour * AFactor);
    ACos := Cos(FClockHour * AFactor);

    Canvas.Pen.Width := FWidthHandMin;
    Canvas.Pen.Color := FColorHandMin;
    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(3 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FWidthHandMin) * ASin),
      FCenterY - Round((FCurMark - FWidthHandMin) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin), FCenterY + Round(3 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin), FCenterY + Round(10 * ACos));

    //Urni kazalec
    ASin := Sin(FClockMin * AFactor);
    ACos := Cos(FClockMin * AFactor);
    Canvas.Pen.Color := FColorHandHour;
    Canvas.Pen.Width := FWidthHandHour;
    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(3 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FCurMark23) * ASin), FCenterY - Round((FCurMark - FCurMark23) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin), FCenterY + Round(3 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin), FCenterY + Round(10 * ACos));

    if FHalfSecond > 50 then
      FHalfSecond := 0;
  end;

  if FSeconds then
  begin
    Canvas.Pen.Width := FWidthHandSec;
    Canvas.Pen.Color := FColorHandSec;
    ASin := Sin(I * AFactor);
    ACos := Cos(I * AFactor);

    Canvas.MoveTo(FCenterX + Round(4 * ASin), FCenterY - Round(4 * ACos));
    Canvas.LineTo(FCenterX + Round(FCurMark * ASin), FCenterY - Round(FCurMark * ACos));

    Canvas.MoveTo(FCenterX - Round(4 * ASin), FCenterY + Round(4 * ACos));
    Canvas.LineTo(FCenterX - Round(15 * ASin), FCenterY + Round(15 * ACos));
    FClockSec := I;

    FSecOver := True;

    // Bianconi - Removed
    //    if FCenterSize > 0 then
    //    begin
    //      Canvas.Pen.Color := FColorHandHour;
    //      Canvas.Brush.Color := FCenterColor;
    //      Canvas.Brush.Style := bsSolid;
    //      Canvas.Pen.Width := 1;
    //      Canvas.Ellipse(FCenterX - FCenterSize, FCenterY - FCenterSize, FCenterX + FCenterSize,
    //        FCenterY + FCenterSize);
    //    end;
    //   End of Bianconi
  end
  else
  begin
    FClockSec := 0;
    //ADateStr := DateToStr(SysUtils.Date);
  end;

  // Draw center point
  if FCenterSize > 0 then
  begin
    Canvas.Pen.Color := FColorHandHour;
    Canvas.Brush.Color := FCenterColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Width := 1;
    Canvas.Ellipse(FCenterX - FCenterSize, FCenterY - FCenterSize,
      FCenterX + FCenterSize, FCenterY + FCenterSize);
  end;

  // Check Alarm information
  if FActive and FAlarm.Enabled then
    if FAlarm.Date <= Now then
    begin
      case FAlarm.Trigger of
        tkOneShot:
          FAlarm.Enabled := False;
        tkEachSecond:
          JvJCLUtils.IncSecond(FAlarm.Date, 1);
        tkEachMinute:
          JvJCLUtils.IncMinute(FAlarm.Date, 1);
        tkEachHour:
          JvJCLUtils.IncHour(FAlarm.Date, 1);
        tkEachDay:
          JvJCLUtils.IncDay(FAlarm.Date, 1);
        tkEachMonth:
          JvJCLUtils.IncMonth(FAlarm.Date, 1);
        tkEachYear:
          JvJCLUtils.IncYear(FAlarm.Date, 1);
      end;
      // We set FAlarm params before call event to allow user make changes on it.
      DoAlarm;
    end;
end;

end.

