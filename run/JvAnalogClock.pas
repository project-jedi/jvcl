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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvTypes, JvDateUtil, JvComponent;

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
    FWidthM: Integer;
    FWidthH: Integer;

    procedure SetAlarmEnabled(AValue: Boolean);
    function GetAlarmDate: TDateTime;
    procedure SetAlarmDate(AValue: TDateTime);
    procedure SetColorOn(AValue: TColor);
    procedure SetColorOff(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidthM(AValue: Integer);
    procedure SetWidthH(AValue: Integer);
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
    property WidthMin: Integer read FWidthM write SetWidthM;
    property WidthHour: Integer read FWidthH write SetWidthH;
  end;
  // End of Bianconi

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
    FColorHr: TColor;
    FColorHourIn: TColor;
    FColorMin: TColor;
    FColorMinIn: TColor;
    FColorHandHr: TColor;
    FColorHandMin: TColor;
    FColorHandSec: TColor;

    FWidthHandMin: Byte;
    FWidthHandHr: Byte;
    FWidthHandSec: Byte;
    FWidthHr: Byte;
    FWidthMin: Byte;

    FCenterSize: Byte;
    FCenterColor: TColor;

    FSecOver: Boolean;

    FHalfSecond: Integer;
    FClockHour: Integer;
    FClockMin: Integer;
    FClockSec: Integer;
    FCenterX, FCenterY: Integer;
    FCurMarkX, FCurMarkY, FCurMark, FCurMark23: Integer;

    FOldHour, FOldMin, FOldSec: Integer;
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
    procedure SetColorHr(Value: TColor);
    procedure SetColorHrIn(Value: TColor);
    procedure SetColorMin(Value: TColor);
    procedure SetColorMinIn(Value: TColor);

    procedure SetColorHandHr(Value: TColor);
    procedure SetColorHandMin(Value: TColor);
    procedure SetColorHandSec(Value: TColor);

    procedure SetWidthHandMin(Value: Byte);
    procedure SetWidthHandHr(Value: Byte);
    procedure SetWidthHandSec(Value: Byte);
    procedure SetWidthHr(Value: Byte);
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
    property ColorHr: TColor read FColorHr write SetColorHr default clBlack;
    property ColorHrIn: TColor read FColorHourIn write SetColorHrIn default clBlack;
    property ColorMin: TColor read FColorMin write SetColorMin default clBlack;
    property ColorMinIn: TColor read FColorMinIn write SetColorMinIn default clBlack;
    property ColorHandHr: TColor read FColorHandHr write SetColorHandHr default clBlack;
    property ColorHandMin: TColor read FColorHandMin write SetColorHandMin default clBlack;
    property ColorHandSec: TColor read FColorHandSec write SetColorHandSec default clBlack;

    property WidthHandSec: Byte read FWidthHandSec write SetWidthHandSec default 1;
    property WidthHandMin: Byte read FWidthHandMin write SetWidthHandMin default 3;
    property WidthHandHr: Byte read FWidthHandHr write SetWidthHandHr default 5;
    property WidthHr: Byte read FWidthHr write SetWidthHr default 2;
    property WidthMin: Byte read FWidthMin write SetWidthMin default 1;

    // Bianconi
    property Alarm: TJvAlarmInfo read FAlarm write FAlarm;
    // End of Bianconi

    //    property MinFont :TFont read pfMinFont write pfMinFont;

    property CenterSize: Byte read FCenterSize write FCenterSize default 5;
    property CenterCol: TColor read FCenterColor write FCenterColor default clBlack;

    property Align;
    property Color default clBtnFace;
    property Cursor;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

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

    property Width default 137;
    property Height default 137;
    property BevelWidth;
    property BevelInner default bvRaised;
    property BevelOuter default bvLowered;
  end;

implementation

{.$R *.res}

// TJvAlarmInfo implementation
// Bianconi

procedure TJvAlarmInfo.SetAlarmEnabled(AValue: Boolean);
begin
  if (FEnabled <> AValue) then
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
  if (FColorOn <> AValue) then
  begin
    FColorOn := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetColorOff(AValue: TColor);
begin
  if (FColorOff <> AValue) then
  begin
    FColorOff := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetVisible(AValue: Boolean);
begin
  if (FVisible <> AValue) then
  begin
    FVisible := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetWidthM(AValue: Integer);
begin
  if (FWidthM <> AValue) then
  begin
    FWidthM := AValue;
    FOwner.Invalidate;
  end;
end;

procedure TJvAlarmInfo.SetWidthH(AValue: Integer);
begin
  if (FWidthH <> AValue) then
  begin
    FWidthH := AValue;
    FOwner.Invalidate;
  end;
end;

constructor TJvAlarmInfo.Create(AOwner: TJvAnalogClock);
begin
  inherited Create;
  FOwner := AOwner;
  FDate := Now;
  FEnabled := False;
  FColorOn := clTeal;
  FColorOff := clGray;
  FTrigger := tkOneShot;
  FWidthH := FOwner.WidthHandHr;
  FWidthM := FOwner.WidthHandMin;
end;

destructor TJvAlarmInfo.Destroy;
begin
  FOwner := nil;
  FEnabled := False;
  inherited Destroy;
end;

// End of Bianconi

constructor TJvAnalogClock.Create(AOwner: TComponent);
var
  h, m, s, ms: Word;
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

  FColorHr := clBlack;
  FColorHourIn := clBlack;
  FColorMin := clBlack;
  FColorMinIn := clBlack;
  FColorHandHr := clBlack;
  FColorHandMin := clBlack;
  FColorHandSec := clBlack;

  FWidthHandSec := 1;
  FWidthHandMin := 3;
  FWidthHandHr := 5;
  FWidthHr := 2;
  FWidthMin := 1;

  FCenterColor := clBlack;
  FCenterSize := 5;

  // Bianconi
  FAlarm := TJvAlarmInfo.Create(Self);
  // End of Bianconi

  DecodeTime(Now, h, m, s, ms);
  FOldMin := m;
  FOldHour := h;
  FOldSec := s;

  FTimer := TTimer.Create(Self);
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

procedure TJvAnalogClock.SetColorHr(Value: TColor);
begin
  FColorHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetColorHrIn(Value: TColor);
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

procedure TJvAnalogClock.SetColorHandHr(Value: TColor);
begin
  FColorHandHr := Value;
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

procedure TJvAnalogClock.SetWidthHandHr(Value: Byte);
begin
  FWidthHandHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetWidthHr(Value: Byte);
begin
  FWidthHr := Value;
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

procedure TJvAnalogClock.InternalPaint;
var
  AFactor: Real;
  i: Integer;
  ACurrMinute: Integer; //??
  npkT, AMinuteX, AMinuteY: Integer;
  ARect: TRect;
  AMinuteStr: ShortString;
  ADateX, ADateY: Integer;
  ADateStr: string;
  AColor: TColor;
begin
  // Bianconi
  if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then Exit;
  // End of Bianconi

  FCenterX := (Width div 2); {Center}
  FCenterY := (Height div 2);
  //if FShowDate then FCenterY := ((Height - pFDate.Height) div 2);
   // if FShowDate then
   //   FCenterY := ((Height - (Font.Height + 4)) div 2);

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
  FCurMark := FCurMark - FWidthHr;
  FCurMark23 := FCurMark div 3;

  ADateStr := DateToStr(Sysutils.Date);
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
      for i := 0 to 59 do
      begin
        if (HourStyle = hsNumberInCircle) or ((i mod 5) > 0) or (HourMarks = hmNone) or
          ((HourMarks = hmFour) and (((i div 5) mod 3) > 0)) then
        begin
          ACurrMinute := i * 100;
          if FSpider then
          begin
            npkT := Round(Sqrt(Abs((FCurMarkY) * Cos(ACurrMinute * AFactor)) * Abs((FCurMarkY) *
              Cos(ACurrMinute * AFactor)) + Abs((FCurMarkX) * Sin(ACurrMinute * AFactor)) * Abs((FCurMarkX) *
              Sin(ACurrMinute * AFactor))));
          end
          else
          begin
            if FCurMarkY < FCurMarkX then
              npkT := FCurMarkY
            else
              npkT := FCurMarkX;
          end;
          Canvas.MoveTo(FCenterX + Round((FCurMark + 1) * Sin(ACurrMinute * AFactor)), FCenterY -
            Round((FCurMark + 1) * Cos(ACurrMinute * AFactor)));
          AMinuteX := FCenterX + Round((npkT + MinuteSize) * Sin(ACurrMinute * AFactor));
          if AMinuteX > FCenterX + FCurMarkX + MinuteSize then
            AMinuteX := FCenterX + FCurMarkX + MinuteSize;
          if AMinuteX < FCenterX - FCurMarkX - MinuteSize then
            AMinuteX := FCenterX - FCurMarkX - MinuteSize;
          AMinuteY := FCenterY - Round((npkT + MinuteSize) * Cos(ACurrMinute * AFactor));
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
      for i := 0 to 59 do
      begin
        if ((i mod 5) > 0) or (HourMarks = hmNone) or ((HourMarks = hmFour) and
          (((i div 5) mod 3) > 0)) then
          //      if ((i mod 5) > 0) then
        begin
          ACurrMinute := i * 100;
          if FCurMarkY < FCurMarkX then
            npkT := FCurMarkY
          else
            npkT := FCurMarkX;

          AMinuteX := FCenterX + 1 + Round((npkT + (MinuteSize div 2)) * Sin(ACurrMinute * AFactor));
          AMinuteY := FCenterY + 1 - Round((npkT + (MinuteSize div 2)) * Cos(ACurrMinute * AFactor));
          ARect := Rect(AMinuteX - (MinuteSize div 2), AMinuteY - (MinuteSize div 2), AMinuteX +
            (MinuteSize div 2), AMinuteY + (MinuteSize div 2));
          AMinuteStr := IntToStr(i);
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
      Canvas.Pen.Color := FColorHr;
      Canvas.Pen.Width := FWidthHr;
      for i := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((i mod 3) = 0)) then
        begin
          ACurrMinute := ((i) * 100) * 30 div 6;
          if FSpider then
          begin
            npkT := Round(Sqrt(Abs((FCurMarkY) * Cos(ACurrMinute * AFactor)) * Abs((FCurMarkY) *
              Cos(ACurrMinute * AFactor)) + Abs((FCurMarkX) * Sin(ACurrMinute * AFactor)) * Abs((FCurMarkX) *
              Sin(ACurrMinute * AFactor))));
          end
          else
          begin
            if FCurMarkY < FCurMarkX then
              npkT := FCurMarkY
            else
              npkT := FCurMarkX;
          end;
          Canvas.MoveTo(FCenterX + Round((FCurMark + 1) * Sin(ACurrMinute * AFactor)), FCenterY -
            Round((FCurMark + 1) * Cos(ACurrMinute * AFactor)));
          AMinuteX := FCenterX + Round((npkT + HourSize) * Sin(ACurrMinute * AFactor));
          if AMinuteX > FCenterX + FCurMarkX + HourSize then
            AMinuteX := FCenterX + FCurMarkX + HourSize;
          if AMinuteX < FCenterX - FCurMarkX - HourSize then
            AMinuteX := FCenterX - FCurMarkX - HourSize;
          AMinuteY := FCenterY - Round((npkT + HourSize) * Cos(ACurrMinute * AFactor));
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
      Canvas.Pen.Color := FColorHr;
      Canvas.Pen.Width := FWidthHr;
      if (HourStyle = hsNumber) or (HourStyle = hsNumberInCircle) then
        Canvas.Font := Font;
      for i := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((i mod 3) = 0)) then
        begin
          ACurrMinute := ((i) * 100) * 30 div 6;
          if FCurMarkY < FCurMarkX then
            npkT := FCurMarkY
          else
            npkT := FCurMarkX;
          AMinuteX := FCenterX + 1 + Round((npkT + (HourSize div 2)) * Sin(ACurrMinute * AFactor));
          AMinuteY := FCenterY + 1 - Round((npkT + (HourSize div 2)) * Cos(ACurrMinute * AFactor));
          ARect := Rect(AMinuteX - (HourSize div 2), AMinuteY - (HourSize div 2), AMinuteX +
            (HourSize div 2), AMinuteY + (HourSize div 2));
          AMinuteStr := inttostr(i);
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
              (Canvas.TextWidth(AMinuteStr) div 2) - 1, ARect.Bottom - ((ARect.Bottom - ARect.Top)
              div 2) - (Canvas.TextHeight(AMinuteStr) div 2) - 1, AMinuteStr);
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
  h, m, s, ms: Word;
  h1, m1, s1, ms1: Word;
  AFactor: Real;
  i: Integer;
  ADateTime: TDateTime;
  ADateStr: string;
  ADateBottom: Boolean;
  ASin, ACos: Extended;
begin
  if not FActive then
    FTimer.Enabled := FActive;

  if ((csLoading in ComponentState) or (csDestroying in ComponentState)) then Exit;

  if FActive then
    ADateTime := Now
  else
    ADateTime := FTime;

  ADateTime := ADateTime + (FOffset / (60 * 24));
  DecodeTime(ADateTime, h, m, s, ms);
  Inc(FHalfSecond);
  if FSecJump then
  begin
    if s = FOldSecond then
      if FActive then
        Exit;
    FOldSecond := s;
    ms := 0;
  end;

  if FShowDate then
  begin
    ADateStr := DateToStr(ADateTime);
    ADateBottom := ((h mod 12) < 2) or ((h mod 12) > 7);
    if (ADateBottom <> DateBottom) or (ADateStr <> FOldDate) then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.TextRect(FDrawRect, FDrawRect.Left, FDrawRect.Top, '');
      Canvas.TextRect(Rect(FDrawRect.left, FDrawRect.Top * 3, FDrawRect.Right,
        FDrawRect.Top * 2 + FDrawRect.Bottom), FDrawRect.Left, FDrawRect.Top * 3, '');
    end;
    DateBottom := ADateBottom;
    FOldDate := ADateStr;
    Canvas.Brush.Style := bsClear;
    if DateBottom then
      Canvas.TextRect(Rect(FDrawRect.left, FDrawRect.Top * 3, FDrawRect.Right,
        FDrawRect.Top * 2 + FDrawRect.Bottom), FDrawRect.Left, FDrawRect.Top * 3, ADateStr)
    else
      Canvas.TextRect(FDrawRect, FDrawRect.Left, FDrawRect.Top, ADateStr);
  end;

  //event handler by Ujlaki Sándor e-mail: ujlaki.sandor@drotposta.hu
  if FActive and (s <> FOldSec) then //every seconds
  begin
    FOldSec := s;
    DoChangeSec(h, m, s);
    DecodeTime(FTime, h1, m1, s1, ms1);

    // Bianconi
    //    if (s1 = s) and (m1 = m) and (h1 = h) then
    //      DoAlarm;
    // End of Bianconi

    if m <> FOldMin then
    begin
      FOldMin := m;
      DoChangeMin(h, m, s);
      if h <> FOldHour then
      begin
        FOldHour := h;
        DoChangeHour(h, m, s);
      end;
    end;
  end;

  i := (((s) * 1000) + ms) div 10;
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
    Canvas.Pen.Width := FWidthHandHr;
    ASin := Sin(FClockMin * AFactor);
    ACos := Cos(FClockMin * AFactor);

    //Urni kazalec
    Canvas.MoveTo(FCenterX + Round(03 * ASin), FCenterY - Round(03 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FCurMark23) * ASin), FCenterY - Round((FCurMark - FCurMark23) * ACos));

    Canvas.MoveTo(FCenterX - Round(03 * ASin), FCenterY + Round(03 * ACos));
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

    FClockHour := (((m) * 60 + S) * 10) div 6;
    FClockMin := (((h) * 60 + m) * 25) div 3;
  end;

  // Draw Alarm mark
  if (FAlarm.Visible) then
  begin
    if (FAlarm.Enabled and FActive) then
    begin
      Canvas.Pen.Color := FAlarm.ColorOn;
    end
    else
    begin
      Canvas.Pen.Color := FAlarm.ColorOff;
    end;

    DecodeTime(FAlarm.Date, H1, M1, S1, ms1);
    H1 := (((H1 * 60) + M1) * 25) div 3;
    M1 := (((M1 * 60) + S1) * 10) div 6;
    //    S1 := ((S1 * 1000) + ms1) div 10;

    // Don'i draw Second
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
  if ((FHalfSecond > 50) or FSecOver) then
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
    Canvas.Pen.Color := FColorHandHr;
    Canvas.Pen.Width := FWidthHandHr;
    Canvas.MoveTo(FCenterX + Round(3 * ASin), FCenterY - Round(03 * ACos));
    Canvas.LineTo(FCenterX + Round((FCurMark - FCurMark23) * ASin), FCenterY - Round((FCurMark - FCurMark23) * ACos));

    Canvas.MoveTo(FCenterX - Round(3 * ASin), FCenterY + Round(03 * ACos));
    Canvas.LineTo(FCenterX - Round(10 * ASin), FCenterY + Round(10 * ACos));

    if (FHalfSecond > 50) then
      FHalfSecond := 0;
  end;

  if FSeconds then
  begin
    Canvas.Pen.Width := FWidthHandSec;
    Canvas.Pen.Color := FColorHandSec;
    ASin := Sin(i * AFactor);
    ACos := Cos(i * AFactor);

    Canvas.MoveTo(FCenterX + Round(4 * ASin), FCenterY - Round(4 * ACos));
    Canvas.LineTo(FCenterX + Round(FCurMark * ASin), FCenterY - Round(FCurMark * ACos));

    Canvas.MoveTo(FCenterX - Round(4 * ASin), FCenterY + Round(4 * ACos));
    Canvas.LineTo(FCenterX - Round(15 * ASin), FCenterY + Round(15 * ACos));
    FClockSec := i;

    FSecOver := True;

    // Bianconi - Removed
    //    if (FCenterSize > 0) then
    //    begin
    //      Canvas.Pen.Color := FColorHandHr;
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
  if (FCenterSize > 0) then
  begin
    Canvas.Pen.Color := FColorHandHr;
    Canvas.Brush.Color := FCenterColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Width := 1;
    Canvas.Ellipse(FCenterX - FCenterSize, FCenterY - FCenterSize,
      FCenterX + FCenterSize, FCenterY + FCenterSize);
  end;

  // Check Alarm information
  if (FActive and FAlarm.Enabled) then
  begin
    if (FAlarm.Date <= Now) then
    begin
      case FAlarm.Trigger of
        tkOneShot:
          begin
            FAlarm.Enabled := False;
          end;
        tkEachSecond:
          begin
            jvDateUtil.IncSecond(FAlarm.Date, 1);
          end;
        tkEachMinute:
          begin
            jvDateUtil.IncMinute(FAlarm.Date, 1);
          end;
        tkEachHour:
          begin
            jvDateUtil.IncHour(FAlarm.Date, 1);
          end;
        tkEachDay:
          begin
            jvDateUtil.IncDay(FAlarm.Date, 1);
          end;
        tkEachMonth:
          begin
            jvDateUtil.IncMonth(FAlarm.Date, 1);
          end;
        tkEachYear:
          begin
            jvDateUtil.IncYear(FAlarm.Date, 1);
          end;
      end;
      // We set FAlarm params before call event to allow user make changes on it.
      DoAlarm;
    end;
  end;
end;

end.

