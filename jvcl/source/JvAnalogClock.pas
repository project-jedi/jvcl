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
  ExtCtrls,
  JvComponent;

type
  TJvNotifyTime = procedure(Sender: TObject; Hour, Min, Sec: Integer) of object;
  TJvHourStyle = (hsLine, hsCircle, hsNumber, hsNumberInCircle);
  TJvHourMarks = (hmNone, hmFour, hmAll);

  TJvAnalogClock = class(TJvCustomPanel)
  private
    FHourStyle: TJvHourStyle;
    FMinuteStyle: TJvHourStyle;
    FHourMarks: TJvHourMarks;
    FHourSize: Integer;
    FMinuteSize: Integer;
    FMinuteFontSize: Integer;

    OldS: Word;

    plSekunde: Boolean;
    plEnabled: Boolean;
    plSpider: Boolean;
    plSecJump: Boolean;
    pdUra: TDateTime;
    pnOffs: Integer;
    plDate: Boolean;

    plMinMarks: Boolean;
    plColHr: TColor;
    plColHrIn: TColor;
    plColMin: TColor;
    plColMinIn: TColor;
    plColHandHr: TColor;
    plColHandMin: TColor;
    plColHandSec: TColor;

    pnWidthHandMin: Byte;
    pnWidthHandHr: Byte;
    pnWidthHandSec: Byte;
    pnWidthHr: Byte;
    pnWidthMin: Byte;

    pnCenterSize: Byte;
    pnCenterCol: TColor;

    FTimer: TTimer;
    lSekOver: Boolean;

    nDeli: Integer;
    nUraM: Integer;
    nUraU: Integer;
    nUraS: Integer;
    npx, npy: Integer;
    npxk, npyk, npk, npy23: Integer;

    OldHour, OldMin, OldSec: Integer;
    datrT: TRect;
    OldDate: string;
    DateBottom: Boolean;

    FOnChangeSec: TJvNotifyTime;
    FOnChangeMin: TJvNotifyTime;
    FOnChangeHour: TJvNotifyTime;
    FOnSameTime: TNotifyEvent;

//    pfMinFont :TFont;
    procedure SetlDate(Value: Boolean);
    procedure SetlSecJump(Value: Boolean);
    procedure SetlSpider(Value: Boolean);
    procedure SetlEnabled(Value: Boolean);
    procedure SetlMinMarks(Value: Boolean);
    procedure SetHourStyle(Value: TJvHourStyle);
    procedure SetMinuteStyle(Value: TJvHourStyle);
    procedure SetHourMarks(Value: TJvHourMarks);
    procedure SetHourSize(Value: Integer);
    procedure SetMinSize(Value: Integer);
    procedure SetMinFontSize(Value: Integer);
    procedure SetdUra(Value: TDateTime);
    procedure SetnOffs(Value: Integer);
    procedure SetlColHr(Value: TColor);
    procedure SetlColHrIn(Value: TColor);
    procedure SetlColMin(Value: TColor);
    procedure SetlColMinIn(Value: TColor);

    procedure SetlColHandHr(Value: TColor);
    procedure SetlColHandMin(Value: TColor);
    procedure SetlColHandSec(Value: TColor);

    procedure SetnWidthHandMin(Value: Byte);
    procedure SetnWidthHandHr(Value: Byte);
    procedure SetnWidthHandSec(Value: Byte);
    procedure SetnWidthHr(Value: Byte);
    procedure SetnWidthMin(Value: Byte);

    procedure InternalPaint;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure ActTimer(Sender: TObject);

    procedure DoAlarm;
    procedure DoChangeSec(nHr, nMin, nSec: Integer);
    procedure DoChangeMin(nHr, nMin, nSec: Integer);
    procedure DoChangeHour(nHr, nMin, nSec: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Date: Boolean read plDate write SetlDate default False;
    property ClockEnabled: Boolean read plEnabled write SetlEnabled default True;
    property TimeSet: TDateTime read pdUra write SetdUra;
    property TimeOffset: Integer read pnOffs write SetnOffs default 0;
    property SpiderClock: Boolean read plSpider write SetlSpider default False;
    property SecJump: Boolean read plSecJump write SetlSecJump default False;
    property Seconds: Boolean read plSekunde write plSekunde default True;
    property MinMarks: Boolean read plMinMarks write SetlMinMarks default True;
    property HourStyle: TJvHourStyle read FHourStyle write SetHourStyle default hsLine;
    property MinuteStyle: TJvHourStyle read FMinuteStyle write SetMinuteStyle default hsLine;
    property HourMarks: TJvHourMarks read FHourMarks write SetHourMarks default hmAll;
    property HourSize: Integer read FHourSize write SetHourSize default 12;
    property MinuteSize: Integer read FMinuteSize write SetMinSize default 7;
    property MinuteFontSize: Integer read FMinuteFontSize write SetMinFontSize default 7;
    property ColorHr: TColor read plColHr write SetlColHr default clBlack;
    property ColorHrIn: TColor read plColHrIn write SetlColHrIn default clBlack;
    property ColorMin: TColor read plColMin write SetlColMin default clBlack;
    property ColorMinIn: TColor read plColMinIn write SetlColMinIn default  clBlack;
    property ColorHandHr: TColor read plColHandHr write SetlColHandHr default clBlack;
    property ColorHandMin: TColor read plColHandMin write SetlColHandMin default clBlack;
    property ColorHandSec: TColor read plColHandSec write SetlColHandSec default clBlack;

    property WidthHandSec: Byte read pnWidthHandSec write SetnWidthHandSec default 1;
    property WidthHandMin: Byte read pnWidthHandMin write SetnWidthHandMin default 3;
    property WidthHandHr: Byte read pnWidthHandHr write SetnWidthHandHr default 5;
    property WidthHr: Byte read pnWidthHr write SetnWidthHr default 2;
    property WidthMin: Byte read pnWidthMin write SetnWidthMin default 1;

//    property MinFont :TFont read pfMinFont write pfMinFont;

    property CenterSize: Byte read pnCenterSize write pnCenterSize default 5;
    property CenterCol: TColor read pnCenterCol write pnCenterCol default clBlack;

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

    property OnSameTime: TNotifyEvent read FOnSameTime write FOnSameTime;
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

constructor TJvAnalogClock.Create(AOwner: TComponent);
var
  h, m, s, hund: Word;
begin
  inherited Create(AOwner);
  BevelInner := bvRaised;
  BevelOuter := bvLowered;
  FHourSize := 12;
  FMinuteSize := 7;
  FMinuteFontSize := 7;

  plSpider := True;
  plSecJump := False;
  plEnabled := True;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := plEnabled;
  FTimer.Interval := 100;
  FTimer.OnTimer := ActTimer;

  Color := clBtnFace;
  Width := 137;
  Height := 137;
  Caption := ' ';
  plSekunde := True;
  plMinMarks := True;
  nDeli := 50;

  FHourStyle := hsLine;
  FMinuteStyle := hsLine;
  FHourMarks := hmAll;

  plColHr := clBlack;
  plColHrIn := clBlack;
  plColMin := clBlack;
  plColMinIn := clBlack;
  plColHandHr := clBlack;
  plColHandMin := clBlack;
  plColHandSec := clBlack;

  pnWidthHandSec := 1;
  pnWidthHandMin := 3;
  pnWidthHandHr := 5;
  pnWidthHr := 2;
  pnWidthMin := 1;

  pnCenterCol := clBlack;
  pnCenterSize := 5;

//  pfMinFont := TFont.Create;
//  pfMinFont := TTextAttributes.Create;
//  pfMinFont.Assign(Font);
//  pfMinFont.Charset := Font.Charset;
//  pfMinFont.Name := Font.Name;
//  pfMinFont.Color := Font.Color;
//  pfMinFont.Size := Font.Size;
//  pfMinFont.Style := Font.Style;
//  pfMinFont.Pitch := Font.Pitch;
//  pfMinFont.FontAdapter := Font.FontAdapter;
//  pfMinFont.OnChange := Font.OnChange;
 //InternalPaint;

  DecodeTime(Now, h, m, s, hund);
  OldMin := m;
  OldHour := h;
  OldSec := s;
end;

destructor TJvAnalogClock.Destroy;
begin
  FTimer.Free;
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

procedure TJvAnalogClock.SetdUra(Value: TDateTime);
begin
  pdUra := Value;
  nDeli := 50;
  ActTimer(Self);
end;

procedure TJvAnalogClock.SetnOffs(Value: Integer);
begin
  pnOffs := Value;
  nDeli := 50;
  if not plEnabled then
    ActTimer(Self);
end;

procedure TJvAnalogClock.SetlEnabled(Value: Boolean);
begin
  plEnabled := Value;
  if plEnabled and (not FTimer.Enabled) then
    FTimer.Enabled := plEnabled;
  nDeli := 50;
  if not plEnabled then
  begin
    pdUra := Now;
    ActTimer(Self);
  end;
end;

procedure TJvAnalogClock.SetlMinMarks(Value: Boolean);
begin
  plMinMarks := Value;
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

procedure TJvAnalogClock.SetlColHr(Value: TColor);
begin
  plColHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColHrIn(Value: TColor);
begin
  plColHrIn := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColMinIn(Value: TColor);
begin
  plColMinIn := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColMin(Value: TColor);
begin
  plColMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColHandHr(Value: TColor);
begin
  plColHandHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColHandMin(Value: TColor);
begin
  plColHandMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlColHandSec(Value: TColor);
begin
  plColHandSec := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetnWidthHandSec(Value: Byte);
begin
  pnWidthHandSec := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetnWidthHandMin(Value: Byte);
begin
  pnWidthHandMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetnWidthHandHr(Value: Byte);
begin
  pnWidthHandHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetnWidthHr(Value: Byte);
begin
  pnWidthHr := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetnWidthMin(Value: Byte);
begin
  pnWidthMin := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlSecJump(Value: Boolean);
begin
  plSecJump := Value;
//if plSecJump then FTimer.Interval := 500 else FTimer.Interval := 100;
end;

procedure TJvAnalogClock.SetlSpider(Value: Boolean);
begin
  plSpider := Value;
  Invalidate;
end;

procedure TJvAnalogClock.SetlDate(Value: Boolean);
begin
  plDate := Value;
  Invalidate;
end;

procedure TJvAnalogClock.DoAlarm;
begin
  if Assigned(FOnSameTime) and not (csDestroying in ComponentState) then
    FOnSameTime(Self);
end;

procedure TJvAnalogClock.DoChangeSec(nHr, nMin, nSec: Integer);
begin
  if Assigned(FOnChangeSec) and not (csDestroying in ComponentState) then
    FOnChangeSec(Self, nHr, nMin, nSec);
end;

procedure TJvAnalogClock.DoChangeMin(nHr, nMin, nSec: Integer);
begin
  if Assigned(FOnChangeMin) and not (csDestroying in ComponentState) then
    FOnChangeMin(Self, nHr, nMin, nSec);
end;

procedure TJvAnalogClock.DoChangeHour(nHr, nMin, nSec: Integer);
begin
  if Assigned(FOnChangeHour) and not (csDestroying in ComponentState) then
    FOnChangeHour(Self, nHr, nMin, nSec);
end;


procedure TJvAnalogClock.InternalPaint;
var
  fak: Real;
  t: Integer;
  nUrca: Integer;                       //??
  npkT, nXT, nYT: Integer;
  rT: TRect;
  sT: ShortString;
  datx, daty: Integer;
  datS: string;

begin

  npx := (Width div 2);               {Center}
  npy := (Height div 2);
//if plDate then npy := ((Height - pFDate.Height) div 2);
 // if plDate then
 //   npy := ((Height - (Font.Height + 4)) div 2);

  npxk := npx - (1 + HourSize);
  if BevelInner <> bvNone then
    npxk := npxk - BevelWidth;
  if BevelOuter <> bvNone then
    npxk := npxk - BevelWidth;

  npyk := npy - (1 + HourSize);
  if BevelInner <> bvNone then
    npyk := npYk - BevelWidth;
  if BevelOuter <> bvNone then
    npyk := npYk - BevelWidth;

  npk := npYk;
  if npXk < npYk then
    npk := npXk;
  npk := npk - pnWidthHr;
  npy23 := npk div 3;

  datS := DateToStr(Sysutils.Date);
  datx :=  npx - ((Canvas.TextWidth(datS)) div 2);
  daty := npy div 2;
  if BevelInner <> bvNone then
    daty := daty - BevelWidth;
  if BevelOuter <> bvNone then
    daty := daty - BevelWidth;
  datrT := Rect(datx, daty, datx + Canvas.TextWidth(datS), daty + Canvas.TextHeight(datS));

  //if plDate then
 // begin
 //  Canvas.Brush.Style := bsClear;
  // Canvas.TextRect(datrT, datrT.Left, datrT.Top, datS);

 // end;

  if plMinMarks then
  begin
    if MinuteStyle = hsLine then
    begin
      Canvas.Pen.Color := plColMin;
      Canvas.Pen.Width := pnWidthMin;
      fak := Pi / 3000;
      for t := 0 to 59 do
      begin
        if (HourStyle = hsNumberInCircle) or ((t mod 5) > 0) or (HourMarks = hmNone) or
          ((HourMarks = hmFour) and (((t div 5) mod 3) > 0)) then
        begin
          nUrca := t * 100;
          if plSpider then
          begin
            npkT := Round(Sqrt(Abs((npYk) * Cos(nUrca * fak)) * Abs((npYk) *
              Cos(nUrca * fak)) + Abs((npXk) * Sin(nUrca * fak)) * Abs((npXk) *
              Sin(nUrca * fak))));
          end
          else
          begin
            if npYk < npXk then
              npkT := npYk
            else
              npkT := npXk;
          end;
          Canvas.MoveTo(npX + Round((npk + 1) * Sin(nUrca * fak)), npY -
            Round((npk + 1) * Cos(nUrca * fak)));
          nXT := npX + Round((npkT + MinuteSize) * Sin(nUrca * fak));
          if nXT > npX + npXK + MinuteSize then
            nXT := npX + npXK + MinuteSize;
          if nXT < npX - npXK - MinuteSize then
            nXT := npX - npXK - MinuteSize;
          nYT := npY - Round((npkT + MinuteSize) * Cos(nUrca * fak));
          if nYT > npY + npYK + MinuteSize then
            nYT := npY + npYK + MinuteSize;
          if nYT < npY - npYK - MinuteSize then
            nYT := npY - npYK - MinuteSize;
          Canvas.LineTo(nXT, nYT);
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := plColMin;
      Canvas.Pen.Width := pnWidthMin;
      if (MinuteStyle = hsNumber) or (MinuteStyle = hsNumberInCircle) then
      begin
        Canvas.Font := Font;
        Canvas.Font.Size := MinuteFontSize;
      end;
      fak := Pi / 3000;
      for t := 0 to 59 do
      begin
        if ((t mod 5) > 0) or (HourMarks = hmNone) or ((HourMarks = hmFour) and
          (((t div 5) mod 3) > 0)) then
//      if ((t mod 5) > 0) then
        begin
          nUrca := t * 100;
          if npYk < npXk then
            npkT := npYk
          else
            npkT := npXk;

          nXT := npX + 1 + Round((npkT + (MinuteSize div 2)) * Sin(nUrca * fak));
          nYT := npY + 1 - Round((npkT + (MinuteSize div 2)) * Cos(nUrca * fak));
          rT := Rect(nXT - (MinuteSize div 2), nYT - (MinuteSize div 2), nXT +
            (MinuteSize div 2), nYT + (MinuteSize div 2));
          sT := IntToStr(t);
          if (MinuteStyle = hsCircle) or (MinuteStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Color := plColMinIn;
            Canvas.Brush.Style := bsSolid;
            Canvas.Ellipse(rT);
          end;
          if (MinuteStyle = hsNumber) or (MinuteStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Style := bsClear;
            Canvas.TextRect(rT, rT.Right - ((rT.Right - rT.Left) div 2) -
              (Canvas.TextWidth(sT) div 2) - 1, rT.Bottom - ((rT.Bottom - rT.Top)
              div 2) - (Canvas.TextHeight(sT) div 2) - 1, sT);
          end;
        end;
      end;
    end;
  end;

  if HourMarks <> hmNone then
  begin
    if HourStyle = hsLine then
    begin
      Canvas.Pen.Color := plColHr;
      Canvas.Pen.Width := pnWidthHr;
      fak := Pi / 3000;
      for t := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((t mod 3) = 0)) then
        begin
          nUrca := ((t) * 100) * 30 div 6;
          if plSpider then
          begin
            npkT := Round(Sqrt(Abs((npYk) * Cos(nUrca * fak)) * Abs((npYk) *
              Cos(nUrca * fak)) + Abs((npXk) * Sin(nUrca * fak)) * Abs((npXk) *
              Sin(nUrca * fak))));
          end
          else
          begin
            if npYk < npXk then
              npkT := npYk
            else
              npkT := npXk;
          end;
          Canvas.MoveTo(npX + Round((npk + 1) * Sin(nUrca * fak)), npY -
            Round((npk + 1) * Cos(nUrca * fak)));
          nXT := npX + Round((npkT + HourSize) * Sin(nUrca * fak));
          if nXT > npX + npXK + HourSize then
            nXT := npX + npXK + HourSize;
          if nXT < npX - npXK - HourSize then
            nXT := npX - npXK - HourSize;
          nYT := npY - Round((npkT + HourSize) * Cos(nUrca * fak));
          if nYT > npY + npYK + HourSize then
            nYT := npY + npYK + HourSize;
          if nYT < npY - npYK - HourSize then
            nYT := npY - npYK - HourSize;
          Canvas.LineTo(nXT, nYT);
        end;
      end;
    end
    else
    begin
      Canvas.Pen.Color := plColHr;
      Canvas.Pen.Width := pnWidthHr;
      if (HourStyle = hsNumber) or (HourStyle = hsNumberInCircle) then
        Canvas.Font := Font;
      fak := Pi / 3000;
      for t := 1 to 12 do
      begin
        if (HourMarks = hmAll) or ((HourMarks = hmFour) and ((t mod 3) = 0)) then
        begin
          nUrca := ((t) * 100) * 30 div 6;
          if npYk < npXk then
            npkT := npYk
          else
            npkT := npXk;
          nXT := npX + 1 + Round((npkT + (HourSize div 2)) * Sin(nUrca * fak));
          nYT := npY + 1 - Round((npkT + (HourSize div 2)) * Cos(nUrca * fak));
          rT := Rect(nXT - (HourSize div 2), nYT - (HourSize div 2), nXT +
            (HourSize div 2), nYT + (HourSize div 2));
          sT := inttostr(t);
          if (HourStyle = hsCircle) or (HourStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Color := plColHrIn;
            Canvas.Brush.Style := bsSolid;
            Canvas.Ellipse(rT);
          end;
          if (HourStyle = hsNumber) or (HourStyle = hsNumberInCircle) then
          begin
            Canvas.Brush.Style := bsClear;
            Canvas.TextRect(rT, rT.Right - ((rT.Right - rT.Left) div 2) -
              (Canvas.TextWidth(sT) div 2) - 1, rT.Bottom - ((rT.Bottom - rT.Top)
              div 2) - (Canvas.TextHeight(sT) div 2) - 1, sT);
          end;
        end;
      end;
    end;
  end;

  if not plEnabled then
  begin
    nDeli := 50;
    ActTimer(Self);
  end;
end;

procedure TJvAnalogClock.ActTimer;
var
  h, m, s, hund: Word;
  h1, m1, s1, hund1: Word;
  fak: Real;
  nUra: Integer;
  dT: TDateTime;
  datS: string;
  newDateBottom: Boolean;
begin
  if not plEnabled then
    FTimer.Enabled := plEnabled;
  if plEnabled then
    dT := Now
  else
    dT := pdUra;
  dT := dT + (pnOffs / (60 * 24));
  DecodeTime(dT, h, m, s, hund);
  Inc(nDeli);
  if plSecJump then
  begin
    if s = OldS then
      if plEnabled then
        Exit;
    OldS := s;
    hund := 0;
  end;

  if plDate then
  begin
   datS := DateToStr(dT);
   newDateBottom := ((h mod 12) <2) or ((h mod 12) > 7);
   if(newDateBottom <> DateBottom) or (datS <> OldDate)then
   begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.TextRect(datrT, datrT.Left, datrT.Top, '');
    Canvas.TextRect(Rect(datrT.left, datrT.Top * 3, datrT.Right,
      datrT.Top * 2 + datrT.Bottom), datrT.Left, datrT.Top * 3, '');
   end;
   DateBottom := newDateBottom;
   OldDate := datS;
   Canvas.Brush.Style := bsClear;
   if DateBottom then
     Canvas.TextRect(Rect(datrT.left, datrT.Top * 3, datrT.Right,
      datrT.Top * 2 + datrT.Bottom), datrT.Left, datrT.Top * 3, datS)
   else
     Canvas.TextRect(datrT, datrT.Left, datrT.Top, datS);
  end;

//event handler by Ujlaki Sándor e-mail: ujlaki.sandor@drotposta.hu
  if plEnabled and (s <> OldSec) then   //every seconds
  begin
    OldSec := s;
    DoChangeSec(h, m, s);
    DecodeTime(pdUra, h1, m1, s1, hund1);
    if (s1 = s) and (m1 = m) and (h1 = h) then
      DoAlarm;
    if m <> OldMin then
    begin
      OldMin := m;
      DoChangeMin(h, m, s);
      if h <> OldHour then
      begin
        OldHour := h;
        DoChangeHour(h, m, s);
      end;
    end;
  end;

  nUra := (((s) * 1000) + hund) div 10;
  fak := Pi / 3000;
  if plSekunde then
    Inc(nDeli);

//Delete sec hand
  if plSekunde or (nUraS > 0) then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := pnWidthHandSec;

    Canvas.MoveTo(npX + Round(4 * Sin(nUraS * fak)), npY - Round(4 * Cos(nUraS *
      fak)));
    Canvas.LineTo(npX + Round(npk * Sin(nUraS * fak)), npY - Round(npk *
      Cos(nUraS * fak)));

    Canvas.MoveTo(npX - Round(4 * Sin(nUraS * fak)), npY + Round(4 * Cos(nUraS *
      fak)));
    Canvas.LineTo(npX - Round(15 * Sin(nUraS * fak)), npY + Round(15 * Cos(nUraS
      * fak)));
  end;

//Delete hand
  if nDeli > 50 then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := pnWidthHandHr;

 //Urni kazalec
    Canvas.MoveTo(npX + Round(03 * Sin(nUraU * fak)), npY - Round(03 * Cos(nUraU
      * fak)));
    Canvas.LineTo(npX + Round((npk - npy23) * Sin(nUraU * fak)), npY - Round((npk
      - npy23) * Cos(nUraU * fak)));

    Canvas.MoveTo(npX - Round(03 * Sin(nUraU * fak)), npY + Round(03 * Cos(nUraU
      * fak)));
    Canvas.LineTo(npX - Round(10 * Sin(nUraU * fak)), npY + Round(10 * Cos(nUraU
      * fak)));

 //Minutni kazalec
    Canvas.Pen.Width := pnWidthHandMin;

    Canvas.MoveTo(npX + Round(3 * Sin(nUraM * fak)), npY - Round(3 * Cos(nUraM *
      fak)));
    Canvas.LineTo(npX + Round((npk - pnWidthHandMin) * Sin(nUraM * fak)), npY -
      Round((npk - pnWidthHandMin) * Cos(nUraM * fak)));

    Canvas.MoveTo(npX - Round(3 * Sin(nUraM * fak)), npY + Round(3 * Cos(nUraM *
      fak)));
    Canvas.LineTo(npX - Round(10 * Sin(nUraM * fak)), npY + Round(10 * Cos(nUraM
      * fak)));

    nUraM := (((m) * 60 + S) * 10) div 6;
    nUraU := (((h) * 60 + m) * 25) div 3;
  end;

//Draw hand
  if ((nDeli > 50) or lSekOver) then
  begin
    lSekOver := False;

 //Minutni kazalec
    Canvas.Pen.Width := pnWidthHandMin;
    Canvas.Pen.Color := plColHandMin;
    Canvas.MoveTo(npX + Round(3 * Sin(nUraM * fak)), npY - Round(3 * Cos(nUraM *
      fak)));
    Canvas.LineTo(npX + Round((npk - pnWidthHandMin) * Sin(nUraM * fak)), npY -
      Round((npk - pnWidthHandMin) * Cos(nUraM * fak)));

    Canvas.MoveTo(npX - Round(3 * Sin(nUraM * fak)), npY + Round(3 * Cos(nUraM *
      fak)));
    Canvas.LineTo(npX - Round(10 * Sin(nUraM * fak)), npY + Round(10 * Cos(nUraM
      * fak)));

 //Urni kazalec
    Canvas.Pen.Color := plColHandHr;
    Canvas.Pen.Width := pnWidthHandHr;
    Canvas.MoveTo(npX + Round(3 * Sin(nUraU * fak)), npY - Round(03 * Cos(nUraU
      * fak)));
    Canvas.LineTo(npX + Round((npk - npy23) * Sin(nUraU * fak)), npY - Round((npk
      - npy23) * Cos(nUraU * fak)));

    Canvas.MoveTo(npX - Round(3 * Sin(nUraU * fak)), npY + Round(03 * Cos(nUraU
      * fak)));
    Canvas.LineTo(npX - Round(10 * Sin(nUraU * fak)), npY + Round(10 * Cos(nUraU
      * fak)));

    if (not plSekunde) and (pnCenterSize > 0) then
    begin
      Canvas.Pen.Color := plColHandHr;
      Canvas.Brush.Color := pnCenterCol;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Width := 1;
      Canvas.Ellipse(nPX - pnCenterSize, nPY - pnCenterSize, nPX + pnCenterSize,
        nPY + pnCenterSize);
    end;
    if (nDeli > 50) then
      nDeli := 0;
  end;

  if plSekunde then
  begin
    Canvas.Pen.Width := pnWidthHandSec;
    Canvas.Pen.Color := plColHandSec;

    Canvas.MoveTo(npX + Round(4 * Sin(nUra * fak)), npY - Round(4 * Cos(nUra *
      fak)));
    Canvas.LineTo(npX + Round(npk * Sin(nUra * fak)), npY - Round(npk * Cos(nUra
      * fak)));

    Canvas.MoveTo(npX - Round(4 * Sin(nUra * fak)), npY + Round(4 * Cos(nUra *
      fak)));
    Canvas.LineTo(npX - Round(15 * Sin(nUra * fak)), npY + Round(15 * Cos(nUra *
      fak)));
    nUraS := nUra;

    lSekOver := True;

    if (pnCenterSize > 0) then
    begin
      Canvas.Pen.Color := plColHandHr;
      Canvas.Brush.Color := pnCenterCol;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Width := 1;
      Canvas.Ellipse(nPX - pnCenterSize, nPY - pnCenterSize, nPX + pnCenterSize,
        nPY + pnCenterSize);
    end;
  end
  else
    nUraS := 0;
  //datS := DateToStr(SysUtils.Date);
end;

end.

