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

Last Modified: 2002-07-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAnalogClock;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JVCLVer;

type
  TJvNotifyTime = procedure(Sender: TObject; nVal: integer) of object;
  TJvHrStyle = (hsLine, hsCircle, hsNumber, hsNumInCir);
  TJvAnalogClock = class(TCustomPanel)
  private
    { Private declarations }
    phStyle: TJvHrStyle;
    pnHrSize: integer;

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
    plColHandHr: TColor;
    plColHandMin: TColor;
    plColHandSec: TColor;

    pnWidthHandMin: Byte;
    pnWidthHandHr: Byte;
    pnWidthHandSec: Byte;
    pnWidthHr: Byte;

    pnCenterSize: Byte;
    pnCenterCol: TColor;

    FTimer: TTimer;
    lSekOver: Boolean;

    nDeli: integer;
    nUraM: integer;
    nUraU: integer;
    nUraS: integer;
    npx, npy: integer;
    npxk, npyk, npk, npy23: integer;

    OldHour, OldMin, OldSec: Integer;
    FOnChangeSec: TJvNotifyTime;
    FOnChangeMin: TJvNotifyTime;
    FOnChangeHour: TJvNotifyTime;
    FOnSameTime: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
  protected

    { Protected declarations }
    procedure PplDate(Value: Boolean);
    procedure PplSecJump(Value: Boolean);
    procedure PplSpider(Value: Boolean);
    procedure PplEnabled(Value: boolean);
    procedure PplMinMarks(Value: boolean);
    procedure PphStyle(Value: TJvHrStyle);
    procedure pphrSize(Value: integer);
    procedure PpdUra(Value: TDateTime);
    procedure PpnOffs(Value: integer);
    procedure Zrisi;
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure ActTimer(Sender: TObject);

    procedure DoAlarm;
    procedure DoChangeSec(nSec: integer);
    procedure DoChangeMin(nMin: integer);
    procedure DoChangeHour(nHr: integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Date: Boolean read plDate write PplDate default True;
    property ClockEnabled: Boolean read plEnabled write PplEnabled default True;
    property TimeSet: TDateTime read pdUra write PpdUra;
    property TimeOffset: Integer read pnOffs write PpnOffs;
    property SpiderClock: Boolean read plSpider write PplSpider;
    property SecJump: Boolean read plSecJump write PplSecJump default False;
    property Seconds: Boolean read plSekunde write plSekunde default True;
    property MinMarks: Boolean read plMinMarks write pplMinMarks default True;
    property HrStyle: TJvHrStyle read phStyle write pphStyle default hsLine;
    property HrSize: integer read pnHrSize write ppHrSize default 12;
    property ColorHr: TColor read plColHr write plColHr default clBlack;
    property ColorHrIn: TColor read plColHrIn write plColHrIn default clBlack;
    property ColorMin: TColor read plColMin write plColMin default clBlack;
    property ColorHandHr: TColor read plColHandHr write plColHandHr default clBlack;
    property ColorHandMin: TColor read plColHandMin write plColHandMin default clBlack;
    property ColorHandSec: TColor read plColHandSec write plColHandSec default clBlack;

    property WidthHandSec: Byte read pnWidthHandSec write pnWidthHandSec default 1;
    property WidthHandMin: Byte read pnWidthHandMin write pnWidthHandMin default 3;
    property WidthHandHr: Byte read pnWidthHandHr write pnWidthHandHr default 3;
    property WidthHr: Byte read pnWidthHr write pnWidthHr default 2;

    property CenterSize: Byte read pnCenterSize write pnCenterSize default 3;
    property CenterCol: TColor read pnCenterCol write pnCenterCol default clPurple;

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

    property Width default 140;
    property Height default 140;
    property BevelWidth;
    property BevelInner default bvNone; // bvLowered;
    property BevelOuter default bvNone; // bvRaised;
  end;

implementation

constructor TJvAnalogClock.Create;
begin
  inherited;
  // (p3) changed some defaults to "dull it down" :)
  BevelOuter := bvNone; // bvLowered;
  BevelInner := bvNone; // bvRaised;
  BevelWidth := 1;
  pnHrSize := 12;

//  plSpider := True;
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

  plColHr := clBlack;
  plColHrIn := clBlack;
  plColMin := clBlack;
  plColHandHr := clBlack;
  plColHandMin := clBlack;
  plColHandSec := clBlack;

  pnWidthHandSec := 1;
  pnWidthHandMin := 3;
  pnWidthHandHr := 5;
  pnWidthHr := 2;

  pnCenterCol := clBlack;
  pnCenterSize := 5;

  //Zrisi;
end;

destructor TJvAnalogClock.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TJvAnalogClock.Loaded;
begin
  inherited Loaded;
  Zrisi;
end;

procedure TJvAnalogClock.Resize;
begin
  inherited;
  Zrisi;
end;

procedure TJvAnalogClock.Paint;
begin
  inherited;
  Zrisi;
end;


procedure TJvAnalogClock.PpdUra(Value: TDateTime);
begin
  pdUra := Value;
  nDeli := 50;
  ActTimer(self);
end;

procedure TJvAnalogClock.PpnOffs(Value: Integer);
begin
  pnOffs := Value;
  nDeli := 50;
  if not plenabled then
    ActTimer(self);
end;

procedure TJvAnalogClock.PplEnabled(Value: Boolean);
begin
  plEnabled := Value;
  if plEnabled and (not FTimer.Enabled) then
    FTimer.Enabled := plEnabled;
  nDeli := 50;
  if not plenabled then
  begin
    pdUra := now;
    ActTimer(self);
  end;
end;

procedure TJvAnalogClock.PplMinMarks(Value: Boolean);
begin
  plMinMarks := Value;
  Invalidate;
end;

procedure TJvAnalogClock.pphStyle(Value: TJvHrStyle);
begin
  phStyle := Value;
  Invalidate;
end;

procedure TJvAnalogClock.pphrSize(Value: integer);
begin
  pnhrSize := Value;
  Invalidate;
end;

procedure TJvAnalogClock.PplSecJump(Value: Boolean);
begin
  plSecJump := Value;
  //if plSecJump then FTimer.Interval := 500 else FTimer.Interval := 100;
end;

procedure TJvAnalogClock.PplSpider(Value: Boolean);
begin
  plSpider := Value;
  Invalidate;
end;

procedure TJvAnalogClock.PplDate(Value: Boolean);
begin
  plDate := Value;
  Invalidate;
  //Zrisi;
end;

procedure TJvAnalogClock.DoAlarm;
begin
  if Assigned(FOnSameTime) and not (csDestroying in ComponentState) then
    FOnSameTime(Self);
end;

procedure TJvAnalogClock.DoChangeSec(nSec: integer);
begin
  if Assigned(FOnChangeSec) and not (csDestroying in ComponentState) then
    FOnChangeSec(Self, nSec);
end;

procedure TJvAnalogClock.DoChangeMin(nMin: integer);
begin
  if Assigned(FOnChangeMin) and not (csDestroying in ComponentState) then
    FOnChangeMin(Self, nMin);
end;

procedure TJvAnalogClock.DoChangeHour(nHr: integer);
begin
  if Assigned(FOnChangeHour) and not (csDestroying in ComponentState) then
    FOnChangeHour(Self, nHr);
end;

procedure TJvAnalogClock.Zrisi;
var
  fak: real;
  t: Integer;
  nUrca: Integer; //??
  npkT, nXT, nYT: Integer;
  rT: Trect;
  sT: ShortString;
begin

  npx := ((Width) div 2); {Center}
  npy := ((Height) div 2);
  //if plDate then npy := ((Height - pFDate.Height) div 2);
  if plDate then
    npy := ((Height - (Font.Height + 4)) div 2);

  npxk := npx - (1 + pnHrSize);
  if BevelInner <> bvNone then
    npxk := npxk - (Bevelwidth);
  if BevelOuter <> bvNone then
    npxk := npxk - (Bevelwidth);

  npyk := npy - (1 + pnHrSize);
  if BevelInner <> bvNone then
    npyk := npYk - (Bevelwidth);
  if BevelOuter <> bvNone then
    npyk := npYk - (Bevelwidth);

  npk := npYk;
  if npXk < npYk then
    npk := npXk;
  npk := npk - pnWidthHr;
  npy23 := npk div 3;

  if plMinMarks then
  begin
    for t := 0 to 59 do
    begin
      if (phStyle = hsNumInCir) or ((t mod 5) > 0) then
      begin
        nUrca := ((t) * 100);
        fak := (Pi / (3000));
        if plSpider then
        begin
          npkT := Round(SQRT(Abs((npYk) * cos(nUrca * fak)) * Abs((npYk) * cos(nUrca * fak)) + Abs((npXk) * sin(nUrca * fak)) * Abs((npXk) * sin(nUrca * fak))));
        end
        else
        begin
          if npYk < npXk then
            npkT := npYk
          else
            npkT := npXk;
        end;
        Canvas.Pen.Color := plColMin;
        Canvas.Pen.Width := 1;
        Canvas.MoveTo(npX + Round((npk + 1) * sin(nUrca * fak)), npY - Round((npk + 1) * cos(nUrca * fak)));
        nXT := npX + Round((npkT + 7) * sin(nUrca * fak));
        if nXT > npX + npXK + 7 then
          nXT := npX + npXK + 7;
        if nXT < npX - npXK - 7 then
          nXT := npX - npXK - 7;
        nYT := npY - Round((npkT + 7) * cos(nUrca * fak));
        if nYT > npY + npYK + 7 then
          nYT := npY + npYK + 7;
        if nYT < npY - npYK - 7 then
          nYT := npY - npYK - 7;
        Canvas.LineTo(nXT, nYT);
      end;
    end;
  end;

  if phStyle = hsLine then
  begin
    Canvas.Pen.Color := plColHr;
    Canvas.Pen.Width := pnWidthHr;
    for t := 1 to 12 do
    begin
      nUrca := ((t) * 100) * 30 div 6;
      fak := (Pi / (3000));
      if plSpider then
      begin
        npkT := Round(SQRT(Abs((npYk) * cos(nUrca * fak)) * Abs((npYk) * cos(nUrca * fak)) + Abs((npXk) * sin(nUrca * fak)) * Abs((npXk) * sin(nUrca * fak))));
      end
      else
      begin
        if npYk < npXk then
          npkT := npYk
        else
          npkT := npXk;
      end;
      Canvas.MoveTo(npX + Round((npk + 1) * sin(nUrca * fak)), npY - Round((npk + 1) * cos(nUrca * fak)));
      nXT := npX + Round((npkT + pnHrSize) * sin(nUrca * fak));
      if nXT > npX + npXK + pnHrSize then
        nXT := npX + npXK + pnHrSize;
      if nXT < npX - npXK - pnHrSize then
        nXT := npX - npXK - pnHrSize;
      nYT := npY - Round((npkT + pnHrSize) * cos(nUrca * fak));
      if nYT > npY + npYK + pnHrSize then
        nYT := npY + npYK + pnHrSize;
      if nYT < npY - npYK - pnHrSize then
        nYT := npY - npYK - pnHrSize;
      Canvas.LineTo(nXT, nYT);
    end;
  end
  else
  begin
    Canvas.Pen.Color := plColHr;
    Canvas.Pen.Width := pnWidthHr;
    if (phStyle = hsNumber) or (phStyle = hsNumInCir) then
      Canvas.Font := Font;
    for t := 1 to 12 do
    begin
      nUrca := ((t) * 100) * 30 div 6;
      fak := (Pi / (3000));
      if npYk < npXk then
        npkT := npYk
      else
        npkT := npXk;
      nXT := npX + 1 + Round((npkT + (pnHrSize div 2)) * sin(nUrca * fak));
      nYT := npY + 1 - Round((npkT + (pnHrSize div 2)) * cos(nUrca * fak));
      rT := rect(nXT - (pnHrSize div 2), nYT - (pnHrSize div 2), nXT + (pnHrSize div 2), nYT + (pnHrSize div 2));
      sT := inttostr(t);
      if (phStyle = hsCircle) or (phStyle = hsNumInCir) then
      begin
        Canvas.Brush.Color := plColHrIn;
        Canvas.Brush.Style := bsSolid;
        Canvas.Ellipse(rT);
      end;
      if (phStyle = hsNumber) or (phStyle = hsNumInCir) then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.TextRect(rT, rT.Right - ((rT.Right - rT.Left) div 2) - (Canvas.TextWidth(sT) div 2) - 1, rT.Bottom - ((rT.Bottom - rT.Top) div 2) - (Canvas.TextHeight(sT) div 2) - 1, sT);
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
  h, m, s, hund: word;
  h1, m1, s1, hund1: word;
  fak: real;
  nUra: Integer;
  dT: TDateTime;
begin

  if not plEnabled then
    FTimer.Enabled := plEnabled;
  if plEnabled then
    dT := now
  else
    dT := pdUra;
  dT := dT + (pnOffs / (60 * 24));
  DecodeTime(dT, h, m, s, hund);
  inc(nDeli);
  if plSecJump then
  begin
    if s = OldS then
      if plEnabled then
        exit;
    OldS := s;
    hund := 0;
  end;

  //event handler by Ujlaki Sándor e-mail: ujlaki.sandor@drotposta.hu
  if plEnabled and (s <> OldSec) then //every seconds
  begin
    OldSec := s;
    DoChangeSec(s);
    DecodeTime(pdUra, h1, m1, s1, hund1);
    if (s1 = s) and (m1 = m) and (h1 = h) then
      DoAlarm;
    if m <> OldMin then
    begin
      OldMin := m;
      DoChangeMin(m);
      if h <> OldHour then
      begin
        OldHour := h;
        DoChangeHour(h);
      end;
    end;
  end;

  nUra := (((s) * 1000) + hund) div 10;
  fak := (Pi / (3000));
  if plSekunde then
    inc(nDeli);

  //Delete sec hand
  if plSekunde or (nUraS > 0) then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := pnWidthHandSec;

    Canvas.MoveTo(npX + Round(4 * sin(nUraS * fak)), npY - Round(4 * cos(nUraS * fak)));
    Canvas.LineTo(npX + Round(npk * sin(nUraS * fak)), npY - Round(npk * cos(nUraS * fak)));

    Canvas.MoveTo(npX - Round(4 * sin(nUraS * fak)), npY + Round(4 * cos(nUraS * fak)));
    Canvas.LineTo(npX - Round(15 * sin(nUraS * fak)), npY + Round(15 * cos(nUraS * fak)));
  end;

  //Delete hand
  if nDeli > 50 then
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := pnWidthHandHr;

    //Urni kazalec
    Canvas.MoveTo(npX + Round(03 * sin(nUraU * fak)), npY - Round(03 * cos(nUraU * fak)));
    Canvas.LineTo(npX + Round((npk - npy23) * sin(nUraU * fak)), npY - Round((npk - npy23) * cos(nUraU * fak)));

    Canvas.MoveTo(npX - Round(03 * sin(nUraU * fak)), npY + Round(03 * cos(nUraU * fak)));
    Canvas.LineTo(npX - Round(10 * sin(nUraU * fak)), npY + Round(10 * cos(nUraU * fak)));

    //Minutni kazalec
    Canvas.Pen.Width := pnWidthHandMin;

    Canvas.MoveTo(npX + Round(3 * sin(nUraM * fak)), npY - Round(3 * cos(nUraM * fak)));
    Canvas.LineTo(npX + Round((npk - pnWidthHandMin) * sin(nUraM * fak)), npY - Round((npk - pnWidthHandMin) * cos(nUraM * fak)));

    Canvas.MoveTo(npX - Round(3 * sin(nUraM * fak)), npY + Round(3 * cos(nUraM * fak)));
    Canvas.LineTo(npX - Round(10 * sin(nUraM * fak)), npY + Round(10 * cos(nUraM * fak)));

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
    Canvas.MoveTo(npX + Round(3 * sin(nUraM * fak)), npY - Round(3 * cos(nUraM * fak)));
    Canvas.LineTo(npX + Round((npk - pnWidthHandMin) * sin(nUraM * fak)), npY - Round((npk - pnWidthHandMin) * cos(nUraM * fak)));

    Canvas.MoveTo(npX - Round(3 * sin(nUraM * fak)), npY + Round(3 * cos(nUraM * fak)));
    Canvas.LineTo(npX - Round(10 * sin(nUraM * fak)), npY + Round(10 * cos(nUraM * fak)));

    //Urni kazalec
    Canvas.Pen.Color := plColHandHr;
    Canvas.Pen.Width := pnWidthHandHr;
    Canvas.MoveTo(npX + Round(03 * sin(nUraU * fak)), npY - Round(03 * cos(nUraU * fak)));
    Canvas.LineTo(npX + Round((npk - npy23) * sin(nUraU * fak)), npY - Round((npk - npy23) * cos(nUraU * fak)));

    Canvas.MoveTo(npX - Round(03 * sin(nUraU * fak)), npY + Round(03 * cos(nUraU * fak)));
    Canvas.LineTo(npX - Round(10 * sin(nUraU * fak)), npY + Round(10 * cos(nUraU * fak)));

    if (not plSekunde) and (pnCenterSize > 0) then
    begin
      Canvas.Pen.Color := plColHandHr;
      Canvas.Brush.Color := pnCenterCol;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Width := 1;
      Canvas.Ellipse(nPX - pnCenterSize, nPY - pnCenterSize, nPX + pnCenterSize, nPY + pnCenterSize);
    end;
    if (nDeli > 50) then
      nDeli := 0;
  end;

  if plSekunde then
  begin
    Canvas.Pen.Width := pnWidthHandSec;
    Canvas.Pen.Color := plColHandSec;

    Canvas.MoveTo(npX + Round(4 * sin(nUra * fak)), npY - Round(4 * cos(nUra * fak)));
    Canvas.LineTo(npX + Round(npk * sin(nUra * fak)), npY - Round(npk * cos(nUra * fak)));

    Canvas.MoveTo(npX - Round(4 * sin(nUra * fak)), npY + Round(4 * cos(nUra * fak)));
    Canvas.LineTo(npX - Round(15 * sin(nUra * fak)), npY + Round(15 * cos(nUra * fak)));
    nUraS := nUra;

    lSekOver := True;

    if (pnCenterSize > 0) then
    begin
      Canvas.Pen.Color := plColHandHr;
      Canvas.Brush.Color := pnCenterCol;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Width := 1;
      Canvas.Ellipse(nPX - pnCenterSize, nPY - pnCenterSize, nPX + pnCenterSize, nPY + pnCenterSize);
    end;

  end
  else
    nUraS := 0;
end;

end.

