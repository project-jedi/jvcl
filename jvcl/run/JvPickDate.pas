{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPickDate.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPickDate;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Variants, Windows, Messages,
  Controls, Graphics, Forms, Buttons, StdCtrls, Grids, ExtCtrls,
  SysUtils, Classes,
  JvTypes, JvExGrids;

type
  TDayOfWeek = 0..6;

  TJvCalendar = class(TJvExCustomGrid)
  private
    FMinDate: TDateTime; // Polaris
    FMaxDate: TDateTime; // Polaris
    FDate: TDateTime;
    FMonthOffset: Integer;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FStartOfWeek: TDayOfWeekName;
    FUpdating: Boolean;
    FUseCurrentDate: Boolean;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    function GetCellText(ACol, ARow: Integer): string;
    function GetDateElement(Index: Integer): Integer;
    procedure SetCalendarDate(Value: TDateTime);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetStartOfWeek(Value: TDayOfWeekName);
    procedure SetUseCurrentDate(Value: Boolean);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    function IsWeekend(ACol, ARow: Integer): Boolean;
    procedure CalendarUpdate(DayOnly: Boolean);
    function StoreCalendarDate: Boolean;
    //>Polaris
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    //<Polaris
  protected
    //>Polaris
    function GetCellDate(ACol, ARow: Integer): TDateTime;
    function CellInRange(ACol, ARow: Integer): Boolean;
    function DateInRange(ADate: TDateTime): Boolean;
    //<Polaris
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    function DaysThisMonth: Integer;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure BoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
published
    property CalendarDate: TDateTime read FDate write SetCalendarDate stored StoreCalendarDate;
    property Day: Integer index 3 read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property Year: Integer index 1 read GetDateElement write SetDateElement stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property MinDate: TDateTime read FMinDate write SetMinDate stored False; // polaris
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored False; // polaris
  end;

{ Calendar dialog }

function SelectDate(Sender: TWinControl; var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings;
  MinDate: TDateTime = 0; MaxDate: TDateTime = 0): Boolean; // Polaris
function SelectDateStr(Sender: TWinControl; var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings;
  MinDate: TDateTime = 0; MaxDate: TDateTime = 0): Boolean; // Polaris
function PopupDate(var Date: TDateTime; Edit: TWinControl;
  MinDate: TDateTime = 0; MaxDate: TDateTime = 0): Boolean;

{ Popup calendar }

function CreatePopupCalendar(AOwner: TComponent;
  ABiDiMode: TBiDiMode = bdLeftToRight;
  MinDate: TDateTime = 0.0; MaxDate: TDateTime = 0.0): TWinControl;
procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean;
  MinDate: TDateTime = 0.0; MaxDate: TDateTime = 0.0);

const
  PopupCalendarSize: TPoint = (X: 187; Y: 124);

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
  Math, MultiMon,
  JvThemes, JvResources, JvJCLUtils, JvToolEdit, JvSpeedButton,
  JvComponent, JvJVCLUtils, JclSysUtils;

procedure FontSetDefault(AFont: TFont);

var
  NonClientMetrics: TNonClientMetrics;

begin
  {$IFDEF RTL210_UP}
  NonClientMetrics.cbSize := TNonClientMetrics.SizeOf;
  {$ELSE}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  {$ENDIF RTL210_UP}
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, NonClientMetrics.cbSize, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
  begin
    AFont.Color := clWindowText;
    AFont.Name := 'MS Sans Serif';
    AFont.Size := 8;
    AFont.Style := [];
  end;
end;

procedure CreateButtonGlyph(Glyph: TBitmap; Idx: Integer);
type
  TPointList = array [0..3] of TPoint;
const
  PointsLeft: TPointList =
   ((X: 2; Y: 0), (X: 2; Y: 5), (X: 0; Y: 3), (X: 0; Y: 2));
  PointsRight: TPointList =
   ((X: 0; Y: 0), (X: 0; Y: 5), (X: 2; Y: 3), (X: 2; Y: 2));
var
  Points: TPointList;

  function OffsetPoints(const Points: TPointList; Offs: Integer): TPointList;
  var
    I: Integer;
  begin
    Result := Points;
    for I := Low(TPointList) to High(TPointList) do
      Inc(Result[I].X, Offs);
  end;

begin
  Glyph.Width := 8;
  Glyph.Height := 6;
  Glyph.PixelFormat := pf1bit;
  Glyph.Canvas.Brush.Color := clBtnFace;
  Glyph.Canvas.FillRect(Rect(0, 0, 8, 6));
  Glyph.Transparent := True;
  Glyph.Canvas.Brush.Color := clBtnText;
  Glyph.Canvas.Pen.Color := clBtnText;
  case Idx of
    0:
      begin
        Glyph.Canvas.Polygon(PointsLeft);
        Points := OffsetPoints(PointsLeft, 4);
        Glyph.Canvas.Polygon(Points);
      end;
    1:
      begin
        Points := OffsetPoints(PointsLeft, 2);
        Glyph.Canvas.Polygon(Points);
      end;
    2:
      begin
        Points := OffsetPoints(PointsRight, 3);
        Glyph.Canvas.Polygon(Points);
      end;
    3:
      begin
        Points := OffsetPoints(PointsRight, 1);
        Glyph.Canvas.Polygon(Points);
        Points := OffsetPoints(PointsRight, 5);
        Glyph.Canvas.Polygon(Points);
      end;
  end;
end;

//=== { TJvTimerSpeedButton } ================================================

type
  TJvTimerSpeedButton = class(TJvSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowTimer default True;
    property Style default bsWin31;
  end;

constructor TJvTimerSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := bsWin31;
  AllowTimer := True;
  ControlStyle := ControlStyle + [csReplicatable];
end;

//=== { TJvCalendar } ========================================================

{ TJvCalendar implementation copied from Borland CALENDAR.PAS sample unit
  and modified }

constructor TJvCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //>Polaris
  FMinDate := NullDate;
  FMaxDate := NullDate;
  //<Polaris
  FUseCurrentDate := True;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 7;
  ScrollBars := ssNone;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected];
  ControlStyle := ControlStyle + [csFramed];
  FDate := Date;
  UpdateCalendar;
end;


procedure TJvCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_BORDER;
  Params.ExStyle := Params.ExStyle and not WS_EX_CLIENTEDGE;
  AddBiDiModeExStyle(Params.ExStyle);
end;


procedure TJvCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCalendar.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if TheCellText <> '' then
    Day := StrToInt(TheCellText);
end;

function TJvCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

procedure TJvCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TheText: string;

  //>Polaris
  procedure DefaultDraw;
  begin
    if TheText <> '' then
    begin
      Canvas.Brush.Style := bsClear;
      // Use Canvas.TextWidth()  not only TextHeight() to have good result when Form.PixelPerInch <> 96
      Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2,
        ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2, TheText);
    end;
  end;

  procedure PoleDraw;
  begin
    if (ARow > 0) and ((FMinDate <> NullDate) or (FMaxDate <> NullDate)) then
      if not CellInRange(ACol, ARow) then
        if TheText <> '' then
        begin
          Canvas.Font.Color := clBtnFace;
          if Color = clBtnFace then
          begin
            Canvas.Font.Color := clBtnHighlight;
            // Use Canvas.TextWidth()  not only TextHeight() to have good result when Form.PixelPerInch <> 96
            Canvas.TextRect(ARect, ARect.Left + (ARect.Right - ARect.Left - Canvas.TextWidth(TheText)) div 2 + 1,
              ARect.Top + (ARect.Bottom - ARect.Top - Canvas.TextHeight(TheText)) div 2 + 1, TheText);
            Canvas.Font.Color := clBtnShadow;
          end;
        end;
    DefaultDraw;
  end;
  //<Polaris

begin
  TheText := CellText[ACol, ARow];
  if IsWeekend(ACol, ARow) and not (gdSelected in AState) then
    Canvas.Font.Color := WeekendColor;

  PoleDraw;
end;

function TJvCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
begin
  if ARow = 0 then { day names at tops of columns }
    Result := JclFormatSettings.ShortDayNames[(Ord(StartOfWeek) + ACol) mod 7 + 1]
  else
  begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth) then
      Result := ''
    else
      Result := IntToStr(DayNum);
  end;
end;

//>Polaris

procedure TJvCalendar.SetMinDate(Value: TDateTime);
begin
  if FMinDate <> Value then
  begin
    FMinDate := Value;
    if FDate < FMinDate then
      SetCalendarDate(FMinDate);
    //    else
    UpdateCalendar;
  end;
end;

procedure TJvCalendar.SetMaxDate(Value: TDateTime);
begin
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    if FDate > FMaxDate then
      SetCalendarDate(FMaxDate);
    //    else
    UpdateCalendar;
  end;
end;

function TJvCalendar.GetCellDate(ACol, ARow: Integer): TDateTime;
var
  DayNum: Integer;
begin
  Result := NullDate;
  if (ARow > 0) and (GetCellText(ACol, ARow) <> '') then
  begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth) then
      Result := NullDate
    else
      Result := EncodeDate(GetDateElement(1), GetDateElement(2), DayNum);
  end;
end;

function TJvCalendar.CellInRange(ACol, ARow: Integer): Boolean;
begin
  if (Row < 1) {or ((FMinDate = NullDate) and (FMaxDate = NullDate))} then
    Result := True
  else
    Result := DateInRange(GetCellDate(ACol, ARow));
end;

function TJvCalendar.DateInRange(ADate: TDateTime): Boolean;
begin
  if ((FMinDate = NullDate) and (FMaxDate = NullDate)) or (ADate = NullDate) then
    Result := True
  else
  begin
    Result := False;
    if ADate = NullDate then
      Result := True
    else
    if (FMinDate <> NullDate) and (FMaxDate <> NullDate) then
      Result := (ADate >= FMinDate) and (ADate <= FMaxDate)
    else
    if FMinDate <> NullDate then
      Result := ADate >= FMinDate
    else
    if FMaxDate <> NullDate then
      Result := ADate <= FMaxDate
  end;
end;
//<Polaris

procedure TJvCalendar.KeyDown(var Key: Word; Shift: TShiftState);
//>Polaris
var
  OldDay: Integer;
//<Polaris
begin
  OldDay := Day;
  if Shift = [] then
    case Key of
      VK_LEFT, VK_SUBTRACT:
        begin
          if Day > 1 then
            Day := Day - 1
          else
            CalendarDate := CalendarDate - 1;
          if not DateInRange(FDate) then
            Day := OldDay;
          Exit;
        end;
      VK_RIGHT, VK_ADD:
        begin
          if Day < DaysThisMonth then
            Day := Day + 1
          else
            CalendarDate := CalendarDate + 1;
          if not DateInRange(FDate) then
            Day := OldDay;
          Exit;
        end;
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCalendar.KeyPress(var Key: Char);
begin
  if (Key = 'T') or (Key = 't') then
  begin
    CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

function TJvCalendar.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and FReadOnly) or (CellText[ACol, ARow] = '') or
    //>Polaris
    not CellInRange(ACol, ARow) then {//<Polaris}
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

procedure TJvCalendar.SetCalendarDate(Value: TDateTime);
begin
  //if FDate <> Value then
  //begin
  if (FMinDate <> NullDate) and (Value < FMinDate) then
    Value := FMinDate
  else
  if (FMaxDate <> NullDate) and (Value > FMaxDate) then
    Value := FMaxDate;
  FDate := Value;
  UpdateCalendar;
  Change;
  //end;
end;

function TJvCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

function TJvCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1:
      Result := AYear;
    2:
      Result := AMonth;
    3:
      Result := ADay;
  else
    Result := -1;
  end;
end;

procedure TJvCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  iValue: Word;
  TYear, TMonth, TDay: Word;
  AYear, AMonth, ADay: Word;
  //>Polaris
  TmpDate: TDateTime;
  //<Polaris
begin
  if Value > 0 then
  begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    iValue := Value;
    case Index of
      1:
        begin
          //>Polaris
          if FMinDate <> NullDate then
          begin
            DecodeDate(FMinDate, TYear, TMonth, TDay);
            if Value < TYear then
              Value := TYear;
            if (Value = TYear) and (AMonth < TMonth) then
              AMonth := TMonth;
            if (Value = TYear) and (AMonth = TMonth) and (ADay < TDay) then
              ADay := TDay;
          end;
          if FMaxDate <> NullDate then
          begin
            DecodeDate(FMaxDate, TYear, TMonth, TDay);
            if Value > TYear then
              Value := TYear;
            if (Value = TYear) and (AMonth > TMonth) then
              AMonth := TMonth;
            if (Value = TYear) and (AMonth = TMonth) and (ADay > TDay) then
              ADay := TDay;
          end;
          //<Polaris
          if AYear <> Value then
            AYear := Value
          else
            Exit;
        end;
      2:
        if (Value <= 12) and (Value <> AMonth) then
        begin
          //>Polaris
          if FMinDate <> NullDate then
          begin
            DecodeDate(FMinDate, TYear, TMonth, TDay);
            if (AYear = TYear) and (Value < TMonth) then
              Value := TMonth;
            if (Value = TYear) and (AMonth = TMonth) and (ADay < TDay) then
              ADay := TDay;
          end;
          if FMaxDate <> NullDate then
          begin
            DecodeDate(FMaxDate, TYear, TMonth, TDay);
            if (AYear = TYear) and (Value > TMonth) then
              Value := TMonth;
            if (Value = TYear) and (AMonth = TMonth) and (ADay > TDay) then
              ADay := TDay;
          end;
          //<Polaris

          AMonth := Value;
          if ADay > DaysPerMonth(Year, Value) then
            ADay := DaysPerMonth(Year, Value);
          //>Polaris
          {
                    TmpDate := EncodeDate(AYear, AMonth, ADay);
                    if (FMinDate <> NullDate) and (TmpDate < FMinDate) then DecodeDate(FMinDate, TYear, TMonth, ADay);
                    if (FMaxDate <> NullDate) and (TmpDate > FMaxDate) then DecodeDate(FMaxDate, TYear, TMonth, ADay)
          }
          //<Polaris
        end
        else
          Exit;
      3:
        if (Value <= DaysThisMonth) and (Value <> ADay) then
        begin
          //>Polaris
          TmpDate := EncodeDate(AYear, AMonth, Value);
          if (FMinDate <> NullDate) and (TmpDate < FMinDate) then
            DecodeDate(FMinDate, TYear, TMonth, iValue);
          if (FMaxDate <> NullDate) and (TmpDate > FMaxDate) then
            DecodeDate(FMaxDate, TYear, TMonth, iValue);
          //<Polaris
          ADay := iValue
        end
        else
          Exit;
    else
      Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    CalendarUpdate(Index = 3);
    Change;
  end;
end;

procedure TJvCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
    Invalidate;
  end;
end;

procedure TJvCalendar.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then
  begin
    FWeekends := Value;
    UpdateCalendar;
  end;
end;

function TJvCalendar.IsWeekend(ACol, ARow: Integer): Boolean;
begin
  Result := TDayOfWeekName((Integer(StartOfWeek) + ACol) mod 7) in FWeekends;
end;

procedure TJvCalendar.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

procedure TJvCalendar.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then
  begin
    FUseCurrentDate := Value;
    if Value then
    begin
      FDate := Date; { use the current date, then }
      UpdateCalendar;
    end;
  end;
end;

{ Given a value of 1 or -1, moves to Next or Prev month accordingly }

procedure TJvCalendar.ChangeMonth(Delta: Integer);
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then
    ADay := DaysPerMonth(AYear, AMonth)
  else
    ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);
  CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

procedure TJvCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TJvCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TJvCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year + 1;
end;

procedure TJvCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year - 1;
end;

procedure TJvCalendar.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  FirstDate: TDateTime;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    FirstDate := EncodeDate(AYear, AMonth, 1);
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - Ord(StartOfWeek) + 7) mod 7);
    { day of week for 1st of month }
    if FMonthOffset = 2 then
      FMonthOffset := -5;
    MoveColRow((ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1, False, False);
    if DayOnly then
      Update
    else
      Invalidate;
  finally
    FUpdating := False;
  end;
end;

procedure TJvCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;

procedure TJvCalendar.BoundsChanged;
var
  GridLinesH, GridLinesW: Integer;
begin
  GridLinesH := 6 * GridLineWidth;
  if (goVertLine in Options) or (goFixedVertLine in Options) then
    GridLinesW := 6 * GridLineWidth
  else
    GridLinesW := 0;
  DefaultColWidth := (ClientWidth - GridLinesW) div 7;
  DefaultRowHeight := (ClientHeight - GridLinesH) div 7;
  inherited BoundsChanged;
end;

//=== { TJvLocCalendar } =====================================================

type
  TJvLocCalendar = class(TJvCalendar)
  protected
    procedure EnabledChanged; override;
    procedure ParentColorChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property GridLineWidth;
    property DefaultColWidth;
    property DefaultRowHeight;
  end;

constructor TJvLocCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  ControlStyle := ControlStyle + [csReplicatable];
  Ctl3D := False;
  Enabled := False;
  BorderStyle := bsNone;
  ParentColor := True;
  CalendarDate := Trunc(Now);
  UseCurrentDate := False;
  FixedColor := Self.Color;
  Options := [goFixedHorzLine];
  TabStop := False;
end;

procedure TJvLocCalendar.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if ParentColor then
    FixedColor := Self.Color;
end;

procedure TJvLocCalendar.EnabledChanged;
begin
  inherited EnabledChanged;
  if HandleAllocated and not (csDesigning in ComponentState) then
    EnableWindow(Handle, True);
end;


procedure TJvLocCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and not (WS_BORDER or WS_TABSTOP or WS_DISABLED);
end;


procedure TJvLocCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

procedure TJvLocCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  D, M, Y: Word;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
  DecodeDate(CalendarDate, Y, M, D);
  D := StrToIntDef(CellText[ACol, ARow], 0);
  if (D > 0) and (D <= DaysPerMonth(Y, M)) then
    if EncodeDate(Y, M, D) = SysUtils.Date then
      Frame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1);
end;

//=== { TJvPopupCalendar } ===================================================

type
  TJvPopupCalendar = class(TJvPopupWindow)
  private
    FCalendar: TJvCalendar;
    FTitleLabel: TLabel;
    FFourDigitYear: Boolean;
    FBtns: array [0..3] of TJvSpeedButton;
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
    //>Polaris
    //    function GetDate(Index: Integer): TDate;
    procedure SetDate(Index: Integer; Value: TDateTime);
    //<Polaris
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    //>Polaris
    procedure CheckButton;
    //<Polaris
  public
    constructor Create(AOwner: TComponent); override;
    //>Polaris
    procedure Invalidate; override;
    procedure Update; override;
    property MinDate: TDateTime index 0 {read GetDate} write SetDate;
    property MaxDate: TDateTime index 1 {read GetDate} write SetDate;
    //<Polaris
  end;

function CreatePopupCalendar(AOwner: TComponent;
  ABiDiMode: TBiDiMode = bdLeftToRight;
  MinDate: TDateTime = 0.0; MaxDate: TDateTime = 0.0): TWinControl;
begin
  Result := TJvPopupCalendar.Create(AOwner);
end;

procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean;
  MinDate: TDateTime; MaxDate: TDateTime);
var
  I: Integer;
begin
  if (PopupCalendar = nil) or not (PopupCalendar is TJvPopupCalendar) then
    Exit;
  // Polaris
  if not (csDesigning in PopupCalendar.Owner.ComponentState) then
  begin
    TJvPopupCalendar(PopupCalendar).SetDate(0, MinDate);
    TJvPopupCalendar(PopupCalendar).SetDate(1, MaxDate);
  end;
  // Polaris
  //  TJvPopupCalendar(PopupCalendar).MaxDate := MaxDate;

  TJvPopupCalendar(PopupCalendar).FFourDigitYear := FourDigitYear;
  if TJvPopupCalendar(PopupCalendar).FCalendar <> nil then
  begin
    with TJvPopupCalendar(PopupCalendar).FCalendar do
    begin
      StartOfWeek := AStartOfWeek;
      WeekendColor := AWeekendColor;
      Weekends := AWeekends;
    end;
    if BtnHints <> nil then
      for I := 0 to Min(BtnHints.Count - 1, 3) do
        if BtnHints[I] <> '' then
          TJvPopupCalendar(PopupCalendar).FBtns[I].Hint := BtnHints[I];
  end;
end;

constructor TJvPopupCalendar.Create(AOwner: TComponent);
{$IFDEF JVCLThemesEnabled}
var
  BtnSide: Integer;
  VertOffset: Integer;
  HorzOffset: Integer;
  Control, BackPanel: TWinControl;
{$ELSE}
const
  BtnSide = 14;
  VertOffset = -1;
  HorzOffset = 1;
var
  Control, BackPanel: TWinControl;
{$ENDIF JVCLThemesEnabled}
begin
  inherited Create(AOwner);
  FFourDigitYear := IsFourDigitYear;
  Height := Max(PopupCalendarSize.Y, 120);
  Width := Max(PopupCalendarSize.X, 180);

  Color := clBtnFace;
  FontSetDefault(Font);
  if AOwner is TControl then
    ShowHint := TControl(AOwner).ShowHint
  else
    ShowHint := True;
  if csDesigning in ComponentState then
    Exit;

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    VertOffset := 0;
    HorzOffset := 0;
    BtnSide := 16
  end
  else
  begin
    VertOffset := -1;
    HorzOffset := 1;
    BtnSide := 14;
  end;
  {$ENDIF JVCLThemesEnabled}

  BackPanel := TPanel.Create(Self);
  with BackPanel as TPanel do
  begin
    Parent := Self;
    Align := alClient;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
    BevelOuter := bvNone;
    BevelInner := bvNone;
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := BackPanel;
    Align := alTop;
    Width := Self.Width - 4;
    Height := 18;
    BevelOuter := bvNone;
    ParentColor := True;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  FCalendar := TJvLocCalendar.Create(Self);
  with TJvLocCalendar(FCalendar) do
  begin
    Parent := BackPanel;
    Align := alClient;
    OnChange := CalendarChange;
    OnMouseUp := CalendarMouseUp;
  end;

  FBtns[0] := TJvTimerSpeedButton.Create(Self);
  with FBtns[0] do
  begin
    Parent := Control;
    SetBounds(0 - HorzOffset, VertOffset, BtnSide, BtnSide);
    CreateButtonGlyph(Glyph, 0);
    OnClick := PrevYearBtnClick;
    Hint := RsPrevYearHint;
  end;

  FBtns[1] := TJvTimerSpeedButton.Create(Self);
  with FBtns[1] do
  begin
    Parent := Control;
    SetBounds(BtnSide - 1 - HorzOffset, VertOffset, BtnSide, BtnSide);
    CreateButtonGlyph(Glyph, 1);
    OnClick := PrevMonthBtnClick;
    Hint := RsPrevMonthHint;
  end;

  FTitleLabel := TLabel.Create(Self);
  with FTitleLabel do
  begin
    Parent := Control;
    AutoSize := False;
    Alignment := taCenter;
    SetBounds(BtnSide * 2 + 1, 1, Control.Width - 4 * BtnSide - 2, 14);
    Transparent := True;
    OnDblClick := TopPanelDblClick;
    ControlStyle := ControlStyle + [csReplicatable];
  end;

  FBtns[2] := TJvTimerSpeedButton.Create(Self);
  with FBtns[2] do
  begin
    Parent := Control;
    SetBounds(Control.Width - 2 * BtnSide + 1 + HorzOffset, VertOffset, BtnSide, BtnSide);
    CreateButtonGlyph(Glyph, 2);
    OnClick := NextMonthBtnClick;
    Hint := RsNextMonthHint;
  end;

  FBtns[3] := TJvTimerSpeedButton.Create(Self);
  with FBtns[3] do
  begin
    Parent := Control;
    SetBounds(Control.Width - BtnSide + HorzOffset, VertOffset, BtnSide, BtnSide);
    CreateButtonGlyph(Glyph, 3);
    OnClick := NextYearBtnClick;
    Hint := RsNextYearHint;
  end;
  //Polaris
  CheckButton;
end;

//>Polaris

procedure TJvPopupCalendar.CheckButton;
var
  //  CurDate: TDate;
  AYear, AMonth, ADay: Word;
begin
  if not Assigned(FCalendar) then
    Exit;
  //  CurDate := TJvLocCalendar(FCalendar).CalendarDate;
  if TJvLocCalendar(FCalendar).MinDate = NullDate then
    for AYear := 0 to 1 do
      FBtns[AYear].Enabled := True
  else
  begin
    DecodeDate(TJvLocCalendar(FCalendar).MinDate, AYear, AMonth, ADay);
    FBtns[0].Enabled := TJvLocCalendar(FCalendar).Year > AYear;
    FBtns[1].Enabled := (TJvLocCalendar(FCalendar).Year > AYear) or ((TJvLocCalendar(FCalendar).Year = AYear) and
      (TJvLocCalendar(FCalendar).Month > AMonth));
  end;
  if TJvLocCalendar(FCalendar).MaxDate = NullDate then
    for AYear := 2 to 3 do
      FBtns[AYear].Enabled := True
  else
  begin
    DecodeDate(TJvLocCalendar(FCalendar).MaxDate, AYear, AMonth, ADay);
    FBtns[2].Enabled := (TJvLocCalendar(FCalendar).Year < AYear) or ((TJvLocCalendar(FCalendar).Year = AYear) and
      (TJvLocCalendar(FCalendar).Month < AMonth));
    FBtns[3].Enabled := TJvLocCalendar(FCalendar).Year < AYear;
  end;
end;

procedure TJvPopupCalendar.Invalidate;
begin
  CheckButton;
  inherited Invalidate;
end;

procedure TJvPopupCalendar.Update;
begin
  CheckButton;
  inherited Update;
end;

{
function TJvPopupCalendar.GetDate(Index: Integer): TDateTime;
begin
  FCalendar.Min
  case Index of
    0:
      Result := TJvLocCalendar(FCalendar).FMinDate;
    1:
      Result := TJvLocCalendar(FCalendar).FMaxDate;
  else
    Result := NullDate;
  end;
end;
}

procedure TJvPopupCalendar.SetDate(Index: Integer; Value: TDateTime);
begin
  case Index of
    0:
      TJvLocCalendar(FCalendar).FMinDate := Value;
    1:
      TJvLocCalendar(FCalendar).FMaxDate := Value;
  end;
end;

//<Polaris

procedure TJvPopupCalendar.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Longint;
begin
  if (Button = mbLeft) and (Shift - [ssLeft] = []) then
  begin
    TJvLocCalendar(FCalendar).MouseToCell(X, Y, Col, Row);
    if (Row > 0) and (FCalendar.CellText[Col, Row] <> '') then
      CloseUp(True);
  end;
end;

procedure TJvPopupCalendar.TopPanelDblClick(Sender: TObject);
begin
  FCalendar.CalendarDate := Trunc(Now);
end;

procedure TJvPopupCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FCalendar <> nil then
    case Key of
      VK_NEXT:
        if ssCtrl in Shift then
          FCalendar.NextYear
        else
          FCalendar.NextMonth;
      VK_PRIOR:
        if ssCtrl in Shift then
          FCalendar.PrevYear
        else
          FCalendar.PrevMonth;
      VK_RETURN:
        Click;
    else
      TJvLocCalendar(FCalendar).KeyDown(Key, Shift);
    end;
end;

procedure TJvPopupCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (FCalendar <> nil) and (Key <> #0) then
    FCalendar.KeyPress(Key);
end;

function TJvPopupCalendar.GetValue: Variant;
begin
  if csDesigning in ComponentState then
    Result := VarFromDateTime(SysUtils.Date)
  else
    Result := VarFromDateTime(FCalendar.CalendarDate);
end;

procedure TJvPopupCalendar.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then
  begin
    try
      if (Trim(ReplaceStr(VarToStr(Value), JclFormatSettings.DateSeparator, '')) = '') or
        VarIsNullEmpty(Value) then
        FCalendar.CalendarDate := VarToDateTime(SysUtils.Date)
      else
        FCalendar.CalendarDate := VarToDateTime(Value);
      CalendarChange(nil);
    except
      FCalendar.CalendarDate := VarToDateTime(SysUtils.Date);
    end;
  end;
end;

procedure TJvPopupCalendar.PrevYearBtnClick(Sender: TObject);
begin
  FCalendar.PrevYear;
end;

procedure TJvPopupCalendar.NextYearBtnClick(Sender: TObject);
begin
  FCalendar.NextYear;
end;

procedure TJvPopupCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  FCalendar.PrevMonth;
end;

procedure TJvPopupCalendar.NextMonthBtnClick(Sender: TObject);
begin
  FCalendar.NextMonth;
end;

procedure TJvPopupCalendar.CalendarChange(Sender: TObject);
begin
  FTitleLabel.Caption := FormatDateTime('MMMM, YYYY', FCalendar.CalendarDate);
  CheckButton; // Polaris
end;

//=== { TJvSelectDateDlg } ===================================================

type
  TJvSelectDateDlg = class(TJvForm)
    Calendar: TJvCalendar;
    TitleLabel: TLabel;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure CalendarDblClick(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FBtns: array [0..3] of TJvSpeedButton;
    procedure SetDate(Date: TDateTime);
    procedure CheckButton; // Polaris
    function GetDate: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read GetDate write SetDate;
  end;

constructor TJvSelectDateDlg.Create(AOwner: TComponent);
var
  Control: TWinControl;
begin
  inherited CreateNew(AOwner, 0); // BCB compatible
  Caption := RsDateDlgCaption;
  BorderStyle := bsToolWindow;
  Color := clBtnFace;
  BorderIcons := [biSystemMenu];
  ClientHeight := 158; // Polaris
  ClientWidth := 222;
  FontSetDefault(Font);
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};
  ShowHint := True;
  KeyPreview := True;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := Self;
    SetBounds(0, 0, 222, 22);
    Align := alTop;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    ParentColor := True;
    ParentFont := True;
  end;

  TitleLabel := TLabel.Create(Self);
  with TitleLabel do
  begin
    Parent := Control;
    SetBounds(35, 4, 152, 14);
    Alignment := taCenter;
    AutoSize := False;
    Caption := '';
    ParentFont := True;
    Font.Color := clNavy;
    Font.Style := [fsBold];
    Transparent := True;
    OnDblClick := TopPanelDblClick;
  end;

  FBtns[0] := TJvTimerSpeedButton.Create(Self);
  with FBtns[0] do
  begin
    Parent := Control;
    SetBounds(3, 3, 16, 16);
    CreateButtonGlyph(Glyph, 0);
    OnClick := PrevYearBtnClick;
    Flat := True;
    Hint := RsPrevYearHint;
  end;

  FBtns[1] := TJvTimerSpeedButton.Create(Self);
  with FBtns[1] do
  begin
    Parent := Control;
    SetBounds(19, 3, 16, 16);
    CreateButtonGlyph(Glyph, 1);
    OnClick := PrevMonthBtnClick;
    Flat := True;
    Hint := RsPrevMonthHint;
  end;

  FBtns[2] := TJvTimerSpeedButton.Create(Self);
  with FBtns[2] do
  begin
    Parent := Control;
    SetBounds(188, 3, 16, 16);
    CreateButtonGlyph(Glyph, 2);
    OnClick := NextMonthBtnClick;
    Flat := True;
    Hint := RsNextMonthHint;
  end;

  FBtns[3] := TJvTimerSpeedButton.Create(Self);
  with FBtns[3] do
  begin
    Parent := Control;
    SetBounds(204, 3, 16, 16);
    CreateButtonGlyph(Glyph, 3);
    OnClick := NextYearBtnClick;
    Flat := True;
    Hint := RsNextYearHint;
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := Self;
    SetBounds(0, 133, 222, 25); // Polaris
    Align := alBottom;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    ParentFont := True;
    ParentColor := True;
  end;

  {  with TButton.Create(Self) do
     begin
      Parent := Control;
      SetBounds(0, 0, 112, 21);
      Caption := ResStr(SOKButton);
      ModalResult := mrOk;
    end;

    with TButton.Create(Self) do
    begin
      Parent := Control;
      SetBounds(111, 0, 111, 21);
      Caption := ResStr(SCancelButton);
      ModalResult := mrCancel;
      Cancel := True;
      end; }// Polaris

  with TButton.Create(Self) do
  begin // Polaris
    Parent := Control;
    SetBounds(0, 0, 111, 25);
    Default := True;
    ModalResult := mrOk;
    Caption := RsButtonOKCaption;
//    Kind := bkOk;
  end;

  with TButton.Create(Self) do
  begin // Polaris
    Parent := Control;
    SetBounds(111, 0, 111, 25);
    Cancel := True;
    ModalResult := mrCancel;
    Caption := RsButtonCancelCaption;
//    Kind := bkCancel;
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do
  begin
    Parent := Self;
    SetBounds(0, 22, 222, 111);
    Align := alClient;
    BevelInner := bvLowered;
    ParentFont := True;
    ParentColor := True;
  end;

  Calendar := TJvCalendar.Create(Self);
  with Calendar do
  begin
    Parent := Control;
    Align := alClient;
    ParentFont := True;
    SetBounds(2, 2, 218, 113);
    Color := clWhite;
    TabOrder := 0;
    UseCurrentDate := False;
    Options := Options - [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    OnChange := CalendarChange;
    OnDblClick := CalendarDblClick;
  end;

  OnKeyDown := FormKeyDown;
  Calendar.CalendarDate := Trunc(Now);
  ActiveControl := Calendar;
end;



procedure TJvSelectDateDlg.SetDate(Date: TDateTime);
begin
  if Date = NullDate then
    Date := SysUtils.Date;
  try
    Calendar.CalendarDate := Date;
    CalendarChange(nil);
  except
    Calendar.CalendarDate := SysUtils.Date;
  end;
end;

function TJvSelectDateDlg.GetDate: TDateTime;
begin
  Result := Calendar.CalendarDate;
end;

procedure TJvSelectDateDlg.TopPanelDblClick(Sender: TObject);
begin
  SetDate(Trunc(Now));
end;

procedure TJvSelectDateDlg.PrevYearBtnClick(Sender: TObject);
begin
  Calendar.PrevYear;
end;

procedure TJvSelectDateDlg.NextYearBtnClick(Sender: TObject);
begin
  Calendar.NextYear;
end;

procedure TJvSelectDateDlg.PrevMonthBtnClick(Sender: TObject);
begin
  Calendar.PrevMonth;
end;

procedure TJvSelectDateDlg.NextMonthBtnClick(Sender: TObject);
begin
  Calendar.NextMonth;
end;

//>Polaris

procedure TJvSelectDateDlg.CheckButton;
var
  //  CurDate: TDate;
  AYear, AMonth, ADay: Word;
begin
  if not Assigned(Calendar) then
    Exit;
  //  CurDate := Calendar.CalendarDate;
  if Calendar.MinDate = NullDate then
    for AYear := 0 to 1 do
      FBtns[AYear].Enabled := True
  else
  begin
    DecodeDate(Calendar.MinDate, AYear, AMonth, ADay);
    FBtns[0].Enabled := Calendar.Year > AYear;
    FBtns[1].Enabled := (Calendar.Year > AYear) or ((Calendar.Year = AYear) and (Calendar.Month > AMonth));
  end;
  if Calendar.MaxDate = NullDate then
    for AYear := 2 to 3 do
      FBtns[AYear].Enabled := True
  else
  begin
    DecodeDate(Calendar.MaxDate, AYear, AMonth, ADay);
    FBtns[2].Enabled := (Calendar.Year < AYear) or ((Calendar.Year = AYear) and (Calendar.Month < AMonth));
    FBtns[3].Enabled := Calendar.Year < AYear;
  end;
end;
//<Polaris

procedure TJvSelectDateDlg.CalendarChange(Sender: TObject);
begin
  TitleLabel.Caption := FormatDateTime('MMMM, YYYY', Calendar.CalendarDate);
  //Polaris
  CheckButton;
end;

procedure TJvSelectDateDlg.CalendarDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TJvSelectDateDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      ModalResult := mrOk;
    VK_ESCAPE:
      ModalResult := mrCancel;
    VK_NEXT:
      begin
        if ssCtrl in Shift then
          Calendar.NextYear
        else
          Calendar.NextMonth;
        TitleLabel.Update;
        CheckButton; // Polaris
      end;
    VK_PRIOR:
      begin
        if ssCtrl in Shift then
          Calendar.PrevYear
        else
          Calendar.PrevMonth;
        TitleLabel.Update;
        CheckButton; // Polaris
      end;
    VK_TAB:
      begin
        if Shift = [ssShift] then
          Calendar.PrevMonth
        else
          Calendar.NextMonth;
        TitleLabel.Update;
        CheckButton; // Polaris
      end;
  end;
end;

{ SelectDate routines }

function CreateDateDialog(const DlgCaption: TCaption;
  MinDate: TDateTime; MaxDate: TDateTime): TJvSelectDateDlg;
begin
  Result := TJvSelectDateDlg.Create(Application);
  try
    if DlgCaption <> '' then
      Result.Caption := DlgCaption;
    Result.Calendar.MinDate := MinDate; // Polaris
    Result.Calendar.MaxDate := MaxDate; // Polaris
    if Screen.PixelsPerInch <> cDefaultPixelsPerInch then
    begin { scale to screen res }
      Result.ScaleBy(Screen.PixelsPerInch, cDefaultPixelsPerInch);
      { The ScaleBy method does not scale the font well, so set the
        font back to the original info. }
      Result.Calendar.ParentFont := True;
      FontSetDefault(Result.Font);
      Result.Left := (Screen.Width div 2) - (Result.Width div 2);
      Result.Top := (Screen.Height div 2) - (Result.Height div 2);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function PopupDate(var Date: TDateTime; Edit: TWinControl;
  MinDate: TDateTime; MaxDate: TDateTime): Boolean;
var
  D: TJvSelectDateDlg;
  P: TPoint;
  W, H, X, Y: Integer;
begin
  Result := False;
  D := CreateDateDialog('', MinDate, MaxDate);
  try
    D.BorderIcons := [];
    D.HandleNeeded;
    D.Position := poDesigned;
    W := D.Width;
    H := D.Height;
    P := (Edit.ClientOrigin);
    Y := P.Y + Edit.Height - 1;
    if (Y + H) > Screen.Height then
      Y := P.Y - H + 1;
    if Y < 0 then
      Y := P.Y + Edit.Height - 1;
    X := (P.X + Edit.Width) - W;
    if X < 0 then
      X := P.X;
    D.Left := X;
    D.Top := Y;
    D.Date := Date;
    if D.ShowModal = mrOk then
    begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDate(Sender: TWinControl; var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings;
  MinDate: TDateTime; MaxDate: TDateTime): Boolean;
var
  D: TJvSelectDateDlg;
  I: Integer;
  P: TPoint;
  Rect: TRect;
  Monitor: TMonitor;
begin
  Result := False;
  D := CreateDateDialog(DlgCaption, MinDate, MaxDate);
  try
    // for popup position
    if Assigned(Sender) then
    begin
      D.Position := poDesigned;
      P := Sender.ClientOrigin;
      D.Top := P.Y + Sender.Height - 1;

      Monitor := FindMonitor(MonitorFromWindow(Sender.Handle, MONITOR_DEFAULTTONEAREST));
      Rect := GetWorkAreaRect(Monitor);

      if (D.Top + D.Height) > Rect.Bottom then
        D.Top := P.Y - D.Height + 1;
      if D.Top < 0 then
        D.Top := P.Y + Sender.Height - 1;
      D.Left := (P.X + Sender.Width) - D.Width;
      if (D.Left + D.Width) > Rect.Right then
        D.Left := Rect.Right - D.Width;
      if D.Left < 0 then
        D.Left := Max(P.X, 0);
    end;

    D.Date := Date;
    with D.Calendar do
    begin
      StartOfWeek := AStartOfWeek;
      Weekends := AWeekends;
      WeekendColor := AWeekendColor;
    end;
    if BtnHints <> nil then
      for I := 0 to Min(BtnHints.Count - 1, 3) do
      begin
        if BtnHints[I] <> '' then
          D.FBtns[I].Hint := BtnHints[I];
      end;
    if D.ShowModal = mrOk then
    begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDateStr(Sender: TWinControl; var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings;
  MinDate: TDateTime; MaxDate: TDateTime): Boolean;
var
  DateValue: TDateTime;
begin
  if StrDate <> '' then
  begin
    try
      DateValue := StrToDateFmt(JclFormatSettings.ShortDateFormat, StrDate);
    except
      DateValue := Date;
    end;
  end
  else
    DateValue := Date;
  Result := SelectDate(Sender, DateValue, DlgCaption, AStartOfWeek, AWeekends,
    AWeekendColor, BtnHints, MinDate, MaxDate); // Polaris
  if Result then
    StrDate := FormatDateTime(JclFormatSettings.ShortDateFormat, DateValue);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
