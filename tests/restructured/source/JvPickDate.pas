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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvPickDate;

interface

uses Windows, Classes,

{$IFDEF COMPILER6_UP}
Variants,
{$ENDIF}

Controls, SysUtils, Graphics, JvDateUtil;

{ Calendar dialog }

function SelectDate(var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
function SelectDateStr(var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
function PopupDate(var Date: TDateTime; Edit: TWinControl): Boolean;

{ Popup calendar }

function CreatePopupCalendar(AOwner: TComponent
  {$IFDEF COMPILER4_UP}; ABiDiMode: TBiDiMode = bdLeftToRight {$ENDIF}): TWinControl;
procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean);

const
  PopupCalendarSize: TPoint = (X: 187; Y: 124);

implementation

uses Messages, Consts, Forms, Buttons, StdCtrls, Grids, ExtCtrls, JvxCtrls,
  JvCConst, JvToolEdit, JvVCLUtils, JvMaxMin, JvStrUtils;

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

const
  SBtnGlyphs: array[0..3] of PChar = ('PREV2', 'PREV1', 'NEXT1', 'NEXT2');

procedure FontSetDefault(AFont: TFont);
{$IFDEF WIN32}
var
  NonClientMetrics: TNonClientMetrics;
{$ENDIF}
begin
{$IFDEF WIN32}
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
{$ENDIF}
  with AFont do begin
    Color := clWindowText;
    Name := 'MS Sans Serif';
    Size := 8;
    Style := [];
  end;
end;

{ TJvTimerSpeedButton }

type
  TJvTimerSpeedButton = class(TJvxSpeedButton)
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
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
end;

{ TJvCalendar }

{ TJvCalendar implementation copied from Borland CALENDAR.PAS sample unit
  and modified }

type
  TDayOfWeek = 0..6;

  TJvCalendar = class(TCustomGrid)
  private
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
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    function DaysThisMonth: Integer;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
  published
    property CalendarDate: TDateTime read FDate write SetCalendarDate
      stored StoreCalendarDate;
    property Day: Integer index 3  read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2  read GetDateElement write SetDateElement stored False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property Year: Integer index 1  read GetDateElement write SetDateElement stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

constructor TJvCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
{$IFDEF WIN32}
  Params.ExStyle := Params.ExStyle and not WS_EX_CLIENTEDGE;
{$ENDIF}
{$IFDEF COMPILER4_UP}
  AddBiDiModeExStyle(Params.ExStyle);
{$ENDIF}
end;

procedure TJvCalendar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvCalendar.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if TheCellText <> '' then Day := StrToInt(TheCellText);
end;

function TJvCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

procedure TJvCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TheText: string;
begin
  TheText := CellText[ACol, ARow];
  with ARect, Canvas do begin
    if IsWeekend(ACol, ARow) and not (gdSelected in AState) then
      Font.Color := WeekendColor;
    TextRect(ARect, Left + (Right - Left - TextWidth(TheText)) div 2,
      Top + (Bottom - Top - TextHeight(TheText)) div 2, TheText);
  end;
end;

function TJvCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
begin
  if ARow = 0 then  { day names at tops of columns }
    Result := ShortDayNames[(Ord(StartOfWeek) + ACol) mod 7 + 1]
  else begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth) then Result := ''
    else Result := IntToStr(DayNum);
  end;
end;

procedure TJvCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_LEFT, VK_SUBTRACT:
        begin
          if (Day > 1) then Day := Day - 1
          else CalendarDate := CalendarDate - 1;
          Exit;
        end;
      VK_RIGHT, VK_ADD:
        begin
          if (Day < DaysThisMonth) then Day := Day + 1
          else CalendarDate := CalendarDate + 1;
          Exit;
        end
    end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCalendar.KeyPress(var Key: Char);
begin
  if Key in ['T', 't'] then begin
    CalendarDate := Trunc(Now);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

function TJvCalendar.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and FReadOnly) or (CellText[ACol, ARow] = '') then
    Result := False
  else Result := inherited SelectCell(ACol, ARow);
end;

procedure TJvCalendar.SetCalendarDate(Value: TDateTime);
begin
  if FDate <> Value then begin
    FDate := Value;
    UpdateCalendar;
    Change;
  end;
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
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

procedure TJvCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1: if AYear <> Value then AYear := Value else Exit;
      2: if (Value <= 12) and (Value <> AMonth) then begin
           AMonth := Value;
           if ADay > DaysPerMonth(Year, Value) then
             ADay := DaysPerMonth(Year, Value);
         end else Exit;
      3: if (Value <= DaysThisMonth) and (Value <> ADay) then
           ADay := Value
         else Exit;
      else Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    CalendarUpdate(Index = 3);
    Change;
  end;
end;

procedure TJvCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    Invalidate;
  end;
end;

procedure TJvCalendar.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then begin
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
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

procedure TJvCalendar.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then begin
    FUseCurrentDate := Value;
    if Value then begin
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
  if Delta > 0 then ADay := DaysPerMonth(AYear, AMonth)
  else ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then ADay := CurDay
  else ADay := DaysPerMonth(AYear, AMonth);
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
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year + 1;
end;

procedure TJvCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
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
    if FMonthOffset = 2 then FMonthOffset := -5;
    MoveColRow((ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1,
      False, False);
    if DayOnly then Update else Invalidate;
  finally
    FUpdating := False;
  end;
end;

procedure TJvCalendar.UpdateCalendar;
begin
  CalendarUpdate(False);
end;

procedure TJvCalendar.WMSize(var Message: TWMSize);
var
  GridLinesH, GridLinesW: Integer;
begin
  GridLinesH := 6 * GridLineWidth;
  if (goVertLine in Options) or (goFixedVertLine in Options) then
    GridLinesW := 6 * GridLineWidth
  else GridLinesW := 0;
  DefaultColWidth := (Message.Width - GridLinesW) div 7;
  DefaultRowHeight := (Message.Height - GridLinesH) div 7;
end;

{ TJvLocCalendar }

type
  TJvLocCalendar = class(TJvCalendar)
  private
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
  protected
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
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
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

procedure TJvLocCalendar.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ParentColor then FixedColor := Self.Color;
end;

procedure TJvLocCalendar.CMEnabledChanged(var Message: TMessage);
begin
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
  if (D > 0) and (D <= DaysPerMonth(Y, M)) then begin
    if (EncodeDate(Y, M, D) = SysUtils.Date) then
      Frame3D(Canvas, ARect, clBtnShadow, clBtnHighlight, 1);
  end;
end;

{ TJvPopupCalendar }

type
  TJvPopupCalendar = class(TJvPopupWindow)
  private
    FCalendar: TJvCalendar;
    FTitleLabel: TLabel;
    FFourDigitYear: Boolean;
    FBtns: array[0..3] of TJvxSpeedButton;
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
{$IFDEF WIN32}
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
{$ELSE}
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  end;

function CreatePopupCalendar(AOwner: TComponent
  {$IFDEF COMPILER4_UP}; ABiDiMode: TBiDiMode = bdLeftToRight {$ENDIF}): TWinControl;
begin
  Result := TJvPopupCalendar.Create(AOwner);
  if (AOwner <> nil) and not (csDesigning in AOwner.ComponentState) and
    (Screen.PixelsPerInch <> 96) then
  begin { scale to screen res }
    Result.ScaleBy(Screen.PixelsPerInch, 96);
    { The ScaleBy method does not scale the font well, so set the
      font back to the original info. }
    TJvPopupCalendar(Result).FCalendar.ParentFont := True;
    FontSetDefault(TJvPopupCalendar(Result).Font);
{$IFDEF COMPILER4_UP}
    Result.BiDiMode := ABiDiMode;
{$ENDIF}
  end;
end;

procedure SetupPopupCalendar(PopupCalendar: TWinControl;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings; FourDigitYear: Boolean);
var
  I: Integer;
begin
  if (PopupCalendar = nil) or not (PopupCalendar is TJvPopupCalendar) then
    Exit;
  TJvPopupCalendar(PopupCalendar).FFourDigitYear := FourDigitYear;
  if TJvPopupCalendar(PopupCalendar).FCalendar <> nil then begin
    with TJvPopupCalendar(PopupCalendar).FCalendar do begin
      StartOfWeek := AStartOfWeek;
      WeekendColor := AWeekendColor;
      Weekends := AWeekends;
    end;
    if (BtnHints <> nil) then
      for I := 0 to Min(BtnHints.Count - 1, 3) do begin
        if BtnHints[I] <> '' then
          TJvPopupCalendar(PopupCalendar).FBtns[I].Hint := BtnHints[I];
      end;
  end;
end;

constructor TJvPopupCalendar.Create(AOwner: TComponent);
const
  BtnSide = 14;
var
  Control, BackPanel: TWinControl;
begin
  inherited Create(AOwner);
  FFourDigitYear := FourDigitYear;
  Height := Max(PopupCalendarSize.Y, 120);
  Width := Max(PopupCalendarSize.X, 180);
  Color := clBtnFace;
  FontSetDefault(Font);
  if AOwner is TControl then ShowHint := TControl(AOwner).ShowHint
  else ShowHint := True;
  if (csDesigning in ComponentState) then Exit;

  BackPanel := TPanel.Create(Self);
  with BackPanel as TPanel do begin
    Parent := Self;
    Align := alClient;
    ParentColor := True;
{$IFDEF WIN32}
    ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do begin
    Parent := BackPanel;
    Align := alTop;
    Width := Self.Width - 4;
    Height := 18;
    BevelOuter := bvNone;
    ParentColor := True;
{$IFDEF WIN32}
    ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  end;

  FCalendar := TJvLocCalendar.Create(Self);
  with TJvLocCalendar(FCalendar) do begin
    Parent := BackPanel;
    Align := alClient;
    OnChange := CalendarChange;
    OnMouseUp := CalendarMouseUp;
  end;

  FBtns[0] := TJvTimerSpeedButton.Create(Self);
  with FBtns[0] do begin
    Parent := Control;
    SetBounds(-1, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[0]);
    OnClick := PrevYearBtnClick;
    Hint := LoadStr(SPrevYear);
  end;

  FBtns[1] := TJvTimerSpeedButton.Create(Self);
  with FBtns[1] do begin
    Parent := Control;
    SetBounds(BtnSide - 2, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[1]);
    OnClick := PrevMonthBtnClick;
    Hint := LoadStr(SPrevMonth);
  end;

  FTitleLabel := TLabel.Create(Self);
  with FTitleLabel do begin
    Parent := Control;
    AutoSize := False;
    Alignment := taCenter;
    SetBounds(BtnSide * 2 + 1, 1, Control.Width - 4 * BtnSide - 2, 14);
    Transparent := True;
    OnDblClick := TopPanelDblClick;
{$IFDEF WIN32}
    ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  end;

  FBtns[2] := TJvTimerSpeedButton.Create(Self);
  with FBtns[2] do begin
    Parent := Control;
    SetBounds(Control.Width - 2 * BtnSide + 2, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[2]);
    OnClick := NextMonthBtnClick;
    Hint := LoadStr(SNextMonth);
  end;

  FBtns[3] := TJvTimerSpeedButton.Create(Self);
  with FBtns[3] do begin
    Parent := Control;
    SetBounds(Control.Width - BtnSide + 1, -1, BtnSide, BtnSide);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[3]);
    OnClick := NextYearBtnClick;
    Hint := LoadStr(SNextYear);
  end;
end;

procedure TJvPopupCalendar.CalendarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Longint;
begin
  if (Button = mbLeft) and (Shift = []) then begin
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
        begin
          if ssCtrl in Shift then FCalendar.NextYear
          else FCalendar.NextMonth;
        end;
      VK_PRIOR:
        begin
          if ssCtrl in Shift then FCalendar.PrevYear
          else FCalendar.PrevMonth;
        end;
      else TJvLocCalendar(FCalendar).KeyDown(Key, Shift);
    end;
end;

procedure TJvPopupCalendar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (FCalendar <> nil) and (Key <> #0) then
    FCalendar.KeyPress(Key);
end;

{$IFDEF WIN32}

function TJvPopupCalendar.GetValue: Variant;
begin
  if (csDesigning in ComponentState) then
    Result := VarFromDateTime(SysUtils.Date)
  else
    Result := VarFromDateTime(FCalendar.CalendarDate);
end;

procedure TJvPopupCalendar.SetValue(const Value: Variant);
begin
  if not (csDesigning in ComponentState) then begin
    try
      if (Trim(ReplaceStr(VarToStr(Value), DateSeparator, '')) = '') or
        VarIsNull(Value) or VarIsEmpty(Value) then
        FCalendar.CalendarDate := VarToDateTime(SysUtils.Date)
      else FCalendar.CalendarDate := VarToDateTime(Value);
      CalendarChange(nil);
    except
      FCalendar.CalendarDate := VarToDateTime(SysUtils.Date);
    end;
  end;
end;

{$ELSE}

function TJvPopupCalendar.GetValue: string;
begin
  if (csDesigning in ComponentState) then
    Result := FormatDateTime(DefDateFormat(FFourDigitYear), SysUtils.Date)
  else
    Result := FormatDateTime(DefDateFormat(FFourDigitYear), FCalendar.CalendarDate);
end;

procedure TJvPopupCalendar.SetValue(const Value: string);
begin
  if not (csDesigning in ComponentState) then begin
    FCalendar.CalendarDate := StrToDateFmtDef(DefDateFormat(FFourDigitYear),
      Value, SysUtils.Date);
    CalendarChange(nil);
  end;
end;

{$ENDIF}

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
end;

{ TJvSelectDateDlg }

type
  TJvSelectDateDlg = class(TForm)
    Calendar: TJvCalendar;
    TitleLabel: TLabel;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure PrevYearBtnClick(Sender: TObject);
    procedure NextYearBtnClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure CalendarDblClick(Sender: TObject);
    procedure TopPanelDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FBtns: array[0..3] of TJvxSpeedButton;
    procedure SetDate(Date: TDateTime);
    function GetDate: TDateTime;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read GetDate write SetDate;
  end;

constructor TJvSelectDateDlg.Create(AOwner: TComponent);
var
  Control: TWinControl;
begin
{$IFDEF CBUILDER}
  inherited CreateNew(AOwner, 0);
{$ELSE}
  inherited CreateNew(AOwner);
{$ENDIF}
  Caption := LoadStr(SDateDlgTitle);
{$IFDEF WIN32}
  BorderStyle := bsToolWindow;
{$ELSE}
  BorderStyle := bsDialog;
{$ENDIF}
  BorderIcons := [biSystemMenu];
  ClientHeight := 154;
  ClientWidth := 222;
  FontSetDefault(Font);
  Color := clBtnFace;
  Position := poScreenCenter;
  ShowHint := True;
  KeyPreview := True;

  Control := TPanel.Create(Self);
  with Control as TPanel do begin
    Parent := Self;
    SetBounds(0, 0, 222, 22);
    Align := alTop;
    BevelInner := bvLowered;
    ParentColor := True;
    ParentFont := True;
  end;

  TitleLabel := TLabel.Create(Self);
  with TitleLabel do begin
    Parent := Control;
    SetBounds(35, 4, 152, 14);
    Alignment := taCenter;
    AutoSize := False;
    Caption := '';
    ParentFont := True;
    Font.Color := clBlue;
    Font.Style := [fsBold];
    Transparent := True;
    OnDblClick := TopPanelDblClick;
  end;

  FBtns[0] := TJvTimerSpeedButton.Create(Self);
  with FBtns[0] do begin
    Parent := Control;
    SetBounds(3, 3, 16, 16);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[0]);
    OnClick := PrevYearBtnClick;
    Hint := LoadStr(SPrevYear);
  end;

  FBtns[1] := TJvTimerSpeedButton.Create(Self);
  with FBtns[1] do begin
    Parent := Control;
    SetBounds(18, 3, 16, 16);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[1]);
    OnClick := PrevMonthBtnClick;
    Hint := LoadStr(SPrevMonth);
  end;

  FBtns[2] := TJvTimerSpeedButton.Create(Self);
  with FBtns[2] do begin
    Parent := Control;
    SetBounds(188, 3, 16, 16);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[2]);
    OnClick := NextMonthBtnClick;
    Hint := LoadStr(SNextMonth);
  end;

  FBtns[3] := TJvTimerSpeedButton.Create(Self);
  with FBtns[3] do begin
    Parent := Control;
    SetBounds(203, 3, 16, 16);
    Glyph.Handle := LoadBitmap(hInstance, SBtnGlyphs[3]);
    OnClick := NextYearBtnClick;
    Hint := LoadStr(SNextYear);
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do begin
    Parent := Self;
    SetBounds(0, 133, 222, 21);
    Align := alBottom;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    ParentFont := True;
    ParentColor := True;
  end;

  with TButton.Create(Self) do begin
    Parent := Control;
    SetBounds(0, 0, 112, 21);
    Caption := ResStr(SOKButton);
    ModalResult := mrOk;
  end;

  with TButton.Create(Self) do begin
    Parent := Control;
    SetBounds(111, 0, 111, 21);
    Caption := ResStr(SCancelButton);
    ModalResult := mrCancel;
    Cancel := True;
  end;

  Control := TPanel.Create(Self);
  with Control as TPanel do begin
    Parent := Self;
    SetBounds(0, 22, 222, 111);
    Align := alClient;
    BevelInner := bvLowered;
    ParentFont := True;
    ParentColor := True;
  end;

  Calendar := TJvCalendar.Create(Self);
  with Calendar do begin
    Parent := Control;
    Align := alClient;
    ParentFont := True;
    SetBounds(2, 2, 218, 113);
    Color := clWhite;
    TabOrder := 0;
    UseCurrentDate := False;
    OnChange := CalendarChange;
    OnDblClick := CalendarDblClick;
  end;

  OnKeyDown := FormKeyDown;
  Calendar.CalendarDate := Trunc(Now);
  ActiveControl := Calendar;
end;

procedure TJvSelectDateDlg.SetDate(Date: TDateTime);
begin
  if Date = NullDate then Date := SysUtils.Date;
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

procedure TJvSelectDateDlg.CalendarChange(Sender: TObject);
begin
  TitleLabel.Caption := FormatDateTime('MMMM, YYYY', Calendar.CalendarDate);
end;

procedure TJvSelectDateDlg.CalendarDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TJvSelectDateDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ModalResult := mrOK;
    VK_ESCAPE: ModalResult := mrCancel;
    VK_NEXT:
      begin
        if ssCtrl in Shift then Calendar.NextYear
        else Calendar.NextMonth;
        TitleLabel.Update;
      end;
    VK_PRIOR:
      begin
        if ssCtrl in Shift then Calendar.PrevYear
        else Calendar.PrevMonth;
        TitleLabel.Update;
      end;
    VK_TAB:
      begin
        if Shift = [ssShift] then Calendar.PrevMonth
        else Calendar.NextMonth;
        TitleLabel.Update;
      end;
  end; {case}
end;

{ SelectDate routines }

function CreateDateDialog(const DlgCaption: TCaption): TJvSelectDateDlg;
begin
  Result := TJvSelectDateDlg.Create(Application);
  try
    if DlgCaption <> '' then Result.Caption := DlgCaption;
    if Screen.PixelsPerInch <> 96 then begin { scale to screen res }
      Result.ScaleBy(Screen.PixelsPerInch, 96);
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

function PopupDate(var Date: TDateTime; Edit: TWinControl): Boolean;
var
  D: TJvSelectDateDlg;
  P: TPoint;
  W, H, X, Y: Integer;
begin
  Result := False;
  D := CreateDateDialog('');
  try
    D.BorderIcons := [];
    D.HandleNeeded;
    D.Position := poDesigned;
    W := D.Width;
    H := D.Height;
    P := (Edit.ClientOrigin);
    Y := P.Y + Edit.Height - 1;
    if (Y + H) > Screen.Height then Y := P.Y - H + 1;
    if Y < 0 then Y := P.Y + Edit.Height - 1;
    X := (P.X + Edit.Width) - W;
    if X < 0 then X := P.X;
    D.Left := X;
    D.Top := Y;
    D.Date := Date;
    if D.ShowModal = mrOk then begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDate(var Date: TDateTime; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
var
  D: TJvSelectDateDlg;
  I: Integer;
begin
  Result := False;
  D := CreateDateDialog(DlgCaption);
  try
    D.Date := Date;
    with D.Calendar do begin
      StartOfWeek := AStartOfWeek;
      Weekends := AWeekends;
      WeekendColor := AWeekendColor;
    end;
    if (BtnHints <> nil) then
      for I := 0 to Min(BtnHints.Count - 1, 3) do begin
        if BtnHints[I] <> '' then
          D.FBtns[I].Hint := BtnHints[I];
      end;
    if D.ShowModal = mrOk then begin
      Date := D.Date;
      Result := True;
    end;
  finally
    D.Free;
  end;
end;

function SelectDateStr(var StrDate: string; const DlgCaption: TCaption;
  AStartOfWeek: TDayOfWeekName; AWeekends: TDaysOfWeek;
  AWeekendColor: TColor; BtnHints: TStrings): Boolean;
var
  DateValue: TDateTime;
begin
  if StrDate <> '' then begin
    try
      DateValue := StrToDateFmt(ShortDateFormat, StrDate);
    except
      DateValue := Date;
    end;
  end
  else DateValue := Date;
  Result := SelectDate(DateValue, DlgCaption, AStartOfWeek, AWeekends,
    AWeekendColor, BtnHints);
  if Result then StrDate := FormatDateTime(ShortDateFormat, DateValue);
end;

end.
