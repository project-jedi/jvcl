{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCalendar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s): Oliver Giesen [ogware att gmx dott net]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

{ @abstract(A wrapper component for the MS MonthCal control available in
    ComCtl32.dll versions 4.70 and above.) }

unit JvCalendar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, CommCtrl, ComCtrls,
  JvComponent, JvTypes, JvExControls;

type
  EMonthCalError = class(EJVCLException);
  TJvMonthCalWeekDay = (mcLocale, mcMonday, mcTuesday, mcWednesday, mcThursday, mcFriday, mcSaturday, mcSunday);
  TJvMonthCalSelEvent = procedure(Sender: TObject; StartDate, EndDate: TDateTime) of object;
  TJvMonthCalStateEvent = procedure(Sender: TObject; Date: TDateTime; Count: Integer; var DayStateArray: array of
    TMonthDayState) of object;

  TJvCustomMonthCalendar = class;

  TJvMonthCalColors = class(TPersistent)
  private
    Calendar: TJvCustomMonthCalendar;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FMonthBackColor: TColor;
    FTrailingTextColor: TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    function GetColor(Index: Integer): TColor;
    procedure SetAllColors;
  public
    constructor Create(AOwner: TJvCustomMonthCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read GetColor write SetColor default clWindow;
    property TextColor: TColor index 1 read GetColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read GetColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read GetColor write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read GetColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read GetColor write SetColor default clInactiveCaptionText;
  end;

  TJvMonthCalAppearance = class(TPersistent)
  private
    FCircleToday: Boolean;
    FShowToday: Boolean;
    FWeekNumbers: Boolean;
    FFirstDoW: TJvMonthCalWeekDay;
    FColors: TJvMonthCalColors;
    FBoldDays: TStringList;
    procedure SetColors(const AValue: TJvMonthCalColors);
    function GetBoldDays: TStrings;
    procedure SetBoldDays(const AValue: TStrings);
    procedure SetCalendar(const AValue: TJvCustomMonthCalendar);
    function GetCalendar: TJvCustomMonthCalendar;
    procedure SetCircleToday(const AValue: Boolean);
    procedure SetFirstDoW(const AValue: TJvMonthCalWeekDay);
    procedure SetShowToday(const AValue: Boolean);
    procedure SetWeekNumbers(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Calendar: TJvCustomMonthCalendar read GetCalendar write SetCalendar;
  published
    property Colors: TJvMonthCalColors read FColors write SetColors;
    property CircleToday: Boolean read FCircleToday write SetCircleToday default True;
    property BoldDays: TStrings read GetBoldDays write SetBoldDays;
    property FirstDayOfWeek: TJvMonthCalWeekDay read FFirstDoW write SetFirstDoW default mcLocale;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
  end;

  TMonthDayStateArray = array [0..11] of TMonthDayState;

  TJvCustomMonthCalendar = class(TJvWinControl)
  private
    FAppearance: TJvMonthCalAppearance;
    FOwnsAppearance: Boolean;
    FMultiSelect: Boolean;
    FMaxSelCount: Word;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FFirstSelDate: TDateTime;
    FLastSelDate: TDateTime;
    FMonthDelta: Integer;
    FToday: TDateTime;
    FBorderStyle: TBorderStyle;
    FOnSelect: TJvMonthCalSelEvent;
    FOnSelChange: TJvMonthCalSelEvent;
    FOnGetState: TJvMonthCalStateEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    FOnSetFocus: TJvFocusChangeEvent;
    FLeaving: Boolean;
    FEntering: Boolean;
    procedure DoBoldDays;
    procedure SetColors(Value: TJvMonthCalColors);
    procedure SetBoldDays(Value: TStrings);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetShowToday(Value: Boolean);
    procedure SetCircleToday(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
    procedure SetFirstDayOfWeek(Value: TJvMonthCalWeekDay);
    procedure SetMaxSelCount(Value: Word);
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    procedure SetFirstSelDate(Value: TDateTime);
    function GetFirstSelDate: TDateTime;
    function GetLastSelDate: TDateTime;
    procedure SetLastSelDate(Value: TDateTime);
    procedure SetSelectedDays(dFrom, dTo: TDateTime);
    procedure SetMonthDelta(Value: Integer);
    procedure SetToday(Value: TDateTime);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetTodayWidth: Integer;
    function GetMinSize: TRect;
    function IsBold(Year, Month, Day: Word): Boolean;
    procedure SetBold(Year, Month, Day: Word; Value: Boolean);

    function GetBoldDays: TStrings;
    function GetCircleToday: Boolean;
    function GetColors: TJvMonthCalColors;
    function GetFirstDayOfWeek: TJvMonthCalWeekDay;
    function GetShowToday: Boolean;
    function GetWeekNumbers: Boolean;

    function GetDays(Year, Month: Word): string;
    procedure SetDays(Year, Month: Word; Value: string);
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure ColorChanged; override;
    procedure FontChanged; override;
    procedure ConstrainedResize(var MinWidth: Integer;
      var MinHeight: Integer; var MaxWidth: Integer;
      var MaxHeight: Integer); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CheckDayState(Year, Month: Word; var DayState: TMonthDayState); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; virtual;
    procedure DoDateSelect(StartDate, EndDate: TDateTime); virtual;
    procedure DoDateSelChange(StartDate, EndDate: TDateTime); virtual;
    procedure DoGetDayState(var DayState: TNMDayState; var StateArray: TMonthDayStateArray); virtual;
    procedure DoKillFocus(FocusedWnd: HWND); override;
    procedure DoSetFocus(FocusedWnd: HWND); override;

    procedure DoSetFocusEvent(const APreviousControl: TWinControl); virtual;
    procedure DoKillFocusEvent(const ANextControl: TWinControl); virtual;

    property MinSize: TRect read GetMinSize;
    property Bold[Year, Month, Day: Word]: Boolean read IsBold write SetBold;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BoldDays: TStrings read GetBoldDays write SetBoldDays;
    property CircleToday: Boolean read GetCircleToday write SetCircleToday default True;
    property Colors: TJvMonthCalColors read GetColors write SetColors;
    property DateFirst: TDateTime read GetFirstSelDate write SetFirstSelDate;
    property DateLast: TDateTime read GetLastSelDate write SetLastSelDate;
    property DateMax: TDateTime read FMaxDate write SetMaxDate;
    property DateMin: TDateTime read FMinDate write SetMinDate;
    property Days[Year, Month: Word]: string read GetDays write SetDays;
    property FirstDayOfWeek: TJvMonthCalWeekDay read GetFirstDayOfWeek write SetFirstDayOfWeek default mcLocale;
    property MaxSelCount: Word read FMaxSelCount write SetMaxSelCount default 7;
    property MonthDelta: Integer read FMonthDelta write SetMonthDelta default 1;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ShowToday: Boolean read GetShowToday write SetShowToday default True;
    property TodayWidth: Integer read GetTodayWidth;
    property WeekNumbers: Boolean read GetWeekNumbers write SetWeekNumbers default False;
    property Today: TDateTime read FToday write SetToday;
    property OnSelect: TJvMonthCalSelEvent read FOnSelect write FOnSelect;
    property OnSelChange: TJvMonthCalSelEvent read FOnSelChange write FOnSelChange;
    property OnGetDayState: TJvMonthCalStateEvent read FOnGetState write FOnGetState;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithAppearance(AOwner: TComponent; const AAppearance: TJvMonthCalAppearance; const
      AOwnsAppearance: Boolean = False);
    destructor Destroy; override;
    function FirstVisibleDate(Partial: Boolean): TDateTime;
    function LastVisibleDate(Partial: Boolean): TDateTime;
    function VisibleMonths: Integer;
    procedure SetDayStates(MonthCount: Integer; DayStates: array of TMonthDayState);

    property Entering: Boolean read FEntering;
    property Leaving: Boolean read FLeaving;
  end;

  TJvMonthCalendar2 = class(TJvCustomMonthCalendar)
  public
    property MinSize;
    property Bold;
    property Days;
  published
    { inherited properties }
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property Height default 160;
    property Width default 190;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnStartDrag;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    { new properties }
    property AutoSize;
    property BoldDays;
    property BorderStyle;
    property CircleToday;
    property Colors;
    property DateMin;
    property DateMax;
    property DateFirst;
    property DateLast;
    property FirstDayOfWeek;
    property MaxSelCount;
    property MonthDelta;
    property MultiSelect;
    property ShowToday;
    property WeekNumbers;
    property Today;
    property OnKillFocus;
    property OnSelect;
    property OnSetFocus;
    property OnSelChange;
    property OnGetDayState;
  end;

function StringToDayStates(const S: string): TMonthDayState;
function DayStatesToString(Days: TMonthDayState): string;
// function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;

implementation

uses
  JvResources;

const
  MCM_GETMAXTODAYWIDTH = (MCM_FIRST + 21);
  MCS_NOTODAYCIRCLE = $0008;
  MCS_NOTODAY = $0010;
  ColorIndex: array [0..5] of Integer = (MCSC_BACKGROUND, MCSC_TEXT,
    MCSC_TITLEBK, MCSC_TITLETEXT, MCSC_MONTHBK, MCSC_TRAILINGTEXT);

  // IE3 and previous:
  //  MCS_NOTODAY     =    $0008;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then
    InitCommonControls;
end;

function IsBlankDate(ST: TSystemTime): Boolean;
begin
  with ST do
    Result := (wMonth = 0) and (wDay = 0);
end;

function StringToDayStates(const S: string): TMonthDayState;
var
  P, L, I, R: Integer;
begin
  Result := 0;
  P := 1;
  L := Length(S);
  if L = 0 then
    Exit;
  while True do
  begin
    while (P <= L) and (S[P] = ',') do
      Inc(P);
    if P > L then
      Break;
    I := P;
    while (P <= L) and (S[P] <> ',') do
      Inc(P);
    R := StrToIntDef(Copy(S, I, P - I), 0);
    if R in [1..31] then
      Result := Result or (1 shl (R - 1));
  end;
end;

type
  // (p3) from ShLwAPI
  TDLLVersionInfo = packed record
    cbSize: DWord;
    dwMajorVersion: DWord;
    dwMinorVersion: DWord;
    dwBuildNumber: DWord;
    dwPlatformID: DWord;
  end;

{
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;
var
  hDLL, hr: THandle;
  pDllGetVersion: function(var dvi: TDLLVersionInfo): Integer; stdcall;
  dvi: TDLLVersionInfo;
begin
  hDLL := LoadLibrary(PChar(DLLName));
  if (hDLL < 32) then
    hDLL := 0;
  if (hDLL <> 0) then
  begin
    Result := True;
    (*  You must get this function explicitly
        because earlier versions of the DLL
        don't implement this function.
        That makes the lack of implementation
        of the function a version marker in itself.   *)
    @pDllGetVersion := GetProcAddress(hDLL, PChar('DllGetVersion'));
    if Assigned(pDllGetVersion) then
    begin
      FillChar(dvi, SizeOf(dvi), #0);
      dvi.cbSize := SizeOf(dvi);
      hr := pDllGetVersion(dvi);
      if (hr = 0) then
      begin
        pdwMajor := dvi.dwMajorVersion;
        pdwMinor := dvi.dwMinorVersion;
      end;
    end
    else (*   If GetProcAddress failed, the DLL is a version previous to the one  shipped with IE 3.x. *)
    begin
      pdwMajor := 4;
      pdwMinor := 0;
    end;
    FreeLibrary(hDLL);
    Exit;
  end;
  Result := False;
end;
}

function DayStatesToString(Days: TMonthDayState): string;
var
  i: Integer;
begin
  Result := '';
  if Days = 0 then
    Exit;
  for i := 0 to 30 do
    if (Days and (1 shl (i))) <> 0 then
      Result := Result + Format('%d,', [i + 1]);
  if AnsiLastChar(Result) = ',' then
    SetLength(Result, Length(Result) - 1);
end;

//=== TJvMonthCalColors ======================================================

constructor TJvMonthCalColors.Create(AOwner: TJvCustomMonthCalendar);
begin
  inherited Create;
  Calendar := AOwner;
  FBackColor := clWindow;
  FTextColor := clWindowText;
  FTitleBackColor := clActiveCaption;
  FTitleTextColor := clWhite;
  FMonthBackColor := clWhite;
  FTrailingTextColor := clInactiveCaptionText;
end;

procedure TJvMonthCalColors.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then
    SourceName := 'nil'
  else
    SourceName := Source.ClassName;
  if Source is TJvMonthCalColors then
  begin
    if Source <> Self then
    begin
      FBackColor := TJvMonthCalColors(Source).BackColor;
      FTextColor := TJvMonthCalColors(Source).TextColor;
      FTitleBackColor := TJvMonthCalColors(Source).TitleBackColor;
      FTitleTextColor := TJvMonthCalColors(Source).TitleTextColor;
      FMonthBackColor := TJvMonthCalColors(Source).MonthBackColor;
      FTrailingTextColor := TJvMonthCalColors(Source).TrailingTextColor;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvMonthCalColors.SetColor(Index: Integer; Value: TColor);
begin
  if (Calendar <> nil) and Calendar.HandleAllocated then
    MonthCal_SetColor(Calendar.Handle, ColorIndex[Index], ColorToRGB(Value));
  case Index of
    0:
      begin
        FBackColor := Value;
        if Calendar <> nil then
          Calendar.Color := FBackColor;
      end;
    1:
      FTextColor := Value;
    2:
      FTitleBackColor := Value;
    3:
      FTitleTextColor := Value;
    4:
      FMonthBackColor := Value;
    5:
      FTrailingTextColor := Value;
  end;
end;

function TJvMonthCalColors.GetColor(Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FBackColor;
    1:
      Result := FTextColor;
    2:
      Result := FTitleBackColor;
    3:
      Result := FTitleTextColor;
    4:
      Result := FMonthBackColor;
    5:
      Result := FTrailingTextColor;
  else
    Result := 0;
  end;
end;

procedure TJvMonthCalColors.SetAllColors;
begin
  SetColor(0, FBackColor);
  SetColor(1, FTextColor);
  SetColor(2, FTitleBackColor);
  SetColor(3, FTitleTextColor);
  SetColor(4, FMonthBackColor);
  SetColor(5, FTrailingTextColor);
end;

//=== TMonthCalStrings =======================================================

type
  TMonthCalStrings = class(TStringList)
  private
    Calendar: TJvCustomMonthCalendar;
  protected
    function GetDateIndex(Year, Month: Word): Integer; virtual;
    function GetBoldDays(Y, M: Word): string; virtual;
  public
    constructor Create;
    { (RB) This is the same as the TStrings.AddString implementation ??? }
    procedure AddStrings(Strings: TStrings); override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    { (RB) no need to override Add, TStringList.Add just calls AddObject }
    //function Add(const S: string): Integer; override;
    function IsBold(Year, Month, Day: Word): Boolean;
    procedure SetBold(Year, Month, Day: Word; Value: Boolean);
    function AddDays(Year, Month: Word; const Days: string): Integer; virtual;
  end;

constructor TMonthCalStrings.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupIgnore;
end;

{ Days is a comma separated list of days to set as bold. If Days is empty, the
  line is removed (if found) }

function TMonthCalStrings.AddDays(Year, Month: Word; const Days: string): Integer;
begin
  if Days = '' then
  begin
    Result := GetDateIndex(Year, Month);
    if Result > -1 then
      Delete(Result);
  end
  else
    Result := Add(Format('%.4d%.2d=%s', [Year, Month, Days]));
end;

{ Note!
  This must be fully qualified, i.e. '199801=1,2,3,4,5' or '000012=25,31' etc
}

(*function TMonthCalStrings.Add(const S: string): Integer;
begin
  if AnsiPos('=', S) <> 7 then
    raise EMonthCalError.CreateResFmt(@RsEInvalidDateStr, [S]);

  Result := IndexOfName(Copy(S, 1, 6));
  if Result > -1 then
  begin
    Sorted := False;
    Strings[Result] := S;
    Sorted := True;
  end
  else
    Result := inherited Add(S);
  if (Calendar <> nil) and Calendar.HandleAllocated then
    Calendar.DoBoldDays;
end;*)

function TMonthCalStrings.IsBold(Year, Month, Day: Word): Boolean;
var
  DayState: TMonthDayState;
begin
  DayState := StringToDayStates(GetBoldDays(Year, Month) + ',' + GetBoldDays(0, Month));
  Result := (DayState and (1 shl (Day - 1))) <> 0;
end;

procedure TMonthCalStrings.SetBold(Year, Month, Day: Word; Value: Boolean);
var
  S: string;
  DayState: TMonthDayState;
begin
  if IsBold(Year, Month, Day) <> Value then
  begin
    S := GetBoldDays(Year, Month) + ',' + GetBoldDays(0, Month);
    if Value then
    begin
      if S = '' then
        S := IntToStr(Day)
      else
        S := S + Format('%d,', [Day]);
      AddDays(Year, Month, S);
      Exit;
    end;
    DayState := StringToDayStates(S);
    DayState := DayState and not (1 shl (Day - 1));
    AddDays(Year, Month, DayStatesToString(DayState));
  end;
end;

procedure TMonthCalStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

function TMonthCalStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  if AnsiPos('=', S) <> 7 then
    raise EMonthCalError.CreateResFmt(@RsEInvalidDateStr, [S]);

  Result := IndexOfName(Copy(S, 1, 6));
  if Result > -1 then
  begin
    Sorted := False;
    Strings[Result] := S;
    Sorted := True;
  end
  else
    Result := inherited AddObject(S, AObject);
  if (Calendar <> nil) and Calendar.HandleAllocated then
    Calendar.DoBoldDays;
end;

function TMonthCalStrings.GetDateIndex(Year, Month: Word): Integer;
var
  S: string;
begin
  if Year = 0 then
    S := Format('0000%.2d', [Month])
  else
    S := Format('%.4d%.2d', [Year, Month]);

  for Result := 0 to Count - 1 do
    if CompareText(Names[Result], S) = 0 then
      Exit;
  Result := -1;
end;

function TMonthCalStrings.GetBoldDays(Y, M: Word): string;
var
  S: string;
begin
  if Y = 0 then
    S := Format('0000%.2d', [M])
  else
    S := Format('%.4d%.2d', [Y, M]);
  Result := Values[S];
end;

//=== TJvCustomMonthCalendar =================================================

constructor TJvCustomMonthCalendar.Create(AOwner: TComponent);
begin
  CreateWithAppearance(AOwner, TJvMonthCalAppearance.Create, True);
end;

constructor TJvCustomMonthCalendar.CreateWithAppearance(AOwner: TComponent;
  const AAppearance: TJvMonthCalAppearance; const AOwnsAppearance: Boolean);
begin
  if not Assigned(AAppearance) then
    raise EMonthCalError.CreateRes(@RsEInvalidAppearance);
  CheckCommonControl(ICC_DATE_CLASSES);
  inherited Create(AOwner);
  FAppearance := AAppearance;
  FOwnsAppearance := AOwnsAppearance;

  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse, csClickEvents, csDoubleClicks, csReflector];

  FAppearance.Calendar := Self;

  FMultiSelect := False;
  FMaxSelCount := 7;
  FMinDate := 0.0;
  FMaxDate := 0.0;
  FFirstSelDate := Date;
  FLastSelDate := 0.0;
  FMonthDelta := 1;
  FToday := Now;
  FBorderStyle := bsNone;
  FEntering := False;
  FLeaving := False;
  inherited Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := MinSize.Right;
  Height := MinSize.Bottom;
end;

destructor TJvCustomMonthCalendar.Destroy;
begin
  if (FOwnsAppearance) then
    FreeAndNil(FAppearance);
  inherited Destroy;
end;

procedure TJvCustomMonthCalendar.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
  MultiSelects: array [Boolean] of DWORD = (0, MCS_MULTISELECT);
  NoTodays: array [Boolean] of DWORD = (MCS_NOTODAY, 0);
  NoCircles: array [Boolean] of DWORD = (MCS_NOTODAYCIRCLE, 0);
  Weeks: array [Boolean] of DWORD = (0, MCS_WEEKNUMBERS);
begin
  InitCommonControl(ICC_DATE_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, MONTHCAL_CLASS);
  with Params do
  begin
    if GetComCtlVersion >= ComCtlVersionIE4 then
      Style := Style or BorderStyles[FBorderStyle] or MultiSelects[FMultiSelect] or NoTodays[FAppearance.ShowToday]
        or NoCircles[FAppearance.CircleToday] or Weeks[FAppearance.WeekNumbers] or MCS_DAYSTATE
    else
      // IE3 doesn't implement the NoTodayCircle style, instead it uses
      // the same constant for MCS_NOTODAY as IE4 does for MCS_NOTODAYCIRCLE ...
      Style := Style or BorderStyles[FBorderStyle] or MultiSelects[FMultiSelect] or NoCircles[FAppearance.ShowToday]
        or Weeks[FAppearance.WeekNumbers] or MCS_DAYSTATE;
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW) or CS_DBLCLKS;
  end;
end;

procedure TJvCustomMonthCalendar.SetColors(Value: TJvMonthCalColors);
begin
  FAppearance.Colors := Value;
end;

procedure TJvCustomMonthCalendar.SetBoldDays(Value: TStrings);
begin
  FAppearance.BoldDays := Value;
end;

function TJvCustomMonthCalendar.IsBold(Year, Month, Day: Word): Boolean;
begin
  Result := TMonthCalStrings(FAppearance.BoldDays).IsBold(Year, Month, Day);
end;

function TJvCustomMonthCalendar.GetDays(Year, Month: Word): string;
begin
  Result := TMonthCalStrings(FAppearance.BoldDays).GetBoldDays(Year, Month);
end;

procedure TJvCustomMonthCalendar.SetDays(Year, Month: Word; Value: string);
begin
  TMonthCalStrings(FAppearance.BoldDays).AddDays(Year, Month, Value);
end;

procedure TJvCustomMonthCalendar.SetBold(Year, Month, Day: Word; Value: Boolean);
begin
  TMonthCalStrings(FAppearance.BoldDays).SetBold(Year, Month, Day, Value);
end;

{ gets the first visible calendar month }

function TJvCustomMonthCalendar.FirstVisibleDate(Partial: Boolean): TDateTime;
var
  rgst: array [0..1] of TSystemTime;
  Flag: Integer;
begin
  Result := 0;
  if Partial then
    Flag := GMR_DAYSTATE
  else
    Flag := GMR_VISIBLE;
  if SendMessage(Handle, MCM_GETMONTHRANGE, Flag, Longint(@rgst)) <> 0 then
    with rgst[0] do
      Result := Trunc(EncodeDate(wYear, wMonth, wDay));
end;

{ gets the last visible calendar month }

function TJvCustomMonthCalendar.LastVisibleDate(Partial: Boolean): TDateTime;
const
  IsPartial: array [Boolean] of Integer = (GMR_VISIBLE, GMR_DAYSTATE);
var
  rgst: array[0..1] of TSystemTime;
  Flag: Integer;
begin
  Result := 0;
  Flag := IsPartial[Partial];
  if SendMessage(Handle, MCM_GETMONTHRANGE, Flag, Longint(@rgst)) <> 0 then
    with rgst[1] do
      Result := Trunc(EncodeDate(wYear, wMonth, wDay));
end;

{ protected }

procedure TJvCustomMonthCalendar.Change;
var
  rgst: array [0..1] of TSystemTime;
  Y, M, D: Word;
begin
  if not HandleAllocated then
    Exit;
  MonthCal_SetFirstDayOfWeek(Handle, Ord(FAppearance.FirstDayOfWeek) - 1);
  MonthCal_SetMaxSelCount(Handle, FMaxSelCount);

  MonthCal_SetMonthDelta(Handle, FMonthDelta);
  SetSelectedDays(FFirstSelDate, FLastSelDate);
  if (FMinDate <> 0) and (FMaxDate <> 0) then
  begin
    DecodeDate(FMinDate, Y, M, D);
    with rgst[0] do
    begin
      wYear := Y;
      wMonth := M;
      wDay := D;
    end;
    DecodeDate(FMaxDate, Y, M, D);
    with rgst[1] do
    begin
      wYear := Y;
      wMonth := M;
      wDay := D;
    end;
    MonthCal_SetRange(Handle, GDTR_MIN or GDTR_MAX, @rgst[0]);
  end
  else
    MonthCal_SetRange(Handle, 0, nil);
  DecodeDate(FTOday, Y, M, D);
  with rgst[0] do
  begin
    wYear := Y;
    wMonth := M;
    wDay := D;
  end;
  MonthCal_SetToday(Handle, rgst[0]);
end;

procedure TJvCustomMonthCalendar.DoBoldDays;
var
  Y, M, D: Word;
  DayArray: TMonthDayStateArray;
  NMDAyState: TNmDayState;
begin
  if not HandleAllocated then
    Exit;
  DecodeDate(FirstVisibleDate(True), Y, M, D);
  FillChar(DayArray, SizeOf(TMonthDayStateArray), 0);
  with NMDayState do
  begin
    stStart.wYear := Y;
    stStart.wMonth := M;
    stStart.wDay := D;
    cDayState := VisibleMonths;
    prgDayState := PMonthDayState(@DayArray);
  end;
  for D := 0 to VisibleMonths - 1 do
  begin
    CheckDayState(Y, M, DayArray[D]);
    Inc(M);
    if M > 12 then
    begin
      M := 1;
      Inc(Y);
    end;
  end;
  SendMessage(Handle, MCM_SETDAYSTATE, VisibleMonths, Longint(@DayArray));
  //  MonthCal_SetDayState(Handle,VisibleMonths,aNMDayState);
end;

procedure TJvCustomMonthCalendar.DoDateSelect(StartDate, EndDate: TDateTime);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self, StartDate, EndDate);
end;

procedure TJvCustomMonthCalendar.DoDateSelChange(StartDate, EndDate: TDateTime);
begin
  if Assigned(FOnSelChange) then
    FOnSelChange(Self, StartDate, EndDate);
end;

procedure TJvCustomMonthCalendar.CheckDayState(Year, Month: Word; var DayState: TMonthDayState);
begin
  DayState := StringToDayStates(TMonthCalStrings(FAppearance.BoldDays).GetBoldDays(Year, Month));
end;

procedure TJvCustomMonthCalendar.DoGetDayState(var DayState: TNMDayState; var StateArray: TMonthDayStateArray);
var
  aDate: TDateTime;
  I: Integer;
  Y, M: Word;
begin
  FillChar(StateArray, SizeOf(TMonthDayStateArray), #0);
  with DayState.stStart do
  begin
    Y := wYear;
    M := wMonth;
  end;
  with DayState do
    for I := 0 to cDayState - 1 do
    begin
      CheckDayState(Y, M, StateArray[I]);
      Inc(M);
      if M > 12 then
      begin
        M := 1;
        Inc(Y);
      end;
    end;

  with DayState.stStart do
    aDate := Trunc(EncodeDate(wYear, wMonth, 1));

  if Assigned(FOnGetState) then
    with DayState do
      FOnGetState(Self, aDate, cDayState, StateArray);
  DayState.prgDayState := PMonthDayState(@StateArray);
end;

procedure TJvCustomMonthCalendar.CreateWnd;
begin
  inherited CreateWnd;
  FAppearance.Colors.SetAllColors;
  Change;
end;

procedure TJvCustomMonthCalendar.ColorChanged;
begin
  inherited ColorChanged;
  InvalidateRect(Handle, nil, True);
end;

procedure TJvCustomMonthCalendar.FontChanged;
begin
  inherited FontChanged;
//  if HandleAllocated then
//    Perform(WM_SIZE,0,0);
  InvalidateRect(Handle, nil, True);
end;

procedure TJvCustomMonthCalendar.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomMonthCalendar.SetShowToday(Value: Boolean);
begin
  FAppearance.ShowToday := Value;
end;

procedure TJvCustomMonthCalendar.SetCircleToday(Value: Boolean);
begin
  FAppearance.CircleToday := Value;
end;

procedure TJvCustomMonthCalendar.SetWeekNumbers(Value: Boolean);
begin
  FAppearance.WeekNumbers := Value;
end;

procedure TJvCustomMonthCalendar.SetFirstDayOfWeek(Value: TJvMonthCalWeekDay);
begin
  FAppearance.FirstDayOfWeek := Value;
end;

procedure TJvCustomMonthCalendar.SetMaxSelCount(Value: Word);
begin
  if FMaxSelCount <> Value then
  begin
    FMaxSelCount := Value;
    Change;
  end;
end;

procedure TJvCustomMonthCalendar.SetMinDate(Value: TDateTime);
begin
  if FMinDate <> Value then
  begin
    FMinDate := Value;
    Change;
  end;
end;

procedure TJvCustomMonthCalendar.SetMaxDate(Value: TDateTime);
begin
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    Change;
  end;
end;

procedure TJvCustomMonthCalendar.SetFirstSelDate(Value: TDateTime);
begin
  FFirstSelDate := Value;
  SetSelectedDays(FFirstSelDate, FLastSelDate);
end;

function TJvCustomMonthCalendar.GetFirstSelDate: TDateTime;
var
  rgst: array [0..1] of TSystemTime;
begin
  Result := FFirstSelDate;
  if not HandleAllocated then
    Exit;
  if FMultiSelect then
    MonthCal_GetSelRange(Handle, @rgst[0])
  else
    MonthCal_GetCurSel(Handle, rgst[0]);
  with rgst[0] do
    FFirstSelDate := EncodeDate(wYear, wMonth, wDay);
end;

procedure TJvCustomMonthCalendar.SetLastSelDate(Value: TDateTime);
begin
  if FLastSelDate <> Value then
  begin
    FLastSelDate := Value;
    SetSelectedDays(FLastSelDate, FFirstSelDate);
  end;
end;

function TJvCustomMonthCalendar.GetLastSelDate: TDateTime;
var
  rgst: array [0..1] of TSystemTime;
begin
  Result := FLastSelDate;
  if not HandleAllocated then
    Exit;
  if not FMultiSelect then
  begin
    Result := FLastSelDate;
    Exit;
  end;
  if MonthCal_GetSelRange(Handle, @rgst[0]) then
    with rgst[1] do
      FLastSelDate := Trunc(EncodeDate(wYear, wMonth, wDay));
end;

procedure TJvCustomMonthCalendar.SetSelectedDays(dFrom, dTo: TDateTime);
var
  rgst: array [0..1] of TSystemTime;
begin
  if not HandleAllocated then
    Exit;
  if FMultiSelect then
  begin
    if (dFrom <> 0) and (dTo <> 0) then
    begin
      with rgst[0] do
        DecodeDate(dFrom, wYear, wMonth, wDay);
      with rgst[1] do
        DecodeDate(dTo, wYear, wMonth, wDay);
      MonthCal_SetSelRange(Handle, @rgst[0]);
    end
    else
      MonthCal_SetSelRange(Handle, nil);
  end
  else
  begin
    with rgst[0] do
      DecodeDate(dFrom, wYear, wMonth, wDay);
    MonthCal_SetCurSel(Handle, rgst[0]);
  end;
end;

procedure TJvCustomMonthCalendar.SetMonthDelta(Value: Integer);
begin
  if FMonthDelta <> Value then
  begin
    FMonthDelta := Value;
    Change;
  end;
end;

procedure TJvCustomMonthCalendar.SetToday(Value: TDateTime);
begin
  if FToday <> Value then
  begin
    FToday := Value;
    Change;
  end;
end;

procedure TJvCustomMonthCalendar.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TJvCustomMonthCalendar.GetTodayWidth: Integer;
begin
  Result := SendMessage(Handle, MCM_GETMAXTODAYWIDTH, 0, 0);
end;

function TJvCustomMonthCalendar.VisibleMonths: Integer;
begin
  Result := 1;
  if not HandleAllocated then
    Exit;
  Result := MonthCal_GetMonthRange(Handle, GMR_DAYSTATE, nil);
end;

procedure TJvCustomMonthCalendar.SetDayStates(MonthCount: Integer; DayStates: array of TMonthDayState);
var
  Index: Integer;
begin
  if not HandleAllocated then
    Exit;
  Index := High(DayStates) - Low(DayStates);
  if (Index < MonthCount) or (Index < VisibleMonths) then
    raise EMonthCalError.CreateRes(@RsEInvalidArgumentToSetDayStates);
  SendMessage(Handle, MCM_SETDAYSTATE, MonthCount, longint(@DayStates));
end;

// first default width  = 166
// next width           = 334 (+ 168)
// next width           = 502 (+ 168)
// next width           = 670 (+ 168)
// first default height = 157
// next height          =  299  (+ 142)
// next height          =  441  (+ 142)
// next height          =  583  (+ 142)

function TJvCustomMonthCalendar.GetMinSize: TRect;
begin
  if HandleAllocated then
  begin
    SendMessage(Handle, MCM_GETMINREQRECT, 0, Longint(@Result));
    OffSetRect(Result, -Result.Left, -Result.Top);
  end
  else
    Result := Rect(0, 0, 191, 154);
end;

procedure TJvCustomMonthCalendar.CNNotify(var Msg: TWMNotify);
var
  dFrom, dTo: TDateTime;
  StateArray: TMonthDayStateArray;
begin
  with Msg.NMHdr^ do
    case Code of
      MCN_GETDAYSTATE:
        DoGetDayState(PNMDayState(Msg.NMHDR)^, StateArray);
      MCN_SELCHANGE:
        begin
          if IsBlankDate(PNMSelChange(Msg.NMHdr)^.stSelStart) then
            Exit;
          with PNMSelChange(Msg.NMHdr)^.stSelStart do
            dFrom := Trunc(EncodeDate(wYear, wMonth, wDay));
          if IsBlankDate(PNMSelChange(Msg.NMHdr)^.stSelEnd) then
            dTo := dFrom
          else
            with PNMSelChange(Msg.NMHdr)^.stSelEnd do
              dTo := Trunc(EncodeDate(wYear, wMonth, wDay));
          DoDateSelChange(dFrom, dTo);
        end;
      MCN_SELECT:
        begin
          if IsBlankDate(PNMSelChange(Msg.NMHdr)^.stSelStart) then
            Exit;
          with PNMSelChange(Msg.NMHdr)^.stSelStart do
            dFrom := Trunc(EncodeDate(wYear, wMonth, wDay));
          if IsBlankDate(PNMSelChange(Msg.NMHdr)^.stSelEnd) then
            dTo := dFrom
          else
            with PNMSelChange(Msg.NMHdr)^.stSelEnd do
              dTo := Trunc(EncodeDate(wYear, wMonth, wDay));
          DoDateSelect(dFrom, dTo);
        end;
    end;
end;

procedure TJvCustomMonthCalendar.ConstrainedResize(var MinWidth, MinHeight,
  MaxWidth, MaxHeight: Integer);
var
  R: TRect;
  CtlMinWidth, CtlMinHeight: Integer;
begin
  if HandleAllocated then
  begin
    MonthCal_GetMinReqRect(Handle, R);
    with R do
    begin
      CtlMinHeight := Bottom - Top;
      CtlMinWidth := Right - Left;
    end;
    if MinHeight < CtlMinHeight then
      MinHeight := CtlMinHeight;
    if MinWidth < CtlMinWidth then
      MinWidth := CtlMinWidth;
  end;
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

function TJvCustomMonthCalendar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    Result := True;
    R := MinSize;
    with R do
    begin
      NewWidth := Right - Left + Ord(BorderStyle = bsSingle) * 2;
      NewHeight := Bottom - Top + Ord(BorderStyle = bsSingle) * 2;
    end;
  end
  else
    Result := False;
end;

procedure TJvCustomMonthCalendar.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows];
end;

procedure TJvCustomMonthCalendar.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  SetFocus;
  inherited;
end;

function TJvCustomMonthCalendar.GetBoldDays: TStrings;
begin
  Result := FAppearance.BoldDays;
end;

function TJvCustomMonthCalendar.GetCircleToday: Boolean;
begin
  Result := FAppearance.CircleToday;
end;

function TJvCustomMonthCalendar.GetColors: TJvMonthCalColors;
begin
  Result := FAppearance.Colors;
end;

function TJvCustomMonthCalendar.GetShowToday: Boolean;
begin
  Result := FAppearance.ShowToday;
end;

function TJvCustomMonthCalendar.GetWeekNumbers: Boolean;
begin
  Result := FAppearance.WeekNumbers;
end;

function TJvCustomMonthCalendar.GetFirstDayOfWeek: TJvMonthCalWeekDay;
begin
  Result := FAppearance.FirstDayOfWeek;
end;

procedure TJvCustomMonthCalendar.DoKillFocus(FocusedWnd: HWND);
begin
  FLeaving := True;
  try
    inherited DoKillFocus(FocusedWnd);
    DoKillFocusEvent(FindControl(FocusedWnd));
  finally
    FLeaving := False;
  end;
end;

procedure TJvCustomMonthCalendar.DoSetFocus(FocusedWnd: HWND);
begin
  FEntering := True;
  try
    inherited DoSetFocus(FocusedWnd);
    DoSetFocusEvent(FindControl(FocusedWnd));
  finally
    FEntering := False;
  end;
end;

procedure TJvCustomMonthCalendar.DoSetFocusEvent(const APreviousControl: TWinControl);
begin
  if Assigned(OnSetFocus) then
    OnSetFocus(Self, APreviousControl);
end;

procedure TJvCustomMonthCalendar.DoKillFocusEvent(const ANextControl: TWinControl);
begin
  if Assigned(OnKillFocus) then
    OnKillFocus(Self, ANextControl);
end;

//=== TJvMonthCalAppearance ==================================================

constructor TJvMonthCalAppearance.Create;
begin
  inherited Create;
  FCircleToday := True;
  FColors := TJvMonthCalColors.Create(nil);
  FBoldDays := TMonthCalStrings.Create;
  FShowToday := True;
  FWeekNumbers := False;
  FFirstDoW := mcLocale;
end;

destructor TJvMonthCalAppearance.Destroy;
begin
  FreeAndNil(FColors);
  FreeAndNil(FBoldDays);
  inherited Destroy;
end;

function TJvMonthCalAppearance.GetCalendar: TJvCustomMonthCalendar;
begin
  Result := FColors.Calendar;
end;

function TJvMonthCalAppearance.GetBoldDays: TStrings;
begin
  Result := FBoldDays;
end;

procedure TJvMonthCalAppearance.SetBoldDays(const AValue: TStrings);
begin
  FBoldDays.Assign(AValue);
  if Assigned(Calendar) then
    Calendar.DoBoldDays;
end;

procedure TJvMonthCalAppearance.SetCalendar(const AValue: TJvCustomMonthCalendar);
begin
  FColors.Calendar := AValue;
  TMonthCalStrings(FBoldDays).Calendar := AValue;
end;

procedure TJvMonthCalAppearance.SetCircleToday(const AValue: Boolean);
begin
  if FCircleToday <> AValue then
  begin
    FCircleToday := AValue;
    if Assigned(Calendar) then
      Calendar.RecreateWnd;
  end;
end;

procedure TJvMonthCalAppearance.SetColors(const AValue: TJvMonthCalColors);
begin
  FColors.Assign(AValue);
end;

procedure TJvMonthCalAppearance.SetFirstDoW(
  const AValue: TJvMonthCalWeekDay);
begin
  if FFirstDoW <> AValue then
  begin
    FFirstDoW := AValue;
    if Assigned(Calendar) then
      Calendar.Change;
  end;
end;

procedure TJvMonthCalAppearance.SetShowToday(const AValue: Boolean);
begin
  if FShowToday <> AValue then
  begin
    FShowToday := AValue;
    if Assigned(Calendar) then
      Calendar.RecreateWnd;
  end;
end;

procedure TJvMonthCalAppearance.SetWeekNumbers(const AValue: Boolean);
begin
  if FWeekNumbers <> AValue then
  begin
    FWeekNumbers := AValue;
    if Assigned(Calendar) then
      Calendar.RecreateWnd;
  end;
end;

end.

