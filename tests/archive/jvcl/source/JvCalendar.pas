{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCalendar.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s): Oliver Giesen [ogware@gmx.net]           

Last Modified: 2002-12-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ @abstract(A wrapper component for the MS MonthCal control available in
    ComCtl32.dll versions 4.70 and above.) }

unit JvCalendar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, CommCtrl,
  ComCtrls, JvComponent, JvTypes;

type
  EMonthCalError = class(EJVCLException);
  TJvMonthCalWeekDay = (mcLocale, mcMonday, mcTuesday, mcWednesday, mcThursday, mcFriday, mcSaturday, mcSunday);
  TJvMonthCalSelEvent = procedure(Sender: TObject; StartDate, EndDate: TDateTime) of object;
  TJvMonthCalStateEvent = procedure(Sender: TObject; Date: TDateTime; Count: integer; var DayStateArray: array of TMonthDayState) of object;

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
    function GetColor(Index:integer):TColor;
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
    FBoldDays: TStrings;
    procedure SetColors(const AValue: TJvMonthCalColors);
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
    property BoldDays: TStrings read FBoldDays write SetBoldDays;
    property FirstDayOfWeek: TJvMonthCalWeekDay read FFirstDoW write SetFirstDoW default mcLocale;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
  end;

  TJvCustomMonthCalendar = class(TJvWinControl)
  private
    { Private declarations }
    FAppearance: TJvMonthCalAppearance;
    FOwnsAppearance: Boolean;
    FMultiSelect: boolean;
    FMaxSelCount: word;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FFirstSelDate: TDateTime;
    FLastSelDate: TDateTime;
    FMonthDelta: integer;
    FToday: TDateTime;
    FBorderStyle: TBorderStyle;
    FOnSelect: TJvMonthCalSelEvent;
    FOnSelChange: TJvMonthCalSelEvent;
    FOnGetState: TJvMonthCalStateEvent;
    FOnKillFocus,
    FOnSetFocus: TJvFocusChangeEvent;
    FLeaving: Boolean;
    FEntering: Boolean;
    procedure DoBoldDays;
    procedure SetColors(Value: TJvMonthCalColors);
    procedure SetBoldDays(Value: TStrings);
    procedure SetMultiSelect(Value: boolean);
    procedure SetShowToday(Value: boolean);
    procedure SetCircleToday(Value: boolean);
    procedure SetWeekNumbers(Value: boolean);
    procedure SetFirstDayOfWeek(Value: TJvMonthCalWeekDay);
    procedure SetMaxSelCount(Value: word);
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    procedure SetFirstSelDate(Value: TDateTime);
    function GetFirstSelDate: TDateTime;
    function GetLastSelDate: TDateTime;
    procedure SetLastSelDate(Value: TDateTime);
    procedure SetSelectedDays(dFrom, dTo: TDateTime);
    procedure SetMonthDelta(Value: integer);
    procedure SetToday(Value: TDateTime);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetTodayWidth: integer;
    function GetMinSize: TRect;
    function IsBold(Year, Month, Day: Word): boolean;
    procedure SetBold(Year, Month, Day: Word; Value: boolean);

    function GetBoldDays: TStrings;
    function GetCircleToday: boolean;
    function GetColors: TJvMonthCalColors;
    function GetFirstDayOfWeek: TJvMonthCalWeekDay;
    function GetShowToday: boolean;
    function GetWeekNumbers: boolean;


    function GetDays(Year, Month: Word): string;
    procedure SetDays(Year, Month: Word; Value: string);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMGetDlgCode(var Message: TMessage);message WM_GETDLGCODE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMKillFocus(var AMessage: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var AMessage: TMessage); message WM_SETFOCUS;

  protected
    { Protected declarations }

    procedure ConstrainedResize(var MinWidth: Integer;
      var MinHeight: Integer; var MaxWidth: Integer;
      var MaxHeight: Integer); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CheckDayState(Year, Month: word; var DayState: TMonthDayState); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; virtual;
    procedure DoDateSelect(StartDate, EndDate: TDateTime); virtual;
    procedure DoDateSelChange(StartDate, EndDate: TDateTime); virtual;
    procedure DoGetDayState(var DayState: TNMDayState); virtual;
    procedure DoKillFocus(const ANextControl: TWinControl); virtual;
    procedure DoSetFocus(const APreviousControl: TWinControl); virtual;

    property MinSize: TRect read GetMinSize;
    property Bold[Year, Month, Day: word]: boolean read IsBold write SetBold;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BoldDays: TStrings read GetBoldDays write SetBoldDays;
    property CircleToday: boolean read GetCircleToday write SetCircleToday default true;
    property Colors: TJvMonthCalColors read GetColors write SetColors;
    property DateFirst: TDateTime read GetFirstSelDate write SetFirstSelDate;
    property DateLast: TDateTime read GetLastSelDate write SetLastSelDate;
    property DateMax: TDateTime read FMaxDate write SetMaxDate;
    property DateMin: TDateTime read FMinDate write SetMinDate;
    property Days[Year, Month: Word]: string read GetDays write SetDays;
    property FirstDayOfWeek: TJvMonthCalWeekDay read GetFirstDayOfWeek write SetFirstDayOfWeek default mcLocale;
    property MaxSelCount: word read FMaxSelCount write SetMaxSelCount default 7;
    property MonthDelta: integer read FMonthDelta write SetMonthDelta default 1;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property ShowToday: boolean read GetShowToday write SetShowToday default true;
    property TodayWidth: integer read GetTodayWidth;
    property WeekNumbers: boolean read GetWeekNumbers write SetWeekNumbers default false;
    property Today: TDateTime read FToday write SetToday;
    property OnSelect: TJvMonthCalSelEvent read FOnSelect write FOnSelect;
    property OnSelChange: TJvMonthCalSelEvent read FOnSelChange write FOnSelChange;
    property OnGetDayState: TJvMonthCalStateEvent read FOnGetState write FOnGetState;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithAppearance(AOwner: TComponent; const AAppearance: TJvMonthCalAppearance; const AOwnsAppearance: Boolean = False);
    destructor Destroy; override;
    function FirstVisibleDate(Partial: boolean): TDateTime;
    function LastVisibleDate(Partial: boolean): TDateTime;
    function VisibleMonths: integer;
    procedure SetDayStates(MonthCount: integer; DayStates: array of TMonthDayState);

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
// function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: integer): boolean;

implementation

const
  MCM_GETMAXTODAYWIDTH = (MCM_FIRST + 21);
  MCS_NOTODAYCIRCLE = $0008;
  MCS_NOTODAY = $0010;
  sInvalidDate = 'Invalid date to TJvMonthCalendar2 (%d,%d,%d)';
  sInvalidDateStr = 'Invalid date specification to TMonthCalStrings (%s)';
  ColorIndex: array[0..5] of Integer = (MCSC_BACKGROUND, MCSC_TEXT,
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

function IsBlankDate(ST: TSystemTime): boolean;
begin
  with ST do
    Result := (wMonth = 0) and (wDay = 0);
end;

function StringToDayStates(const S: string): TMonthDayState;
var P, L, I, R: integer;
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
    cbSize:DWord;
    dwMajorVersion:DWord;
    dwMinorVersion:DWord;
    dwBuildNumber:DWord;
    dwPlatformID:DWord;
  end;

{
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: integer): boolean;
var hDLL, hr: THandle;
  pDllGetVersion: function(var dvi: TDLLVersionInfo): integer; stdcall;
  dvi: TDLLVersionInfo;
begin
  hDLL := LoadLibrary(PChar(DLLName));
  if (hDLL < 32) then
    hDLL := 0;
  if (hDLL <> 0) then
  begin
    Result := true;
    (*  You must get this function explicitly
        because earlier versions of the DLL
        don't implement this function.
        That makes the lack of implementation
        of the function a version marker in itself.   *)
    @pDllGetVersion := GetProcAddress(hDLL, PChar('DllGetVersion'));
    if Assigned(pDllGetVersion) then
    begin
      ZeroMemory(@dvi, sizeof(dvi));
      dvi.cbSize := sizeof(dvi);
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
  Result := false;
end;
}

function DayStatesToString(Days: TMonthDayState): string;
var i: integer;
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

{ TJvMonthCalColors }

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
  if (Source = nil) or not (Source is TJvMonthCalColors) then
    raise EConvertError.CreateFmt('Cannot assign %s to a %s', [SourceName, ClassName]);
  FBackColor := TJvMonthCalColors(Source).BackColor;
  FTextColor := TJvMonthCalColors(Source).TextColor;
  FTitleBackColor := TJvMonthCalColors(Source).TitleBackColor;
  FTitleTextColor := TJvMonthCalColors(Source).TitleTextColor;
  FMonthBackColor := TJvMonthCalColors(Source).MonthBackColor;
  FTrailingTextColor := TJvMonthCalColors(Source).TrailingTextColor;
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
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FMonthBackColor := Value;
    5: FTrailingTextColor := Value;
  end;
end;

function TJvMonthCalColors.GetColor(Index: integer): TColor;
begin
  case Index of
    0: Result := FBackColor;
    1: Result := FTextColor;
    2: Result := FTitleBackColor;
    3: Result := FTitleTextColor;
    4: Result := FMonthBackColor;
    5: Result := FTrailingTextColor;
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

type
  TMonthCalStrings = class(TStringList)
  private
    Calendar: TJvCustomMonthCalendar;
  protected
    function GetDateIndex(Year, Month: word): integer; virtual;
    function GetBoldDays(Y, M: word): string; virtual;
  public
    constructor Create; 
    procedure AddStrings(Strings: TStrings); override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function Add(const S: string): Integer; override;
    function IsBold(Year, Month, Day: Word): boolean;
    procedure SetBold(Year, Month, Day: Word; Value: boolean);
    function AddDays(Year, Month: Word; const Days: string): integer; virtual;
  end;

  { TMonthCalStrings }

constructor TMonthCalStrings.Create;
begin
  inherited;
  Sorted := True;
  Duplicates := dupIgnore;
end;

  { Days is a comma separated list of days to set as bold. If Days is empty, the
  line is removed (if found) }

function TMonthCalStrings.AddDays(Year, Month: Word; const Days: string): integer;
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

function TMonthCalStrings.Add(const S: string): Integer;
begin
  if AnsiPos('=', S) <> 7 then
    raise EMonthCalError.CreateFmt(sInvalidDateStr, [S]);

  Result := IndexOfName(Copy(S, 1, 6));
  if Result > -1 then
  begin
    Sorted := false;
    Strings[Result] := S;
    Sorted := true;
  end
  else
       Result := inherited Add(S);
  if (Calendar <> nil) and (Calendar.HandleAllocated) then
    Calendar.DoBoldDays;
end;

function TMonthCalStrings.IsBold(Year, Month, Day: Word): boolean;
var aDayState: TMonthDayState;
begin
  aDayState := StringToDayStates(GetBoldDays(Year, Month) + ',' + GetBoldDays(0, Month));
  Result := (aDayState and (1 shl (Day - 1))) <> 0;
end;

procedure TMonthCalStrings.SetBold(Year, Month, Day: Word; Value: boolean);
var S: string; aDayState: TMonthDayState;
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
    aDayState := StringToDayStates(S);
    aDayState := aDayState and not (1 shl (Day - 1));
    AddDays(Year, Month, DayStatesToString(aDayState));
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
  Result := Add(S);
end;

function TMonthCalStrings.GetDateIndex(Year, Month: word): integer;
var S: string;
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

function TMonthCalStrings.GetBoldDays(Y, M: word): string;
var S: string;
begin
  if Y = 0 then
    S := Format('0000%.2d', [M])
  else
    S := Format('%.4d%.2d', [Y, M]);
  Result := Values[S];
end;


{ TJvCustomMonthCalendar }

constructor TJvCustomMonthCalendar.Create(AOwner: TComponent);
begin
  CreateWithAppearance(AOwner, TJvMonthCalAppearance.Create, True);
end;

constructor TJvCustomMonthCalendar.CreateWithAppearance(AOwner: TComponent;
  const AAppearance: TJvMonthCalAppearance; const AOwnsAppearance: Boolean);
begin
  Assert(Assigned(AAppearance));
  CheckCommonControl(ICC_DATE_CLASSES);
  inherited Create(AOwner);
  FAppearance := AAppearance;
  FOwnsAppearance := AOwnsAppearance;

  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse, csClickEvents, csDoubleClicks, csReflector];

  FAppearance.Calendar := Self;

  FMultiSelect := false;
  FMaxSelCount := 7;
  FMinDate := 0.0;
  FMaxDate := 0.0;
  FFirstSelDate := Date;
  FLastSelDate := 0.0;
  FMonthDelta := 1;
  FToday := Now;
  FBorderStyle := bsNone;
  FEntering:= False;
  FLeaving:= False;
  inherited Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := MinSize.Right;
  Height := MinSize.Bottom;
end;

destructor TJvCustomMonthCalendar.Destroy;
begin
  if(FOwnsAppearance) then
    FreeAndNil(FAppearance);
  inherited Destroy;
end;

procedure TJvCustomMonthCalendar.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  MultiSelects: array[boolean] of DWORD = (0, MCS_MULTISELECT);
  NoTodays: array[boolean] of DWORD = (MCS_NOTODAY, 0);
  NoCircles: array[boolean] of DWORD = (MCS_NOTODAYCIRCLE, 0);
  Weeks: array[boolean] of DWORD = (0, MCS_WEEKNUMBERS);
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

function TJvCustomMonthCalendar.IsBold(Year, Month, Day: Word): boolean;
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

procedure TJvCustomMonthCalendar.SetBold(Year, Month, Day: Word; Value: boolean);
begin
  TMonthCalStrings(FAppearance.BoldDays).SetBold(Year, Month, Day, Value);
end;

{ gets the first visible calendar month }

function TJvCustomMonthCalendar.FirstVisibleDate(Partial: boolean): TDateTime;
var
  rgst: array[0..1] of TSystemTime;
  Flag: integer;
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

function TJvCustomMonthCalendar.LastVisibleDate(Partial: boolean): TDateTime;
const
  isPartial: array[boolean] of integer = (GMR_VISIBLE, GMR_DAYSTATE);
var
  rgst: array[0..1] of TSystemTime;
  Flag: integer;
begin
  Result := 0;
  Flag := isPartial[Partial];
  if SendMessage(Handle, MCM_GETMONTHRANGE, Flag, Longint(@rgst)) <> 0 then
    with rgst[1] do
      Result := Trunc(EncodeDate(wYear, wMonth, wDay));
end;

{ protected }

procedure TJvCustomMonthCalendar.Change;
var rgst: array[0..1] of TSystemTime;Y,M,D:word;
begin
  if not HandleAllocated then Exit;
  MonthCal_SetFirstDayOfWeek(Handle, Ord(FAppearance.FirstDayOfWeek) - 1);
  MonthCal_SetMaxSelCount(Handle, FMaxSelCount);

  MonthCal_SetMonthDelta(Handle, FMonthDelta);
  SetSelectedDays(FFirstSelDate, FLastSelDate);
  if (FMinDate <> 0) and (FMaxDate <> 0) then
  begin
    DecodeDate(FMinDate,Y,M,D);
    with rgst[0] do
    begin
      wYear := Y;
      wMonth := M;
      wDay := D;
    end;
    DecodeDate(FMaxDate,Y,M,D);
    with rgst[1] do
    begin
      wYear := Y;
      wMonth := M;
      wDay := D;
    end;
    MonthCal_SetRange(Handle, GDTR_MIN or GDTR_MAX, @rgst);
  end
  else
    MonthCal_SetRange(Handle, 0, nil);
  DecodeDate(FTOday,Y,M,D);
  with rgst[0] do
  begin
    wYear := Y;
    wMonth := M;
    wDay := D;
  end;
  MonthCal_SetToday(Handle, rgst[0]);
end;

procedure TJvCustomMonthCalendar.DoBoldDays;
var Y, M, D: word; aDayArray: array[0..11] of TMonthDayState; aNMDAyState: TNmDayState;
begin
  if not HandleAllocated then Exit;
  DecodeDate(FirstVisibleDate(true), Y, M, D);
  FillChar(aDayArray, sizeof(TMonthDayState) * 12, 0);
  with aNMDayState do
  begin
    stStart.wYear := Y;
    stStart.wMonth := M;
    stStart.wDay := D;
    cDayState := VisibleMonths;
    prgDayState := @aDayArray;
  end;
  for D := 0 to VisibleMonths - 1 do
  begin
    CheckDayState(Y, M, aDayArray[D]);
    Inc(M);
    if M > 12 then
    begin
      M := 1;
      Inc(Y);
    end;
  end;
  SendMessage(Handle, MCM_SETDAYSTATE, VisibleMonths, longint(@aDayArray));
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

procedure TJvCustomMonthCalendar.CheckDayState(Year, Month: word; var DayState: TMonthDayState);
begin
  DayState := StringToDayStates(TMonthCalStrings(FAppearance.BoldDays).GetBoldDays(Year, Month));
end;

procedure TJvCustomMonthCalendar.DoGetDayState(var DayState: TNMDayState);
var
  aDate: TDateTime;
  i: integer;
  aStateArray: array[0..11] of TMonthDayState;
  Y, M: word;
begin
  FillChar(aStateArray, sizeof(aStateArray), #0);
  with DayState.stStart do
  begin
    Y := wYear;
    M := wMonth;
  end;
  with DayState do
    for i := 0 to cDayState - 1 do
    begin
      CheckDayState(Y, M, aStateArray[i]);
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
      FOnGetState(self, aDate, cDayState, aStateArray);
  DayState.prgDayState := @aStateArray;
end;

procedure TJvCustomMonthCalendar.CreateWnd;
begin
  inherited CreateWnd;
  FAppearance.Colors.SetAllColors;
  Change;
end;

procedure TJvCustomMonthCalendar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  InvalidateRect(Handle, nil, True);
end;

procedure TJvCustomMonthCalendar.CMFontChanged(var Message: TMessage);
begin
  inherited;
//  if HandleAllocated then
//    Perform(WM_SIZE,0,0);
  InvalidateRect(Handle, nil, True);
end;

procedure TJvCustomMonthCalendar.SetMultiSelect(Value: boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomMonthCalendar.SetShowToday(Value: boolean);
begin
  FAppearance.ShowToday := Value;
end;

procedure TJvCustomMonthCalendar.SetCircleToday(Value: boolean);
begin
  FAppearance.CircleToday := Value;
end;

procedure TJvCustomMonthCalendar.SetWeekNumbers(Value: boolean);
begin
  FAppearance.WeekNumbers := Value;
end;

procedure TJvCustomMonthCalendar.SetFirstDayOfWeek(Value: TJvMonthCalWeekDay);
begin
  FAppearance.FirstDayOfWeek := Value;
end;

procedure TJvCustomMonthCalendar.SetMaxSelCount(Value: word);
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
var rgst: array[0..1] of TSystemTime;
begin
  Result := FFirstSelDate;
  if not HandleAllocated then Exit;
  if FMultiSelect then
    MonthCal_GetSelRange(Handle, @rgst)
  else
    MonthCal_GetCurSel(Handle, rgst[0]);
  with rgst[0] do
    FFirstSelDate := EncodeDate(wYear, wMonth, wDay);
end;

procedure TJvCustomMonthCalendar.SetLastSelDate(Value: TDateTime);
begin
  if (FLastSelDate <> Value) then
  begin
    FLastSelDate := Value;
    SetSelectedDays(FLastSelDate, FFirstSelDate);
  end;
end;

function TJvCustomMonthCalendar.GetLastSelDate: TDateTime;
var rgst: array[0..1] of TSystemTime;
begin
  Result := FLastSelDate;
  if not HandleAllocated then Exit;
  if not FMultiSelect then
  begin
    Result := FLastSelDate;
    Exit;
  end;
  if MonthCal_GetSelRange(Handle, @rgst) then
    with rgst[1] do
      FLastSelDate := Trunc(EncodeDate(wYear, wMonth, wDay));
end;

procedure TJvCustomMonthCalendar.SetSelectedDays(dFrom, dTo: TDateTime);
var rgst: array[0..1] of TSystemTime;
begin
  if not HandleAllocated then Exit;
  if FMultiSelect then
  begin
    if (dFrom <> 0) and (dTo <> 0) then
    begin
      with rgst[0] do
        DecodeDate(dFrom, wYear, wMonth, wDay);
      with rgst[1] do
        DecodeDate(dTo, wYear, wMonth, wDay);
      MonthCal_SetSelRange(Handle, @rgst);
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

procedure TJvCustomMonthCalendar.SetMonthDelta(Value: integer);
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

function TJvCustomMonthCalendar.GetTodayWidth: integer;
begin
  Result := SendMessage(Handle, MCM_GETMAXTODAYWIDTH, 0, 0);
end;

function TJvCustomMonthCalendar.VisibleMonths: integer;
begin
  Result := 1;
  if not HandleAllocated then Exit;
  Result := MonthCal_GetMonthRange(Handle, GMR_DAYSTATE, nil);
end;

procedure TJvCustomMonthCalendar.SetDayStates(MonthCount: integer; DayStates: array of TMonthDayState);
var Index: integer;
begin
  if not HandleAllocated then Exit;
  Index := High(DayStates) - Low(DayStates);
  if (Index < MonthCount) or (Index < VisibleMonths) then
    raise EMonthCalError.Create('Invalid argument to SetDayStates');
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
    OffSetRect(Result,-Result.Left,-Result.Top);
  end
  else
    Result := Rect(0,0,191,154);
end;

procedure TJvCustomMonthCalendar.CNNotify(var Message: TWMNotify);
var dFrom, dTo: TDateTime;
begin
  with Message.NMHdr^ do
    case Code of
      MCN_GETDAYSTATE:
        DoGetDayState(PNMDayState(Message.NMHDR)^);
      MCN_SELCHANGE:
        begin
          if IsBlankDate(PNMSelChange(Message.NMHdr)^.stSelStart) then
            Exit;
          with PNMSelChange(Message.NMHdr)^.stSelStart do
            dFrom := Trunc(EncodeDate(wYear, wMonth, wDay));
          if IsBlankDate(PNMSelChange(Message.NMHdr)^.stSelEnd) then
            dTo := dFrom
          else
            with PNMSelChange(Message.NMHdr)^.stSelEnd do
              dTo := Trunc(EncodeDate(wYear, wMonth, wDay));
          DoDateSelChange(dFrom, dTo);
        end;
      MCN_SELECT:
        begin
          if IsBlankDate(PNMSelChange(Message.NMHdr)^.stSelStart) then
            Exit;
          with PNMSelChange(Message.NMHdr)^.stSelStart do
            dFrom := Trunc(EncodeDate(wYear, wMonth, wDay));
          if IsBlankDate(PNMSelChange(Message.NMHdr)^.stSelEnd) then
            dTo := dFrom
          else
            with PNMSelChange(Message.NMHdr)^.stSelEnd do
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
    if MinHeight < CtlMinHeight then MinHeight := CtlMinHeight;
    if MinWidth < CtlMinWidth then MinWidth := CtlMinWidth;
  end;
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

function TJvCustomMonthCalendar.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
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
      NewHeight := Bottom - Top + Ord(BorderStyle = bsSingle)* 2;
    end;
  end
  else Result := False;
end;

procedure TJvCustomMonthCalendar.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TJvCustomMonthCalendar.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SetFocus;
  inherited;
end;

function TJvCustomMonthCalendar.GetBoldDays: TStrings;
begin
  result := FAppearance.BoldDays;
end;

function TJvCustomMonthCalendar.GetCircleToday: Boolean;
begin
  result := FAppearance.CircleToday;
end;

function TJvCustomMonthCalendar.GetColors: TJvMonthCalColors;
begin
  result := FAppearance.Colors;
end;

function TJvCustomMonthCalendar.GetShowToday: Boolean;
begin
  result := FAppearance.ShowToday;
end;

function TJvCustomMonthCalendar.GetWeekNumbers: Boolean;
begin
  result := FAppearance.WeekNumbers;
end;

function TJvCustomMonthCalendar.GetFirstDayOfWeek: TJvMonthCalWeekDay;
begin
  result := FAppearance.FirstDayOfWeek;
end;

procedure TJvCustomMonthCalendar.DoKillFocus(
  const ANextControl: TWinControl);
begin
  if Assigned(OnKillFocus) then
    OnKillFocus(Self, ANextControl);
end;

procedure TJvCustomMonthCalendar.DoSetFocus(
  const APreviousControl: TWinControl);
begin
  if Assigned(OnSetFocus) then
    OnSetFocus(Self, APreviousControl);
end;

procedure TJvCustomMonthCalendar.WMKillFocus(var AMessage: TMessage);
begin
  FLeaving:= True;
  try
    inherited;
    DoKillFocus(FindControl(AMessage.WParam));
  finally
    FLeaving:= False;
  end;
end;

procedure TJvCustomMonthCalendar.WMSetFocus(var AMessage: TMessage);
begin
  FEntering:= True;
  try
    inherited;
    DoSetFocus(FindControl(AMessage.WParam));
  finally
    FEntering:= False;
  end;

end;

{ TJvMonthCalAppearance }

constructor TJvMonthCalAppearance.Create;
begin
  inherited;
  FCircleToday := True;
  FColors := TJvMonthCalColors.Create(NIL);
  FBoldDays := TMonthCalStrings.Create;
  FShowToday := True;
  FWeekNumbers := False;
  FFirstDoW := mcLocale;
end;

destructor TJvMonthCalAppearance.Destroy;
begin
  FreeAndNil(FColors);
  FreeAndNil(FBoldDays);
  inherited;
end;

function TJvMonthCalAppearance.GetCalendar: TJvCustomMonthCalendar;
begin
  result := FColors.Calendar;
end;

procedure TJvMonthCalAppearance.SetBoldDays(const AValue: TStrings);
begin
  FBoldDays.Assign(AValue);
  if(Assigned(Calendar)) then
    Calendar.DoBoldDays;
end;

procedure TJvMonthCalAppearance.SetCalendar(
  const AValue: TJvCustomMonthCalendar);
begin
  FColors.Calendar := AValue;
  TMonthCalStrings(FBoldDays).Calendar := AValue;
end;

procedure TJvMonthCalAppearance.SetCircleToday(const AValue: Boolean);
begin
  if(FCircleToday <> AValue) then
  begin
    FCircleToday := AValue;
    if(Assigned(Calendar)) then
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
  if(FFirstDoW <> AValue) then
  begin
    FFirstDoW := AValue;
    if(Assigned(Calendar)) then
      Calendar.Change;
  end;
end;

procedure TJvMonthCalAppearance.SetShowToday(const AValue: Boolean);
begin
  if(FShowToday <> AValue) then
  begin
    FShowToday := AValue;
    if(Assigned(Calendar)) then
      Calendar.RecreateWnd;
  end;
end;

procedure TJvMonthCalAppearance.SetWeekNumbers(const AValue: Boolean);
begin
  if(FWeekNumbers <> AValue) then
  begin
    FWeekNumbers := AValue;
    if(Assigned(Calendar)) then
      Calendar.RecreateWnd;
  end;
end;

end.
