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

Contributor(s):            

Last Modified: 2002-06-05

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ @abstract(A wrapper component for the MS MonthCal control available in
    ComCtl32.dll versions 4.70 and above.) }

unit JvCalendar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, CommCtrl,
  ComCtrls, JvShlWApi, JvComponent;

type
  EMonthCalError = class(Exception);
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
    procedure SetAllColors;
  public
    constructor Create(AOwner: TJvCustomMonthCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read FMonthBackColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read FTrailingTextColor
      write SetColor default clInactiveCaptionText;
  end;

  TJvCustomMonthCalendar = class(TJvWinControl)
  private
    { Private declarations }
    FMultiSelect: boolean;
    FShowToday: boolean;
    FCircleToday: boolean;
    FWeekNumbers: boolean;
    FFirstDayOfWeek: TJvMonthCalWeekDay;
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
    FBoldDays: TStrings;
    FColors: TJvMonthCalColors;
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
    
    function GetDays(Year, Month: Word): string;
    procedure SetDays(Year, Month: Word; Value: string);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
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
    property MinSize: TRect read GetMinSize;
    property Bold[Year, Month, Day: word]: boolean read IsBold write SetBold;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BoldDays: TStrings read FBoldDays write SetBoldDays;
    property CircleToday: boolean read FCircleToday write SetCircleToday default true;
    property Colors: TJvMonthCalColors read FColors write SetColors;
    property DateFirst: TDateTime read GetFirstSelDate write SetFirstSelDate;
    property DateLast: TDateTime read GetLastSelDate write SetLastSelDate;
    property DateMax: TDateTime read FMaxDate write SetMaxDate;
    property DateMin: TDateTime read FMinDate write SetMinDate;
    property Days[Year, Month: Word]: string read GetDays write SetDays;
    property FirstDayOfWeek: TJvMonthCalWeekDay read FFirstDayOfWeek write SetFirstDayOfWeek default mcLocale;
    property MaxSelCount: word read FMaxSelCount write SetMaxSelCount default 7;
    property MonthDelta: integer read FMonthDelta write SetMonthDelta default 1;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property ShowToday: boolean read FShowToday write SetShowTOday default true;
    property TodayWidth: integer read GetTodayWidth;
    property WeekNumbers: boolean read FWeekNumbers write SetWeekNumbers default false;
    property Today: TDateTime read FToday write SetToday;
    property OnSelect: TJvMonthCalSelEvent read FOnSelect write FOnSelect;
    property OnSelChange: TJvMonthCalSelEvent read FOnSelChange write FOnSelChange;
    property OnGetDayState: TJvMonthCalStateEvent read FOnGetState write FOnGetState;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FirstVisibleDate(Partial: boolean): TDateTime;
    function LastVisibleDate(Partial: boolean): TDateTime;
    function VisibleMonths: integer;
    procedure SetDayStates(MonthCount: integer; DayStates: array of TMonthDayState);
  end;

  TJvMonthCalendar2 = class(TJvCustomMonthCalendar)
  public
    property MinSize;
    property Bold;
    property Days;
  published
    { inherited properties }
    property Align;
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
    property OnSelect;
    property OnSelChange;
    property OnGetDayState;
  end;

function StringToDayStates(const S: string; Bold: boolean): TMonthDayState;
function DayStatesToString(Days: TMonthDayState): string;
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: integer): boolean;

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

function StringToDayStates(const S: string; Bold: boolean): TMonthDayState;
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
    else (*   If GetProcAddress failed,
      the DLL is a version previous
      to the one  shipped with IE 3.x. *)
    begin
      pdwMajor := 4;
      pdwMinor := 0;
    end;
    FreeLibrary(hDLL);
    Exit;
  end;
  Result := false;
end;


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
  if (Calendar = nil) or not Calendar.HandleAllocated then Exit;
  MonthCal_SetColor(Calendar.Handle, ColorIndex[Index], ColorToRGB(Value));
  case Index of
    0:
      begin
        FBackColor := Value;
        Calendar.Color := FBackColor;
      end;
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FMonthBackColor := Value;
    5: FTrailingTextColor := Value;
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
    procedure AddStrings(Strings: TStrings); override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function Add(const S: string): Integer; override;
    function IsBold(Year, Month, Day: Word): boolean;
    procedure SetBold(Year, Month, Day: Word; Value: boolean);
    function AddDays(Year, Month: Word; const Days: string): integer; virtual;
  end;

  { TMonthCalStrings }

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
  aDayState := StringToDayStates(GetBoldDays(Year, Month) + ',' + GetBoldDays(0, Month), true);
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
    aDayState := StringToDayStates(S, true);
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
  CheckCommonControl(ICC_DATE_CLASSES);
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csDoubleClicks, csReflector];
  FColors := TJvMonthCalColors.Create(self);

  FBoldDays := TMonthCalStrings.Create;
  TMonthCalStrings(FBoldDays).Calendar := self;
  TMonthCalStrings(FBoldDays).Sorted := true;
  TMonthCalStrings(FBoldDays).Duplicates := dupIgnore;

  FMultiSelect := false;
  FShowToday := true;
  FCircleToday := true;
  FWeekNumbers := false;
  FFirstDayOfWeek := mcLocale;
  FMaxSelCount := 7;
  FMinDate := 0.0;
  FMaxDate := 0.0;
  FFirstSelDate := Date;
  FLastSelDate := 0.0;
  FMonthDelta := 1;
  FToday := Now;
  FBorderStyle := bsNone;
  inherited Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := MinSize.Right;
  Height := MinSize.Bottom;
end;

destructor TJvCustomMonthCalendar.Destroy;
begin
  FBoldDays.Free;
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
      Style := Style or BorderStyles[FBorderStyle] or MultiSelects[FMultiSelect] or NoTodays[FShowToday]
        or NoCircles[FCircleToday] or Weeks[FWeekNumbers] or MCS_DAYSTATE
    else
      // IE3 doesn't implement the NoTodayCircle style, instead it uses
      // the same constant for MCS_NOTODAY as IE4 does for MCS_NOTODAYCIRCLE ...
      Style := Style or BorderStyles[FBorderStyle] or MultiSelects[FMultiSelect] or NoCircles[FShowToday]
        or Weeks[FWeekNumbers] or MCS_DAYSTATE;
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
  FColors.Assign(Value);
end;

procedure TJvCustomMonthCalendar.SetBoldDays(Value: TStrings);
begin
  FBoldDays.Assign(Value);
  DoBoldDays;
end;

function TJvCustomMonthCalendar.IsBold(Year, Month, Day: Word): boolean;
begin
  Result := TMonthCalStrings(FBoldDays).IsBold(Year, Month, Day);
end;

function TJvCustomMonthCalendar.GetDays(Year, Month: Word): string;
begin
  Result := TMonthCalStrings(FBoldDays).GetBoldDays(Year, Month);
end;

procedure TJvCustomMonthCalendar.SetDays(Year, Month: Word; Value: string);
begin
  TMonthCalStrings(FBoldDays).AddDays(Year, Month, Value);
end;

procedure TJvCustomMonthCalendar.SetBold(Year, Month, Day: Word; Value: boolean);
begin
  TMonthCalStrings(FBoldDays).SetBold(Year, Month, Day, Value);
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
  MonthCal_SetFirstDayOfWeek(Handle, Ord(FFirstDayOfWeek) - 1);
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
  DayState := StringToDayStates(TMonthCalStrings(FBoldDays).GetBoldDays(Year, Month), true);
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
  FColors.SetAllColors;
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
  if FShowToday <> Value then
  begin
    FShowToday := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomMonthCalendar.SetCircleToday(Value: boolean);
begin
  if FCircleToday <> value then
  begin
    FCircleToday := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomMonthCalendar.SetWeekNumbers(Value: boolean);
begin
  if FWeekNumbers <> Value then
  begin
    FWeekNumbers := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomMonthCalendar.SetFirstDayOfWeek(Value: TJvMonthCalWeekDay);
begin
  if FFirstDayOfWeek <> Value then
  begin
    FFirstDayOfWeek := Value;
    Change;
  end;
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
    SendMessage(Handle, MCM_GETMINREQRECT, 0, Longint(@Result))
  else
    Result := Rect(0,0,190,160);
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

end.

