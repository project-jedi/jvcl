{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDatePickerEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen@lucatec.de]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): Peter Thörnqvist.

Last Modified: 2002-11-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JVCL.INC}

{ A replacement for TDateTimePicker which is better suited for keyboard-input by
 ultimately descending from TCustomMaskEdit.

 Other notable features (especially in comparison to the native DATETIMEPICKER):

 - The control is able to construct a suitable EditMask from a valid date format
  string such as the global ShortDateFormat (the default) which should make it
  adapt well to regional settings / individual requirements.

 - It is possible to specify a NoDateText which will be displayed when no date
  is selected. The original datetimepicker would display 1899-12-31 in such
  cases. This feature could be further controlled by the AllowNoDate and
  NoDateShortcut properties.


 Known issues / not (yet) implemented features:

 -the control does (currently) not allow for time entry
  - it really is a control for date entry only.

 -the Min/MaxYear contstraints are currently commented out as they are not
   functional in the current state. they would still require some work to make up
   for two-digit year entries.

 -currently there is quite an ugly hack in place to solve an issue with
   un/focusing of the calendar control. This would look a whole lot better if it
   was implemented at the TJvCustomMonthCalendar level.

 -there is currently no way to manipulate the appearance of the dropdown
   calendar. this is due to the fact that I only instantiate the dropdown on
   demand. To control the calendar's appearance I would have to store the colors
   and possible other settings in some kind of proxy structure which would get
   applied to the control upon instantiation. As one of the primary goals was to
   maintain the system look and feel, customization of the calendar was simply
   not a requirement.
}

unit JvDatePickerEdit;

interface

uses
  Classes,
  Controls,
  Graphics,
  JvTypes,  
  JvCalendar,
  JvDropDownForm,
  JvCheckedMaskEdit,
  Messages,
  ComCtrls,
  Buttons;

type
  {Types used to handle and convert between date format strings and EditMasks:}
  TJvDateFigure = (dfNone, dfYear, dfMonth, dfDay);
  TJvDateFigureInfo = record
    Figure: TJvDateFigure;
    Start: Byte;
    Length: Byte;
    Index: Byte;
  end;
  TJvDateFigures = array[0..2] of TJvDateFigureInfo;

  {A dropdown form with an embedded calendar control.}
  TJvDropCalendar = class(TJvCustomDropDownForm)
  private
    FCal: TJvCustomMonthCalendar;

    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FOnCancel: TNotifyEvent;

    procedure CalSelChange(Sender: TObject; StartDate, EndDate: TDateTime);
    procedure CalSelect(Sender: TObject; StartDate, EndDate: TDateTime);
    procedure CalKeyPress(Sender: TObject; var Key: Char);
    procedure CalLoseFocus(const ASender: TObject;
      const AFocusControl: TWinControl);
  protected
    procedure DoCancel;
    procedure DoChange;
    procedure DoSelect;
    procedure DoShow; override;

    function GetSelDate: TDateTime;
    procedure SetSelDate(const AValue: TDateTime);

  public
    constructor CreateWithAppearance(AOwner: TComponent;
      const AAppearance: TJvMonthCalAppearance);
    destructor Destroy; override;

    procedure SetFocus; override;

    property SelDate: TDateTime read GetSelDate write SetSelDate;

    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TJvCustomDatePickerEdit = class(TJvCustomCheckedMaskEdit)
  private
    FAllowNoDate: Boolean;
    FBut: TSpeedButton;
    FCalAppearance: TJvMonthCalAppearance;
    FDate: TDateTime;
    FDateError: Boolean;
    FDateFigures: TJvDateFigures;
    FDateFormat: String;
    FDropFo: TJvDropCalendar;
    FEnableValidation: Boolean;
    FMask: String;
    FNoDateShortcut: TShortcut;
    FNoDateText: String;

//    FMinYear: Word;
//    FMaxYear: Word;

    procedure ButClick(Sender: TObject);
    procedure CalChange(Sender: TObject);
    procedure CalDestroy(Sender: TObject);
    procedure CalSelect(Sender: TObject);

    function AttemptTextToDate(const AText: String; var ADate: TDateTime;
      const AForce: Boolean = False; const ARaise: Boolean = False): Boolean;
    function DateFormatToEditMask(const ADateFormat: String): String;
    function DateToText(const ADate: TDateTime): String;
    procedure ParseFigures(var AFigures: TJvDateFigures; AFormat: String);
    procedure RaiseNoDate;

    procedure SetAllowNoDate(const AValue: Boolean);
    procedure SetCalAppearance(const AValue: TJvMonthCalAppearance);
    function GetDate: TDateTime;
    procedure SetDate(const AValue: TDateTime);
    procedure SetDateFormat(const AValue: String);
    function GetDropped: Boolean;
    procedure SetNoDateText(const AValue: String);

  protected
    FStoreDate: Boolean;
    function IsDateFormatStored: Boolean;
    function IsNoDateShortcutStored: Boolean;
    function IsNoDateTextStored: Boolean;
  protected
    procedure Change; override;
    procedure Loaded; override;
    procedure LoseFocus(const AFocusControl: TWinControl); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;

    procedure GetInternalMargins(var ALeft, ARight: Integer); override;

    procedure DoCtl3DChanged; override;
    procedure DoEnabledChanged; override;

    function GetChecked: Boolean; override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetShowCheckbox(const AValue: Boolean); override;

    function GetEnableValidation: Boolean; virtual;

    procedure UpdateDisplay; virtual;

    function ActiveFigure: TJvDateFigureInfo;

    procedure CloseUp;
    procedure DropDown;

    procedure ClearMask;
    procedure RestoreMask;

    property AllowNoDate: Boolean read FAllowNoDate write SetAllowNoDate;
    property CalendarAppearance: TJvMonthCalAppearance read FCalAppearance write SetCalAppearance;
    property Date: TDateTime read GetDate write SetDate;
    property DateFormat: String read FDateFormat write SetDateFormat;
    property Dropped: Boolean read GetDropped;
    property EnableValidation: Boolean read GetEnableValidation write FEnableValidation;
//    property MaxYear: Word read FMaxYear write FMaxYear;
//    property MinYear: Word read FMinYear write FMinYear;
    property NoDateShortcut: TShortcut read FNoDateShortcut write FNoDateShortcut;
    property NoDateText: String read FNoDateText write SetNoDateText;
    property StoreDate: Boolean read FStoreDate write FStoreDate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsEmpty: Boolean; virtual;
  end;

  TJvDatePickerEdit = class(TJvCustomDatePickerEdit)
  public
    property Dropped;
  published
    property AllowNoDate default True;
    property Anchors;
    property AutoSelect;
    property AutoSize default False;
    property BorderStyle;
    property CalendarAppearance;
    property CharCase;
    property Checked;
    property Color;
    property Constraints;
    property Cursor;
    property Ctl3D;
    property Date stored FStoreDate;
    property DateFormat stored IsDateFormatStored;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableValidation default True;
    property Font;
    property HintColor default clInfoBk;
    property HotTrack default False;
//    property MaxYear default 2900;
//    property MinYear default 1900;
    property NoDateShortcut stored IsNoDateShortcutStored;
    property NoDateText stored IsNoDateTextStored;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckbox default False;
    property StoreDate default False;
    property TabOrder;
    property Visible;

    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnCtl3DChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnabledChanged;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetFocus;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoseFocus;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChanged;
    property OnStartDrag;
  end;

implementation

uses
  Menus, //Shortcut
  Windows,
  SysUtils,
  JclStrings,
  JclGraphUtils;

const
  DefaultNoDateShortcut = 'Alt+Del';


{ TLucaCustomDatePickerEdit }

procedure TJvCustomDatePickerEdit.ButClick(Sender: TObject);
begin
  if(Dropped) then
    CloseUp
  else
    DropDown;
end;

procedure TJvCustomDatePickerEdit.CalChange(Sender: TObject);
begin
  Text := DateToText(FDropFo.SelDate);
end;

procedure TJvCustomDatePickerEdit.CalSelect(Sender: TObject);
begin
  Self.Date := FDropFo.SelDate;
end;

procedure TJvCustomDatePickerEdit.CloseUp;
begin
  if(Dropped) then
  begin
    Date := FDropFo.SelDate;
    FreeAndNil(FDropFo);
  end;
  if not((Leaving) or(csDestroying in ComponentState)) then
    SetFocus;
end;

constructor TJvCustomDatePickerEdit.Create(AOwner: TComponent);
begin
  inherited;

  FAllowNoDate := True;
  FDate := SysUtils.Date;
  FDateError := False;
  FDropFo := NIL;
  FEnableValidation := True;
//  FMaxYear := 2900;
//  FMinYear := 1800;
  FNoDateShortcut := TextToShortCut(DefaultNoDateShortcut);
  FNoDateText := EmptyStr;
  FStoreDate := False;

  FBut := TSpeedButton.Create(Self);
  with(FBut) do
  begin
    Parent := Self;
    Align := alRight;
    Cursor := crArrow;
    Flat := True;
    Width := GetSystemMetrics(SM_CXVSCROLL) + 1;
    Glyph.Handle := LoadBitmap(0,PChar(OBM_COMBO)); // 	 PChar(32738));
    OnClick := ButClick;
    Visible := True;
  end;

  FCalAppearance := TJvMonthCalAppearance.Create;

  SetDateFormat(ShortDateFormat);
  EditMask := FMask;
end;

destructor TJvCustomDatePickerEdit.Destroy;
begin
  CloseUp;
  FBut.OnClick := NIL;
  FreeAndNil(FCalAppearance);
  inherited;
end;

procedure TJvCustomDatePickerEdit.SetCalAppearance(
  const AValue: TJvMonthCalAppearance);
begin
  FCalAppearance.Assign(AValue);
end;

procedure TJvCustomDatePickerEdit.DropDown;
begin
  if(not Dropped) then
  begin
    if(IsEmpty) then
      Self.Date := SysUtils.Date;

    FDropFo := TJvDropCalendar.CreateWithAppearance(Self, FCalAppearance);
    with(FDropFo) do
    begin
      SelDate := Self.Date;
      OnChange := Self.CalChange;
      OnSelect := Self.CalSelect;
      OnDestroy := Self.CalDestroy;
      Show;
      SetFocus;
    end;
  end;
end;

function TJvCustomDatePickerEdit.DateToText(const ADate: TDateTime): String;
begin
  result := FormatDateTime(DateFormat, ADate);
end;

function TJvCustomDatePickerEdit.GetDate: TDateTime;
begin
  result := FDate;
end;

function TJvCustomDatePickerEdit.GetDropped: Boolean;
begin
  result := Assigned(FDropFo) and not(csDestroying in FDropFo.ComponentState);
end;

procedure TJvCustomDatePickerEdit.GetInternalMargins( var ALeft, ARight: Integer);
begin
  inherited;
  ARight := ARight + FBut.Width;
end;

function TJvCustomDatePickerEdit.IsEmpty: Boolean;
begin
  result := (FDate = 0);
end;

function TJvCustomDatePickerEdit.IsNoDateTextStored: Boolean;
begin
  result := (NoDateText <> EmptyStr);
end;

procedure TJvCustomDatePickerEdit.RaiseNoDate;
begin
  raise Exception.CreateFmt('%s must have a date!', [Name]);
end;

procedure TJvCustomDatePickerEdit.SetAllowNoDate(const AValue: Boolean);
begin
  if(AllowNoDate <> AValue) then
  begin
    FAllowNoDate := AValue;

    if(AValue and IsEmpty) then
      if(csDesigning in ComponentState) then
        Self.Date := SysUtils.Date
      else
        RaiseNoDate;

    if(not AValue) then
      ShowCheckbox := False;
  end;
end;

procedure TJvCustomDatePickerEdit.SetDate(const AValue: TDateTime);
begin
  if(not AllowNoDate) and(AValue = 0) then
    RaiseNoDate;

  FDate := AValue;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.SetNoDateText(const AValue: String);
begin
  FNoDateText := AValue;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.SetShowCheckbox(const AValue: Boolean);
begin
  inherited;
  if(AValue) then
    AllowNoDate := True;
  UpdateDisplay;
end;

function TJvCustomDatePickerEdit.AttemptTextToDate(const AText: String;
  var ADate: TDateTime; const AForce: Boolean; const ARaise: Boolean): Boolean;
var
  lFormatBup: String;
  lDate: TDateTime;
  lDummy: Integer;
begin
  result := Validate(AText, lDummy);
  {only attempt to convert, if at least the Mask is matched
    - otherwise we'd be swamped by exceptions during input}
  if(result or AForce) then     
  begin
    lDate := ADate;
    lFormatBup := ShortDateFormat;
    try
      ShortDateFormat := Self.DateFormat;
      try
        ADate := StrToDate( AText);
        result := True;
      except
        result := False;
        if(ARaise) then
          raise
        else
          ADate := lDate;
      end;
    finally
      ShortDateFormat := lFormatBup;
    end;
  end;
end;

procedure TJvCustomDatePickerEdit.UpdateDisplay;
begin
  if(InternalChanging) then Exit;

  inherited SetChecked(not IsEmpty);
  if(IsEmpty) then
  begin
    if not(csDesigning in ComponentState) then
    begin
      ClearMask;
      Text := NoDateText;
    end;
  end
  else
  begin
    RestoreMask;
    Text:= DateToText( Date)
  end;
end;

procedure TJvCustomDatePickerEdit.Change;
var
  lDate: TDateTime;
  lFigVal: Word;
  lActFig: TJvDateFigureInfo;

  procedure SetActiveFigVal(const AValue: Word);
  begin
    BeginInternalChange;
    try
      SelStart := lActFig.Start-1;
      SelLength := lActFig.Length;
      SelText := Format('%.*d', [lActFig.Length, AValue]);
    finally
      EndInternalChange;
    end;
  end;

  procedure EnforceRange(const AMin, AMax: Word);
  begin
    if(lFigVal > AMax) then
      SetActiveFigVal(AMax)
    else if(lFigVal < AMin) then
      SetActiveFigVal(AMin);
  end;

begin
  if(InternalChanging) then
    Exit;

  inherited;

  FDateError := False;

  if([csDesigning, csDestroying] * ComponentState <> []) then
    Exit;

  if(Text <> NoDateText) then
  begin
    lDate := Self.Date;
    if(AttemptTextToDate(Text, lDate)) then
    begin
      BeginInternalChange;
      try
        Self.Date := lDate
      finally
        EndInternalChange;
      end;
    end
    else
      if(EnableValidation) then
      begin
        lActFig:= ActiveFigure;

        if(lActFig.Figure <> dfNone) then
        begin
          lFigVal := StrToIntDef(Trim(Copy(Text, lActFig.Start, lActFig.Length)), 0);
          if(SelStart = lActFig.Start + lActFig.Length-1) then
            case lActFig.Figure of
              dfDay:
                EnforceRange(1, 31);

              dfMonth:
                EnforceRange(1, 12);

              dfYear:
                {EnforceRange( MinYear, MaxYear)}; //year-validation still under development
            end;
        end;
      end;
  end;
end;

function TJvCustomDatePickerEdit.IsNoDateShortcutStored: Boolean;
begin
  result := (NoDateShortcut <> TextToShortCut(DefaultNoDateShortcut));
end;

procedure TJvCustomDatePickerEdit.CalDestroy(Sender: TObject);
begin
  CloseUp;
  FDropFo := NIL;
end;

procedure TJvCustomDatePickerEdit.ClearMask;
begin
  if(EditMask <> EmptyStr) then
  begin
    FMask := EditMask;
    if not(csDesigning in ComponentState) then
      EditMask := EmptyStr;
  end;
end;

procedure TJvCustomDatePickerEdit.RestoreMask;
begin
  if(EditMask = EmptyStr) then
    EditMask := FMask;
end;

procedure TJvCustomDatePickerEdit.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  if(Text = NoDateText) then
  begin
    Text := EmptyStr;
    RestoreMask;
  end;

  if ( AllowNoDate)
  and( Shortcut( AKey, AShift) = NoDateShortcut) then
  begin
    Date := 0;
  end
  else
    case AKey of
      27 :
      begin    // ESC
        CloseUp;
        Reset;
      end;
      VK_DOWN:
        if AShift = [ssAlt] then
          DropDown;
    end;
  inherited;
end;

procedure TJvCustomDatePickerEdit.SetChecked(const AValue: Boolean);
begin
  if(Checked <> AValue) then
  begin
    if(AValue) then
    begin
      if(Self.Date = 0) then
        Self.Date := SysUtils.Date;
    end
    else
    begin
      Self.Date := 0;
    end;
    Change;
  end;
end;

function TJvCustomDatePickerEdit.GetChecked: Boolean;
begin
  result := not IsEmpty;
end;

function TJvCustomDatePickerEdit.DateFormatToEditMask(
  const ADateFormat: String): String;
begin
  result := ADateFormat;

  StrReplace(result, 'dddddd',      LongDateFormat,  []);
  StrReplace(result, 'ddddd',       ShortDateFormat, []);
  StrReplace(result, 'dddd',        '',              []); // unsupported: DoW as full name
  StrReplace(result, 'ddd',         '',              []); // unsupported: DoW as abbrev
  StrReplace(result, 'dd',          '00',            []);
  StrReplace(result, 'd',           '90',            []);
  StrReplace(result, 'mmmm',        '90',            [rfIgnoreCase]);
  StrReplace(result, 'mmm',         '00',            [rfIgnoreCase]);
  StrReplace(result, 'mm',          '00',            [rfIgnoreCase]);
  StrReplace(result, 'm',           '90',            [rfIgnoreCase]);
  StrReplace(result, 'yyyy',        '9900',          []);
  StrReplace(result, 'yy',          '00',            []);
  StrReplace(result, DateSeparator, '/',             [rfReplaceAll]);

  result := '!' + Trim(result) + ';1;_';
end;

function TJvCustomDatePickerEdit.IsDateFormatStored: Boolean;
begin
  result := (FDateFormat <> ShortDateFormat);
end;

procedure TJvCustomDatePickerEdit.SetDateFormat(const AValue: String);
begin
  FDateFormat := AValue;
  ParseFigures(FDateFigures, FDateFormat);
  FMask := DateFormatToEditMask(FDateFormat);
  Text := EmptyStr;
  EditMask := FMask;
  UpdateDisplay;
end;

function TJvCustomDatePickerEdit.ActiveFigure: TJvDateFigureInfo;
var
  i: Integer;
begin
  for i := 2 downto 0 do
    if(SelStart >= FDateFigures[i].Start) then
    begin
      result := FDateFigures[i];
      Exit;
    end;
  result.Figure := dfNone;
end;

procedure TJvCustomDatePickerEdit.ParseFigures(var AFigures: TJvDateFigures;
  AFormat: String);
var
  i: Integer;
begin
 {Determines the order and position of the individual figures in the format string.}
  AFigures[0].Start := 1;
  AFigures[1].Start := Pos(DateSeparator, AFormat) + 1;
  AFigures[2].Start := StrLastPos(DateSeparator, AFormat) +1;

  AFigures[0].Length := AFigures[1].Start - 2;
  AFigures[1].Length := AFigures[2].Start - AFigures[1].Start - 1;
  AFigures[2].Length := Length(AFormat) - AFigures[2].Start + 1;

  AFormat := UpperCase(AFormat);

  for i := 0 to 2 do
  begin
    case AFormat[AFigures[i].Start] of
      'D' : AFigures[i].Figure:= dfDay;
      'M' : AFigures[i].Figure:= dfMonth;
      'Y' : AFigures[i].Figure:= dfYear;
    end;
    AFigures[i].Index := i;
  end;
end;

procedure TJvCustomDatePickerEdit.LoseFocus(const AFocusControl: TWinControl);
var
  lDate: TDateTime;
begin
  if (AFocusControl <> NIL)
  and(AFocusControl <> FDropFo)
  and(AFocusControl.Owner <> FDropFo) then
    if(not FDateError) then
    begin
      CloseUp;
      inherited;
      if(EnableValidation) then
        try
          lDate := Self.Date;
          if (Text <> NoDateText)
          and(AttemptTextToDate(Text, lDate, True, True)) then
            Self.Date:= lDate;
        except
          on EConvertError do
            if not(csDestroying in ComponentState) then
            begin
              FDateError := True;
              SetFocus;
              raise;
            end
            else
              Self.Date := 0;
        end;
    end
    else
      inherited;
end;

procedure TJvCustomDatePickerEdit.Loaded;
begin
  inherited;
  UpdateDisplay;
end;

function TJvCustomDatePickerEdit.GetEnableValidation: Boolean;
begin
  result := FEnableValidation;
end;

procedure TJvCustomDatePickerEdit.DoCtl3DChanged;
begin
  inherited;
  FBut.Flat := not Self.Ctl3d;
end;

procedure TJvCustomDatePickerEdit.DoEnabledChanged;
begin
  inherited;
  if( not(Self.Enabled) and Dropped) then
    CloseUp;
  FBut.Enabled:= Self.Enabled;
end;


{ TUnfocusingMonthCalendar }

type
  TUnfocusingMonthCalendar = class( TJvCustomMonthCalendar)
  private
    FOnLoseFocus: TJvFocusChangeEvent;
    procedure WMKillFocus(var AMessage: TMessage); message WM_KILLFOCUS;
  protected
    procedure LoseFocus(const AFocusControl: TWinControl); dynamic;
    property OnLoseFocus: TJvFocusChangeEvent read FOnLoseFocus
      write FOnLoseFocus;
  end;

procedure TUnfocusingMonthCalendar.LoseFocus(const AFocusControl: TWinControl);
begin
  if(Assigned(OnLoseFocus)) then
    OnLoseFocus(Self, AFocusControl);
end;

procedure TUnfocusingMonthCalendar.WMKillFocus(var AMessage: TMessage);
begin
  inherited;
  if(not IsChildOf(AMessage.WParam, Self.Handle)) then
    LoseFocus(FindControl(AMessage.WParam));
end;


{ TLucaDropCalendar }

constructor TJvDropCalendar.CreateWithAppearance(AOwner: TComponent;
  const AAppearance: TJvMonthCalAppearance);
begin
  inherited Create(AOwner);
  FCal := TUnfocusingMonthCalendar.CreateWithAppearance(Self, AAppearance);
  with(TUnfocusingMonthCalendar(FCal)) do
  begin
    Parent := Self;
    ParentFont := True;
    OnSelChange := CalSelChange;
    OnSelect := CalSelect;
    OnLoseFocus := CalLoseFocus;
    OnKeyPress := CalKeyPress;
    Visible := True;
    AutoSize := True;
  end;
end;

destructor TJvDropCalendar.Destroy;
begin
  if(Assigned(FCal)) then
    with(TUnfocusingMonthCalendar(FCal)) do
    begin
      OnSelChange := NIL;
      OnSelect := NIL;
      OnKeyPress := NIL;
    end;
  inherited;
end;

procedure TJvDropCalendar.CalSelChange(Sender: TObject;
  StartDate, EndDate: TDateTime);
begin
  DoChange;
end;

procedure TJvDropCalendar.DoChange;
begin
  if(Assigned(OnChange)) then
    OnChange(Self);
end;

procedure TJvDropCalendar.CalSelect(Sender: TObject;
  StartDate, EndDate: TDateTime);
begin
  DoSelect;
end;

procedure TJvDropCalendar.DoSelect;
begin
  if(Assigned(OnSelect)) then
    OnSelect(Self);
  Release;
end;

procedure TJvDropCalendar.CalKeyPress(Sender: TObject; var Key: Char);
begin
  MessageBeep( 0);
  case Key of
    #13: DoSelect;
    #27: DoCancel;
  else
    DoChange;
  end;
end;

procedure TJvDropCalendar.DoCancel;
begin
  if(Assigned(OnCancel)) then
    OnCancel(Self)
  else
    Release;
end;

procedure TJvDropCalendar.DoShow;
begin
  inherited;
  {in the constructor the calendar will sometimes report the wrong width, so
   we do this here.}
  AutoSize := True;
end;

function TJvDropCalendar.GetSelDate: TDateTime;
begin
  result := TUnfocusingMonthCalendar(FCal).DateFirst;
end;

procedure TJvDropCalendar.SetSelDate(const AValue: TDateTime);
begin
  TUnfocusingMonthCalendar(FCal).DateFirst := AValue;
end;

procedure TJvDropCalendar.CalLoseFocus(const ASender: TObject;
  const AFocusControl: TWinControl);
begin
  Self.LoseFocus( AFocusControl);
end;

procedure TJvDropCalendar.SetFocus;
begin
  if FCal.CanFocus then
    FCal.SetFocus
  else
    inherited;
end;

end.
