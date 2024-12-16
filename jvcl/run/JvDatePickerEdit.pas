{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDatePickerEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen att lucatec dott de]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): Peter Thörnqvist.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A replacement for TDateTimePicker which is better suited for keyboard-input by
  ultimately descending from TCustomMaskEdit.

  Other notable features (especially in comparison to the native DATETIMEPICKER):

  - The control is able to construct a suitable EditMask from a valid date format
    string such as the global ShortDateFormat (the default) which should make it
    adapt well to regional settings / individual requirements.

  - It is possible to specify a NoDateText which will be displayed when no date
    is selected. The original datetimepicker would display 1899-12-31 in such
    cases. This feature could be further controlled by the AllowNoDate and
    NoDateShortcut properties. With the NoDateValue you can control what TDateTime
    value should be used. It defaults to 0, what means 1899-12-31.

Known issues / not (yet) implemented features:

  - there is no real support for DateFormats containing any literal characters
    other than the defined DateSeparator, especially spaces. it /might/ work in
    some cases but in the majority of cases it will not.
    TODO: simply disallow such characters or implement proper handling?

  - as the embedded MS-calendar does not support dates prior to 1752-09-14,
    neither does this control. this is not yet handled gracefully in absolutely
    all situations though.

  - the Min/MaxYear contstraints are currently commented out as they are not
    functional in the current state. They would still require some work to make
    up for two-digit year entries.

  - the control does (currently) not allow for time entry
  - it really is a control for date entry only.

-----------------------------------------------------------------------------}
// $Id$

unit JvDatePickerEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Types, Classes, Controls, ImgList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvCalendar, JvDropDownForm, JvCheckedMaskEdit, JvToolEdit;

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

  TJvPopupMonthCalendar2 = class(TJvMonthCalendar2)
  protected
    procedure LButtonDownFocus; override;
    procedure WndProc(var Msg: TMessage); override;
  end;

  {A dropdown form with an embedded calendar control.}
  TJvDropCalendar = class(TJvCustomDropDownForm)
  private
    FCal: TJvCustomMonthCalendar;
    FWithBeep: Boolean;
    FOnChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    procedure CalSelChange(Sender: TObject; StartDate, EndDate: TDateTime);
    procedure CalSelect(Sender: TObject; StartDate, EndDate: TDateTime);
    procedure CalKeyPress(Sender: TObject; var Key: Char);
    procedure CalKillFocus(const ASender: TObject; const ANextControl: TWinControl);
  protected
    procedure DoCancel;
    procedure DoChange;
    procedure DoSelect;
    procedure DoShow; override;
    function GetSelDate: TDateTime;
    procedure SetSelDate(const AValue: TDateTime);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMShowingChanged(var Msg: TMessage); override;
  public
    constructor CreateWithAppearance(AOwner: TComponent;
      const AAppearance: TJvMonthCalAppearance);
    destructor Destroy; override;
    procedure SetFocus; override;
    property SelDate: TDateTime read GetSelDate write SetSelDate;
    property WithBeep: Boolean read FWithBeep write FWithBeep;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TJvGetValidDateStringEvent = procedure(Sender: TObject; var DateText: string) of object;

  TJvCustomDatePickerEdit = class(TJvCustomCheckedMaskEdit)
  private
    FAllowNoDate: Boolean;
    FCalAppearance: TJvMonthCalAppearance;
    FDate: TDateTime;
    FDateError: Boolean;
    FDeleting: Boolean;
    FDateFigures: TJvDateFigures;
    FInternalDateFormat,
    FDateFormat: string;
    FEnableValidation: Boolean;
    FMask: string;
    FNoDateShortcut: TShortcut;
    FNoDateText: string;
    FStoreDate: Boolean;
    FAlwaysReturnEditDate: Boolean;
    FEmptyMaskText: string;
    FStoreDateFormat: Boolean;
    FDateSeparator: Char;
    FPopupDate: TDateTime;
    FNoDateValue: TDateTime;
    FOnGetValidDateString: TJvGetValidDateStringEvent;
    //    FMinYear: Word;
    //    FMaxYear: Word;
    procedure CalDestroy(Sender: TObject);
    procedure CalSelect(Sender: TObject);
    procedure CalCloseQuery(Sender: TObject; var CanClose: Boolean);
    function AttemptTextToDate(const AText: string; var ADate: TDateTime;
      const AForce: Boolean = False; const ARaise: Boolean = False): Boolean;
    function DateFormatToEditMask(var ADateFormat: string): string;
    function DateToText(const ADate: TDateTime): string;
    function DetermineDateSeparator(AFormat: string): Char;
    procedure ResetDateFormat;
    procedure FindSeparators(var AFigures: TJvDateFigures; const AText: string; const AGetLengths: Boolean = True);
    procedure ParseFigures(var AFigures: TJvDateFigures; AFormat: string; const AMask: string);
    procedure RaiseNoDate;
    procedure SetAllowNoDate(const AValue: Boolean);
    procedure SetCalAppearance(const AValue: TJvMonthCalAppearance);
    function GetDate: TDateTime;
    procedure SetDate(const AValue: TDateTime);
    procedure SetDateFormat(AValue: string);
    function GetDropped: Boolean;
    procedure SetNoDateText(const AValue: string);
    procedure SetNoDateValue(const AValue: TDateTime);
    procedure SetDateSeparator(const AValue: Char);
    function GetEditMask: string;
    procedure SetEditMask(const AValue: string);
    function GetText: TCaption;
    procedure SetText(const AValue: TCaption);
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure CMExit(var Msg: TMessage); message CM_EXIT;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TMessage); message WM_KEYUP;
    procedure WMChar(var Message: TMessage); message WM_CHAR;
  protected
    {$IFDEF JVCLThemesEnabled}
    function GetDatePickerThemeButtonMinTextSize: Integer; override;
    {$ENDIF JVCLThemesEnabled}
    function ValidateEditText: string;
    procedure CalChanged; virtual;
    procedure RestoreMaskForKeyPress;
    function GetValidDateString(const Text: string): string; virtual;
    procedure AcceptValue(const Value: Variant); override;
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure ResetPopupValue; override;
    function ConvertStrToDate(const AText: string; var ADate: TDateTime; ARaise: Boolean): Boolean; virtual;
    function IsNoDateShortcutStored: Boolean;
    function IsNoDateTextStored: Boolean;
    function IsNoDateValueStored: Boolean;
    procedure PopupChange; override;
    procedure Change; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure DoKillFocus(const ANextControl: TWinControl); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreatePopup; override;
    procedure HidePopup; override;
    procedure ShowPopup(Origin: TPoint); override;
    procedure DoCtl3DChanged; override;
    procedure EnabledChanged; override;
    function GetChecked: Boolean; override;
    function GetPopupValue: Variant; override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetPopupValue(const Value: Variant); override;
    procedure SetShowCheckbox(const AValue: Boolean); override;
    function GetEnableValidation: Boolean; virtual;
    procedure UpdateDisplay; virtual;
    function ValidateDate(const ADate: TDateTime): Boolean; virtual;
    function ActiveFigure: TJvDateFigureInfo;
    procedure ClearMask;
    procedure RestoreMask;
    function IsEmptyMaskText(const AText: string): Boolean;
    property AllowNoDate: Boolean read FAllowNoDate write SetAllowNoDate;
    property AlwaysReturnEditDate: Boolean read FAlwaysReturnEditDate write FAlwaysReturnEditDate default True;
    property CalendarAppearance: TJvMonthCalAppearance read FCalAppearance write SetCalAppearance;
    property Date: TDateTime read GetDate write SetDate stored FStoreDate;
    property DateFormat: string read FDateFormat write SetDateFormat stored FStoreDateFormat;
    property DateSeparator: Char read FDateSeparator write SetDateSeparator stored FStoreDateFormat;
    property Dropped: Boolean read GetDropped;
    property EnableValidation: Boolean read GetEnableValidation write FEnableValidation default True;
    property ImageKind default ikDatePicker;
    //    property MaxYear: Word read FMaxYear write FMaxYear;
    //    property MinYear: Word read FMinYear write FMinYear;
    property NoDateShortcut: TShortcut read FNoDateShortcut write FNoDateShortcut stored IsNoDateShortcutStored;
    property NoDateText: string read FNoDateText write SetNoDateText stored IsNoDateTextStored;
    property NoDateValue: TDateTime read FNoDateValue write SetNoDateValue stored IsNoDateValueStored;
    property ShowButton default True;
    property StoreDate: Boolean read FStoreDate write FStoreDate default False;
    property StoreDateFormat: Boolean read FStoreDateFormat write FStoreDateFormat default False;

    property OnGetValidDateString: TJvGetValidDateStringEvent read FOnGetValidDateString write FOnGetValidDateString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function DefaultImageIndex: TImageIndex; override;
    procedure Clear; override;
    function IsEmpty: Boolean; virtual;

    function HasValidDate: Boolean;

    property EditMask: string read GetEditMask write SetEditMask;
    property Text: TCaption read GetText write SetText;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDatePickerEdit = class(TJvCustomDatePickerEdit)
  public
    property Dropped;
  published
    property Action;
    property Align;
    property AllowNoDate;
    property AlwaysReturnEditDate;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property ButtonWidth;
    property CalendarAppearance;
    property Caret;
    property CharCase;
    property Checked;
    property ClickKey;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property Date;
    property DateFormat;
    property DateSeparator;
    {property BiDiMode;}
    {property ParentBiDiMode;}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Flat;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentFlat;
    property OnEndDock;
    property OnStartDock;
    property DirectInput;
    property DisabledColor;
    property DisabledTextColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableValidation;
    property Font;
    property Glyph;
    property GroupIndex;
    property HideSelection;
    property HintColor;
    property HotTrack;
    //    property MaxYear default 2900;
    //    property MinYear default 1900;
    property ImageIndex;
    property ImageKind;
    property Images;
    property NoDateShortcut;
    property NoDateText;
    property NoDateValue;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowCheckBox;
    property ShowHint;
    property ShowButton;
    property StoreDate;
    property StoreDateFormat;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnabledChanged;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnSetFocus;
    property OnStartDrag;

    property OnGetValidDateString;
    property OnPopupShown;
    property OnPopupHidden;
    property OnPopupChange;
    property OnPopupValueAccepted;
  end;

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
  Variants, SysUtils, Menus, Forms,
  {$IFDEF HAS_UNIT_CHARACTER}
  Character, // for inline
  {$ENDIF HAS_UNIT_CHARACTER}
  JclStrings,
  JvConsts, JvTypes, JvResources, JclSysUtils;

const
  DateMaskSuffix = '!;1;_';

//=== { TJvCustomDatePickerEdit } ============================================

procedure TJvCustomDatePickerEdit.AcceptValue(const Value: Variant);
var
  TextBefore: string;
  TmpDate: TDateTime;
  TmpValue: Variant;
  OldFormat: string;
  OldSeparator: Char;
begin
  TextBefore := Text;

  // Mantis 3056: If the date format is not the system's default, the value
  // displayed in the text box after having selected a date in the popup
  // will be 30.12.1899. This is because the variant will be converted to a
  // string using ShortDateFormat. So we change it here, to ensure it is
  // the one for the control. We also have to do the cast to a string
  // ourselves because VarToStr (called in TJvCustomComboEdit) ignores the
  // ShortDateFormat variable.
  // And we only call the inherited method this way if the variant is a
  // date, or we would risk an exception trying to convert something to a
  // date when it is not.
  if VarIsType(Value, varDate) then
  begin
    OldFormat := JclFormatSettings.ShortDateFormat;
    OldSeparator := JclFormatSettings.DateSeparator;
    try
      JclFormatSettings.ShortDateFormat := FInternalDateFormat;
      JclFormatSettings.DateSeparator := FDateSeparator;
      TmpDate := Value;
      TmpValue := DateToStr(TmpDate);
      inherited AcceptValue(TmpValue);
    finally
      JclFormatSettings.ShortDateFormat := OldFormat;
      JclFormatSettings.DateSeparator := OldSeparator;
    end;
  end
  else
    inherited AcceptValue(TmpValue);

  // Inherited AcceptValue will change the base class Text property, thus not
  // calling our SetText method. As a result, we must set the date in this case
  if Text <> TextBefore then
  begin
    AttemptTextToDate(Text, TmpDate, False);
    Self.Date := TmpDate;
  end;
end;

function TJvCustomDatePickerEdit.ActiveFigure: TJvDateFigureInfo;
var
  I: Integer;
begin
  for I := 2 downto 0 do
    { SelStart is 0-based, FDateFigures[I].Start is 1-based }
    if SelStart + 1 >= FDateFigures[I].Start then
    begin
      Result := FDateFigures[I];
      Exit;
    end;
  Result.Figure := dfNone;
end;

function TJvCustomDatePickerEdit.GetValidDateString(const Text: string): string;
begin
  Result := Text;
  if Assigned(FOnGetValidDateString) then
    FOnGetValidDateString(Self, Result);
end;

function TJvCustomDatePickerEdit.ConvertStrToDate(const AText: string; var ADate: TDateTime;
  ARaise: Boolean): Boolean;
var
  OldSeparator: Char;
  OldFormat: string;
begin
  OldFormat := JclFormatSettings.ShortDateFormat;
  OldSeparator := JclFormatSettings.DateSeparator;
  try
    JclFormatSettings.DateSeparator := FDateSeparator;
    JclFormatSettings.ShortDateFormat := FInternalDateFormat;
    if ARaise then
    begin
      ADate := StrToDate(AText);
      Result := True;
    end
    else
      Result := TryStrToDate(AText, ADate);
  finally
    JclFormatSettings.DateSeparator := OldSeparator;
    JclFormatSettings.ShortDateFormat := OldFormat;
  end;
end;

function TJvCustomDatePickerEdit.AttemptTextToDate(const AText: string;
  var ADate: TDateTime; const AForce: Boolean; const ARaise: Boolean): Boolean;
var
  OldDate: TDateTime;
  Dummy: Integer;
begin
  {only attempt to convert, if at least the Mask is matched
  - otherwise we'd be swamped by exceptions during input}
  if AForce or Validate(AText, Dummy) then
  begin
    Result := True;
    OldDate := ADate;
    if AllowNoDate and ((Text = NoDateText) or IsEmptyMaskText(AText)) then
      ADate := NoDateValue
    else
    begin
      if not ConvertStrToDate(StrRemoveChars(GetValidDateString(AText), [' ']), ADate, ARaise) then
      begin
        if AText = '' then
          ADate := Now
        else
          ADate := OldDate;
        Result := False;
      end;
    end;
  end
  else
    Result := False;
end;

procedure TJvCustomDatePickerEdit.CalChanged;
var
  NewDate: TDateTime;
begin
  if (FPopup is TJvDropCalendar) then
  begin
    NewDate := TJvDropCalendar(FPopup).SelDate;
    try
      if (NewDate <> Date) and EditCanModify then
        Date := NewDate;
    except
      on E: Exception do
      begin
        { If the EditCanModify method raises an exception the popup calendar is
          destroyed in the modal message loop of the exception dialog and when
          it returns we are still in the WM_LBUTTONUP handler of the now destroyed
          calendar. To prevent this the following code gracefully closes the popup
          calendar. }
        PopupCloseUp(Self, False);
        raise;
      end;
    end;
  end;
end;

procedure TJvCustomDatePickerEdit.CalCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  P: TPoint;
begin
  {If we would let the calendar close itself while clicking the button, the
   DropButtonClick method would simply reopen it again as it would find the
   calendar closed.}
  GetCursorPos(P);
  CanClose := not PtInRect(Button.BoundsRect, Button.ScreenToClient(P));
end;

procedure TJvCustomDatePickerEdit.CalDestroy(Sender: TObject);
begin
  PopupCloseUp(Self, False);
end;

procedure TJvCustomDatePickerEdit.CalSelect(Sender: TObject);
begin
  CalChanged;
  PopupCloseUp(Self, True);
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
      SelStart := lActFig.Start - 1;
      SelLength := lActFig.Length;
      SelText := Format('%.*d', [lActFig.Length, AValue]);
    finally
      EndInternalChange;
    end;
  end;

  procedure EnforceRange(const AMin, AMax: Word);
  begin
    if lFigVal > AMax then
      SetActiveFigVal(AMax)
    else
    if lFigVal < AMin then
      SetActiveFigVal(AMin);
  end;

begin
  if InternalChanging then
    Exit;

  FDateError := False;

  if [csDesigning, csDestroying] * ComponentState <> [] then
    Exit;

  if (Text <> NoDateText) and (Text <> '') then
  begin
    lDate := Self.Date;
    if AttemptTextToDate(Text, lDate) then
    begin
      BeginInternalChange;
      try
        Self.Date := lDate;
      finally
        EndInternalChange;
      end;
    end
    else
    if (not FDeleting) and EnableValidation then
    begin
      lActFig := ActiveFigure;

      if lActFig.Figure <> dfNone then
      begin
        lFigVal := StrToIntDef(Trim(Copy(Text, lActFig.Start, lActFig.Length)), 0);
        //only enforce range if the cursor is at the end of the current figure:
        if SelStart = lActFig.Start + lActFig.Length - 1 then
          case lActFig.Figure of
            dfDay:
              EnforceRange(1, 31);
            dfMonth:
              EnforceRange(1, 12);
            dfYear:
              {EnforceRange( MinYear, MaxYear)}; //year-validation still under development
          end;
      end;
      {make sure querying the date in an OnChange event handler always reflects
       the current contents of the control and not just the last valid value.}
      lDate := NoDateValue;
      AttemptTextToDate(Text, lDate, lActFig.Index = High(TJvDateFigures));
      if AlwaysReturnEditDate then
        FDate := lDate;
    end;
  end;
  inherited Change;
end;

procedure TJvCustomDatePickerEdit.Clear;
begin
  Checked := False;
end;

procedure TJvCustomDatePickerEdit.ClearMask;
begin
  if EditMask <> '' then
  begin
    FMask := EditMask;
    if not (csDesigning in ComponentState) then
      EditMask := '';
  end;
end;

constructor TJvCustomDatePickerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAllowNoDate := True;
  FAlwaysReturnEditDate := True;
  FDate := SysUtils.Date;
  FDateError := False;
  FDeleting := False;
  FEnableValidation := True;
  //  FMaxYear := 2900;
  //  FMinYear := 1800;
  FNoDateShortcut := TextToShortCut(RsDefaultNoDateShortcut);
  FNoDateText := '';
  FStoreDate := False;
  FStoreDateFormat := False;

  FCalAppearance := TJvMonthCalAppearance.Create;

  ControlState := ControlState + [csCreating];
  try
    ImageKind := ikDatePicker; { force update }
    ShowButton := True;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure TJvCustomDatePickerEdit.CreatePopup;
begin
  if not Assigned(FPopup) then
  begin
    FPopup := TJvDropCalendar.CreateWithAppearance(Self, FCalAppearance);
    with TJvDropCalendar(FPopup) do
    begin
//      SelDate := ;
      //OnChange := Self.CalChange;
      OnSelect := Self.CalSelect;
      OnDestroy := Self.CalDestroy;
      OnCloseQuery := Self.CalCloseQuery;
//      OnKillFocus := Self.CalKillFocus;
//      Show;
//      SetFocus;
    end;
  end;
end;

procedure TJvCustomDatePickerEdit.CreateWnd;
begin
  inherited CreateWnd;

  // obones: changed to DateFormat instead of ShortDateFormat, it was
  // preventing any date format different from the system's value to be
  // set at design time
  SetDateFormat(DateFormat);
end;

function TJvCustomDatePickerEdit.DateFormatToEditMask(
  var ADateFormat: string): string;
begin
  StrReplace(ADateFormat, 'dddddd', JclFormatSettings.LongDateFormat, []);
  StrReplace(ADateFormat, 'ddddd', JclFormatSettings.ShortDateFormat, []);
  StrReplace(ADateFormat, 'dddd', '', []); // unsupported: DoW as full name
  StrReplace(ADateFormat, 'ddd', '', []); // unsupported: DoW as abbrev
  StrReplace(ADateFormat, 'MMMM', 'MM', []);
  StrReplace(ADateFormat, 'MMM', 'M', []);
  Result := ADateFormat;
  StrReplace(Result, 'dd', '00', []);
  StrReplace(Result, 'd', '99', []);
  StrReplace(Result, 'MM', '00', [rfIgnoreCase]);
  StrReplace(Result, 'M', '99', [rfIgnoreCase]);
  StrReplace(Result, 'yyyy', '0099', []);
  StrReplace(Result, 'yy', '00', []);
  StrReplace(Result, ' ', '_', []);
  Result := Trim(Result) + DateMaskSuffix;
end;

function TJvCustomDatePickerEdit.DateToText(const ADate: TDateTime): string;
var
  OldSep: Char;
begin
  OldSep := JclFormatSettings.DateSeparator;
  // without this a slash would always be converted to SysUtils.DateSeparator
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    Result := FormatDateTime(FInternalDateFormat, ADate);
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

class function TJvCustomDatePickerEdit.DefaultImageIndex: TImageIndex;
begin
  Result := TJvDateEdit.DefaultImageIndex;
end;

destructor TJvCustomDatePickerEdit.Destroy;
begin
  FreeAndNil(FCalAppearance);
  inherited Destroy;
end;

function TJvCustomDatePickerEdit.DetermineDateSeparator(AFormat: string): Char;
begin
  AFormat := StrRemoveChars(Trim(AFormat), ['d', 'm', 'M', 'y']);
  if AFormat <> '' then
    Result := AFormat[1]
  else
    Result := JclFormatSettings.DateSeparator;
end;

procedure TJvCustomDatePickerEdit.DoCtl3DChanged;
begin
  inherited DoCtl3DChanged;
  { (rb) Conflicts with ButtonFlat property }
  Button.Flat := not Self.Ctl3D;
end;

function TJvCustomDatePickerEdit.ValidateEditText: string;
var
  lDate: TDateTime;
begin
  Result := '';
  if EnableValidation then
  try
    lDate := Self.Date;
    if (Text <> NoDateText) and AttemptTextToDate(Text, lDate, True, True) then
      Self.Date := lDate;
  except
    on EConvertError do
      if not (csDestroying in ComponentState) then
      begin
        FDateError := True;
        SetFocus;
        if RaiseException then
          raise;
      end
      else
        Self.Date := NoDateValue;
  end;
  if AllowNoDate and (lDate = NoDateValue) then
  begin
    Result := EditText;
    EditText := '';
  end;
end;

procedure TJvCustomDatePickerEdit.CMExit(var Msg: TMessage);
var
  OrgEditText: string;
begin
  OrgEditText := ValidateEditText;
  inherited;
  if OrgEditText <> '' then
    EditText := OrgEditText;
end;

procedure TJvCustomDatePickerEdit.DoKillFocus(const ANextControl: TWinControl);
begin
  if ((ANextControl = nil) or ((ANextControl <> FPopup) and
     (ANextControl.Owner <> FPopup))) and not FDateError then
    PopupCloseUp(Self, False);
  inherited DoKillFocus(ANextControl);
end;

procedure TJvCustomDatePickerEdit.WMKeyDown(var Message: TMessage);
begin
  if PopupVisible and (FPopup is TJvDropCalendar) and FPopup.Visible and (Message.LParam <> 1) then // protect against TCustomMaskEdit.SetCursor
    TJvDropCalendar(FPopup).FCal.Perform(WM_KEYDOWN, Message.WParam, Message.LParam)
  else
    inherited;
end;

procedure TJvCustomDatePickerEdit.WMKeyUp(var Message: TMessage);
begin
  if PopupVisible and (FPopup is TJvDropCalendar) and FPopup.Visible and (Message.LParam <> 1) then // protect against TCustomMaskEdit.SetCursor
    TJvDropCalendar(FPopup).FCal.Perform(WM_KEYUP, Message.WParam, Message.LParam)
  else
    inherited;
end;

procedure TJvCustomDatePickerEdit.WMChar(var Message: TMessage);
begin
  if PopupVisible and (FPopup is TJvDropCalendar) and FPopup.Visible then
    TJvDropCalendar(FPopup).FCal.Perform(WM_CHAR, Message.WParam, Message.LParam)
  else
    inherited;
end;

{$IFDEF JVCLThemesEnabled}
function TJvCustomDatePickerEdit.GetDatePickerThemeButtonMinTextSize: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  Size: TSize;
  S: string;
begin
  if HandleAllocated then
  begin
    S := DateToText(FDate);
    Size.cx := 0;
    Size.cy := 0;

    DC := GetDC(HWND_DESKTOP);
    SaveFont := SelectObject(DC, Font.Handle);
    Windows.GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
    SelectObject(DC, SaveFont);
    ReleaseDC(HWND_DESKTOP, DC);

    Result := Size.cx;
  end
  else
    Result := inherited GetDatePickerThemeButtonMinTextSize;
end;
{$ENDIF JVCLThemesEnabled}

//procedure TJvCustomDatePickerEdit.DropButtonClick(Sender: TObject);
//begin
//  if Dropped then
//    CloseUp
//  else
//    DropDown;
//end;

//procedure TJvCustomDatePickerEdit.DropDown;
//begin
//  if not Dropped then
//  begin
//    if IsEmpty then
//      Self.Date := SysUtils.Date;
//
//    FDropFo := TJvDropCalendar.CreateWithAppearance(Self, FCalAppearance);
//    with FDropFo do
//    begin
//      SelDate := Self.Date;
//      OnChange := Self.CalChange;
//      OnSelect := Self.CalSelect;
//      OnDestroy := Self.CalDestroy;
//      OnCloseQuery := Self.CalCloseQuery;
//      OnKillFocus := Self.CalKillFocus;
//      Show;
//      SetFocus;
//    end;
//  end;
//end;

procedure TJvCustomDatePickerEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if not Enabled and Dropped then
    PopupCloseUp(Self, False);
end;

procedure TJvCustomDatePickerEdit.FindSeparators(var AFigures: TJvDateFigures;
  const AText: string; const AGetLengths: Boolean);
begin
  //TODO 3 : make up for escaped characters in EditMask
  AFigures[0].Start := 1;
  AFigures[1].Start := Pos(DateSeparator, AText) + 1;
  AFigures[2].Start := StrLastPos(DateSeparator, AText) + 1;

  if AGetLengths then
  begin
    AFigures[0].Length := AFigures[1].Start - 2;
    AFigures[1].Length := AFigures[2].Start - AFigures[1].Start - 1;
    AFigures[2].Length := Length(AText) - AFigures[2].Start + 1;
  end;
end;

function TJvCustomDatePickerEdit.GetChecked: Boolean;
begin
  Result := not IsEmpty;
end;

function TJvCustomDatePickerEdit.GetDate: TDateTime;
begin
  Result := FDate;
end;

function TJvCustomDatePickerEdit.GetDropped: Boolean;
begin
  //Result := Assigned(FDropFo) and not (csDestroying in FDropFo.ComponentState);
  Result := PopupVisible;
end;

function TJvCustomDatePickerEdit.GetEditMask: string;
begin
  Result := inherited EditMask;
end;

function TJvCustomDatePickerEdit.GetEnableValidation: Boolean;
begin
  Result := FEnableValidation;
end;

function TJvCustomDatePickerEdit.GetPopupValue: Variant;
begin
  if FPopup is TJvDropCalendar then
    Result := TJvDropCalendar(FPopup).SelDate;
end;

function TJvCustomDatePickerEdit.GetText: TCaption;
var
  OldSep: Char;
begin
  OldSep := JclFormatSettings.DateSeparator;
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    Result := inherited Text;
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

function TJvCustomDatePickerEdit.HasValidDate: Boolean;
var
  TmpDate: TDateTime;
begin
  Result := AttemptTextToDate(Text, TmpDate, False, False);
end;

procedure TJvCustomDatePickerEdit.HidePopup;
begin
//  inherited;
  if FPopup is TJvDropCalendar then
  begin
    TJvDropCalendar(FPopup).Hide;
    if Assigned(OnPopupHidden) then
      OnPopupHidden(Self);
  end;
end;

function TJvCustomDatePickerEdit.IsEmpty: Boolean;
begin
  Result := FDate = NoDateValue;
end;

function TJvCustomDatePickerEdit.IsEmptyMaskText(const AText: string): Boolean;
begin
  Result := AnsiSameStr(AText, FEmptyMaskText);
end;

function TJvCustomDatePickerEdit.IsNoDateShortcutStored: Boolean;
begin
  Result := (NoDateShortcut <> TextToShortCut(RsDefaultNoDateShortcut));
end;

function TJvCustomDatePickerEdit.IsNoDateTextStored: Boolean;
begin
  Result := NoDateText <> '';
end;

function TJvCustomDatePickerEdit.IsNoDateValueStored: Boolean;
begin
  Result := NoDateValue <> 0;
end;

procedure TJvCustomDatePickerEdit.RestoreMaskForKeyPress;
begin
  try
    if ((EditMask = '') or (EditMask <> FMask)) and (Text = NoDateText) {and EditCanModify} then
    begin
      Text := '';
      RestoreMask;
    end;
  except
    Text := '';
    RestoreMask;
    raise;
  end;
end;

procedure TJvCustomDatePickerEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  // Indicates whether FDeleting is set here from False to True.
  DeleteSetHere: Boolean;
  OrgEditText: string;
  WasUnmasked: Boolean;
begin
  DeleteSetHere := False;
  WasUnmasked := not IsMasked;
  RestoreMaskForKeyPress;

  if AllowNoDate and (ShortCut(Key, Shift) = NoDateShortcut) and EditCanModify then
    Date := NoDateValue
  else
  if Shift * KeyboardShiftStates = [] then
    case Key of
      VK_BACK, VK_CLEAR, VK_DELETE, VK_EREOF, VK_OEM_CLEAR:
        begin
          DeleteSetHere := not FDeleting;
          FDeleting := True;

          { Workaround for an TMaskEdit bug: If the NoDateText is visible and the
            user presses VK_BACK the TCustomMaskEdit.DeleteKeys will raise an
            access violation.
            Fortunately we have already cleared the edit in RestoreMaskForKeyPress(). }
          if (Key = VK_BACK) and (SelStart = 0) and (SelLength = 1) and WasUnmasked then
            Key := 0;
        end;
      VK_RETURN:
        begin
          OrgEditText := ValidateEditText;
          if OrgEditText <> '' then
            EditText := OrgEditText;
        end;
    end;
  inherited KeyDown(Key, Shift);
  FDeleting := FDeleting and not DeleteSetHere;
end;

procedure TJvCustomDatePickerEdit.KeyPress(var Key: Char);
var
  OldSep: Char;
begin
  { If used in JvDBGrid the KeyDown event isn't invoked, so the EditMask isn't set
    when the KeyPress event triggers. }
  RestoreMaskForKeyPress;

  { this makes the transition easier for users used to non-mask-aware edit controls
    as they could continue typing the separator character without the cursor
    auto-advancing to the next figure when they don't expect it : }
  if ((Key = Self.DateSeparator) and (Text[SelStart] = Self.DateSeparator)) or
     ((CharIsPrintable(Key) or (Key = #8)) and not EditCanModify) then
  begin
    Key := #0;
    Exit;
  end;
  { EditCanModify could have triggered a ClearMask. }
  RestoreMaskForKeyPress;

  OldSep := JclFormatSettings.DateSeparator;
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    inherited KeyPress(Key);
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

procedure TJvCustomDatePickerEdit.Loaded;
var
  SavedWidth : Integer;
begin
  // (obones) Mantis 2491: After a copy and paste operation in the IDE, the new
  // control would be one pixel less in width. This is caused by a call to
  // SetText that triggers a call to TCustomMaskEdit.CheckCursor that sends
  // WM_LEFT to the control. Somehow, this ends up being eaten by the designer
  // and reduces the width. Add a call to CheckCursor just before UpdateDisplay
  // below, you'll see it's reduced by two. What's weird is that if you do the
  // exact same thing in Loaded in TJvCustomCheckedMaskedEdit, the width does
  // not get reduced. So it must be something in this class, but I cannot
  // figure out exactly what is done here to trigger this. For now, Let's just
  // save and restore the width.
  SavedWidth := Width;
  try
    inherited Loaded;
    UpdateDisplay;
  finally
    Width := SavedWidth;
  end;
end;

procedure TJvCustomDatePickerEdit.ParseFigures(var AFigures: TJvDateFigures;
  AFormat: string; const AMask: string);
var
  I: Integer;
  DummyFigures: TJvDateFigures;
begin
  {Determine the position of the individual figures in the mask string.}
  FindSeparators(AFigures, AMask);
  AFigures[2].Length := AFigures[2].Length - Length(DateMaskSuffix);

  AFormat := UpperCase(AFormat);

  {Determine the order of the individual figures in the format string.}
  FindSeparators(DummyFigures, AFormat, False);

  for I := 0 to 2 do
  begin
    case AFormat[DummyFigures[I].Start] of
      'D':
        AFigures[I].Figure := dfDay;
      'M':
        AFigures[I].Figure := dfMonth;
      'Y':
        AFigures[I].Figure := dfYear;
    end;
    AFigures[I].Index := I;
  end;
end;

procedure TJvCustomDatePickerEdit.PopupChange;
begin
  inherited PopupChange;
  DoChange;
end;

procedure TJvCustomDatePickerEdit.RaiseNoDate;
begin
  raise EJVCLException.CreateResFmt(@RsEMustHaveADate, [Name]);
end;

procedure TJvCustomDatePickerEdit.ResetDateFormat;
begin
  FInternalDateFormat := FDateFormat;
  FMask := DateFormatToEditMask(FInternalDateFormat);
  ParseFigures(FDateFigures, FInternalDateFormat, FMask);
  BeginInternalChange;
  try
    EditMask := '';
    Text := '';
    EditMask := FMask;
    FEmptyMaskText := Text;
  finally
    EndInternalChange;
  end;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.RestoreMask;
begin
  if EditMask = '' then
    EditMask := FMask;
end;

procedure TJvCustomDatePickerEdit.SetAllowNoDate(const AValue: Boolean);
begin
  if AllowNoDate <> AValue then
  begin
    FAllowNoDate := AValue;

    if AValue and IsEmpty then
      if csDesigning in ComponentState then
        Self.Date := SysUtils.Date
      else
        RaiseNoDate;

    if not AValue then
      ShowCheckBox := False;
  end;
end;

procedure TJvCustomDatePickerEdit.SetCalAppearance(const AValue: TJvMonthCalAppearance);
begin
  FCalAppearance.Assign(AValue);
end;

procedure TJvCustomDatePickerEdit.SetChecked(const AValue: Boolean);
begin
  inherited SetChecked(AValue);
  if Checked <> AValue then
  begin
    if AValue then
    begin
      if Self.Date = NoDateValue then
        Self.Date := SysUtils.Date;
    end
    else
      Self.Date := NoDateValue;
    Change;
  end;
end;

procedure TJvCustomDatePickerEdit.SetDate(const AValue: TDateTime);
begin
  if (FDate <> AValue) and ValidateDate(AValue) then
  begin
    StoreDate := Trunc(AValue) = Trunc(FDate);
    FDate := AValue;
    if AValue <> NoDateValue then
      Checked := True;
    DoChange;
  end;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.SetDateFormat(AValue: string);
var
  UseDefaultDateFormat: Boolean;
begin
  UseDefaultDateFormat := False;
  if AValue = '' then
  begin
    AValue := JclFormatSettings.ShortDateFormat;
    // XE+ changed ShortDateFormat to always use '/'
    FDateSeparator := JclFormatSettings.DateSeparator;
    UseDefaultDateFormat := True;
  end;
  if AValue <> FDateFormat then
  begin
    FDateFormat := AValue;
    if not UseDefaultDateFormat then
      FDateSeparator := DetermineDateSeparator(FDateFormat);
    StoreDateFormat := FDateFormat <> JclFormatSettings.ShortDateFormat;
    ResetDateFormat;
  end;
end;

procedure TJvCustomDatePickerEdit.SetDateSeparator(const AValue: Char);
begin
  if AValue <> FDateSeparator then
  begin
    FDateSeparator := AValue;
    ResetDateFormat;
  end;
end;

{ The only purpose of the following overrides is to overcome a known issue in
  Mask.pas where it is impossible to use the slash character in an EditMask if
  SysUtils.DateSeparator is set to something else even if the slash was escaped
  as a literal. By inheritance the following methods all end up eventually in
  Mask.MaskIntlLiteralToChar which performs the unwanted conversion. By
  temporarily setting SysUtils.DateSeparator we could circumvent this. }

procedure TJvCustomDatePickerEdit.SetEditMask(const AValue: string);
var
  OldSep: Char;
  Designing: Boolean;
begin
{  if csDesigning in ComponentState then
    Exit;}

  OldSep := JclFormatSettings.DateSeparator;
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    Designing := False;
    if csDesigning in ComponentState then
    begin
      // If SetEditMask is called from CreateWnd via SetDateFormat, the TMaskEdit.SetCursor emulates
      // a Shift+Left/Right key press. The form designer catches the key press and the
      // IDE's Designer Guidelines code throws an access violation.
      // With this we disable the form designer until "inherted EditMask" was executed.
      Designing := True;
      SetDesigning(False, False);
    end;
    try
      inherited EditMask := AValue;
    finally
      if Designing then
        SetDesigning(True, False);
    end;
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

procedure TJvCustomDatePickerEdit.SetNoDateText(const AValue: string);
begin
  FNoDateText := AValue;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.SetNoDateValue(const AValue: TDateTime);
begin
  if AValue <> FNoDateValue then
  begin
    FNoDateValue := AValue;
    UpdateDisplay;
  end;
end;

procedure TJvCustomDatePickerEdit.SetPopupValue(const Value: Variant);
var
  NewDate: TDateTime;
begin
  if FPopup is TJvDropCalendar then
  begin
    // We must do the conversion ourselves as the date format might
    // have been personalized. (Mantis 3628)
    // Default to Now if the Value is not valid. (Mantis 3733)
    if (Value = Null) or (Value = NoDateText) or not AttemptTextToDate(VarToStr(Value), NewDate) then
      NewDate := Now;
    FPopupDate := NewDate;
    TJvDropCalendar(FPopup).SelDate := NewDate;
  end;
end;

function TJvCustomDatePickerEdit.AcceptPopup(var Value: Variant): Boolean;
begin
  Result := inherited AcceptPopup(Value);
  if Result then
    Result := Value <> FPopupDate;
end;

procedure TJvCustomDatePickerEdit.SetShowCheckbox(const AValue: Boolean);
begin
  inherited SetShowCheckbox(AValue);
  if AValue then
    AllowNoDate := True;
  UpdateDisplay;
end;

procedure TJvCustomDatePickerEdit.SetText(const AValue: TCaption);
var
  OldSep: Char;
begin
  OldSep := JclFormatSettings.DateSeparator;
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    inherited Text := AValue;
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

procedure TJvCustomDatePickerEdit.ShowPopup(Origin: TPoint);
begin
  if FPopup is TJvDropCalendar then
  begin
    FPopup.SetBounds(Origin.X, Origin.Y, FPopup.Width, FPopup.Height);
    FPopup.Visible := True; // overriden CM_SHOWINGCHANGED will take care of SW_SHOWNOACTIVATE

    // Sometimes the popup windows is placed behind the window, so bring it to the top
    SetWindowPos(FPopup.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);

    // Emulate a LButton-Click to give the month calendar the look of being focused
    SendMessage(TJvDropCalendar(FPopup).FCal.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLong(Word(-1), Word(-1)));
    SendMessage(TJvDropCalendar(FPopup).FCal.Handle, WM_LBUTTONUP, 0, MakeLong(Word(-1), Word(-1)));

    if Assigned(OnPopupShown) then
      OnPopupShown(Self);
  end
  else
    inherited ShowPopup(Origin);
end;

procedure TJvCustomDatePickerEdit.UpdateDisplay;
begin
  if InternalChanging or (csLoading in ComponentState) then
    Exit;

  // (obones): We need a valid handle here, because setting the text
  // will read the value of DateSeparator. This value is #0 until the
  // CreateWnd method is called.
  // If we don't do that, setting any property that changes the display
  // (like checked) just after having created the control at runtime
  // would trigger an "Invalid date" exception because the date, month
  // and day would not be separated at all.
  // Doing this means that a parent is required for the change to work.
  HandleNeeded;

  BeginInternalChange;
  try
    inherited SetChecked(not IsEmpty);
    if IsEmpty then
    begin
      if not (csDesigning in ComponentState) then
      begin
        ClearMask;
        Text := NoDateText;
      end;
    end
    else
    begin
      RestoreMask;
      Text := DateToText(Self.Date)
    end;
  finally
    EndInternalChange;
  end;
end;

function TJvCustomDatePickerEdit.ValidateDate(const ADate: TDateTime): Boolean;
begin
  if not AllowNoDate and (ADate = NoDateValue) then
    RaiseNoDate;
  if (ADate < EncodeDate(1752, 09, 14)) or ((ADate > EncodeDate(1752, 09, 19)) and (ADate < EncodeDate(1752, 10, 1))) then
    { For historical/political reasons the days 1752-09-03 - 1752-09-13 do not
      exist in the Gregorian calendar - for some unknown reason the Microsoft
      calendar treats the period between 1752-09-20 and 1752-09-30 as missing
      instead, even though dates before 1752-09-14 are considered invalid as
      well (MS' offical explanation saying they only support the Gregorian
      calendar as of British adoption of it is not accurate: Britain adopted the
      Gregorian calendar starting 1752-01-01).}
    Result := False
  else
    Result := True;
end;

procedure TJvCustomDatePickerEdit.WMPaste(var Msg: TMessage);
var
  OldSep: Char;
begin
  if csDesigning in ComponentState then
    Exit;

  OldSep := JclFormatSettings.DateSeparator;
  JclFormatSettings.DateSeparator := Self.DateSeparator;
  try
    inherited;
  finally
    JclFormatSettings.DateSeparator := OldSep;
  end;
end;

procedure TJvCustomDatePickerEdit.ResetPopupValue;
begin
  // do nothing and keep the current value
end;


//=== { TJvPopupMonthCalendar2 } =============================================

procedure TJvPopupMonthCalendar2.LButtonDownFocus;
begin
  // no inherited, as it would lead to an endless SetFocus recursion
end;

procedure TJvPopupMonthCalendar2.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_MOUSEACTIVATE:
      Msg.Result := MA_NOACTIVATE;
  else
    inherited WndProc(Msg);
  end;
end;

//=== { TJvDropCalendar } ====================================================

procedure TJvDropCalendar.CalKeyPress(Sender: TObject; var Key: Char);
begin
  if WithBeep then
    SysUtils.Beep;
  case Word(Key) of
    VK_RETURN:
      DoSelect;
    VK_ESCAPE:
      DoCancel;
  else
    DoChange;
  end;
end;

procedure TJvDropCalendar.CalKillFocus(const ASender: TObject;
  const ANextControl: TWinControl);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if PtInRect(BoundsRect, P) then
    Exit;
  if Assigned(ANextControl) then
    FocusKilled(ANextControl.Handle)
  else
    FocusKilled(0);
end;

procedure TJvDropCalendar.CalSelChange(Sender: TObject;
  StartDate, EndDate: TDateTime);
begin
  DoChange;
end;

procedure TJvDropCalendar.CalSelect(Sender: TObject; StartDate, EndDate: TDateTime);
begin
  DoSelect;
end;

procedure TJvDropCalendar.CMShowingChanged(var Msg: TMessage);
begin
  // Override TCustomForm.CMShowingChanged so that we can use SW_SHOWNOACTIVATE

  if not Showing then
  begin
    inherited;
    Exit;
  end;

  Include(FFormState, fsShowing);
  try
    try
      DoShow;
    except
      Application.HandleException(Self);
    end;
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
  finally
    Exclude(FFormState, fsShowing);
  end;
end;

procedure TJvDropCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER; // MonthCal2 has its own border
end;

constructor TJvDropCalendar.CreateWithAppearance(AOwner: TComponent;
  const AAppearance: TJvMonthCalAppearance);
begin
  inherited Create(AOwner);
  FWithBeep := False;
  FCal := TJvPopupMonthCalendar2.CreateWithAppearance(Self, AAppearance);
  with TJvPopupMonthCalendar2(FCal) do
  begin
    ParentFont := True;
    OnSelChange := CalSelChange;
    OnSelect := CalSelect;
    OnKillFocus := CalKillFocus;
    OnKeyPress := CalKeyPress;
    Visible := True;
    AutoSize := True;
    Parent := Self;
  end;
end;

destructor TJvDropCalendar.Destroy;
begin
  if Assigned(FCal) then
    with TJvMonthCalendar2(FCal) do
    begin
      OnSelChange := nil;
      OnSelect := nil;
      OnKeyPress := nil;
    end;
  inherited Destroy;
end;

procedure TJvDropCalendar.DoCancel;
begin
  if Assigned(OnCancel) then
    OnCancel(Self)
  else
    Release;
end;

procedure TJvDropCalendar.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvDropCalendar.DoSelect;
var
  LastCloseOnLeave: Boolean;
begin
  { Protect against releasing the calendar in the message loop of the
    Application.HandleException dialog. }
  LastCloseOnLeave := CloseOnLeave;
  try
    CloseOnLeave := False;
    if Assigned(OnSelect) then
      OnSelect(Self);
  finally
    CloseOnLeave := LastCloseOnLeave;
  end;
end;

procedure TJvDropCalendar.DoShow;
begin
  {
   In the constructor the calendar will sometimes report
   the wrong size, so we do this here.
  }
  AutoSize := True;
  TJvMonthCalendar2(FCal).Today := Date; { update the current day }
  inherited DoShow;
end;

function TJvDropCalendar.GetSelDate: TDateTime;
begin
  Result := TJvMonthCalendar2(FCal).DateFirst;
end;

procedure TJvDropCalendar.SetFocus;
begin
  if FCal.CanFocus then
    FCal.SetFocus
  else
    inherited SetFocus;
end;

procedure TJvDropCalendar.SetSelDate(const AValue: TDateTime);
begin
  TJvMonthCalendar2(FCal).DateFirst := AValue;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
