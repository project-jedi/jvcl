{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTypedEdit.pas, released November 1999.

The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2001 Anthony Steele.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTypedEdit;

{Comments - controls to capture numbers of various kinds (Int, Float, Money)
 The money edit also does some display formatting

   Still to do

    - test some more

   - The controls do not work well if zero is not within the min/max range
     What should happen when 0 is not in the range?
     control should start up with ? min ?max ? 0? displayed
     User should set the value?

     Consider only checking the full range on exit.
     During data capture, extend the range to include 0

     - See if a base class is useful for these edit controls
     - Display formatting for int & float edits?

     The code in these controls is a bit repetitive, but unfortunately
     you can't have a base class property with polymorphic type   }

interface

uses
  Classes,
  JvEdit;

type
  TCustomIntegerEdit = class(TJvCustomEdit)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FHasMinValue: Boolean;
    FHasMaxValue: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure SetValue(iNewValue: Integer); virtual;
    function GetValue: Integer; virtual;
    procedure SetMinValue(iNewValue: Integer);
    procedure SetMaxValue(iNewValue: Integer);
    function ChopToRange(iValue: Integer): Integer;
    function IsInRange(iValue: Integer): Boolean; overload;
    function IsInRange(sValue: string): Boolean; overload;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property HasMaxValue: Boolean read FHasMaxValue write FHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write FHasMinValue;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property Value: Integer read GetValue write SetValue;
  published
    property AutoSize default True;
  end;

  { to capture and display float values }
  TCustomFloatEdit2 = class(TJvCustomEdit)
  private
    FMaxValue: Extended;
    FMinValue: Extended;
    FHasMaxValue: Boolean;
    FHasMinValue: Boolean;
    FMaxDecimals: Integer;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure SetValue(eNewValue: Extended);
    function GetValue: Extended;
    procedure SetMinValue(eNewValue: Extended);
    procedure SetMaxValue(eNewValue: Extended);
    procedure SetMaxDecimals(iNewValue: Integer);
    function ChopToRange(eValue: Extended): Extended;
    function IsInRange(eValue: Extended): Boolean; overload;
    function IsInRange(sValue: string): Boolean; overload;
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property MaxDecimals: Integer read FMaxDecimals write SetMaxDecimals;
    property HasMaxValue: Boolean read FHasMaxValue write FHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write FHasMinValue;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property Value: Extended read GetValue write SetValue;
  published
    property AutoSize default True;
  end;

  { control to capture a year
    int edit with automatic 2 digit -> 4 digit expansion
    in a range controlld by the WindowsillYear
    which is initialised from the program/system settings  }
  TCustomYearEdit = class(TCustomIntegerEdit)
  private
    FWindowsillYear: Integer;
  protected
    procedure DoExit; override;
    procedure SetValue(iNewValue: Integer); override;
    function GetValue: Integer; override;
    procedure FixYear;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property WindowsillYear: Integer read FWindowsillYear write FWindowsillYear;
  end;

  { to capture and display Currency values
    when the control has focus, the number is displayed simply
    when the control does not have focus, the number is formatted
    as per control panel Currency settings }
  TCustomCurrencyEdit = class(TJvCustomEdit)
  private
    FMaxValue: Currency;
    FMinValue: Currency;
    FHasMaxValue: Boolean;
    FHasMinValue: Boolean;
    FOriginalValue: Currency;
    FOriginalValueOK: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure SetValue(const peNewValue: Currency); virtual;
    function GetValue: Currency;
    procedure SetMinValue(const peNewValue: Currency);
    procedure SetMaxValue(const peNewValue: Currency);
    function ChopToRange(eValue: Currency): Currency;
    function IsInRange(eValue: Currency): Boolean; overload;
    function IsInRange(sValue: string): Boolean; overload;
    property MaxValue: Currency read FMaxValue write FMaxValue;
    property MinValue: Currency read FMinValue write FMinValue;
    property HasMaxValue: Boolean read FHasMaxValue write FHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write FHasMinValue;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property Value: Currency read GetValue write SetValue;
  published
    property AutoSize default True;
  end;

  { another bit of functionality
    - when the control does not have focus, display a formatted Currency string
     (as per control panel)
    - when it has focus, display a raw number string for editing

    To do
    - Extend this functionality to int & float edits?

    I would put code this in as a base class below all the number edits,
    but unfortunately it uses the Value property which
    is different per subclass.
    it could easily be duplicated for int & float edit controls
    but I don't want to go there until I am convinced that there is no other way

    - Make the format mask a published property, not a constant?
      this is more applicable to Int & Float edits }
  TCustomFormattedCurrencyEdit = class(TCustomCurrencyEdit)
  private
    FSupressOnChange: Boolean;
    FChanging: Boolean;
  protected
    procedure UnFormatText; virtual;
    procedure SetValue(const peNewValue: Currency); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure FormatText; virtual;
  end;

  TJvIntegerEdit = class(TCustomIntegerEdit)
  published
    { most properties of TMemo
      no Text or WordWrap properties }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
//    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//    property WantReturns;
//    property WantTabs;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { new properties }
    property AutoSelect;
    property Value;
    property MaxValue;
    property MinValue;
    property HasMaxValue;
    property HasMinValue;
    (* ++ RDB ++ *)
    property ClipBoardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  TJvFloatEdit2 = class(TCustomFloatEdit2)
  published
    { most properties of TMemo
      no Text or WordWrap properties }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
//    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//    property WantReturns;
//    property WantTabs;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { new properties }
    property AutoSelect;
    property Value;
    property MaxValue;
    property MinValue;
    property MaxDecimals;
    property HasMaxValue;
    property HasMinValue;
    (* ++ RDB ++ *)
    property ClipBoardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  TJvYearEdit = class(TCustomYearEdit)
  published
    { most properties of TMemo
      no Text or WordWrap properties }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
//    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//    property WantReturns;
//    property WantTabs;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { new properties }
    property AutoSelect;
    property Value;
    property MaxValue;
    property MinValue;
    property HasMaxValue;
    property HasMinValue;
    property WindowsillYear;
    (* ++ RDB ++ *)
    property ClipBoardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  { surface class }
  TJvCurrencyEdit = class(TCustomFormattedCurrencyEdit)
  published
    { most properties of TMemo
     no Text or WordWrap properties }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
//    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//    property WantReturns;
//    property WantTabs;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { new properties }
    property AutoSelect;
    property Value;
    property MaxValue;
    property MinValue;
    property HasMaxValue;
    property HasMinValue;
    (* ++ RDB ++ *)
    property ClipBoardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

implementation

uses
  SysUtils,
  JclStrings, JclDateTime,
  JvFunctions;

const
  MONEY_FORMAT = '%m'; // see help on "format strings"

//=== TCustomIntegerEdit =====================================================

constructor TCustomIntegerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { default is right aligned }
  Alignment := taRightJustify;
  Text := '';
  AutoSize := True;
  { by default no min & max }
  FMinValue := 0;
  FMaxValue := 0;
  FHasMinValue := False;
  FHasMaxValue := False;
end;

function TCustomIntegerEdit.ChopToRange(iValue: Integer): Integer;
begin
  Result := iValue;
  if HasMaxValue and (iValue > MaxValue) then
    Result := FMaxValue;
  if HasMinValue and (iValue < MinValue) then
    Result := FMinValue;
end;

procedure TCustomIntegerEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomIntegerEdit;
begin
  inherited Assign(Source);
  if Source is TCustomIntegerEdit then
  begin
    lcSource := Source as TCustomIntegerEdit;
    MinValue := lcSource.MinValue;
    MaxValue := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
  // (rom) why not else inherited Assign?
end;

function TCustomIntegerEdit.GetValue: Integer;
begin
  if Text = '' then
    Result := 0
  else
    Result := StrToIntDef(Text, 0);
end;

function TCustomIntegerEdit.IsInRange(iValue: Integer): Boolean;
begin
  Result := True;
  if HasMaxValue and (iValue > MaxValue) then
    Result := False;
  if HasMinValue and (iValue < MinValue) then
    Result := False;
end;

function TCustomIntegerEdit.IsInRange(sValue: string): Boolean;
var
  liValue: Integer;
begin
  if sValue = '' then
    liValue := 0
  else
    liValue := StrToIntDef(sValue, 0);
  Result := IsInRange(liValue);
end;

procedure TCustomIntegerEdit.KeyPress(var Key: Char);
var
  lsNewText: string;
begin
  { not interested in control chars }
  if Ord(Key) < Ord(' ') then
  begin
    if Key = #13 then
      Key := #0
    else
      inherited KeyPress(Key);
    Exit;
  end;

  { allow only digits and minus sign }
  if not CharIsDigit(Key) and (Key <> '-') then
    Key := #0;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first Char }
    else
    if SelStart <> 0 then
      Key := #0;

    { only 1 minus sign }
    if StrLeft(lsNewText, 2) = '--' then
      Key := #0;
  end;

  { no leading zeros }
  if (SelStart = 0) and (Key = '0') and (StrLeft(lsNewText, 1) = '0') then
    Key := #0;

  { no leading '-0' }
  if (SelStart = 1) and (Key = '0') and (StrLeft(lsNewText, 2) = '-0') then
    Key := #0;

  { disallow the keypress if the value would go out of range }
  lsNewText := GetChangedText(Text, SelStart, SelLength, Key);
  if not IsInRange(lsNewText) then
    Key := #0;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TCustomIntegerEdit.SetMaxValue(iNewValue: Integer);
begin
  FMaxValue := iNewValue;
  { make min value consistent }
  if MinValue > MaxValue then
    FMinValue := MaxValue;
end;

procedure TCustomIntegerEdit.SetMinValue(iNewValue: Integer);
begin
  FMinValue := iNewValue;
  { make max value consistent }
  if MaxValue < MaxValue then
    FMaxValue := MinValue;
end;

procedure TCustomIntegerEdit.SetValue(iNewValue: Integer);
begin
  Text := IntToStr(ChopToRange(iNewValue));
end;

const
  { The Extended data type handles up to 20 digits - no point in
    setting max decimals to more than this }
  FLOAT_EDIT_MAX_MAX_DECIMALS = 20;

  { most data capture is more like this -
    typing in 19 decimal digits usually indicates forehead-on-the-keyboard syndrome }
  FLOAT_EDIT_DEFAULT_MAX_DECIMALS = 5;

//=== TCustomFloatEdit2 ======================================================

constructor TCustomFloatEdit2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { default is right aligned }
  Alignment := taRightJustify;
  AutoSize := True;
  Text := '';
  { by default no min & max }
  FMinValue := 0.0;
  FMaxValue := 0.0;
  FHasMinValue := False;
  FHasMaxValue := False;
  FMaxDecimals := FLOAT_EDIT_DEFAULT_MAX_DECIMALS;
end;

procedure TCustomFloatEdit2.Assign(Source: TPersistent);
var
  lcSource: TCustomFloatEdit2;
begin
  inherited Assign(Source);
  if Source is TCustomFloatEdit2 then
  begin
    lcSource := Source as TCustomFloatEdit2;
    MinValue := lcSource.MinValue;
    MaxValue := lcSource.MaxValue;
    MaxDecimals := lcSource.MaxDecimals;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
end;

function TCustomFloatEdit2.ChopToRange(eValue: Extended): Extended;
begin
  Result := eValue;
  if HasMaxValue and (eValue > MaxValue) then
    Result := MaxValue;
  if HasMinValue and (eValue < MinValue) then
    Result := MinValue;
end;

function TCustomFloatEdit2.GetValue: Extended;
begin
  if Text = '' then
    Result := 0.0
  else
    Result := StrToFloatDef(Text, 0.0);
end;

function TCustomFloatEdit2.IsInRange(eValue: Extended): Boolean;
begin
  Result := True;
  if HasMaxValue and (eValue > MaxValue) then
    Result := False;
  if HasMinValue and (eValue < MinValue) then
    Result := False;
end;

function TCustomFloatEdit2.IsInRange(sValue: string): Boolean;
var
  leValue: Extended;
begin
  if sValue = '' then
    leValue := 0.0
  else
    leValue := StrToFloatDef(sValue, 0.0);
  Result := IsInRange(leValue);
end;

procedure TCustomFloatEdit2.KeyPress(var Key: Char);
var
  lsNewText: string;
  iDotPos: Integer;
begin
  { not intersted in control chars }
  if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    Exit;
  end;

  { allow only digits, '.' and minus sign }
  if not CharIsNumber(Key) then
    Key := #0;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first Char }
    else
    if SelStart <> 0 then
      Key := #0;

    { only 1 minus sign }
    if StrLeft(lsNewText, 2) = '--' then
      Key := #0;
  end;

  { no leading zeros }
  if (SelStart = 0) and (Key = '0') and (StrLeft(lsNewText, 1) = '0') then
    Key := #0;

  { no leading '-0' }
  if (SelStart = 1) and (Key = '0') and (StrLeft(lsNewText, 2) = '-0') then
    Key := #0;

  iDotPos := Pos('.', Text);

  if Key = '.' then
  begin
    { allow only one dot, but we can overwrite it with another }
    if (iDotPos > 0) and not ((SelLength > 0) and (SelStart <= iDotPos) and ((SelStart + SelLength) >= iDotPos)) then
      Key := #0
  end;

  { check number of decimal digits }
  if (iDotPos > 0) and (SelStart > iDotPos) and
    ((Length(Text) - iDotPos) >= MaxDecimals) then
  begin
    Key := #0;
  end;

  { disallow the keypress if the value would go out of range }
  lsNewText := GetChangedText(Text, SelStart, SelLength, Key);
  if not IsInRange(lsNewText) then
    Key := #0;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TCustomFloatEdit2.SetMaxDecimals(iNewValue: Integer);
begin
  if iNewValue < 0 then
    iNewValue := 0;
  if iNewValue > FLOAT_EDIT_MAX_MAX_DECIMALS then
    iNewValue := FLOAT_EDIT_MAX_MAX_DECIMALS;
  FMaxDecimals := iNewValue;
end;

procedure TCustomFloatEdit2.SetMaxValue(eNewValue: Extended);
begin
  FMaxValue := eNewValue;
  { make min value consistent }
  if MinValue > MaxValue then
    FMinValue := MaxValue;
end;

procedure TCustomFloatEdit2.SetMinValue(eNewValue: Extended);
begin
  FMinValue := eNewValue;
  { make max value consistent }
  if MaxValue < MaxValue then
    FMaxValue := MinValue;
end;

procedure TCustomFloatEdit2.SetValue(eNewValue: Extended);
begin
  Text := FloatToStr(ChopToRange(eNewValue));
end;

//=== TCustomYearEdit ========================================================

constructor TCustomYearEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { the system var is 'TwoDigitYearCenturyWindow', which by default will have a value of 50
    we then want FWindowsillYear to be 1950,
    if current date is 2020, and TwoDigitYearCenturyWindow is 90,
      then FWindowsillYear should be 1990
    if current date is 2120, and TwoDigitYearCenturyWindow is 90, (this code may last that long?)
      then FWindowsillYear should be 2090.
    It helps that FWindowsillYear is assumed to be in the past 100 years
  }
  FWindowsillYear := CenturyOfDate(now) + TwoDigitYearCenturyWindow;
  if FWindowsillYear > YearOfDate(now) then
    FWindowsillYear := FWindowsillYear - 100;
  HasMinValue := True;
  MinValue := 0;
  HasMaxValue := True;
  MaxValue := 9999;
end;

procedure TCustomYearEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomYearEdit;
begin
  inherited Assign(Source);
  if Source is TCustomYearEdit then
  begin
    lcSource := Source as TCustomYearEdit;
    WindowsillYear := lcSource.WindowsillYear;
  end;
end;

procedure TCustomYearEdit.DoExit;
begin
  { allow the user to type & on exit, translate any 2 digit years to 4 digits }
  FixYear;
  inherited DoExit;
end;

procedure TCustomYearEdit.FixYear;
begin
  Value := MakeYear4Digit(Value, WindowsillYear);
end;

function TCustomYearEdit.GetValue: Integer;
var
  li: Integer;
begin
  li := inherited GetValue;
  Result := MakeYear4Digit(li, WindowsillYear);
end;

procedure TCustomYearEdit.SetValue(iNewValue: Integer);
var
  li: Integer;
begin
  li := MakeYear4Digit(iNewValue, WindowsillYear);
  inherited SetValue(li);
end;

//=== TCustomCurrencyEdit ====================================================

constructor TCustomCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { default is right aligned }
  Alignment := taRightJustify;
  AutoSize := True;
  Text := '';
  { by default no min & max }
  FMinValue := 0.0;
  FMaxValue := 0.0;
  FHasMinValue := False;
  FHasMaxValue := False;
  FOriginalValue := 0;
  FOriginalValueOK := False;
end;

procedure TCustomCurrencyEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomCurrencyEdit;
begin
  inherited Assign(Source);
  if Source is TCustomCurrencyEdit then
  begin
    lcSource := Source as TCustomCurrencyEdit;
    MinValue := lcSource.MinValue;
    MaxValue := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
end;

function TCustomCurrencyEdit.ChopToRange(eValue: Currency): Currency;
begin
  Result := eValue;
  if HasMaxValue and (eValue > MaxValue) then
    Result := MaxValue;
  if HasMinValue and (eValue < MinValue) then
    Result := MinValue;
end;

function TCustomCurrencyEdit.GetValue: Currency;
var
  lsText: string;
begin
  if FOriginalValueOK then
  begin
    Result := FOriginalValue;
    Exit;
  end;
  { Currency string may include a decimal dot (.) so remove it wholesesale }
  lsText := StrDelete(CurrencyString, Text);
  lsText := StrStripNonNumberChars(lsText);
  if lsText = '' then
    Result := 0.0
  else
    Result := StrToCurrDef(lsText, 0.0);
end;

function TCustomCurrencyEdit.IsInRange(eValue: Currency): Boolean;
begin
  Result := True;
  if HasMaxValue and (eValue > MaxValue) then
    Result := False;
  if HasMinValue and (eValue < MinValue) then
    Result := False;
end;

function TCustomCurrencyEdit.IsInRange(sValue: string): Boolean;
var
  leValue: Extended;
begin
  if sValue = '' then
    leValue := 0.0
  else
    leValue := StrToCurrDef(sValue, 0.0);
  Result := IsInRange(leValue);
end;

procedure TCustomCurrencyEdit.KeyPress(var Key: Char);
var
  lsNewText: string;
  iDotPos: Integer;
begin
  { not interested in control chars }
  if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    if Key <> #0 then
      FOriginalValueOK := False;
    Exit;
  end;

  { allow only digits, '.' and minus sign }
  if not CharIsNumber(Key) then
    Key := #0;

  if Key <> #0 then
    FOriginalValueOK := False;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first Char }
    else
    if SelStart <> 0 then
      Key := #0;

    { only 1 minus sign }
    if StrLeft(lsNewText, 2) = '--' then
      Key := #0;
  end;

  { no leading zeros }
  if (SelStart = 0) and (Key = '0') and (StrLeft(lsNewText, 1) = '0') then
    Key := #0;

  { no leading '-0' }
  if (SelStart = 1) and (Key = '0') and (StrLeft(lsNewText, 2) = '-0') then
    Key := #0;

  iDotPos := Pos('.', Text);

  if Key = '.' then
  begin
    { allow only one dot, but we can overwrite it with another }
    if (iDotPos > 0) and not ((SelLength > 0) and (SelStart <= iDotPos) and ((SelStart + SelLength) >= iDotPos)) then
      Key := #0
  end;

  { check number of decimal digits }
  if (iDotPos > 0) and (SelStart > iDotPos) and
    ((Length(Text) - iDotPos) >= CurrencyDecimals) then
  begin
    Key := #0;
  end;

  { disallow the keypress if the value would go out of range }
  lsNewText := GetChangedText(Text, SelStart, SelLength, Key);
  if not IsInRange(lsNewText) then
    Key := #0;

  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TCustomCurrencyEdit.SetMaxValue(const peNewValue: Currency);
begin
  FMaxValue := peNewValue;
  { make min value consistent }
  if MinValue > MaxValue then
    FMinValue := MaxValue;
end;

procedure TCustomCurrencyEdit.SetMinValue(const peNewValue: Currency);
begin
  FMinValue := peNewValue;
  { make max value consistent }
  if MaxValue < MaxValue then
    FMaxValue := MinValue;
end;

procedure TCustomCurrencyEdit.SetValue(const peNewValue: Currency);
begin
  FOriginalValue := peNewValue;
  Text := Format(MONEY_FORMAT, [ChopToRange(peNewValue)]);
  FOriginalValueOK := True;
end;

//=== TCustomFormattedCurrencyEdit ===========================================

constructor TCustomFormattedCurrencyEdit.Create(AOwner: TComponent);
begin
  FSupressOnChange := True;
  FChanging := False;
  inherited Create(AOwner);
end;

procedure TCustomFormattedCurrencyEdit.Loaded;
begin
  inherited Loaded;
  FSupressOnChange := False;
end;

procedure TCustomFormattedCurrencyEdit.Change;
begin
  if not FSupressOnChange then
    inherited Change;
  { change after lose focus? o-er! }
  if not Focused and not FChanging then
  begin
    FChanging := True;
    try
      FormatText;
    finally
      FChanging := False;
    end;
  end;
end;

procedure TCustomFormattedCurrencyEdit.DoEnter;
begin
  FSupressOnChange := True;
  try
    UnFormatText;
    inherited DoEnter;
  finally
    FSupressOnChange := False;
  end;
end;

procedure TCustomFormattedCurrencyEdit.DoExit;
begin
  FSupressOnChange := True;
  try
    inherited DoExit;
    FormatText;
  finally
    FSupressOnChange := False;
  end;
end;

procedure TCustomFormattedCurrencyEdit.FormatText;
begin
  Text := Format(MONEY_FORMAT, [Value]);
end;

procedure TCustomFormattedCurrencyEdit.UnFormatText;
var
  lsText: string;
begin
  lsText := StrDelete(CurrencyString, Text);
  lsText := StrStripNonNumberChars(lsText);
  Text := lsText;
end;

{ this sets the text to then non-formatted value, the reads it, then
  sets it to the formatted value. This could be improved }

procedure TCustomFormattedCurrencyEdit.SetValue(const peNewValue: Currency);
begin
  inherited SetValue(peNewValue);
  FormatText;
 { what if the control has focus? It won't if this occurs as the result of a button click }
  FOriginalValueOK := True;
end;

end.

