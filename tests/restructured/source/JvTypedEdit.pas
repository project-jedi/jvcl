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

{Comments - controls to capure numbers of various kinds (Int, Float, Money)
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
  { delphi } Classes,
  { local } JvEdit;

type
  TCustomIntegerEdit = class(TJvCustomEdit)
  private
    { property implementation }
    fiMaxValue, fiMinValue: integer;
    fbHasMaxValue, fbHasMinValue: boolean;

  protected
    { overrides }
    procedure KeyPress(var key: char); override;

    { property implementation }
    procedure SetValue(iNewValue: integer); virtual;
    function GetValue: integer; virtual;

    procedure SetMinValue(iNewValue: integer);
    procedure SetMaxValue(iNewValue: integer);

   { worker procs }
    function ChopToRange(iValue: integer): integer;
    function IsInRange(iValue: integer): boolean; overload;
    function IsInRange(sValue: string): boolean; overload;

    property MaxValue: integer read fiMaxValue write fiMaxValue;
    property MinValue: integer read fiMinValue write fiMinValue;

    property HasMaxValue: boolean read fbHasMaxValue write fbHasMaxValue;
    property HasMinValue: boolean read fbHasMinValue write fbHasMinValue;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    property Value: integer read GetValue write SetValue;
  published
    property AutoSize default true;


  end;

  { to capture and display float values }
  TCustomFloatEdit2 = class(TJvCustomEdit)
  private
    { property implementation }
    feMaxValue, feMinValue: extended;
    fbHasMaxValue, fbHasMinValue: boolean;
    fiMaxDecimals: integer;

  protected
    { overrides }
    procedure KeyPress(var key: char); override;

    { property implementation }
    procedure SetValue(eNewValue: extended);
    function GetValue: extended;

    procedure SetMinValue(eNewValue: extended);
    procedure SetMaxValue(eNewValue: extended);

    procedure SetMaxDecimals(iNewValue: integer);

   { worker procs }
    function ChopToRange(eValue: extended): extended;
    function IsInRange(eValue: extended): boolean; overload;
    function IsInRange(sValue: string): boolean; overload;

    property MaxValue: extended read feMaxValue write feMaxValue;
    property MinValue: extended read feMinValue write feMinValue;
    property MaxDecimals: integer read fiMaxDecimals write SetMaxDecimals;

    property HasMaxValue: boolean read fbHasMaxValue write fbHasMaxValue;
    property HasMinValue: boolean read fbHasMinValue write fbHasMinValue;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    property Value: extended read GetValue write SetValue;
  published
    property AutoSize default true;
  end;


  { control to capture a year
    int edit with automatic 2 digit -> 4 digit expansion
    in a range controlld by the WindowsillYear
    which is initialised from the program/system settings  }
  TCustomYearEdit = class(TCustomIntegerEdit)
  private
    fiWindowsillYear: integer;
  protected
    procedure DoExit; override;

    procedure SetValue(iNewValue: integer); override;
    function GetValue: integer; override;

    procedure FixYear;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    property WindowsillYear: integer read fiWindowsillYear write fiWindowsillYear;

  end;

{ to capture and display currency values
    when the control has focus, the number is displayed simply
    when the control does not have focus, the number is formatted
    as per control panel currency settings
  }
  TCustomCurrencyEdit = class(TJvCustomEdit)
  private
    { property implementation }
    feMaxValue, feMinValue: currency;
    fbHasMaxValue, fbHasMinValue: boolean;
    fcOriginalValue: Currency;
    fbOriginalValueOK: Boolean;

  protected

    { overrides }
    procedure KeyPress(var key: char); override;

    { property implementation }
    procedure SetValue(const peNewValue: currency); virtual;
    function GetValue: currency;

    procedure SetMinValue(const peNewValue: currency);
    procedure SetMaxValue(const peNewValue: currency);


   { worker procs }
    function ChopToRange(eValue: currency): currency;
    function IsInRange(eValue: currency): boolean; overload;
    function IsInRange(sValue: string): boolean; overload;

    property MaxValue: currency read feMaxValue write feMaxValue;
    property MinValue: currency read feMinValue write feMinValue;

    property HasMaxValue: boolean read fbHasMaxValue write fbHasMaxValue;
    property HasMinValue: boolean read fbHasMinValue write fbHasMinValue;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    property Value: currency read GetValue write SetValue;
  published
    property AutoSize default true;
  end;


 { another bit of functionality
    - when the control does not have focus, display a formatted currency string
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
      this is more applicable to Int & Float edits
    }
  TCustomFormattedCurrencyEdit = class(TCustomCurrencyEdit)
  private
    fbSupressOnChange: boolean;
    fbChanging: Boolean;

  protected
    procedure UnFormatText; virtual;

    procedure SetValue(const peNewValue: currency); override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;

  public
    procedure Loaded; override;
    procedure FormatText; virtual;
    constructor Create(AOwner: TComponent); override;

  end;

type

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
  end;

implementation

uses
  { delphi } SysUtils,
  { jcl } JclStrings, JclDateTime,
  { local } JvFunctions;

const
  MONEY_FORMAT = '%m'; // see help on "format strings"
{--------------------------------------------------------------------------------
  Custom Integer Edit }

function TCustomIntegerEdit.ChopToRange(iValue: integer): integer;
begin
  Result := iValue;

  if HasMaxValue and (iValue > MaxValue) then
    Result := fiMaxValue;

  if HasMinValue and (iValue < MinValue) then
    Result := fiMinValue;
end;

constructor TCustomIntegerEdit.Create(AOwner: TComponent);
begin
  inherited;

  { default is right aligned }
  Alignment := taRightJustify;
  Text      := '';
  AutoSize  := true;

  { by default no min & max }
  fiMinValue    := 0;
  fiMaxValue    := 0;
  fbHasMinValue := False;
  fbHasMaxValue := False;
end;

procedure TCustomIntegerEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomIntegerEdit;
begin
  inherited;

  if Source is TCustomIntegerEdit then
  begin
    lcSource    := Source as TCustomIntegerEdit;
    MinValue    := lcSource.MinValue;
    MaxValue    := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
end;

function TCustomIntegerEdit.GetValue: integer;
begin
  if Text = '' then
    Result := 0
  else
    Result := StrToIntDef(Text, 0);
end;

function TCustomIntegerEdit.IsInRange(iValue: integer): boolean;
begin
  Result := True;

  if HasMaxValue and (iValue > MaxValue) then
    Result := False;

  if HasMinValue and (iValue < MinValue) then
    Result := False;
end;

function TCustomIntegerEdit.IsInRange(sValue: string): boolean;
var
  liValue: integer;
begin
  if sValue = '' then
    liValue := 0
  else
    liValue := StrToIntDef(sValue, 0);

  Result := IsInRange(liValue);
end;

procedure TCustomIntegerEdit.KeyPress(var key: char);
var
  lsNewText: string;
begin
  { not intersted in control chars }
  if (Ord(Key)) < Ord(' ') then
  begin
    if Key = #13 then Key := #0 else
    inherited KeyPress(Key);
    exit;
  end;

  { allow only digits and minus sign }
  if not CharIsDigit(Key) and (Key <> '-') then
    Key := #0;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first char }
    else if SelStart <> 0 then
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
    inherited;
end;

procedure TCustomIntegerEdit.SetMaxValue(iNewValue: integer);
begin
  fiMaxValue := iNewValue;

  { make min value consistent }
  if MinValue > MaxValue then
    fiMinValue := MaxValue;
end;

procedure TCustomIntegerEdit.SetMinValue(iNewValue: integer);
begin
  fiMinValue := iNewValue;

  { make max value consistent }
  if MaxValue < MaxValue then
    fiMaxValue := MinValue;
end;

procedure TCustomIntegerEdit.SetValue(iNewValue: integer);
begin
  Text := IntToStr(ChopToRange(iNewValue));
end;

{-------------------------------------------------------------------------------
   TCustomFloatEdit2 }

const
{ The extended data type handles up to 20 digits - no point in
   setting max decimals to more than this }
  FLOAT_EDIT_MAX_MAX_DECIMALS = 20;

{ most data capture is more like this -
 typing in 19 decimal digits usually indicates forehead-on-the-keyboard syndrome }
  FLOAT_EDIT_DEFAULT_MAX_DECIMALS = 5;



procedure TCustomFloatEdit2.Assign(Source: TPersistent);
var
  lcSource: TCustomFloatEdit2;
begin
  inherited;

  if Source is TCustomFloatEdit2 then
  begin
    lcSource    := Source as TCustomFloatEdit2;
    MinValue    := lcSource.MinValue;
    MaxValue    := lcSource.MaxValue;
    MaxDecimals := lcSource.MaxDecimals;

    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
end;

function TCustomFloatEdit2.ChopToRange(eValue: extended): extended;
begin
  Result := eValue;

  if HasMaxValue and (eValue > MaxValue) then
    Result := MaxValue;

  if HasMinValue and (eValue < MinValue) then
    Result := MinValue;
end;

constructor TCustomFloatEdit2.Create(AOwner: TComponent);
begin
  inherited;

  { default is right aligned }
  Alignment := taRightJustify;
  AutoSize  := true;
  Text      := '';

  { by default no min & max }
  feMinValue    := 0.0;
  feMaxValue    := 0.0;
  fbHasMinValue := False;
  fbHasMaxValue := False;
  fiMaxDecimals := FLOAT_EDIT_DEFAULT_MAX_DECIMALS;
end;

function TCustomFloatEdit2.GetValue: extended;
begin
  if Text = '' then
    Result := 0.0
  else
    Result := StrToFloatDef(Text, 0.0);
end;

function TCustomFloatEdit2.IsInRange(eValue: extended): boolean;
begin
  Result := True;

  if HasMaxValue and (eValue > MaxValue) then
    Result := False;

  if HasMinValue and (eValue < MinValue) then
    Result := False;
end;

function TCustomFloatEdit2.IsInRange(sValue: string): boolean;
var
  leValue: extended;
begin
  if sValue = '' then
    leValue := 0.0
  else
    leValue := StrToFloatDef(sValue, 0.0);

  Result := IsInRange(leValue);
end;

procedure TCustomFloatEdit2.KeyPress(var key: char);
var
  lsNewText: string;
  iDotPos:   integer;
begin
  { not intersted in control chars }
  if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    exit;
  end;

  { allow only digits, '.' and minus sign }
  if not CharIsNumber(Key) then
    Key := #0;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first char }
    else if SelStart <> 0 then
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
    inherited;
end;

procedure TCustomFloatEdit2.SetMaxDecimals(iNewValue: integer);
begin
  if iNewValue < 0 then
    iNewValue := 0;
  if iNewValue > FLOAT_EDIT_MAX_MAX_DECIMALS then
    iNewValue := FLOAT_EDIT_MAX_MAX_DECIMALS;

  fiMaxDecimals := iNewValue;
end;

procedure TCustomFloatEdit2.SetMaxValue(eNewValue: extended);
begin
  feMaxValue := eNewValue;

  { make min value consistent }
  if MinValue > MaxValue then
    feMinValue := MaxValue;
end;

procedure TCustomFloatEdit2.SetMinValue(eNewValue: extended);
begin
  feMinValue := eNewValue;

  { make max value consistent }
  if MaxValue < MaxValue then
    feMaxValue := MinValue;
end;

procedure TCustomFloatEdit2.SetValue(eNewValue: extended);
begin
  Text := FloatToStr(ChopToRange(eNewValue));
end;

{-------------------------------------------------------------------------------
  TCustomYearEdit }

procedure TCustomYearEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomYearEdit;
begin
  inherited;

  if Source is TCustomYearEdit then
  begin
    lcSource       := Source as TCustomYearEdit;
    WindowsillYear := lcSource.WindowsillYear;
  end;
end;

constructor TCustomYearEdit.Create(AOwner: TComponent);
begin
  inherited;

  { the system var is 'TwoDigitYearCenturyWindow', which by default will have a value of 50
    we then want fiWindowsillYear to be 1950,
    if current date is 2020, and TwoDigitYearCenturyWindow is 90,
      then fiWindowsillYear should be 1990
    if current date is 2120, and TwoDigitYearCenturyWindow is 90, (this code may last that long?)
      then fiWindowsillYear should be 2090.
    It helps that fiWindowsillYear is assumed to be in the past 100 years
  }
  fiWindowsillYear := CenturyOfDate(now) + TwoDigitYearCenturyWindow;
  if fiWindowsillYear > YearOfDate(now) then
    fiWindowsillYear := fiWindowsillYear - 100;

  HasMinValue := True;
  MinValue    := 0;
  HasMaxValue := True;
  MaxValue    := 9999;
end;

procedure TCustomYearEdit.DoExit;
begin
  { allow the user to type & on exit, translate any 2 digit years to 4 digits }
  FixYear;
  inherited;
end;

procedure TCustomYearEdit.FixYear;
begin
  Value := MakeYear4Digit(Value, WindowsillYear);
end;

function TCustomYearEdit.GetValue: integer;
var
  li: integer;
begin
  li := inherited GetValue;
  Result := MakeYear4Digit(li, WindowsillYear);
end;

procedure TCustomYearEdit.SetValue(iNewValue: integer);
var
  li: integer;
begin
  li := MakeYear4Digit(iNewValue, WindowsillYear);
  inherited SetValue(li);
end;

{ TCustomCurrencyEdit }

constructor TCustomCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited;

  { default is right aligned }
  Alignment := taRightJustify;
  AutoSize  := true;
  Text      := '';

  { by default no min & max }
  feMinValue    := 0.0;
  feMaxValue    := 0.0;
  fbHasMinValue := False;
  fbHasMaxValue := False;

  fcOriginalValue := 0;
  fbOriginalValueOK := False;
end;


procedure TCustomCurrencyEdit.Assign(Source: TPersistent);
var
  lcSource: TCustomCurrencyEdit;
begin
  inherited;

  if Source is TCustomCurrencyEdit then
  begin
    lcSource    := Source as TCustomCurrencyEdit;
    MinValue    := lcSource.MinValue;
    MaxValue    := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
  end;
end;

function TCustomCurrencyEdit.ChopToRange(eValue: currency): currency;
begin
  Result := eValue;

  if HasMaxValue and (eValue > MaxValue) then
    Result := MaxValue;

  if HasMinValue and (eValue < MinValue) then
    Result := MinValue;
end;

function TCustomCurrencyEdit.GetValue: currency;
var
  lsText: string;
begin
  if fbOriginalValueOK then
  begin
    Result := fcOriginalValue;
    exit;
  end;

  { currency string may include a decimal dot (.) so remove it wholesesale }
  lsText := StrDelete(CurrencyString, Text);

  lsText := StrStripNonNumberChars(lsText);

  if lsText = '' then
    Result := 0.0
  else
    Result := StrToCurrDef(lsText, 0.0);
end;

function TCustomCurrencyEdit.IsInRange(eValue: currency): boolean;
begin
  Result := True;

  if HasMaxValue and (eValue > MaxValue) then
    Result := False;

  if HasMinValue and (eValue < MinValue) then
    Result := False;
end;

function TCustomCurrencyEdit.IsInRange(sValue: string): boolean;
var
  leValue: extended;
begin
  if sValue = '' then
    leValue := 0.0
  else
    leValue := StrToCurrDef(sValue, 0.0);

  Result := IsInRange(leValue);
end;


procedure TCustomCurrencyEdit.KeyPress(var key: char);
var
  lsNewText: string;
  iDotPos:   integer;
begin

  { not interested in control chars }
  if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    if Key <> #0 then
      fbOriginalValueOK := False;

    exit;
  end;

  { allow only digits, '.' and minus sign }
  if not CharIsNumber(Key) then
    Key := #0;

  if Key <> #0 then
    fbOriginalValueOK := False;

  if Key = '-' then
  begin
    { are any neg. numbers in range ? }
    if HasMinValue and (MinValue >= 0) then
      Key := #0
    { minus sign only as first char }
    else if SelStart <> 0 then
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
    inherited;
end;

procedure TCustomCurrencyEdit.SetMaxValue(const peNewValue: currency);
begin
  feMaxValue := peNewValue;

  { make min value consistent }
  if MinValue > MaxValue then
    feMinValue := MaxValue;
end;

procedure TCustomCurrencyEdit.SetMinValue(const peNewValue: currency);
begin
  feMinValue := peNewValue;

  { make max value consistent }
  if MaxValue < MaxValue then
    feMaxValue := MinValue;
end;

procedure TCustomCurrencyEdit.SetValue(const peNewValue: currency);
begin
  fcOriginalValue := peNewValue;
  Text := Format(MONEY_FORMAT, [ChopToRange(peNewValue)]);
  fbOriginalValueOK := True;
end;

{-------------------------------------------------------------------------------
 TFormattedCurrencyEdit }

procedure TCustomFormattedCurrencyEdit.Change;
begin
  if not fbSupressOnChange then
    inherited;

  { change after lose focus? o-er! }
  if not Focused and (not fbChanging) then
  begin
    fbChanging := True;
    try
      FormatText;
    finally
      fbChanging := False;
    end;
  end;

end;

procedure TCustomFormattedCurrencyEdit.DoEnter;
begin
  fbSupressOnChange := True;
  try
    UnFormatText;
    inherited;
  finally
    fbSupressOnChange := False;
  end;
end;

procedure TCustomFormattedCurrencyEdit.DoExit;
begin
  fbSupressOnChange := True;
  try
    inherited;
    FormatText;
  finally
    fbSupressOnChange := False;
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
procedure TCustomFormattedCurrencyEdit.SetValue(const peNewValue: currency);
begin
  inherited;
  FormatText;
 { what if the control has focus? It won't if this occurs as the result of a button click }
 fbOriginalValueOK := True;
end;

constructor TCustomFormattedCurrencyEdit.Create(AOwner: TComponent);
begin
  fbSupressOnChange := True;
  fbChanging := False;
  inherited;
end;

procedure TCustomFormattedCurrencyEdit.Loaded;
begin
  inherited;
  fbSupressOnChange := False;
end;

end.
