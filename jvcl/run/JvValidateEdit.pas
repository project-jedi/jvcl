{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is JvValidateEdit, released on 20 February 2003,
  by Christopher Latta
Portions created by Christopher Latta are Copyright (C) 2003 Christopher Latta.
All Rights Reserved.

Contributor(s): Peter Thornqvist
                Peter Schraut (http://www.console-dev.de)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
TJvValidateFormat uses the SysUtils.Format function to format numeric values.
While this uses the Windows regional settings for the currency symbol, decimal
separator and thousands separator, it does not format using the negative symbol,
negative number format, negative currency format and positive currency format.
This could be rectified by a custom-written formatting routine.

-----------------------------------------------------------------------------}
// $Id$

unit JvValidateEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Controls, Graphics,
  SysUtils, Classes, FMTBcd,
  JvEdit, JvDataSourceIntf;

type
  TJvValidateEditDisplayFormat = (dfAlphabetic, dfAlphaNumeric, dfBinary,
    dfCheckChars, dfCurrency, dfCustom, dfFloat, dfFloatGeneral, dfHex, dfInteger,
    dfNonCheckChars, dfNone, dfOctal, dfPercent, dfScientific, dfYear, dfDecimal,
    dfIdentifier, dfFloatFixed, dfBcd);

  TJvValidateEditCriticalPointsCheck = (cpNone, cpMinValue, cpMaxValue, cpBoth);

  TJvCustomValidateEdit = class;

  TJvValidateEditDataConnector = class(TJvFieldDataConnector)
  private
    FEdit: TJvCustomValidateEdit;
    FNullValue: Variant;
    procedure SetNullValue(const Value: Variant);
    function IsNullValueStored: Boolean;
  protected
    procedure RecordChanged; override;
    procedure UpdateData; override;
  public
    constructor Create(AEdit: TJvCustomValidateEdit);
    procedure Assign(Source: TPersistent); override;
    property Control: TJvCustomValidateEdit read FEdit;
  published
    property NullValue: Variant read FNullValue write SetNullValue stored IsNullValueStored;
  end;

  TJvValidateEditCriticalPoints = class(TPersistent)
  private
    FCheckPoints: TJvValidateEditCriticalPointsCheck;
    FColorAbove: TColor;
    FColorBelow: TColor;
    FMaxValue: Double;
    FMinValue: Double;
    FMaxValueIncluded: Boolean;
    FMinValueIncluded: Boolean;
    FOnChange: TNotifyEvent;
    FDefCheckPoints: TJvValidateEditCriticalPointsCheck;
    FDefColorAbove: TColor;
    FDefColorBelow: TColor;
    procedure DoChanged;
    procedure SetMinValue(NewValue: Double);
    procedure SetMaxValue(NewValue: Double);
    procedure SetColorAbove(NewValue: TColor);
    procedure SetColorBelow(NewValue: TColor);
    procedure SetCheckPoints(NewValue: TJvValidateEditCriticalPointsCheck);
    function IsCheckPointsStored: Boolean;
    function IsColorAboveStored: Boolean;
    function IsColorBelowStored: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure SetDefaults(ACheckPoints: TJvValidateEditCriticalPointsCheck;
      AColorAbove, AColorBelow: TColor);
    constructor Create;
  published
    property CheckPoints: TJvValidateEditCriticalPointsCheck read FCheckPoints
      write SetCheckPoints stored IsCheckPointsStored;
    property ColorAbove: TColor read FColorAbove write SetColorAbove stored IsColorAboveStored;
    property ColorBelow: TColor read FColorBelow write SetColorBelow stored IsColorBelowStored;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValueIncluded: Boolean read FMaxValueIncluded write FMaxValueIncluded;
    property MinValueIncluded: Boolean read FMinValueIncluded write FMinValueIncluded;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvCustomTextValidateEvent = procedure(Sender: TObject; Key: Char;
    const AText: string; const Pos: Integer; var IsValid: Boolean) of object;
  TJvCustomIsValidEvent = procedure(Sender: TObject; var IsValid: Boolean) of object;
  TJvCustomDecimalRoundingEvent = procedure(Sender: TObject; var DecimalRoundedValue: Double;
    const Value: Double) of object;

  TJvCustomValidateEdit = class(TJvCustomEdit)
  private
    FSelfChange: Boolean;
    FCheckChars: string;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: TJvValidateEditDisplayFormat;
    FEditText: string;
    FHasMaxValue: Boolean;
    FHasMinValue: Boolean;
    FMaxValue: Double;
    FMinValue: Double;
    FOnCustomValidate: TJvCustomTextValidateEvent;
    FOnValueChanged: TNotifyEvent;
    FZeroEmpty: Boolean;
    FEnterText: string;
    FDisplayPrefix: string;
    FDisplaySuffix: string;
    FCriticalPoints: TJvValidateEditCriticalPoints;
    FStandardFontColor: TColor;
    FAutoAlignment: Boolean;
    FTrimDecimals: Boolean;
    FOldFontChange: TNotifyEvent;
    FOnIsValid: TJvCustomIsValidEvent;
    FOnDecimalRounding: TJvCustomDecimalRoundingEvent;
    FAllowEmpty: Boolean;
    FEnforcingMinMaxValue: Boolean;
    FForceDecimalSeparatorInput: Boolean;
    FKeepPrefixSuffixIntact: Boolean;
    FHidePrefixSuffixIfEmpty: Boolean;
    FLastDownKey: Word;
    procedure DisplayText;
    function ScientificStrToFloat(SciString: string): Double;
    procedure SetHasMaxValue(NewValue: Boolean);
    procedure SetHasMinValue(NewValue: Boolean);
    procedure SetMaxValue(NewValue: Double);
    procedure SetMinValue(NewValue: Double);
    procedure SetDecimalPlaces(NewValue: Cardinal);
    procedure SetDisplayFormat(NewValue: TJvValidateEditDisplayFormat);
    procedure SetZeroEmpty(NewValue: Boolean);
    function GetAsInteger: Int64;
    procedure SetAsInteger(NewValue: Int64);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(NewValue: Currency);
    function GetAsFloat: Double;
    procedure SetAsFloat(NewValue: Double);
    function GetAsBcd: TBcd;
    procedure SetAsBcd(const NewValue: TBcd);
    function GetValue: Variant;
    procedure SetValue(NewValue: Variant);
    procedure SetCheckChars(const NewValue: string);
    function IsCheckCharsStored: Boolean;
    function CurrRangeValue(CheckValue: Currency): Currency;
    function FloatRangeValue(CheckValue: Double): Double;
    function IntRangeValue(CheckValue: Int64): Int64;
    function BcdRangeValue(const CheckValue: TBcd): TBcd;
    function GetEditText: string;
    procedure SetEditText(const NewValue: string);
    procedure ChangeText(const NewValue: string);
    function BaseToInt(const BaseValue: string; Base: Byte): Int64;
    function IntToBase(NewValue: Int64; Base: Byte): string;
    procedure DoValueChanged;
    procedure SetDisplayPrefix(const NewValue: string);
    procedure SetDisplaySuffix(const NewValue: string);
    procedure CriticalPointsChange(Sender: TObject);
    procedure SetFontColor;
    procedure FontChange(Sender: TObject);
    procedure EnforceMaxValue;
    procedure EnforceMinValue;
    procedure SetTrimDecimals(const Value: Boolean);
    function GetUnprefixedUnsuffixedText(const Value: string): string;
  protected
    function IsValidChar(const S: string; var Key: Char; Posn: Integer): Boolean; virtual;
    function MakeValid(const ParseString: string): string;virtual;
    procedure Change; override;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure SetText(const NewValue: TCaption); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function DoValidate(const Key: Char; const AText: string; const Posn: Integer): Boolean;
    procedure Loaded; override;

    function CreateDataConnector: TJvFieldDataConnector; override;

    property CheckChars: string read FCheckChars write SetCheckChars stored IsCheckCharsStored;
    property TrimDecimals: Boolean read FTrimDecimals write SetTrimDecimals;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces;
    property DisplayFormat: TJvValidateEditDisplayFormat read FDisplayFormat write SetDisplayFormat;
    property EditText: string read GetEditText write SetEditText;
    property HasMaxValue: Boolean read FHasMaxValue write SetHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write SetHasMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property OnCustomValidate: TJvCustomTextValidateEvent read FOnCustomValidate write FOnCustomValidate;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
    property OnDecimalRounding: TJvCustomDecimalRoundingEvent read FOnDecimalRounding write FOnDecimalRounding;
    property Value: Variant read GetValue write SetValue stored False;
    property AllowEmpty: Boolean read FAllowEmpty write FAllowEmpty;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty;
    property DisplayPrefix: string read FDisplayPrefix write SetDisplayPrefix;
    property DisplaySuffix: string read FDisplaySuffix write SetDisplaySuffix;
    property KeepPrefixSuffixIntact: Boolean read FKeepPrefixSuffixIntact write FKeepPrefixSuffixIntact default False;
    property HidePrefixSuffixIfEmpty: Boolean read FHidePrefixSuffixIfEmpty write FHidePrefixSuffixIfEmpty default True;
    property CriticalPoints: TJvValidateEditCriticalPoints read FCriticalPoints write FCriticalPoints;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    property OnIsValid: TJvCustomIsValidEvent read FOnIsValid write FOnIsValid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsValid: Boolean; virtual; // fires OnIsValid if assigned
    function IsEmpty: Boolean; override;

    // When the DecimalSeparator variable has changed, one should call
    // RecalcCheckChars to ensure that it contains the new value (Mantis 4682)
    procedure RecalcCheckChars;

    procedure Assign(Source: TPersistent); override;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBcd: TBcd read GetAsBcd write SetAsBcd;

    // If true and the user presses the VK_DECIMAL key, the key read in KeyPress
    // will always be replaced by the value of DecimalSeparator. This is made
    // to overcome the problem where some keyboard layouts send "." instead of
    // the decimal separator when using the decimal key on the numerical keypad.
    // The most commonly encountered layout is the French AZERTY one.
    // Note that this property will be set automatically to True by the
    // constructor when the conversion of VK_DECIMAL into a character does not
    // return the DecimalSeparator value
    property ForceDecimalSeparatorInput: Boolean read FForceDecimalSeparatorInput write FForceDecimalSeparatorInput;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvValidateEdit = class(TJvCustomValidateEdit)
  published
    property AllowEmpty default False;
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property AutoAlignment default True;

    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property Flat;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentFlat;
    property OnEndDock;
    property OnStartDock;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BorderStyle;
    property Caret;
    property CheckChars;
    property CharCase;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property CriticalPoints;
    property DisabledColor;
    property DisabledTextColor;
    property TrimDecimals default False;
    property DisplayFormat default dfInteger;
    property DecimalPlaces default 0;
    property DisplayPrefix;
    property DisplaySuffix;
    property DragMode;
    property EditText;
    property Enabled;
    property Font;
    property HasMaxValue default False;
    property HasMinValue default False;
    property HideSelection;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text stored False;
    property Value;
    property Visible;
    property ZeroEmpty default False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomValidate;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnValueChanged;
    property OnIsValid;
    property OnDecimalRounding;
    property DataConnector;
    property KeepPrefixSuffixIntact;
    property HidePrefixSuffixIfEmpty;

    {$IFDEF COMPILER12_UP}
    //property NumbersOnly;
    {$ENDIF}
    {$IFDEF COMPILER14_UP}
    property Touch;
    {$ENDIF COMPILER14_UP}
    property TextHint;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
      '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  VarUtils, Variants,
  JclStrings, JvJCLUtils, JvResources, JclSysUtils;

function IsGreater(Value, MaxValue: Double; MaxValueIncluded: Boolean): Boolean;
begin
  if MaxValueIncluded then
    Result := Value >= MaxValue
  else
    Result := Value > MaxValue;
end;

function IsLower(Value, MinValue: Double; MinValueIncluded: Boolean): Boolean;
begin
  if MinValueIncluded then
    Result := Value <= MinValue
  else
    Result := Value < MinValue;
end;

function BcdToStrFDefault(const Bcd: TBcd; DecimalPlaces: Integer): string;
begin
  Result := BcdToStrF(Bcd, ffNumber, 64, DecimalPlaces);
end;

function BcdToInt64(const Bcd: TBcd; Truncate: Boolean = False): Int64;
var
  ABcd: TBcd;
begin
  if Truncate and (BcdScale(Bcd) > 0) then
    NormalizeBcd(Bcd, ABcd, Bcd.Precision, 0)
  else
    ABcd := Bcd;
  Result := StrToInt64(BcdToStr(ABcd));
end;

function IsBcdZero(const Bcd: TBcd): Boolean;
var
  I, Precision: Integer;
begin
  Result := False;
  I := 0;
  Precision := Bcd.Precision;
  while I < Precision div 2 do
  begin
    if Byte(Bcd.Fraction[I]) <> 0 then
      Exit;
    Inc(I);
  end;
  Result := (Precision mod 2 = 0) or (Byte(Bcd.Fraction[I]) shr 4 = 0);
end;

procedure ZeroBcd(var Bcd: TBcd);
var
  I: Integer;
begin
  Bcd.Precision := 10;
  Bcd.SignSpecialPlaces := 2;
  for I := 0 to 31 do
    Bcd.Fraction[I] := 0;
end;

//=== { TJvValidateEditDataConnector } =======================================

procedure TJvValidateEditDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvFieldDataConnector then
  begin
    NullValue := TJvValidateEditDataConnector(Source).NullValue;
  end;
end;

constructor TJvValidateEditDataConnector.Create(AEdit: TJvCustomValidateEdit);
begin
  inherited Create;
  FEdit := AEdit;
  VarClear(FNullValue);
end;

function TJvValidateEditDataConnector.IsNullValueStored: Boolean;
begin
  Result := not VarIsClear(NullValue);
end;

procedure TJvValidateEditDataConnector.RecordChanged;
begin
  if Field.IsValid then
  begin
    FEdit.ReadOnly := not Field.CanModify;
    if not Field.IsNull then
      FEdit.Value := Field.Value
    else
    if NullValue <> Null then
      FEdit.Value := NullValue
    else
      FEdit.Text := '';
  end
  else
  begin
    FEdit.Text := '';
    FEdit.ReadOnly := False;
  end;
end;

procedure TJvValidateEditDataConnector.SetNullValue(const Value: Variant);
begin
  if Value <> FNullValue then
  begin
    FNullValue := Value;
    Reset;
  end;
end;

procedure TJvValidateEditDataConnector.UpdateData;
begin
  if Field.CanModify and Field.IsValid then
  begin
    if FEdit.Value <> Null then
      Field.Value := FEdit.Value
    else
    if NullValue <> Null then
      Field.Value := FNullValue
    else
      RecordChanged;
  end;
end;

//=== { TJvCustomValidateEdit } ==============================================

constructor TJvCustomValidateEdit.Create(AOwner: TComponent);
var
  MappedDecimal: Cardinal;
const
  MAPVK_VK_TO_CHAR = 2;
begin
  inherited Create(AOwner);
  FSelfChange := False;
  FAutoAlignment := True;
  FCriticalPoints := TJvValidateEditCriticalPoints.Create;
  FCriticalPoints.OnChange := CriticalPointsChange;
  FDisplayFormat := dfInteger;
  FCheckChars := '01234567890';
  Alignment := taRightJustify;
  FEditText := '';
  Text := ''; // doesn't trigger OnChange because FEnterText = ''. That's what we want.
  AutoSize := True;
  FMinValue := 0;
  FMaxValue := 0;
  FHasMinValue := False;
  FHasMaxValue := False;
  FZeroEmpty := False;
  FHidePrefixSuffixIfEmpty := True;
  FStandardFontColor := Font.Color;
  FOldFontChange := Font.OnChange;
  Font.OnChange := FontChange;

  MappedDecimal := MapVirtualKey(VK_DECIMAL, MAPVK_VK_TO_CHAR);
  if MappedDecimal <> 0 then
    FForceDecimalSeparatorInput := Char(MappedDecimal) <> JclFormatSettings.DecimalSeparator;
end;

destructor TJvCustomValidateEdit.Destroy;
begin
  FreeAndNil(FCriticalPoints);
  inherited Destroy;
end;

procedure TJvCustomValidateEdit.Assign(Source: TPersistent);
var
  lcSource: TJvCustomValidateEdit;
begin
  if Source is TJvCustomValidateEdit then
  begin
    lcSource := TJvCustomValidateEdit(Source);
    CriticalPoints.Assign(lcSource.CriticalPoints);
    DisplayFormat := lcSource.DisplayFormat;
    DecimalPlaces := lcSource.DecimalPlaces;
    MinValue := lcSource.MinValue;
    MaxValue := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
    ZeroEmpty := lcSource.ZeroEmpty;
    AllowEmpty := lcSource.AllowEmpty;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCustomValidateEdit.Loaded;
begin
  // (obones) Why is this necessary? It overrides DecimalPlaces set to 0 by the user
{  if DisplayFormat = dfCurrency then
    if FDecimalPlaces = 0 then
      FDecimalPlaces := CurrencyDecimals;}
  DataConnector.Active := False;
  try
    EditText := FEditText;
  finally
    DataConnector.Active := True;
  end;
  inherited Loaded;
end;

function TJvCustomValidateEdit.CreateDataConnector: TJvFieldDataConnector;
begin
  Result := TJvValidateEditDataConnector.Create(Self);
end;

procedure TJvCustomValidateEdit.SetHasMaxValue(NewValue: Boolean);
begin
  if FHasMaxValue <> NewValue then
  begin
    FHasMaxValue := NewValue;
    if not (csLoading in ComponentState) then
      EnforceMaxValue;
  end;
end;

procedure TJvCustomValidateEdit.SetHasMinValue(NewValue: Boolean);
begin
  if FHasMinValue <> NewValue then
  begin
    FHasMinValue := NewValue;
    if not (csLoading in ComponentState) then
      EnforceMinValue;
  end;
end;

procedure TJvCustomValidateEdit.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    { make MinValue consistent }
    if FMinValue > FMaxValue then
      FMinValue := FMaxValue;
    if not (csLoading in ComponentState) then
      EnforceMaxValue;
  end;
end;

procedure TJvCustomValidateEdit.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    { make MaxValue consistent }
    if FMaxValue < FMinValue then
      FMaxValue := FMinValue;
    if not (csLoading in ComponentState) then
      EnforceMinValue;
  end;
end;

procedure TJvCustomValidateEdit.SetTrimDecimals(const Value: Boolean);
begin
  if Value <> FTrimDecimals then
  begin
    FTrimDecimals := Value;
    if not (csLoading in ComponentState) then
      EditText := FEditText;
  end;
end;

procedure TJvCustomValidateEdit.SetDecimalPlaces(NewValue: Cardinal);
begin
  if ControlState = [csReadingState] then
    FDecimalPlaces := NewValue
  else
  if FDisplayFormat in [dfCurrency, dfFloat, dfFloatGeneral, dfScientific, dfPercent, dfFloatFixed, dfBcd] then
    FDecimalPlaces := NewValue;
  if not (csLoading in ComponentState) then
    EditText := FEditText;
end;

procedure TJvCustomValidateEdit.SetDisplayFormat(NewValue: TJvValidateEditDisplayFormat);
var
  OldFormat: TJvValidateEditDisplayFormat;
begin
  if FDisplayFormat <> NewValue then
  begin
    OldFormat := FDisplayFormat;
    FDisplayFormat := NewValue;

    RecalcCheckChars;

    case FDisplayFormat of
      dfAlphabetic, dfAlphaNumeric, dfIdentifier,
      dfCheckChars, dfNonCheckChars, dfCustom, dfNone:
        if FAutoAlignment then
          Alignment := taLeftJustify;
      dfCurrency:
        begin
          if FAutoAlignment then
            Alignment := taRightJustify;
          if not (csLoading in ComponentState) then
            if FDecimalPlaces = 0 then
              FDecimalPlaces := JclFormatSettings.CurrencyDecimals;
        end;
      dfBinary, dfFloat, dfFloatGeneral, dfPercent, dfDecimal, dfHex,
      dfInteger, dfOctal, dfScientific, dfFloatFixed, dfBcd:
        if FAutoAlignment then
          Alignment := taRightJustify;
      dfYear:
        begin
          if FAutoAlignment then
            Alignment := taRightJustify;
          MaxLength := 4;
        end;
    end;

    if OldFormat = dfYear then
      MaxLength := 0;

    if not IsEmpty then
    begin
      // Convert non-base 10 numbers to base 10 and base-10 numbers to non-base 10
      if (OldFormat = dfBinary) and
        (NewValue in [dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) then
        SetAsInteger(BaseToInt(FEditText, 2))
      else
      if (OldFormat in [dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfPercent, dfFloatFixed, dfBcd]) and
        (NewValue in [dfBinary, dfHex, dfOctal]) then
        SetAsFloat(JvSafeStrToFloatDef(FEditText, 0))
      else
      if (OldFormat = dfHex) and
        (NewValue in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfInteger, dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) then
        SetAsInteger(BaseToInt(FEditText, 16))
      else
      if (OldFormat in [dfInteger, dfYear]) and
        (NewValue in [dfBinary, dfHex, dfOctal]) then
        SetAsInteger(StrToIntDef(FEditText, 0))
      else
      if (OldFormat = dfOctal) and
        (NewValue in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfHex, dfInteger, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) then
        SetAsInteger(BaseToInt(FEditText, 8))
      else
      begin
        // ...or just display the value
        if not (csLoading in ComponentState) then
          EditText := FEditText;
      end;
    end;
  end;
end;

procedure TJvCustomValidateEdit.SetZeroEmpty(NewValue: Boolean);
begin
  if FZeroEmpty <> NewValue then
  begin
    FZeroEmpty := NewValue;
    if not (csLoading in ComponentState) then
      EditText := FEditText;
  end;
end;

function TJvCustomValidateEdit.GetAsInteger: Int64;
begin
  case FDisplayFormat of
    dfBinary:
      Result := BaseToInt(FEditText, 2);
    dfHex:
      Result := BaseToInt(FEditText, 16);
    dfOctal:
      Result := BaseToInt(FEditText, 8);
  else
    Result := StrToInt64Def(FEditText, 0);
  end;
  Result := IntRangeValue(Result);
end;

procedure TJvCustomValidateEdit.SetAsInteger(NewValue: Int64);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfIdentifier, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := IntToStr(NewValue);
    dfBinary:
      EditText := IntToBase(NewValue, 2);
    dfHex:
      EditText := IntToBase(NewValue, 16);
    dfOctal:
      EditText := IntToBase(NewValue, 8);
    dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfInteger, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd:
      EditText := IntToStr(IntRangeValue(NewValue));
  end;
end;

function TJvCustomValidateEdit.GetAsCurrency: Currency;
begin
  case FDisplayFormat of
    dfBinary:
      Result := BaseToInt(FEditText, 2);
    dfHex:
      Result := BaseToInt(FEditText, 16);
    dfOctal:
      Result := BaseToInt(FEditText, 8);
  else
    Result := StrToCurrDef(FEditText, 0);
  end;
  Result := CurrRangeValue(Result);
end;

procedure TJvCustomValidateEdit.SetAsCurrency(NewValue: Currency);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfIdentifier, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := CurrToStr(NewValue);
    dfBinary:
      EditText := IntToBase(Trunc(NewValue), 2);
    dfHex:
      EditText := IntToBase(Trunc(NewValue), 16);
    dfOctal:
      EditText := IntToBase(Trunc(NewValue), 8);
    dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfInteger, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd:
      EditText := CurrToStr(CurrRangeValue(NewValue));
  end;
end;

function TJvCustomValidateEdit.GetAsFloat: Double;
var
  Cur: Currency;
begin
  case FDisplayFormat of
    dfBinary:
      Result := BaseToInt(FEditText, 2);
    dfHex:
      Result := BaseToInt(FEditText, 16);
    dfOctal:
      Result := BaseToInt(FEditText, 8);
    dfScientific:
      Result := ScientificStrToFloat(FEditText);
    dfCurrency:
      begin
        // Mantis 3494: The Edit text may contain extra characters such as
        // parenthesis that indicate the amount is negative. Using StrToFloatDef
        // would not catch the negative part, hence the need to use a function
        // that knows how to do the conversion.

        VarCyFromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(FEditText), LOCALE_USER_DEFAULT, 0, Cur);
        Result := Cur;
      end;
    dfBcd:
      Result := BcdToDouble(AsBcd);
  else
    Result := JvSafeStrToFloatDef(FEditText, 0);
  end;
  Result := FloatRangeValue(Result);
end;

procedure TJvCustomValidateEdit.SetAsFloat(NewValue: Double);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfIdentifier, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := FloatToStr(NewValue);
    dfBinary:
      EditText := IntToBase(Trunc(NewValue), 2);
    dfHex:
      EditText := IntToBase(Trunc(NewValue), 16);
    dfOctal:
      EditText := IntToBase(Trunc(NewValue), 8);
    dfInteger, dfYear:
      EditText := IntToStr(IntRangeValue(Trunc(NewValue)));
    dfCurrency:
      EditText := Format('%.*m', [FDecimalPlaces, FloatRangeValue(NewValue)]);
    dfFloat, dfPercent:
      EditText := Format('%.*n', [FDecimalPlaces, FloatRangeValue(NewValue)]);
    dfFloatGeneral:
      EditText := Format('%.*g', [FDecimalPlaces, FloatRangeValue(NewValue)]);
    dfFloatFixed:
      EditText := Format('%.*f', [FDecimalPlaces, FloatRangeValue(NewValue)]);
    dfDecimal:
      EditText := FloatToStr(FloatRangeValue(NewValue));
    dfScientific:
      EditText := Format('%e', [FloatRangeValue(NewValue)]);
    dfBcd:
      EditText := BcdToStrFDefault(DoubleToBcd(FloatRangeValue(NewValue)), FDecimalPlaces);
  end;
end;

function TJvCustomValidateEdit.GetAsBcd: TBcd;
var
  IntValue: Int64;
begin
  case FDisplayFormat of
    dfBinary, dfHex, dfOctal:
      begin
        IntValue := GetAsInteger;
        if Abs(IntValue) < MaxInt then
          Result := IntegerToBcd(IntValue)
        else
          Result := StrToBcd(IntToStr(IntValue));
      end;
    dfScientific:
      Result := DoubleToBcd(ScientificStrToFloat(FEditText));
    dfCurrency:
      CurrToBCD(GetAsCurrency, Result);
    dfFloat, dfFloatGeneral, dfFloatFixed:
      Result := DoubleToBcd(AsFloat);
  else
    if not TryStrToBcd(FEditText, Result) then
      ZeroBcd(Result);
  end;
  Result := BcdRangeValue(Result);
end;

procedure TJvCustomValidateEdit.SetAsBcd(const NewValue: TBcd);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfIdentifier, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := BcdToStrFDefault(NewValue, FDecimalPlaces);
    dfBinary:
      EditText := IntToBase(BcdToInt64(NewValue), 2);
    dfHex:
      EditText := IntToBase(BcdToInt64(NewValue), 16);
    dfOctal:
      EditText := IntToBase(BcdToInt64(NewValue), 8);
    dfInteger, dfYear:
      EditText := IntToStr(IntRangeValue(BcdToInt64(NewValue)));
    dfCurrency:
      EditText := Format('%.*m', [FDecimalPlaces, FloatRangeValue(BcdToDouble(NewValue))]);
    dfFloat, dfPercent:
      EditText := Format('%.*n', [FDecimalPlaces, FloatRangeValue(BcdToDouble(NewValue))]);
    dfFloatGeneral:
      EditText := Format('%.*g', [FDecimalPlaces, FloatRangeValue(BcdToDouble(NewValue))]);
    dfFloatFixed:
      EditText := Format('%.*f', [FDecimalPlaces, FloatRangeValue(BcdToDouble(NewValue))]);
    dfDecimal:
      EditText := FloatToStr(FloatRangeValue(BcdToDouble(NewValue)));
    dfScientific:
      EditText := Format('%e', [FloatRangeValue(BcdToDouble(NewValue))]);
    dfBcd:
      EditText := BcdToStrFDefault(BcdRangeValue(NewValue), FDecimalPlaces);
  end;
end;

function TJvCustomValidateEdit.GetValue: Variant;
var
  DisplayedText: string;
  Cur: Currency;
  Bcd: TBcd;
begin
  case FDisplayFormat of
    dfCurrency:
      begin
        // Mantis 3494: The Edit text may contain extra characters such as
        // parenthesis that indicate the amount is negative. Using StrToFloatDef
        // would not catch the negative part, hence the need to use a function
        // that knows how to do the conversion.
        VarCyFromStr({$IFDEF RTL240_UP}PChar{$ENDIF RTL240_UP}(FEditText), LOCALE_USER_DEFAULT, 0, Cur);
        Result := CurrRangeValue(Cur);
      end;
    dfFloat, dfFloatGeneral, dfDecimal, dfPercent, dfScientific, dfFloatFixed:
      Result := FloatRangeValue(JvSafeStrToFloatDef(FEditText, 0));
    dfInteger, dfYear:
      Result := IntRangeValue(StrToIntDef(FEditText, 0));
    dfHex:
      Result := IntRangeValue(StrToUIntDef('$' + FEditText, 0));
    dfBcd:
      if TryStrToBcd(FEditText, Bcd) then
        Result := VarFMTBcdCreate(Bcd)
      else
        Result := VarFMTBcdCreate; // Null
  else
    DisplayedText := inherited Text;

    // Remove DisplayPrefix and DisplaySuffix
    DisplayedText := StrEnsureNoPrefix(DisplayPrefix, DisplayedText);
    DisplayedText := StrEnsureNoSuffix(DisplaySuffix, DisplayedText);
    Result := DisplayedText;
  end;
end;

procedure TJvCustomValidateEdit.SetValue(NewValue: Variant);
begin
  if AllowEmpty and (VarIsNull(NewValue) or VarIsEmpty(NewValue)) then
    Clear
  else
    case FDisplayFormat of
      dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfNonCheckChars, dfIdentifier, dfNone, dfCustom:
        EditText := NewValue;
      dfBinary, dfHex, dfInteger, dfOctal, dfYear:
        SetAsInteger(NewValue);
      dfCurrency, dfFloat, dfDecimal, dfFloatGeneral, dfPercent, dfScientific, dfFloatFixed:
        SetAsFloat(NewValue);
      dfBcd:
        if VarIsFMTBcd(NewValue) then
          SetAsBcd(VarToBcd(NewValue))
        else
          SetAsBcd(StrToBcd(VarToStr(NewValue)));
    end;
end;

procedure TJvCustomValidateEdit.SetCheckChars(const NewValue: string);
begin
  if (csLoading in ComponentState) or
     ((FDisplayFormat in [dfNone, dfCheckChars, dfNonCheckChars]) and
      (FCheckChars <> NewValue)) then
  begin
    FCheckChars := NewValue;
    EditText := MakeValid(FEditText);
  end;
end;

function TJvCustomValidateEdit.IsCheckCharsStored: Boolean;
begin
  Result := (FDisplayFormat in [dfNone, dfCheckChars, dfNonCheckChars]);
end;

procedure TJvCustomValidateEdit.KeyPress(var Key: Char);
var
  StrippedText: string;
begin
  // Mantis 4952:
  // - Must not take the prefix/suffix into account when checking a character's validity
  // - Must not take into account the CurrencyString into account when checking a character's validity
  StrippedText := GetUnprefixedUnsuffixedText(Text);

  if not IsValidChar(StrippedText, Key, SelStart + 1) and (Key >= #32) then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TJvCustomValidateEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FLastDownKey := 0;
  inherited KeyUp(Key, Shift);
end;

procedure TJvCustomValidateEdit.WMPaste(var Msg: TMessage);
begin
  inherited;
  EditText := MakeValid(GetUnprefixedUnsuffixedText(inherited Text));
end;

function TJvCustomValidateEdit.MakeValid(const ParseString: string): string;
var
  C: Char;
  I: Integer;
  L: Integer;
begin
  SetLength(Result, Length(ParseString));
  L := 0;
  for I := 1 to Length(ParseString) do
  begin
    C := ParseString[I];
    if IsValidChar(Copy(ParseString, 1, I - 1), C, I) then
    begin
      Result[L + 1] := C;
      Inc(L);
    end;
  end;
  SetLength(Result, L);
end;

procedure TJvCustomValidateEdit.RecalcCheckChars;
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  Numbers = '0123456789';
begin
  case FDisplayFormat of
    dfAlphabetic:
      FCheckChars := Alphabet;
    dfAlphaNumeric:
      FCheckChars := Alphabet + Numbers;
    dfIdentifier:
      FCheckChars := Alphabet + Numbers + '_';
    dfBinary:
      FCheckChars := '01';
    dfCustom, dfNone:
      if (FDisplayFormat = dfCustom) or not (csLoading in ComponentState) then
        FCheckChars := '';
    dfCurrency,
    dfFloat, dfFloatGeneral, dfPercent, dfDecimal, dfFloatFixed, dfBcd:
      FCheckChars := Numbers + JclFormatSettings.DecimalSeparator;
    dfHex:
      FCheckChars := Numbers + 'ABCDEFabcdef';
    dfInteger, dfYear:
      FCheckChars := Numbers;
    dfOctal:
      FCheckChars := '01234567';
    dfScientific:
      FCheckChars := Numbers + 'Ee' + JclFormatSettings.DecimalSeparator;
  end;
end;

function TJvCustomValidateEdit.IsValidChar(const S: string;
  var Key: Char; Posn: Integer): Boolean;
var
  iPosE: Integer;
  ExpectedNegPos: Integer;
  ExpectedNegChar: Char;
begin
  if (FLastDownKey = VK_DECIMAL) and ForceDecimalSeparatorInput then
    Key := JclFormatSettings.DecimalSeparator;

  case FDisplayFormat of
    dfBinary, dfCheckChars, dfHex, dfOctal, dfYear:
      Result := Pos(Key, FCheckChars) > 0;
    dfAlphabetic:
      Result := IsCharAlpha(Key);
    dfAlphaNumeric:
      Result := IsCharAlphaNumeric(Key);
    dfCustom:
      Result := DoValidate(Key, S, Posn);
    dfInteger:
      Result := (Pos(Key, FCheckChars) > 0) or
        ((Key = '+') and (Posn = 1) and ((Pos('+', S) = 0) or (SelLength > 0))) or
        ((Key = '-') and (Posn = 1) and ((Pos('-', S) = 0) or (SelLength > 0)));
    dfFloat, dfFloatGeneral, dfDecimal, dfPercent, dfFloatFixed, dfBcd:
      Result := ((Pos(Key, FCheckChars) > 0) and
        (((Key = JclFormatSettings.DecimalSeparator) and (Pos(JclFormatSettings.DecimalSeparator, S) = 0)) or (Key <> JclFormatSettings.DecimalSeparator))) or
        ((Key = '+') and (Posn = 1) and ((Pos('+', S) = 0) or (SelLength > 0))) or
        ((Key = '-') and (Posn = 1) and ((Pos('-', S) = 0) or (SelLength > 0)));
    dfCurrency:
      begin
        // The currency negative format can be quite complicated. The current
        // one is indicated by the value of NegCurrFormat, and can have any
        // value from 0 to 15 according to the MSDN and Delphi's help.
        // So we must take into account that some format require the negative
        // sign to be at the end, while some others replace it by parenthesis.
        // See http://www.delphibasics.co.uk/RTL.asp?Name=NegCurrFormat for
        // an online version of Delphi's help.
        // If we were not to use this, it would trigger Mantis 3494, where
        // the number would go from negative to positive simply by focusing out
        // of the control.
        ExpectedNegChar := '-';
        ExpectedNegPos := 1;
        case JclFormatSettings.NegCurrFormat of
          0, 4, 14, 15:
            begin
              ExpectedNegPos := 1;
              ExpectedNegChar := '(';
            end;
          1, 5, 8, 9:
            ExpectedNegPos := 1;
          2:
            ExpectedNegPos := 2;
          3, 7, 10, 11:
            ExpectedNegPos := Length(S);
          6:
            ExpectedNegPos := Length(S) - 1;
          12:
            ExpectedNegPos := 3;
          13:
            ExpectedNegPos := Length(S) - 2;
        end;

        if (Key = '(') and (Posn = 1) and (JclFormatSettings.NegCurrFormat in [0, 4, 14, 15]) then
          Key := '-';

        Result := ((Pos(Key, FCheckChars) > 0) and
          (((Key = JclFormatSettings.DecimalSeparator) and (Pos(JclFormatSettings.DecimalSeparator, S) = 0)) or (Key <> JclFormatSettings.DecimalSeparator))) or
          ((Key = '+') and (Posn = 1) and ((Pos('+', S) = 0) or (SelLength > 0))) or
          ((Key = '-') and (Posn = ExpectedNegPos) and ((Pos(ExpectedNegChar, S) = 0) or (SelLength > 0)));
      end;
    dfNonCheckChars:
      Result := Pos(Key, FCheckChars) = 0;
    dfNone:
      Result := True;
    dfScientific:
      begin
        Result := (Pos(Key, FCheckChars) > 0) or CharInSet(Key, ['+', '-']);
        if Result then
        begin
          iPosE := Pos('e', LowerCase(S));
          if Key = JclFormatSettings.DecimalSeparator then
          begin
            if iPosE = 0 then
              Result := (Pos(JclFormatSettings.DecimalSeparator, S) = 0)
            else
              Result := ((Posn <= iPosE) and (Pos(JclFormatSettings.DecimalSeparator, Copy(S, 1, iPosE - 1)) = 0));
               //or ((Posn > iPosE) and (Pos(DecimalSeparator, Copy(S, iPosE + 1, Length(S))) = 0));
               // (outchy) XXXeY,YY are not valid scientific numbers, Y must be an integer value
          end
          else
          if CharInSet(Key, ['E', 'e']) then
            Result := (iPosE = 0) and (Posn > 1)
          else
          if Key = '+' then
            Result := (Posn = 1) or (Posn = iPosE + 1)
          else
          if Key = '-' then
            Result := (Posn = 1) or (Posn = iPosE + 1);
        end;
      end;
    dfIdentifier:
      begin
        if Posn = 1 then
          Result := (Key = '_') or (IsCharAlpha(Key))
        else
          Result := Pos(Key, FCheckChars) > 0;
      end
  else
    Result := False;
  end;
end;

function TJvCustomValidateEdit.DoValidate(const Key: Char;
  const AText: string; const Posn: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomValidate) then
    FOnCustomValidate(Self, Key, AText, Posn, Result);
end;

procedure TJvCustomValidateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FLastDownKey := Key;
// if Key = VK_DELETE then    EditText := MakeValid(inherited Text);
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    EditText := FEnterText;
    SelStart := 0;
    SelLength := Length(FEditText);
  end;
  inherited KeyDown(Key, Shift);
end;

function TJvCustomValidateEdit.CurrRangeValue(CheckValue: Currency): Currency;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TJvCustomValidateEdit.FloatRangeValue(CheckValue: Double): Double;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TJvCustomValidateEdit.IntRangeValue(CheckValue: Int64): Int64;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := Trunc(FMaxValue)
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := Trunc(FMinValue);
end;

function TJvCustomValidateEdit.BcdRangeValue(const CheckValue: TBcd): TBcd;
begin
  Result := CheckValue;
  if FHasMaxValue and (BcdCompare(CheckValue, DoubleToBcd(FMaxValue)) = 1) then // CheckValue > FMaxValue
    Result := DoubleToBcd(FMaxValue)
  else
  if FHasMinValue and (BcdCompare(CheckValue, DoubleToBcd(FMinValue)) = -1) then // CheckValue < FMinValue
    Result := DoubleToBcd(FMinValue);
end;

function TJvCustomValidateEdit.GetEditText: string;
begin
  Result := FEditText;
end;

function TJvCustomValidateEdit.GetUnprefixedUnsuffixedText(const Value: string): string;
begin
  if not FKeepPrefixSuffixIntact then
  begin
    Result := StrEnsureNoPrefix(DisplayPrefix, StrEnsureNoSuffix(DisplaySuffix, Value));
    Result := StrEnsureNoPrefix(JclFormatSettings.CurrencyString, StrEnsureNoSuffix(JclFormatSettings.CurrencyString, Result));
  end
  else
    Result := Value;
end;

procedure TJvCustomValidateEdit.SetEditText(const NewValue: string);
begin
  FEditText := MakeValid(GetUnprefixedUnsuffixedText(NewValue));
  if (FDisplayFormat = dfYear) and ((not FHasMaxValue) or
    (FHasMaxValue and (FMaxValue > 2000 + JclFormatSettings.TwoDigitYearCenturyWindow))) and
    ((MaxLength = 0) or (MaxLength > 3)) then
    FEditText := IntToStr(MakeYear4Digit(StrToIntDef(FEditText, 0), JclFormatSettings.TwoDigitYearCenturyWindow));
  if (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfHex, dfInteger,
    dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) then
  begin
    EnforceMaxValue;
    EnforceMinValue;
  end;
//  ChangeText(FEditText);
  DisplayText;
  DoValueChanged;
end;

procedure TJvCustomValidateEdit.FocusSet(PrevWnd: THandle);
begin
  DisplayText;
  inherited FocusSet(PrevWnd);
end;

procedure TJvCustomValidateEdit.FocusKilled(NextWnd: THandle);
var
  DisplayedText: string;
begin
  if not (csDestroying in ComponentState) then
  begin
    DisplayedText := inherited Text;
    EditText := GetUnprefixedUnsuffixedText(DisplayedText);
  end;
  inherited FocusKilled(NextWnd);
end;

procedure TJvCustomValidateEdit.ChangeText(const NewValue: string);
var
  S, Exponent, DisplayValue: string;
  Ps, I: Integer;
begin
  FSelfChange := True;
  try
    Ps := 0;
    if TrimDecimals then
    begin
      I := Pos('e', LowerCase(NewValue));
      if (DisplayFormat = dfScientific) and (I <> 0) then
      begin
        Exponent := Copy(NewValue, I, Length(NewValue));
        Dec(I);
      end
      else
      begin
        Exponent := '';
        I := Length(NewValue);
      end;
      Ps := Pos(JclFormatSettings.DecimalSeparator, NewValue);
      if Ps > 0 then
      begin
        while (I > Ps) and (NewValue[I] = '0') do
          Dec(I);
        if Ps = I then
          Dec(I); // skip decimal separator (Ivo Bauer)
        S := FDisplayPrefix + Copy(NewValue, 1, I) + Exponent + FDisplaySuffix;

        DisplayValue := Copy(NewValue, 1, I);
        if HidePrefixSuffixIfEmpty and (DisplayValue = '') and (Exponent = '') then
          S := ''
        else
          S := FDisplayPrefix + DisplayValue + Exponent + FDisplaySuffix;
      end;
    end;
    if Ps = 0 then
    begin
      if HidePrefixSuffixIfEmpty and (NewValue = '') and (Exponent = '') then
        S := ''
      else
        S := FDisplayPrefix + NewValue + FDisplaySuffix;
    end;
    if S <> inherited Text then
      inherited SetText(S);
  finally
    FSelfChange := False;
  end;
end;

procedure TJvCustomValidateEdit.DisplayText;

  function FormatedValue(Value: Double): Double;
  begin
    if Assigned(FOnDecimalRounding) then
      FOnDecimalRounding(Self, Result, Value)
    else
      Result := Value;
  end;

begin
  // The number types need to be formatted
  if FAllowEmpty and (FEditText = '') then
    ChangeText('')
  else
  if (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfInteger, dfOctal,
                         dfPercent, dfScientific, dfYear, dfFloatFixed]) and
    (AsFloat = 0) and FZeroEmpty then
    ChangeText('')
  else if (FDisplayFormat = dfBcd) and FZeroEmpty and IsBcdZero(AsBcd) then
    ChangeText('')
  else
  begin
    case FDisplayFormat of
      dfCurrency:
        ChangeText(Format('%.*m', [FDecimalPlaces, AsCurrency]));
      dfInteger:
        ChangeText(IntToStr(AsInteger));
      dfFloat:
        ChangeText(Format('%.*n', [FDecimalPlaces, FormatedValue(AsFloat)]));
      dfFloatGeneral:
        ChangeText(Format('%.*g', [FDecimalPlaces, FormatedValue(AsFloat)]));
      dfFloatFixed:
        ChangeText(Format('%.*f', [FDecimalPlaces, FormatedValue(AsFloat)]));
      dfScientific:
        ChangeText(Format('%.*e', [FDecimalPlaces, FormatedValue(AsFloat)]));
      dfPercent:
        ChangeText(Format('%.*n%', [FDecimalPlaces, FormatedValue(AsFloat)]));
      dfBcd:
        ChangeText(BcdToStrFDefault(AsBcd, FDecimalPlaces));
    else
      ChangeText(FEditText);
    end;

    // This needs to be done AFTER the text has been changed so that the color
    // is directly shown correctly. (Mantis 3493)
    if (FCriticalPoints.CheckPoints <> cpNone) and
      (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral, dfDecimal, dfHex, dfInteger,
                          dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) then
      SetFontColor;
  end;
end;

function TJvCustomValidateEdit.ScientificStrToFloat(SciString: string): Double;
var
  I: Cardinal;
  sMantissa, sExponent: string;
  bInExp: Boolean;
begin
  if Pos('E', UpperCase(SciString)) = 0 then
    Result := JvSafeStrToFloatDef(SciString, 0)
  else
  begin
    sMantissa := '';
    sExponent := '';
    bInExp := False;
    for I := 1 to Length(SciString) do
    begin
      if UpperCase(SciString[I]) = 'E' then
        bInExp := True
      else
      begin
        if bInExp then
          sExponent := sExponent + SciString[I]
        else
          sMantissa := sMantissa + SciString[I];
      end;
    end;
     // NOTE: StrToFloatDefIgnoreInvalidCharacters now called JvSafeStrToFloatDef:
    Result := JvSafeStrToFloatDef(sMantissa, 0) * Power(10, JvSafeStrToFloatDef(sExponent, 0));
  end;
end;

function TJvCustomValidateEdit.BaseToInt(const BaseValue: string; Base: Byte): Int64;
begin
  Assert(Base <= 36, RsEBaseTooBig);
  Assert(Base > 1, RsEBaseTooSmall);
  Result := Numb2Dec(BaseValue, Base);
end;

function TJvCustomValidateEdit.IntToBase(NewValue: Int64; Base: Byte): string;
begin
  Assert(Base <= 36, RsEBaseTooBig);
  Assert(Base > 1, RsEBaseTooSmall);
  Result := Dec2Numb(NewValue, 0, Base);
end;

procedure TJvCustomValidateEdit.DoValueChanged;
begin
  try
    if Assigned(FOnValueChanged) and not (csLoading in ComponentState) and (FEnterText <> FEditText) then
      FOnValueChanged(Self);
  finally
    FEnterText := FEditText;
  end;
end;

procedure TJvCustomValidateEdit.Change;
var
  DisplayedText: string;
begin
  // Update FEditText for User changes, so that the AsInteger, etc,
  // functions work while editing
  if not FSelfChange then
  begin
    DisplayedText := inherited Text;
    FEditText := GetUnprefixedUnsuffixedText(DisplayedText);
  end;
  inherited Change;
end;

procedure TJvCustomValidateEdit.SetText(const NewValue: TCaption);
begin
  // If we are actually changing our value ourselves, there is no need
  // to do it again. This may even trigger an infinite recursion, especially
  // when in a derived component the display format is set in the constructor.
  // In that case, the recursion would kill Delphi almost instantly.
  if not FSelfChange then
  begin
    EditText := NewValue;
    DoValueChanged;
  end;
end;

procedure TJvCustomValidateEdit.SetDisplayPrefix(const NewValue: string);
begin
  FDisplayPrefix := NewValue;
  DisplayText;
end;

procedure TJvCustomValidateEdit.SetDisplaySuffix(const NewValue: string);
begin
  FDisplaySuffix := NewValue;
  DisplayText;
end;

procedure TJvCustomValidateEdit.CriticalPointsChange(Sender: TObject);
begin
  SetFontColor;
  Invalidate;
end;

function TJvCustomValidateEdit.IsValid: Boolean;
begin
  Result := True;
  case FCriticalPoints.CheckPoints of
    cpMaxValue:
      Result := IsLower(AsFloat, FCriticalPoints.MaxValue, FCriticalPoints.MaxValueIncluded);
    cpMinValue:
      Result := IsGreater(AsFloat, FCriticalPoints.MinValue, FCriticalPoints.MinValueIncluded);
    cpBoth:
      Result := IsLower(AsFloat, FCriticalPoints.MaxValue, FCriticalPoints.MaxValueIncluded) and
        IsGreater(AsFloat, FCriticalPoints.MinValue, FCriticalPoints.MinValueIncluded);
  end;
  if Assigned(FOnIsValid) then
    FOnIsValid(Self, Result);
end;

function TJvCustomValidateEdit.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty or (GetUnprefixedUnsuffixedText(Text) = '');
end;

procedure TJvCustomValidateEdit.SetFontColor;
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    Font.OnChange := nil;
    case FCriticalPoints.CheckPoints of
      cpNone:
        Font.Color := FStandardFontColor;
      cpMinValue:
        if IsLower(AsFloat, FCriticalPoints.MinValue, not FCriticalPoints.MinValueIncluded) then
          Font.Color := FCriticalPoints.ColorBelow
        else
          Font.Color := FStandardFontColor;
      cpMaxValue:
        if IsGreater(AsFloat, FCriticalPoints.MaxValue, not FCriticalPoints.MaxValueIncluded) then
          Font.Color := FCriticalPoints.ColorAbove
        else
          Font.Color := FStandardFontColor;
      cpBoth:
        if IsGreater(AsFloat, FCriticalPoints.MaxValue, not FCriticalPoints.MaxValueIncluded) then
          Font.Color := FCriticalPoints.ColorAbove
        else if IsLower(AsFloat, FCriticalPoints.MinValue, not FCriticalPoints.MinValueIncluded) then
          Font.Color := FCriticalPoints.ColorBelow
        else
          Font.Color := FStandardFontColor;
    end;
    Font.OnChange := FontChange;
  end;
  Invalidate;
end;

procedure TJvCustomValidateEdit.FontChange(Sender: TObject);
begin
  FStandardFontColor := Font.Color;
  if Assigned(FOldFontChange) then
    FOldFontChange(Sender);
end;

procedure TJvCustomValidateEdit.EnforceMaxValue;
begin
  { Check the Value is within this range }
  if FHasMaxValue and (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral,
    dfDecimal, dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) and
    (AsFloat > FMaxValue) and not FEnforcingMinMaxValue then
  begin
    if not (AllowEmpty and IsEmpty) then
    begin
      FEnforcingMinMaxValue := True;
      try
        SetAsFloat(FMaxValue);
      finally
        FEnforcingMinMaxValue := False;
      end;
    end;
  end;
end;

procedure TJvCustomValidateEdit.EnforceMinValue;
begin
  { Check the Value is within this range }
  if FHasMinValue and (FDisplayFormat in [dfBinary, dfCurrency, dfFloat, dfFloatGeneral,
    dfDecimal, dfHex, dfInteger, dfOctal, dfPercent, dfScientific, dfYear, dfFloatFixed, dfBcd]) and
    (AsFloat < FMinValue) and not FEnforcingMinMaxValue then
  begin
    if not (AllowEmpty and IsEmpty) then
    begin
      FEnforcingMinMaxValue := True;
      try
        SetAsFloat(FMinValue);
      finally
        FEnforcingMinMaxValue := False;
      end;
    end;
  end;
end;

//=== { TJvValidateEditCriticalPoints } ======================================

constructor TJvValidateEditCriticalPoints.Create;
begin
  inherited Create;
  SetDefaults(cpNone, clBlue, clRed);
  FMaxValueIncluded := False;
  FMinValueIncluded := False;
end;

procedure TJvValidateEditCriticalPoints.SetCheckPoints(NewValue: TJvValidateEditCriticalPointsCheck);
begin
  if FCheckPoints <> NewValue then
  begin
    FCheckPoints := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetColorAbove(NewValue: TColor);
begin
  if FColorAbove <> NewValue then
  begin
    FColorAbove := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetColorBelow(NewValue: TColor);
begin
  if FColorBelow <> NewValue then
  begin
    FColorBelow := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    DoChanged;
  end;
end;

procedure TJvValidateEditCriticalPoints.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvValidateEditCriticalPoints.Assign(Source: TPersistent);
var
  LocalSource: TJvValidateEditCriticalPoints;
begin
  if Source is TJvValidateEditCriticalPoints then
  begin
    LocalSource := TJvValidateEditCriticalPoints(Source);
    CheckPoints := LocalSource.CheckPoints;
    ColorAbove := LocalSource.ColorAbove;
    ColorBelow := LocalSource.ColorBelow;
    MaxValue := LocalSource.MaxValue;
    MinValue := LocalSource.MinValue;
  end
  else
    inherited Assign(Source);
end;

function TJvValidateEditCriticalPoints.IsCheckPointsStored: Boolean;
begin
  Result := (FCheckPoints <> FDefCheckPoints);
end;

function TJvValidateEditCriticalPoints.IsColorAboveStored: Boolean;
begin
  Result := (FColorAbove <> FDefColorAbove);
end;

function TJvValidateEditCriticalPoints.IsColorBelowStored: Boolean;
begin
  Result := (FColorBelow <> FDefColorBelow);
end;

procedure TJvValidateEditCriticalPoints.SetDefaults(ACheckPoints: TJvValidateEditCriticalPointsCheck;
  AColorAbove, AColorBelow: TColor);
begin
  FDefCheckPoints := ACheckPoints;
  FCheckPoints := ACheckPoints;
  FDefColorAbove := AColorAbove;
  FColorAbove := AColorAbove;
  FDefColorBelow := AColorBelow;
  FColorBelow := AColorBelow;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
