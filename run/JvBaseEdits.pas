{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCurrEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  Andreas Hausladen

Last Modified: 2003-04-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBaseEdits;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Menus,
  Forms, StdCtrls, Mask, Buttons,
  JvToolEdit;

type
  TJvCustomNumEdit = class(TJvCustomComboEdit)
  private
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FValue: Extended;
    FMinValue: Extended;
    FMaxValue: Extended;
    FDecimalPlaces: Cardinal;
    FBeepOnError: Boolean;
    FCheckOnExit: Boolean;
    FZeroEmpty: Boolean;
    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    FDisplayFormat: string;
    // Polaris
    FDecimalPlaceRound: Boolean;
    procedure SetDecimalPlaceRound(Value: Boolean);

    procedure SetFocused(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBeepOnError(Value: Boolean);
    procedure SetDisplayFormat(const Value: string);
    function GetDisplayFormat: string;
    procedure SetDecimalPlaces(Value: Cardinal);
    function GetValue: Extended;
    procedure SetValue(AValue: Extended);
    function GetAsInteger: Longint;
    procedure SetAsInteger(AValue: Longint);
    procedure SetMaxValue(AValue: Extended);
    procedure SetMinValue(AValue: Extended);
    procedure SetZeroEmpty(Value: Boolean);
    procedure SetFormatOnEditing(Value: Boolean);
    function GetText: string;
    procedure SetText(const AValue: string);
    function TextToValText(const AValue: string): string;
    //Polaris    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    function IsFormatStored: Boolean;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
  protected
    //Polaris up to protected
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    procedure AcceptValue(const Value: Variant); override;
    procedure Change; override;
    procedure ReformatEditText; dynamic;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
    procedure DataChanged; virtual;
    function DefaultDisplayFormat: string; virtual;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    function FormatDisplayText(Value: Extended): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData;
    procedure UpdatePopup; virtual;
    procedure DoBeepOnError; virtual;
    property Formatting: Boolean read FFormatting;
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property GlyphKind default gkDefault;
    property ButtonWidth default 21; //Polaris 20;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces default 2;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat stored IsFormatStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property FormatOnEditing: Boolean read FFormatOnEditing write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
    //Polaris
    property DecimalPlaceRound: Boolean read FDecimalPlaceRound write SetDecimalPlaceRound default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property PopupVisible;
    property Value: Extended read GetValue write SetValue;
  end;

  TJvxCurrencyEdit = class(TJvCustomNumEdit)
  protected
    function DefaultDisplayFormat: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align; //Polaris
    property DecimalPlaceRound; //Polaris

    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property Ctl3D;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  TJvCustomCalcEdit = class(TJvCustomNumEdit)
  private
    FEnablePopupChange: Boolean;
  protected
    procedure PopupChange; override;
    property EnablePopupChange: Boolean read FEnablePopupChange write FEnablePopupChange default False;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvCalcEdit = class(TJvCustomCalcEdit)
  published
    property Align; //Polaris
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DecimalPlaceRound; //Polaris
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EnablePopupChange;
    property Font;
    property FormatOnEditing;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnButtonClick;
    property OnChange;
    property OnClick;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

implementation

uses
  Consts, Math,
  JvJCLUtils, JvCalc, JvResources;

{$R ..\resources\JvBaseEdits.res}

const
  sCalcBmp = 'JV_CEDITBMP'; { Numeric editor button glyph }

// (rom) changed to var
var
  CalcBitmap: TBitmap = nil;

type
  TJvPopupWindowHack = class(TJvPopupWindow);

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array [0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue, fvExtended);
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in ['-', '+']);
  if IsSign then
    MinSym := 2
  else
    MinSym := 1;
  I := Pos(DecimalSeparator, S);
  if I > 0 then
    MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then
    MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do
  begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then
    begin
      Group := 0;
      Result := ThousandSeparator + Result;
    end;
  end;
  if IsSign then
    Result := S[1] + Result;
end;

//=== TJvCustomNumEdit =======================================================

constructor TJvCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FDecimalPlaceRound := False; // Polaris
  MaxLength := 0;
  FBeepOnError := True;
  FAlignment := taRightJustify;
  FDisplayFormat := DefaultDisplayFormat;
  FDecimalPlaces := 2;
  FZeroEmpty := True;
  inherited Text := '';
  inherited Alignment := taLeftJustify;
  FDefNumGlyphs := 2;
  { forces update }
  DataChanged;
  ControlState := ControlState + [csCreating];
  try
    GlyphKind := gkDefault;
    //Polaris ButtonWidth := 20;
    ButtonWidth := 21;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvCustomNumEdit.Destroy;
begin
  FCanvas.Free;
  if FPopup <> nil then
    TJvPopupWindow(FPopup).OnCloseUp := nil;
  FreeAndNil(FPopup);
  inherited Destroy;
end;

//Polaris

procedure TJvCustomNumEdit.SetDecimalPlaceRound(Value: Boolean);
begin
  if FDecimalPlaceRound <> Value then
  begin
    FDecimalPlaceRound := Value;
    SetValue(CheckValue(FValue, False));
    Invalidate;
  end;
end;

function TJvCustomNumEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if CalcBitmap = nil then
  begin
    CalcBitmap := TBitmap.Create;
    CalcBitmap.LoadFromResourceName(HInstance, sCalcBmp);
  end;
  Result := CalcBitmap;
end;

function TJvCustomNumEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

function TJvCustomNumEdit.IsFormatStored: Boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

function TJvCustomNumEdit.IsValidChar(Key: Char): Boolean;
var
  S: string;
  SelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  S := EditText;
  GetSel(SelStart, SelStop);
  System.Delete(S, SelStart + 1, SelStop - SelStart);
  System.Insert(Key, S, SelStart + 1);
  S := TextToValText(S);
  DecPos := Pos(DecimalSeparator, S);
  if DecPos > 0 then
  begin
    SelStart := Pos('E', UpperCase(S));
    if SelStart > DecPos then
      DecPos := SelStart - DecPos
    else
      DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then
      Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

procedure TJvCustomNumEdit.KeyPress(var Key: Char);
begin
  if PopupVisible and (UpCase(Key) in ['0'..'9', DecimalSeparator, '.', ',',
    '+', '-', '*', '/', '_', '=', 'C', 'R', 'Q', '%', #8, #13] -
      [ThousandSeparator]) then
  begin
    TJvPopupWindowHack(FPopup).KeyPress(Key);
    Key := #0;
  end;
  if Key in ['.', ','] - [ThousandSeparator] then
    Key := DecimalSeparator;
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and not IsValidChar(Key) then
  begin
    DoBeepOnError;
    Key := #0;
  end
  else
  if Ord(Key) = VK_ESCAPE then
  begin
    Reset;
    Key := #0;
  end;
end;

procedure TJvCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TJvCustomNumEdit.SetZeroEmpty(Value: Boolean);
begin
  if FZeroEmpty <> Value then
  begin
    FZeroEmpty := Value;
    DataChanged;
  end;
end;

procedure TJvCustomNumEdit.SetBeepOnError(Value: Boolean);
begin
  if FBeepOnError <> Value then
  begin
    FBeepOnError := Value;
    UpdatePopup;
  end;
end;

procedure TJvCustomNumEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvCustomNumEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
    DataChanged;
  end;
end;

function TJvCustomNumEdit.GetDisplayFormat: string;
begin
  Result := FDisplayFormat;
end;

procedure TJvCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;

procedure TJvCustomNumEdit.SetFormatOnEditing(Value: Boolean);
begin
  if FFormatOnEditing <> Value then
  begin
    FFormatOnEditing := Value;
    if FFormatOnEditing then
      inherited Alignment := Alignment
    else
      inherited Alignment := taLeftJustify;
    if FFormatOnEditing and FFocused then
      ReformatEditText
    else
    if FFocused then
    begin
      UpdateData;
      DataChanged;
    end;
  end;
end;

procedure TJvCustomNumEdit.SetDecimalPlaces(Value: Cardinal);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    SetValue(CheckValue(FValue, False)); // Polaris (?)
    DataChanged;
    Invalidate;
  end;
end;

function TJvCustomNumEdit.FormatDisplayText(Value: Extended): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else
    Result := FloatToStr(Value);
end;

function TJvCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(FValue);
end;

procedure TJvCustomNumEdit.Clear;
begin
  Text := '';
end;

procedure TJvCustomNumEdit.DataChanged;
var
  EditFormat: string;
begin
  EditFormat := '0';
  if FDecimalPlaces > 0 then
    EditFormat := EditFormat + '.' + MakeStr('#', FDecimalPlaces);
  if (FValue = 0.0) and FZeroEmpty then
    EditText := ''
  else
    EditText := FormatFloat(EditFormat, CheckValue(FValue, False));
end;

function TJvCustomNumEdit.CheckValue(NewValue: Extended;
  RaiseOnError: Boolean): Extended;
var
  DP: Integer;
begin
  if FDecimalPlaceRound then
  begin //Polaris
    DP := FDecimalPlaces;
    NewValue := Int(NewValue * Exp(DP * Ln(10)) + Sign(NewValue) * 0.50000001) * Exp(-DP * Ln(10));
  end;
  Result := NewValue;
  if FMaxValue <> FMinValue then
  begin
    if FMaxValue > FMinValue then
    begin
      if NewValue < FMinValue then
        Result := FMinValue
      else
      if NewValue > FMaxValue then
        Result := FMaxValue;
    end
    else
    begin
      if FMaxValue = 0 then
      begin
        if NewValue < FMinValue then
          Result := FMinValue;
      end
      else
      if FMinValue = 0 then
      begin
        if NewValue > FMaxValue then
          Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateFmt(RsEOutOfRangeXFloat,
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;

procedure TJvCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TJvCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

procedure TJvCustomNumEdit.UpdatePopup;
begin
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, DefCalcPrecision, BeepOnError);
end;

procedure TJvCustomNumEdit.DoBeepOnError;
begin
  if BeepOnError then
    MessageBeep(0);
end;

function TJvCustomNumEdit.GetValue: Extended;
begin
  if not (csDesigning in ComponentState) then
  try
    UpdateData;
  except
    FValue := FMinValue;
  end;
  Result := FValue;
end;

procedure TJvCustomNumEdit.SetValue(AValue: Extended);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

function TJvCustomNumEdit.GetAsInteger: Longint;
begin
  Result := trunc(Value);
end;

procedure TJvCustomNumEdit.SetAsInteger(AValue: Longint);
begin
  SetValue(AValue);
end;

procedure TJvCustomNumEdit.SetMinValue(AValue: Extended);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    Value := FValue;
  end;
end;

procedure TJvCustomNumEdit.SetMaxValue(AValue: Extended);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    Value := FValue;
  end;
end;

function TJvCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TJvCustomNumEdit.TextToValText(const AValue: string): string;
var
  I: Integer;
  X: Char;
begin
  Result := DelRSpace(AValue);
  if DecimalSeparator <> ThousandSeparator then
    Result := DelChars(Result, ThousandSeparator);
  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', DecimalSeparator);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', DecimalSeparator);

// Aquarius
  I := 1;
  while I <= Length(Result) do
  begin
    X := Result[I];
    if (X = DecimalSeparator) or (X = '-') or ((X >= '0') and (X <= '9')) then
    begin
      I := I + 1;
      Continue;
    end
    else
      Result := Copy(Result, 1, I - 1) + Copy(Result, I + 1, Length(Result) - 1);
  end;

  if Result = '' then
    Result := '0'
  else
  if Result = '-' then
    Result := '-0';
end;

procedure TJvCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then
  begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TJvCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: Boolean;
  OldLen, SelStart, SelStop: Integer;
begin
  FFormatting := True;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if HandleAllocated then
      GetSel(SelStart, SelStop);
    if not IsEmpty then
      S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
    if HandleAllocated and (GetFocus = Handle) and
      not (csDesigning in ComponentState) then
    begin
      Inc(SelStart, Length(S) - OldLen);
      SetCursor(SelStart);
    end;
  finally
    FFormatting := False;
  end;
end;

procedure TJvCustomNumEdit.Change;
begin
  if not FFormatting then
  begin
    if FFormatOnEditing and FFocused then
      ReformatEditText;
    inherited Change;
  end;
end;

procedure TJvCustomNumEdit.AcceptValue(const Value: Variant);
begin
  inherited AcceptValue(Value);
  Self.Value := CheckValue(Value, False); //Polaris
end;

procedure TJvCustomNumEdit.WMPaste(var Msg: TMessage);
var
  S: string;
begin
  S := EditText;
  try
    inherited;
    UpdateData;
  except
    EditText := S;
    SelectAll;
    if CanFocus then
      SetFocus;
    DoBeepOnError;
  end;
end;

procedure TJvCustomNumEdit.CMEnter(var Msg: TCMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then
    ReformatEditText;
  inherited;
end;

procedure TJvCustomNumEdit.CMExit(var Msg: TCMExit);
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  SetFocused(False);
  SetCursor(0);
  DoExit;
end;

procedure TJvCustomNumEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if NewStyleControls and not FFocused then
    Invalidate;
end;

procedure TJvCustomNumEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if PopupVisible then
    S := TJvPopupWindow(FPopup).GetPopupText
  else
    S := GetDisplayText;
  if not PaintComboEdit(Self, S, FAlignment, FFocused and not PopupVisible,
    FCanvas, Msg) then
    inherited;
end;

procedure TJvCustomNumEdit.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

//=== TJvxCurrencyEdit =======================================================

constructor TJvxCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
    ButtonWidth := 0;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

function TJvxCurrencyEdit.DefaultDisplayFormat: string;
var
  CurrStr: string;
  I: Integer;
  C: Char;
begin
  Result := ',0.' + MakeStr('0', CurrencyDecimals);
  CurrStr := '';
  for I := 1 to Length(CurrencyString) do
  begin
    C := CurrencyString[I];
    if C in [',', '.'] then
      CurrStr := CurrStr + '''' + C + ''''
    else
      CurrStr := CurrStr + C;
  end;
  if Length(CurrStr) > 0 then
    case CurrencyFormat of
      0:
        Result := CurrStr + Result; { '$1' }
      1:
        Result := Result + CurrStr; { '1$' }
      2:
        Result := CurrStr + ' ' + Result; { '$ 1' }
      3:
        Result := Result + ' ' + CurrStr; { '1 $' }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

//=== TJvCustomCalcEdit ======================================================

constructor TJvCustomCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
    FPopup := TJvPopupWindow(CreatePopupCalculator(Self, BiDiMode));
    TJvPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    UpdatePopup;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure TJvCustomCalcEdit.PopupChange;
begin
  inherited PopupChange;
  if EnablePopupChange then
    DoChange;
end;

initialization

finalization
  FreeAndNil(CalcBitmap);

end.

