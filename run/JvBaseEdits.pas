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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  (rb) Compare property names with those of TJvSpinEdit, JvValidateEdit, for
       example DecimalPlaces/Decimal, CheckMinValue (name indicates action?
       maybe better: TJvValidateEdit's HasMinValue) etc.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBaseEdits;

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, ImgList,
  {$IFDEF VisualCLX}
  QTypes, 
  {$ENDIF VisualCLX}
  JvToolEdit;

type
  TJvCustomNumEdit = class(TJvCustomComboEdit)
  private
    FCanvas: TControlCanvas; // asn: never created
    FAlignment: TAlignment;
    FFocused: Boolean;
    FValue: Extended;
    FMinValue: Extended;
    FMaxValue: Extended;
    FDecimalPlaces: Cardinal;
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
    function GetText: string; {$IFDEF VisualCLX} reintroduce; {$ENDIF}
    procedure SetText(const AValue: string); {$IFDEF VisualCLX} reintroduce; {$ENDIF}
//    function TextToValText(const AValue: string): string;
    //Polaris    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    function IsFormatStored: Boolean;
    {$IFDEF VCL}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure Paint; override;
    {$ENDIF VisualCLX}
    procedure DoClipboardPaste; override;
    procedure SetBeepOnError(Value: Boolean); override;

    procedure EnabledChanged; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure FontChanged; override;
    //Polaris up to protected
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    procedure AcceptValue(const Value: Variant); override;
    procedure Change; override;
    procedure ReformatEditText; dynamic;
    class function DefaultImageIndex: TImageIndex; override;
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
    property Formatting: Boolean read FFormatting;
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property ImageKind default ikDefault;
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
    {$IFDEF VCL}
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property Align; //Polaris
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CheckOnExit;
    property ClipboardCommands; // RDB
    property Color;
    property Constraints;
    property DecimalPlaceRound; //Polaris
    property DecimalPlaces;
    property DisabledColor; // RDB
    property DisabledTextColor; // RDB
    property DisplayFormat;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property HideSelection;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
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
    property OnContextPopup;
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
    property OnStartDrag;
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

  { (rb) why no ButtonFlat property? }
  TJvCalcEdit = class(TJvCustomCalcEdit)
  published
    {$IFDEF VCL}
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property Action;
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
    property DecimalPlaceRound; //Polaris
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragMode;
    property Enabled;
    property EnablePopupChange;
    property Font;
    property FormatOnEditing;
    property ImageIndex;
    property Images;
    property ImageKind;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
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
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

implementation

uses
  Consts,
  Math, JvJCLUtils, JvCalc, JvConsts, JvResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvBaseEdits.Res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvBaseEdits.Res}
{$ENDIF LINUX}

const
  sCalcBmp = 'JV_CEDITBMP'; { Numeric editor button glyph }

var
  GCalcImageIndex: TImageIndex = -1;

type
  TJvPopupWindowAccessProtected = class(TJvPopupWindow);

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(PChar(Value), RetValue, fvExtended);
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in SignSymbols);
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
  FAlignment := taRightJustify;
  FDisplayFormat := DefaultDisplayFormat;
  FDecimalPlaces := 2;
  FZeroEmpty := True;
  inherited Text := '';
  inherited Alignment := taLeftJustify;
  { forces update }
  DataChanged;
  ControlState := ControlState + [csCreating];
  try
    { TODO : Check }
    ImageKind := ikDefault;
    //Polaris ButtonWidth := 20;
    ButtonWidth := 21;
  finally
    ControlState := ControlState - [csCreating];
  end;
  {$IFDEF VisualCLX}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  {$ENDIF VisualCLX}
end;

destructor TJvCustomNumEdit.Destroy;
begin
  FCanvas.Free;
  if FPopup <> nil then
  begin
    TJvPopupWindow(FPopup).OnCloseUp := nil;
    FPopup.Parent := nil;
  end;
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
  if PopupVisible and (UpCase(Key) in
    DigitSymbols +
    [DecimalSeparator, '.', ',', '+', '-', '*', '/', '_', '=', 'C', 'R', 'Q', '%', Backspace, Cr] -
    [ThousandSeparator]) then
  begin
    TJvPopupWindowAccessProtected(FPopup).KeyPress(Key);
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
  if Key = Esc then
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
  if BeepOnError <> Value then
  begin
    inherited SetBeepOnError(Value);
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
  WasModified: Boolean;
begin
  EditFormat := '0';
  if FDecimalPlaces > 0 then
    EditFormat := EditFormat + '.' + MakeStr('#', FDecimalPlaces);
  { Changing EditText sets Modified to false }
  WasModified := Modified;
  try
    if (FValue = 0.0) and FZeroEmpty then
      EditText := ''
    else
      EditText := FormatFloat(EditFormat, CheckValue(FValue, False));
  finally
    Modified := WasModified;
  end;
end;

function TJvCustomNumEdit.CheckValue(NewValue: Extended;
  RaiseOnError: Boolean): Extended;
var
  DP: Integer;
begin
  if FDecimalPlaceRound then
  begin //Polaris
    DP := FDecimalPlaces;
    { (rb) Probably: Round to the nearest, and if two are equally near, away from zero
           Ln, Exp are slow; make more generic (why only this one?), see
           http://www.merlyn.demon.co.uk/pas-chop.htm
    }
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
      raise ERangeError.CreateResFmt(@RsEOutOfRangeXFloat,
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

(*
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
    if (X = DecimalSeparator) or (X = '-') or (X in DigitSymbols) then
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
*)

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

procedure TJvCustomNumEdit.DoClipboardPaste;
var
  S: string;
  WasModified: Boolean;
begin
  WasModified := Modified;
  S := EditText;
  try
    inherited DoClipboardPaste;
    UpdateData;
  except
    { Changing EditText sets Modified to false }
    EditText := S;
    Modified := WasModified;
    SelectAll;
    if CanFocus then
      SetFocus;
    DoBeepOnError;
  end;
end;

procedure TJvCustomNumEdit.DoEnter;
begin
  SetFocused(True);
  if FFormatOnEditing then
    ReformatEditText;
  inherited DoEnter;
end;

procedure TJvCustomNumEdit.DoExit;
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
  inherited DoExit;
end;

procedure TJvCustomNumEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if NewStyleControls and not FFocused then
    Invalidate;
end;

{$IFDEF VCL}
procedure TJvCustomNumEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if PopupVisible then
    S := TJvPopupWindow(FPopup).GetPopupText
  else
    S := GetDisplayText;
  if not PaintComboEdit(Self, S, FAlignment,
    FFocused and not PopupVisible, FCanvas, Msg) then
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCustomNumEdit.Paint;
var
  S: string;
begin
  if PopupVisible then
    S := TJvPopupWindow(FPopup).GetPopupText
  else
    S := GetDisplayText;
  if not PaintComboEdit(Self, S, FAlignment,
    True, FFocused and not PopupVisible, FCanvas) then
    inherited;
end;
{$ENDIF VisualCLX}

procedure TJvCustomNumEdit.FontChanged;
begin
  inherited FontChanged;
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
  ControlStyle := ControlStyle - [csAcceptsControls];
  ControlState := ControlState + [csCreating];
  try
    FPopup := TJvPopupWindow(CreatePopupCalculator(Self {$IFDEF VCL}, BiDiMode {$ENDIF VCL}));
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

class function TJvCustomNumEdit.DefaultImageIndex: TImageIndex;
var
  Bmp: TBitmap;
begin
  if GCalcImageIndex < 0 then
  begin
    Bmp := TBitmap.Create;
    //Bmp.Handle := LoadBitmap(HInstance, sCalcBmp);
    Bmp.LoadFromResourceName(HInstance, sCalcBmp);
    GCalcImageIndex := DefaultImages.AddMasked(Bmp, clFuchsia);
    Bmp.Free;
  end;

  Result := GCalcImageIndex;
end;

end.

