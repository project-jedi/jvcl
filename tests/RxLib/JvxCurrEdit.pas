{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxCurrEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}



unit JvxCurrEdit;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Menus, Forms, StdCtrls, Mask,
  Buttons, JvxToolEdit;

type

{ TJvxCustomNumEdit }

  TJvxCustomNumEdit = class(TJvxCustomComboEdit)
  private
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FValue: Extended;
    FMinValue, FMaxValue: Extended;
    FDecimalPlaces: Cardinal;
    FBeepOnError: Boolean;
    FCheckOnExit: Boolean;
    FZeroEmpty: Boolean;
    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    FDisplayFormat: String;
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
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    function IsFormatStored: Boolean;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
{$IFDEF WIN32}
    procedure AcceptValue(const Value: Variant); override;
{$ELSE}
    procedure AcceptValue(const Value: string); override;
{$ENDIF}
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
    property Formatting: Boolean read FFormatting;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taRightJustify;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError
      default True;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property GlyphKind default gkDefault;
    property ButtonWidth default 20;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces
      default 2;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
      stored IsFormatStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property FormatOnEditing: Boolean read FFormatOnEditing
      write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; {$IFDEF Delphi5_Up} override; {$ENDIF}
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property PopupVisible;
    property Value: Extended read GetValue write SetValue;
  end;

{ TJvxCurrencyEdit }

  TJvxCurrencyEdit = class(TJvxCustomNumEdit)
  protected
    function DefaultDisplayFormat: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
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
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
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
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxCustomCalcEdit }

  TJvxCustomCalcEdit = class(TJvxCustomNumEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TJvxCalcEdit }

  TJvxCalcEdit = class(TJvxCustomCalcEdit)
  published
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
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
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
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

implementation

uses Consts, JvxStrUtils, JvxVCLUtils, JvxMaxMin, JvxCalc;

 {$R *.Res}

const
  sCalcBmp = 'CEDITBMP'; { Numeric editor button glyph }
  CalcBitmap: TBitmap = nil;

type
  TJvxHack = class(TJvxPopupWindow);

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not (Value[I] in [DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue {$IFDEF WIN32}, fvExtended {$ENDIF});
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and (S[1] in ['-', '+']);
  if IsSign then MinSym := 2
  else MinSym := 1;
  I := Pos(DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then begin
      Group := 0;
      Result := ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

{ TJvxCustomNumEdit }

constructor TJvxCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
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
    ButtonWidth := 20;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvxCustomNumEdit.Destroy;
begin
  FCanvas.Free;
  //DisposeStr(FDisplayFormat);
  if FPopup <> nil then begin
    TJvxPopupWindow(FPopup).OnCloseUp := nil;
    FPopup.Free;
    FPopup := nil;
  end;
  inherited Destroy;
end;

function TJvxCustomNumEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if CalcBitmap = nil then begin
    CalcBitmap := TBitmap.Create;
    CalcBitmap.Handle := LoadBitmap(hInstance, sCalcBmp);
  end;
  Result := CalcBitmap;
end;

function TJvxCustomNumEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

function TJvxCustomNumEdit.IsFormatStored: Boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

function TJvxCustomNumEdit.IsValidChar(Key: Char): Boolean;
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
  if (DecPos > 0) then begin
    SelStart := Pos('E', UpperCase(S));
    if (SelStart > DecPos) then DecPos := SelStart - DecPos
    else DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

procedure TJvxCustomNumEdit.KeyPress(var Key: Char);
begin
  if PopupVisible and (UpCase(Key) in ['0'..'9', DecimalSeparator, '.', ',',
    '+', '-', '*', '/', '_', '=', 'C', 'R', 'Q', '%', #8, #13] -
    [ThousandSeparator]) then
  begin
    TJvxHack(FPopup).KeyPress(Key);
    Key := #0;
  end;
  if Key in ['.', ','] - [ThousandSeparator] then
    Key := DecimalSeparator;
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and not IsValidChar(Key) then begin
    if BeepOnError then MessageBeep(0);
    Key := #0;
  end
  else if Key = #27 then begin
    Reset;
    Key := #0;
  end;
end;

procedure TJvxCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TJvxCustomNumEdit.SetZeroEmpty(Value: Boolean);
begin
  if FZeroEmpty <> Value then begin
    FZeroEmpty := Value;
    DataChanged;
  end;
end;

procedure TJvxCustomNumEdit.SetBeepOnError(Value: Boolean);
begin
  if FBeepOnError <> Value then begin
    FBeepOnError := Value;
    UpdatePopup;
  end;
end;

procedure TJvxCustomNumEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvxCustomNumEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then begin
    FDisplayFormat := Value;
    Invalidate;
    DataChanged;
  end;
end;

function TJvxCustomNumEdit.GetDisplayFormat: string;
begin
  Result := FDisplayFormat;
end;

procedure TJvxCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then begin
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

procedure TJvxCustomNumEdit.SetFormatOnEditing(Value: Boolean);
begin
  if FFormatOnEditing <> Value then begin
    FFormatOnEditing := Value;
    if FFormatOnEditing then inherited Alignment := Alignment
    else inherited Alignment := taLeftJustify;
    if FFormatOnEditing and FFocused then ReformatEditText
    else if FFocused then begin
      UpdateData;
      DataChanged;
    end;
  end;
end;

procedure TJvxCustomNumEdit.SetDecimalPlaces(Value: Cardinal);
begin
  if FDecimalPlaces <> Value then begin
    FDecimalPlaces := Value;
    DataChanged;
    Invalidate;
  end;
end;

function TJvxCustomNumEdit.FormatDisplayText(Value: Extended): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else
    Result := FloatToStr(Value);
end;

function TJvxCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(FValue);
end;

procedure TJvxCustomNumEdit.Clear;
begin
  Text := '';
end;

procedure TJvxCustomNumEdit.DataChanged;
var
  EditFormat: string;
begin
  EditFormat := '0';
  if FDecimalPlaces > 0 then
    EditFormat := EditFormat + '.' + MakeStr('#', FDecimalPlaces);
  if (FValue = 0.0) and FZeroEmpty then
    EditText := ''
  else
    EditText := FormatFloat(EditFormat, FValue);
end;

function TJvxCustomNumEdit.CheckValue(NewValue: Extended;
  RaiseOnError: Boolean): Extended;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then begin
    if (FMaxValue > FMinValue) then begin
      if NewValue < FMinValue then Result := FMinValue
      else if NewValue > FMaxValue then Result := FMaxValue;
    end
    else begin
      if FMaxValue = 0 then begin
        if NewValue < FMinValue then Result := FMinValue;
      end
      else if FMinValue = 0 then begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateFmt(ReplaceStr(ResStr(SOutOfRange), '%d', '%.*f'),
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;

procedure TJvxCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TJvxCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

procedure TJvxCustomNumEdit.UpdatePopup;
begin
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, DefCalcPrecision, BeepOnError);
end;

function TJvxCustomNumEdit.GetValue: Extended;
begin
  if not (csDesigning in ComponentState) then
    try
      UpdateData;
    except
      FValue := FMinValue;
    end;
  Result := FValue;
end;

procedure TJvxCustomNumEdit.SetValue(AValue: Extended);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

function TJvxCustomNumEdit.GetAsInteger: Longint;
begin
  Result := Trunc(Value);
end;

procedure TJvxCustomNumEdit.SetAsInteger(AValue: Longint);
begin
  SetValue(AValue);
end;

procedure TJvxCustomNumEdit.SetMinValue(AValue: Extended);
begin
  if FMinValue <> AValue then begin
    FMinValue := AValue;
    Value := FValue;
  end;
end;

procedure TJvxCustomNumEdit.SetMaxValue(AValue: Extended);
begin
  if FMaxValue <> AValue then begin
    FMaxValue := AValue;
    Value := FValue;
  end;
end;

function TJvxCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TJvxCustomNumEdit.TextToValText(const AValue: string): string;
begin
  Result := DelRSpace(AValue);
  if DecimalSeparator <> ThousandSeparator then begin
    Result := DelChars(Result, ThousandSeparator);
  end;
  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', DecimalSeparator);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', DecimalSeparator);
  if Result = '' then Result := '0'
  else if Result = '-' then Result := '-0';
end;

procedure TJvxCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TJvxCustomNumEdit.ReformatEditText;
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
    if HandleAllocated then GetSel(SelStart, SelStop);
    if not IsEmpty then S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
    if HandleAllocated and (GetFocus = Handle) and not
      (csDesigning in ComponentState) then
    begin
      Inc(SelStart, Length(S) - OldLen);
      SetCursor(SelStart);
    end;
  finally
    FFormatting := False;
  end;
end;

procedure TJvxCustomNumEdit.Change;
begin
  if not FFormatting then begin
    if FFormatOnEditing and FFocused then ReformatEditText;
    inherited Change;
  end;
end;

{$IFDEF WIN32}
procedure TJvxCustomNumEdit.AcceptValue(const Value: Variant);
{$ELSE}
procedure TJvxCustomNumEdit.AcceptValue(const Value: string);
{$ENDIF}
begin
  inherited AcceptValue(Value);
end;

procedure TJvxCustomNumEdit.WMPaste(var Message: TMessage);
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
    if CanFocus then SetFocus;
    if BeepOnError then MessageBeep(0);
  end;
end;

procedure TJvxCustomNumEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited;
end;

procedure TJvxCustomNumEdit.CMExit(var Message: TCMExit);
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  SetCursor(0);
  DoExit;
end;

procedure TJvxCustomNumEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if NewStyleControls and not FFocused then Invalidate;
end;

procedure TJvxCustomNumEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if PopupVisible then S := TJvxPopupWindow(FPopup).GetPopupText
  else S := GetDisplayText;
  if not PaintComboEdit(Self, S, FAlignment, FFocused and not PopupVisible,
    FCanvas, Message) then inherited;
end;

procedure TJvxCustomNumEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ TJvxCurrencyEdit }

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
  for I := 1 to Length(CurrencyString) do begin
    C := CurrencyString[I];
    if C in [',', '.'] then CurrStr := CurrStr + '''' + C + ''''
    else CurrStr := CurrStr + C;
  end;
  if Length(CurrStr) > 0 then
    case CurrencyFormat of
      0: Result := CurrStr + Result; { '$1' }
      1: Result := Result + CurrStr; { '1$' }
      2: Result := CurrStr + ' ' + Result; { '$ 1' }
      3: Result := Result + ' ' + CurrStr; { '1 $' }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

{ TJvxCustomCalcEdit }

constructor TJvxCustomCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
    FPopup := TJvxPopupWindow(CreatePopupCalculator(Self
      {$IFDEF Delphi4_Up}, BiDiMode {$ENDIF}));
    TJvxPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    UpdatePopup;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure DestroyLocals; far;
begin
  CalcBitmap.Free;
  CalcBitmap := nil;
end;

{$IFDEF WIN32}
initialization
finalization
  DestroyLocals;
{$ELSE}
initialization
  AddExitProc(DestroyLocals);
{$ENDIF}
end.
