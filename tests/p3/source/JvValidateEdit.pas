{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidateEdit.pas, released on yyyy-mm-dd

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: yyyy-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Overview:
  A custom edit control that can restrict the characters accepted as input by setting the
  CharType property to the type of restriction wanted. The following types are supported:

  efAny - all characters are accepted, works like normal edit
  efInteger - only 0..9,-,+ are accepted
  efFloat   - same as efInteger, but also accepts DecimalSeparator
  efScientific - same as efFloat but accepts E,e also
  efCurrency   - same as efFloat but inserts currency symbol on Exit
  efHex     - accepts 0-9,A-F,a-f
  efAlpha   - accepts A-Z,a-z
  efAlphaNum - same as efInteger (without -,+) and efAlpha combined
  efValidChars - the characters in Characters contains the characters accepted. This is case-sensitive
  efInvalidChars - the characters in Characters contains the characters not accepted. This is case-sensitive
  efCustom - the OnCustomValidate event is called for validation

Known Issues:
  This component doesn't ensure that the characters are entered in the correct order: just
  that only correct characters can be entered. F ex efFloat would accept 1+2-.20 and
  efInteger would accept 1+2-3. You will have to check the validity of the entered text
  before using it!

  There are probably some issues with the efCurrency format - I haven't tested it with all
  formats and positive / negative values yet.

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvValidateEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, JVCLVer, JvTypes;

type
  TJvCharType = (efAny, efAlpha, efInteger, efAlphaNum, efHex, efFloat, efScientific, efCurrency, efValidChars,
    efInvalidChars, efCustom);
  TJvCharacters = string;
  TJvCustomTextValidateEvent = procedure(Sender: TObject; Key: Char; const AText: string; var IsValid: boolean) of
    object;

  TJvCustomValidateEdit = class(TCustomEdit)
  private
    { Private declarations }
    FCharType: TJvCharType;
    FCharacters: TJvCharacters;
    FOnCustomValidate: TJvCustomTextValidateEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FOnSetFocus: TJvFocusChangeEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    procedure SetCharType(Value: TJvCharType);
    procedure WMPaste(var Message: Tmessage); message WM_PASTE;
    procedure SetText(Value: TCaption);
    function GetText: TCaption;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWmKillFocus); message WM_KILLFOCUS;
  protected
    { Protected declarations }
    function RemoveCurrency(const S: string): string;
    function InsertCurrency(const S: string): string;
    procedure DoKillFocus(Msg: TWmKillFocus); dynamic;
    procedure DoSetFocus(Msg: TWmSetFocus); dynamic;
    function IsValidChar(const S: string; Key: Char): boolean; virtual;
    procedure KeyPress(var Key: Char); override;
    function DoValidate(const Key: Char; const AText: string): boolean; dynamic;
    property CharType: TJvCharType read FCharType write SetCharType default efAny;
    // Characters is used with the efValidChars and efInvalidChars types only!
    property Characters: TJvCharacters read FCharacters write FCharacters;
    property Text: TCaption read GetText write SetText;
    property OnCustomValidate: TJvCustomTextValidateEvent read FOnCustomValidate write FOnCustomValidate;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure MakeValid;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvValidateEdit = class(TJvCustomValidateEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharType;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Characters;
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
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomValidate;
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
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSetFocus;
    property OnStartDock;
    property OnStartDrag;
  end;

// procedure Register;

implementation

{
procedure Register;
begin
  RegisterComponents('Jv Converters', [TJvValidateEdit]);
end;
}

constructor TJvCustomValidateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCharType := efAny;
end;

procedure TJvCustomValidateEdit.DoKillFocus(Msg: TWmKillFocus);
begin
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Self, FindControl(Msg.FocusedWnd));
end;

procedure TJvCustomValidateEdit.DoSetFocus(Msg: TWmSetFocus);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Self, FindControl(Msg.FocusedWnd));
end;

function TJvCustomValidateEdit.DoValidate(const Key: Char;
  const AText: string): boolean;
begin
  Result := True;
  if Assigned(FOnCustomValidate) then
    FOnCustomValidate(Self, Key, AText, Result);
end;

function TJvCustomValidateEdit.GetText: TCaption;
begin
  Result := inherited Text;
end;

function DeleteChars(const S: string; Ch: TSysCharSet): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if not (S[i] in Ch) then
      Result := Result + S[i];
end;

function TJvCustomValidateEdit.InsertCurrency(const S: string): string;
begin
  if S = '' then
    Exit;
  Result := RemoveCurrency(S);
  if (Pos('-', Result) > 0) or (Pos('(', Result) > 0) then
  begin
    Result := DeleteChars(Result, [' ', '-', '+', '(', ')']);
    case NegCurrFormat of
      0: // ($1)
        Result := Format('(%s%s)', [CurrencyString, S]);
      1: // -$1
        Result := Format('-%s%s', [CurrencyString, S]);
      2: // $-1
        Result := Format('%s-%s', [CurrencyString, Result]);
      3: // $1-
        Result := Format('%s%s-', [CurrencyString, Result]);
      4: // (1$)
        Result := Format('(%s%s)', [Result, CurrencyString]);
      5: // -1$
        Result := Format('-%s%s', [Result, CurrencyString]);
      6: // 1-$
        Result := Format('%s-%s', [Result, CurrencyString]);
      7: // 1$-
        Result := Format('%s%s-', [Result, CurrencyString]);
      8: // -1 $
        Result := Format('-%s %s', [Result, CurrencyString]);
      9: // -$ 1
        Result := Format('-%s %s', [CurrencyString, Result]);
      10: // 1 $-
        Result := Format('%s %s-', [Result, CurrencyString]);
      11: // $ 1-
        Result := Format('%s %s-', [CurrencyString, Result]);
      12: // $ -1
        Result := Format('%s -%s', [CurrencyString, Result]);
      13: // 1- $
        Result := Format('%s- %s', [Result, CurrencyString]);
      14: // ($ 1)
        Result := Format('(%s %s)', [CurrencyString, Result]);
      15: // (1 $)
        Result := Format('(%s %s)', [Result, CurrencyString]);
    end
  end
  else
  begin
    Result := DeleteChars(Result, [' ', '+', '(', ')']);
    case CurrencyFormat of
      0: // $1
        Result := Format('%s%s', [CurrencyString, Result]);
      1: // 1$
        Result := Format('%s%s', [Result, CurrencyString]);
      2: // $ 1
        Result := Format('%s %s', [CurrencyString, Result]);
      3: // 1 $
        Result := Format('%s %s', [Result, CurrencyString]);
    end;
  end;
end;

function TJvCustomValidateEdit.IsValidChar(const S: string; Key: Char): boolean;
const
  IntCodes = [#8, '0'..'9'];
  HexCodes = IntCodes + ['A'..'F', 'a'..'f'];
//var
//  FTmpKey:string;FKeyInfo:word;
begin
//  FKeyInfo := 0;
//  FTmpKey := Key;
//  GetStringTypeEx(LOCALE_USER_DEFAULT,CT_CTYPE1,PChar(FTmpKey),1,FKeyInfo);
  case FCharType of
    efAny: Result := True;
    efAlpha: Result := IsCharAlpha(Key);
    efInteger: Result := (Key in IntCodes) or ((Key = '+') and (Pos('+', S) = 0)) or ((Key = '-') and (Pos('-', S) =
        0));
    efAlphaNum: Result := IsCharAlphaNumeric(Key);
    efHex: Result := (Key in HexCodes);
    efFloat, efCurrency: Result := (Key in IntCodes)
      or ((Key = DecimalSeparator) and (Pos(DecimalSeparator, S) = 0))
        or ((Key = '+') and (Pos('+', S) = 0)) or ((Key = '-') and (Pos('-', S) = 0));
    efScientific: Result := (Key in IntCodes)
      or ((Key = DecimalSeparator) and (Pos(DecimalSeparator, S) = 0))
        or ((Key in ['E', 'e']) and (Pos('e', LowerCase(S)) = 0))
        or ((Key = '+') and (Pos('+', S) = 0)) or ((Key = '-') and (Pos('-', S) = 0));
    efValidChars: Result := Pos(Key, Characters) > 0;
    efInvalidChars: Result := Pos(Key, Characters) = 0;
  else // efCustom
    Result := DoValidate(Key, S);
  end;
end;

procedure TJvCustomValidateEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Text, Key) then
    Key := #0;
  inherited;
end;

procedure TJvCustomValidateEdit.MakeValid;
var
  S: string;
  i: integer;
begin
  S := '';
  for i := 1 to Length(Text) do
    if IsValidChar(Copy(Text, 1, i - 1), Text[i]) then
      S := S + Text[i];
  if not Focused and (CharType = efCurrency) then
    inherited Text := InsertCurrency(S)
  else
    inherited Text := S;
end;

function TJvCustomValidateEdit.RemoveCurrency(const S: string): string;
begin
  Result := StringReplace(DeleteChars(S, [' ', '(', ')']), CurrencyString, '', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TJvCustomValidateEdit.SetCharType(Value: TJvCharType);
begin
  if FCharType <> Value then
  begin
    FCharType := Value;
    MakeValid;
  end;
end;

procedure TJvCustomValidateEdit.SetText(Value: TCaption);
begin
  inherited Text := Value;
  MakeValid;
end;

procedure TJvCustomValidateEdit.WMKillFocus(var Msg: TWmKillFocus);
begin
  inherited;
  DoKillFocus(Msg);
  if CharType = efCurrency then
    inherited Text := InsertCurrency(Text);
end;

procedure TJvCustomValidateEdit.WMPaste(var Message: Tmessage);
begin
  inherited;
  MakeValid;
end;

procedure TJvCustomValidateEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  DoSetFocus(Msg);
  if CharType = efCurrency then
    inherited Text := RemoveCurrency(Text);
end;

end.

