{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeLine.PAS, released on 2002-10-13.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-10-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Overview:
  A custom edit control that can restrict the characters accepted as input by setting the
  CharType property to the type of restriction wanted. The following types are supported:

  efAny - all characters are accepted, works like normal edit
  efInteger - only 0..9,-,+ are accepted
  efFloat   - same as efInteger, but also accepts DecimalSeparator
  efCurrency   - same as efFloat but inserts currency symbol on KillFocus and removes it on SetFocus
  efHex     - accepts #8,0-9,A-F,a-f
  efAlpha   - accepts #8,A-Z,a-z and any locale specific characters
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

unit JvValidateCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, StdCtrls, JVCLVer, JvTypes, JvErrProvider, JvValidators;

type
  TJvCharType = (efAny, efAlpha, efInteger, efAlphaNum, efHex,
    efFloat, efCurrency, efValidChars, efInvalidChars, efCustom);
  TJvCharacters = string;
  TJvCustomValidateChar = procedure(Sender: TObject; Key: Char; const AText: string; var IsValid: boolean) of object;
  TJvCustomValidateText = procedure(Sender: TObject; const AText: string; var IsValid: boolean) of object;
(*
  IJvValidateText = interface
  ['{C67F2E97-7DF3-499B-9141-44BBB654D917}']
    function RemoveCurrency(const S: string): string;
    function InsertCurrency(const S: string): string;
    procedure MakeValid;
    function IsValid:boolean;overload;
    function IsValid(const AText:string):boolean;overload;
    function IsValid(const AText:string;AChar:char):boolean;overload;
    property Text:string read GetText write SetText;
    property CharType: TJvCharType read GetCharType write SetCharType default efAny;
    property Characters: TJvCharacters read GetCharacters write SetCharacters;
    property OnMakeValid:TNotifyEvent read GetOnMakeValid write SetOnMakeValid;
    property OnValidateError:TNotifyEvent read GetOnValidateError write SetOnValidateError;
  end;
*)

  TJvValidateText = class(TPersistent)
  private
    FOnValidateChar: TJvCustomValidateChar;
    FOnValidateText: TJvCustomValidateText;
    FOnValidateError: TNotifyEvent;
    FOnMakeValid: TNotifyEvent;
    FCharacters:TJvCharacters;
    FCharType:TJvCharType;
    FText:string;
    function GetCharacters: TJvCharacters;
    function GetCharType: TJvCharType;
    function GetText: string;
    procedure SetCharacters(const Value: TJvCharacters);
    procedure SetCharType(const Value: TJvCharType);
    procedure SetText(const Value: string);
    function GetOnValidateChar: TJvCustomValidateChar;
    function GetOnValidateError: TNotifyEvent;
    function GetOnValidateText: TJvCustomValidateText;
    procedure SetOnValidateChar(const Value: TJvCustomValidateChar);
    procedure SetOnValidateError(const Value: TNotifyEvent);
    procedure SetOnValidateText(const Value: TJvCustomValidateText);
    function GetOnMakeValid: TNotifyEvent;
    procedure SetOnMakeValid(const Value: TNotifyEvent);
  protected
    procedure DoMakeValid;dynamic;
    procedure DoValidateError;dynamic;
    function DoValidateText(const AText:string):boolean;dynamic;
    function DoValidateChar(const Key: Char; const AText: string): boolean; dynamic;
  public
    function RemoveCurrency(const S: string): string;
    function InsertCurrency(const S: string): string;
    procedure MakeValid;
    function IsValid:boolean;overload;
    function IsValid(const AText:string):boolean;overload;
    function IsValid(const AText:string;AChar:char):boolean;overload;
  published
    property Text:string read GetText write SetText;
    property CharType: TJvCharType read GetCharType write SetCharType default efAny;
    property Characters: TJvCharacters read GetCharacters write SetCharacters;
    property OnMakeValid:TNotifyEvent read GetOnMakeValid write SetOnMakeValid;
    property OnValidateError:TNotifyEvent read GetOnValidateError write SetOnValidateError;
    property OnValidateText:TJvCustomValidateText read GetOnValidateText write SetOnValidateText;
    property OnValidateChar: TJvCustomValidateChar read GetOnValidateChar write SetOnValidateChar;
  end;

  TJvCustomValidateEdit = class(TCustomEdit, IUnknown, IJvErrorProviderClient,IJvValidationProperty)
  private
    { Private declarations }
    FValidateText:TJvValidateText;
    FAboutJVCL: TJVCLAboutInfo;
    FOnSetFocus: TJvFocusChangeEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    FErrorProvider: IJvErrorProvider;
    FErrorMessage: WideString;
{$IFNDEF COMPILER6_UP}
    FErrorProviderComponent: TComponent;
    procedure setErrorProviderComponent(const Value: TComponent);
{$ENDIF}
    procedure WMPaste(var Message: Tmessage); message WM_PASTE;
    procedure SetText(Value: TCaption);
    function GetText: TCaption;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWmKillFocus); message WM_KILLFOCUS;

    procedure UpdateProvider;
    procedure ClearProvider;
  protected
    {IErrorProviderCient}
    procedure setErrorProvider(const Value: IJvErrorProvider);virtual;
    function getErrorProvider: IJvErrorProvider;virtual;
    function getControl: TControl;virtual;
    procedure setErrorMessage(const Value: WideString);virtual;
    function getErrorMessage: WideString;virtual;
    {IJvValidationProperty}
    function GetValidationPropertyValue: Variant;virtual;
    function GetValidationPropertyName:WideString;virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoKillFocus(Msg: TWmKillFocus); dynamic;
    procedure DoSetFocus(Msg: TWmSetFocus); dynamic;
    procedure DoMakeValid(Sender:TObject);dynamic;
    procedure DoValidateError(Sender:TObject);dynamic;
    procedure KeyPress(var Key: Char); override;
    property Text: TCaption read GetText write SetText;
    property Validator:TJvValidateText read FValidateText;
    property OnSetFocus: TJvFocusChangeEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TJvFocusChangeEvent read FOnKillFocus write FOnKillFocus;
{$IFDEF COMPILER6_UP}
    property ErrorProvider: IJvErrorProvider read getErrorProvider write setErrorProvider;
{$ELSE}
    property ErrorProvider: TComponent read FErrorProviderComponent write setErrorProviderComponent;
{$ENDIF}
    property ErrorMessage: WideString read getErrorMessage write setErrorMessage;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored false;
  end;

  TJvValidateEdit = class(TJvCustomValidateEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ErrorProvider;
    property ErrorMessage;
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
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Validator;
    property Visible;
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
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSetFocus;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation


function DeleteChars(const S: string; Ch: TSysCharSet): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if not (S[i] in Ch) then
      Result := Result + S[i];
end;

{ TJvValidateText }

procedure TJvValidateText.DoMakeValid;
begin
  if Assigned(FOnMakeValid) then
    FOnMakeValid(self);
end;

function TJvValidateText.DoValidateChar(const Key: Char;
  const AText: string): boolean;
begin
  Result := true;
  if Assigned(FOnValidateChar) then
    FOnValidateChar(self,Key,AText,Result);
end;

procedure TJvValidateText.DoValidateError;
begin
  if Assigned(FOnValidateError) then
    FOnValidateError(self);
end;

function TJvValidateText.DoValidateText(const AText:string): boolean;
begin
  Result := true;
  if Assigned(FOnValidateText) then
    FOnValidateText(self,AText,Result)
  else
    Result := false;
end;

function TJvValidateText.GetCharacters: TJvCharacters;
begin
  Result := FCharacters;
end;

function TJvValidateText.GetCharType: TJvCharType;
begin
  Result := FCharType;
end;

function TJvValidateText.GetOnMakeValid: TNotifyEvent;
begin
  Result := FOnMakeValid;
end;

function TJvValidateText.GetOnValidateChar: TJvCustomValidateChar;
begin
  Result := FOnValidateChar;
end;

function TJvValidateText.GetOnValidateError: TNotifyEvent;
begin
  Result := FOnValidateError;
end;

function TJvValidateText.GetOnValidateText: TJvCustomValidateText;
begin
  Result := FOnValidateText;
end;

function TJvValidateText.GetText: string;
begin
  Result := FText;
end;

function TJvValidateText.InsertCurrency(const S: string): string;
begin
  if S = '' then Exit;
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

function TJvValidateText.IsValid(const AText: string;
  AChar: char): boolean;
const
  IntCodes = [#8, '0'..'9'];
  FloatCodes = IntCodes;
  HexCodes = IntCodes + ['A'..'F', 'a'..'f'];
begin
  case FCharType of
    efAny: Result := True;
    efAlpha: Result := IsCharAlpha(AChar);
    efInteger: Result := (AChar in IntCodes) or ((AChar = '+') and (Pos('+', AText) = 0)) or ((AChar = '-') and (Pos('-', AText) = 0));
    efAlphaNum: Result := IsCharAlphaNumeric(AChar);
    efHex: Result := (AChar in HexCodes);
    efFloat, efCurrency: Result := (AChar in IntCodes)
      or ((AChar = DecimalSeparator) and (Pos(DecimalSeparator, AText) = 0))
        or ((AChar = '+') and (Pos('+', AText) = 0)) or ((AChar = '-') and (Pos('-', AText) = 0));
    efValidChars: Result := (Pos(AChar, Characters) > 0);
    efInvalidChars: Result := (Pos(AChar, Characters) < 1);
  else // efCustom
    Result := DoValidateChar(AChar, AText);
  end;
  if not Result then
    DoValidateError;
end;

function TJvValidateText.IsValid(const AText: string): boolean;
var i:integer;
begin
  Result := DoValidateText(AText);
  if Result then Exit;
  for i := 1 to Length(AText) do
    if not IsValid(Copy(AText, 1, i - 1), AText[i]) then
    begin
      DoValidateError;
      Exit;
    end;
  Result := true;
end;

function TJvValidateText.IsValid: boolean;
begin
  Result := IsValid(Text);
end;

procedure TJvValidateText.MakeValid;
var S: string; i: integer;
begin
  S := '';
  for i := 1 to Length(Text) do
    if IsValid(Copy(Text, 1, i - 1), Text[i]) then
      S := S + Text[i];
  if (CharType = efCurrency) then
    Text := InsertCurrency(S)
  else
    Text := S;
  DoMakeValid;
end;

function TJvValidateText.RemoveCurrency(const S: string): string;
begin
  Result := StringReplace(DeleteChars(S, [' ', '(', ')']), CurrencyString, '', [rfReplaceAll, rfIgnoreCase]);
  MakeValid;
end;

procedure TJvValidateText.SetCharacters(const Value: TJvCharacters);
begin
  if FCharacters <> Value then
  begin
    FCharacters := Value;
    MakeValid;
  end;
end;

procedure TJvValidateText.SetCharType(const Value: TJvCharType);
begin
  if FCharType <> Value then
  begin
    FCharType := Value;
    MakeValid;
  end;
end;

procedure TJvValidateText.SetOnMakeValid(const Value: TNotifyEvent);
begin
  FOnMakeValid := Value;
end;

procedure TJvValidateText.SetOnValidateChar(const Value: TJvCustomValidateChar);
begin
  FOnValidateChar := Value;
end;

procedure TJvValidateText.SetOnValidateError(const Value: TNotifyEvent);
begin
  FOnValidateError := Value;
end;

procedure TJvValidateText.SetOnValidateText(const Value: TJvCustomValidateText);
begin
  FOnValidateText := Value;
end;

procedure TJvValidateText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    MakeValid;
  end;
end;

{ TJvCustomValidateEdit }

constructor TJvCustomValidateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidateText := TJvValidateText.Create;
  FValidateText.CharType := efAny;
  FValidateText.OnMakeValid := DoMakeValid;
  FValidateText.OnValidateError := DoValidateError;
end;

destructor TJvCustomValidateEdit.Destroy;
begin
  FValidateText.Free;
  inherited;
end;

// Text Validator

function TJvCustomValidateEdit.GetText: TCaption;
begin
  Result := inherited Text;
end;

procedure TJvCustomValidateEdit.KeyPress(var Key: Char);
begin
  if not FValidateText.IsValid(Text, Key) then
    Key := #0
  else
    FValidateText.Text := Text;
  inherited;
end;

procedure TJvCustomValidateEdit.SetText(Value: TCaption);
begin
  FValidateText.Text := Value;
  if not FValidateText.IsValid then
    UpdateProvider;
  FValidateText.MakeValid;
end;

procedure TJvCustomValidateEdit.WMKillFocus(var Msg: TWmKillFocus);
begin
  inherited;
  DoKillFocus(Msg);
  if FValidateText.CharType = efCurrency then
    inherited Text := FValidateText.InsertCurrency(Text);
end;

procedure TJvCustomValidateEdit.WMPaste(var Message: Tmessage);
begin
  inherited;
  FValidateText.Text := Text;
  FValidateText.MakeValid;
end;

procedure TJvCustomValidateEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  DoSetFocus(Msg);
  if FValidateText.CharType = efCurrency then
    inherited Text := FValidateText.RemoveCurrency(Text);
end;

procedure TJvCustomValidateEdit.DoMakeValid(Sender:TObject);
begin
  inherited Text := FValidateText.Text;
end;

// Focus
procedure TJvCustomValidateEdit.DoKillFocus(Msg: TWmKillFocus);
begin
  if Assigned(FOnKillFocus) then
    FOnKillFocus(self, FindControl(Msg.FocusedWnd));
end;

procedure TJvCustomValidateEdit.DoSetFocus(Msg: TWmSetFocus);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(self, FindControl(Msg.FocusedWnd));
end;

// IJvErrorProvider

function TJvCustomValidateEdit.getControl: TControl;
begin
  Result := self;
end;

function TJvCustomValidateEdit.getErrorMessage: WideString;
begin
  Result := FErrorMessage;
end;

function TJvCustomValidateEdit.getErrorProvider: IJvErrorProvider;
begin
  Result := FErrorProvider;
end;

{$IFNDEF COMPILER6_UP}

procedure TJvCustomValidateEdit.setErrorProviderComponent(const Value: TComponent);
var obj: IJvErrorProvider;
begin
  if FErrorProviderComponent <> nil then
    FErrorProviderComponent.RemoveFreeNotification(self);
  if Value = nil then
  begin
    FErrorProviderComponent := nil;
    setErrorProvider(nil);
    Exit;
  end;
  if not Supports(Value, IJvErrorProvider, obj) then
    raise Exception.Create('Component does not support the IJvErrorProvider interface');
  FErrorProviderComponent := Value;
  FErrorProviderComponent.FreeNotification(self);
  setErrorProvider(obj);
end;
{$ENDIF}

procedure TJvCustomValidateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
{$IFDEF COMPILER6_UP}
    if (Assigned(ErrorProvider)) and (AComponent.IsImplementorOf(ErrorProvider)) then
      ErrorProvider := nil;
{$ELSE}
    if AComponent = FErrorProviderComponent then
      setErrorProviderComponent(nil);
{$ENDIF}
  end;
end;

procedure TJvCustomValidateEdit.setErrorMessage(const Value: WideString);
begin
  FErrorMessage := Value;
  UpdateProvider;
end;

procedure TJvCustomValidateEdit.setErrorProvider(const Value: IJvErrorProvider);
begin
  ClearProvider;
{$IFDEF COMPILER6_UP}
  ReferenceInterface(FErrorProvider, opRemove);
  FErrorProvider := Value;
  ReferenceInterface(FErrorProvider, opInsert);
{$ELSE}
  FErrorProvider := Value;
{$ENDIF}
  UpdateProvider;
end;


procedure TJvCustomValidateEdit.UpdateProvider;
begin
  if (FErrorProvider <> nil) then
    FErrorProvider.SetClientError(self);
end;

procedure TJvCustomValidateEdit.ClearProvider;
var tmp:string;
begin
  if (FErrorProvider <> nil) and not (csFreeNotification in ComponentState) then
  begin
    tmp := FErrorMessage;
    try
      FErrorMessage := '';
      FErrorProvider.SetClientError(self);
    finally
      FErrorMessage := tmp;
    end;
  end;
end;

procedure TJvCustomValidateEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  ClearProvider;
  UpdateProvider;
end;

procedure TJvCustomValidateEdit.DoValidateError(Sender: TObject);
begin
  UpdateProvider;
end;

// IJvValidationProperty
function TJvCustomValidateEdit.GetValidationPropertyName: WideString;
begin
  Result := 'Text';
end;

function TJvCustomValidateEdit.GetValidationPropertyValue: Variant;
begin
  Result := Text;
end;


end.

