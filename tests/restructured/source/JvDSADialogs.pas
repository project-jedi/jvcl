{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDSADialogs.PAS, released on 2002-08-23.

The Initial Developer of the Original Code is Marcel Bestebroer [marcelb@zeelandnet.nl]
Portions created by Marcel Bestebroer are Copyright (C) 2002 Marcel Bestebroer.
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-08-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvDSADialogs;

interface

{$I JVCL.INC}

uses
  Classes, Contnrs, Controls, Dialogs, Forms, Graphics, SysUtils, Windows,
  JclBase, JvComponent, JvTypes;

type
  TDlgCenterKind = (dckScreen, dckMainForm, dckActiveForm);

//--------------------------------------------------------------------------------------------------
// DSA storage and registration classes, types, constants and exceptions
//--------------------------------------------------------------------------------------------------

type
  TDSACheckTextKind = type Integer;

const
  ctkShow = 0;
  ctkAsk = 1;
  ctkWarn = 2;

type
  TDSAStorage = class;

  TDSARegItem = record
    ID: Integer;
    Name: string;
    Description: string;
    Storage: TDSAStorage;
    ChkTextKind: TDSACheckTextKind;
  end;

  TDSACustomData = procedure(const Storage: TDSAStorage; const DSAInfo: TDSARegItem) of object;

  TDSAStorage = class
  private
    FStates: TStack;
  protected
    procedure BeginCustomRead(const DSAInfo: TDSARegItem); virtual;
    procedure BeginCustomWrite(const DSAInfo: TDSARegItem); virtual;
    procedure BeginRead(const DSAInfo: TDSARegItem); virtual;
    procedure BeginWrite(const DSAInfo: TDSARegItem); virtual;
    procedure EndCustomRead(const DSAInfo: TDSARegItem); virtual;
    procedure EndCustomWrite(const DSAInfo: TDSARegItem); virtual;
    procedure EndRead(const DSAInfo: TDSARegItem); virtual;
    procedure EndWrite(const DSAInfo: TDSARegItem); virtual;
    function IsKeyNameAllowed(const Key: string): Boolean;

    function GetCheckMarkTextSuffix: string; virtual; abstract;
    procedure SetCheckMarkTextSuffix(Value: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function GetState(const DSAInfo: TDSARegItem; out LastResult: Integer;
      const OnCustomData: TDSACustomData = nil): Boolean; virtual;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; virtual; abstract;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Boolean): Boolean; virtual; abstract;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; virtual; abstract;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Extended): Extended; virtual; abstract;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; virtual; abstract;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Int64): Int64; virtual; abstract;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; virtual; abstract;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Integer): Integer; virtual; abstract;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; virtual; abstract;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: string): string; virtual; abstract;
    procedure SetState(const DSAInfo: TDSARegItem; const DontShowAgain: Boolean;
      const LastResult: Integer; const OnCustomData: TDSACustomData = nil); virtual;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Boolean); virtual; abstract;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Extended); virtual; abstract;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Int64); virtual; abstract;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Integer); virtual; abstract;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string;
      const Value: string); virtual; abstract;

    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix;
  end;

  TDSARegStorage = class(TDSAStorage)
  private
    FRootKey: HKEY;
    FKey: string;
  protected
    procedure CreateKey(const DSAInfo: TDSARegItem); virtual;
    function GetCheckMarkTextSuffix: string; override;
    procedure SetCheckMarkTextSuffix(Value: string); override;
  public
    constructor Create(const ARootKey: HKEY; const AKey: string);

    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string;
      const Value: string); override;

    property RootKey: HKEY read FRootKey write FRootKey;
    property Key: string read FKey write FKey;
  end;

  TDSAQueueStorage = class(TDSAStorage)
  private
    FList: TStrings;
    FCheckMarkSuffix: string;
  protected
    procedure AddDSA(const DSAInfo: TDSARegItem);
    procedure DeleteDSA(const Index: Integer);
    function FindDSA(const DSAInfo: TDSARegItem): Integer;
    function GetCheckMarkTextSuffix: string; override;
    function GetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
      const Kind: Integer): string;
    function HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): Boolean;
    procedure SetCheckMarkTextSuffix(Value: string); override;
    procedure SetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
      const Kind: Integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
      const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
      const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string;
      const Value: string); override;

    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix write SetCheckMarkTextSuffix;
  end;

const
  ssCustomRead: Pointer = @TDSAStorage.BeginCustomRead;
  ssCustomWrite: Pointer = @TDSAStorage.BeginCustomWrite;
  ssRead: Pointer = @TDSAStorage.BeginRead;
  ssWrite: Pointer = @TDSAStorage.BeginWrite;

//--------------------------------------------------------------------------------------------------
// MessageDlg replacements and extensions
//--------------------------------------------------------------------------------------------------

// Additional values for DefaultButton, CancelButton and HelpButton parameters
const
  mbNone = TMsgDlgBtn(-1);
  mbDefault = TMsgDlgBtn(-2);

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind = dckScreen);
procedure ShowMessageFmt(const Msg: string; const Params: array of const;
  const Center: TDlgCenterKind = dckScreen);

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault;
  const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult; overload;

//--------------------------------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//--------------------------------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: Integer; const Msg: string;
  const Center: TDlgCenterKind = dckScreen);
procedure DSAShowMessageFmt(const DlgID: Integer; const Msg: string; const Params: array of const;
  const Center: TDlgCenterKind = dckScreen);

function DSAMessageDlg(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;
function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string;
  const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;
function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult; overload;

function DSAMessageDlgEx(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1): Integer; overload;
function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string;
  const DlgType: TMsgDlgType; const Buttons: array of string;
  const Results: array of Integer; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1): TModalResult; overload;
function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1): Integer; overload;

//--------------------------------------------------------------------------------------------------
// DSA registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
procedure UnregisterDSA(const DlgID: Integer);
function LocateDSAReg(const DlgID: Integer): TDSARegItem;

//--------------------------------------------------------------------------------------------------
// DSA state setting/retrieving
//--------------------------------------------------------------------------------------------------

function GetDSAState(const DlgID: Integer): Boolean; overload;
function GetDSAState(const DlgID: Integer; out ResCode: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean; overload;
procedure SetDSAState(const DlgID: Integer; const DontShowAgain: Boolean;
  const LastResult: Integer = mrNone; const OnCustomData: TDSACustomData = nil);

//--------------------------------------------------------------------------------------------------
// Iterating the DSA registration
//--------------------------------------------------------------------------------------------------

function DSACount: Integer;
function DSAItem(const Index: Integer): TDSARegItem;

//--------------------------------------------------------------------------------------------------
// DSA check box text registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSACheckMarkText(const ID: TDSACheckTextKind; const Text: string);
procedure UnregisterDSACheckMarkText(const ID: TDSACheckTextKind);
function GetDSACheckMarkText(const ID: TDSACheckTextKind): string;

//--------------------------------------------------------------------------------------------------
// Standard DSA storage devices
//--------------------------------------------------------------------------------------------------

function DSARegStore: TDSARegStorage;
function DSAQueueStore: TDSAQueueStorage;

//--------------------------------------------------------------------------------------------------
// VCL component
//--------------------------------------------------------------------------------------------------

type
  EJvDSADialog = class(EJVCLException);

  TJvDSADataEvent = procedure(Sender: TObject; const DSAInfo: TDSARegItem; const Storage: TDSAStorage) of object;

  TJvDSADialog = class(TJvComponent)
  private
    FCheckControl: TWinControl;
    FDialogID: Integer;
    FIgnoreDSAChkMrkTxt: Boolean;
    FOnUpdateKeys: TJvDSADataEvent;
    FOnApplyKeys: TJvDSADataEvent;
    FOrgOwner: TComponent;
    FOrgShowModalPtr: Pointer;
  protected
    procedure ApplySavedState; virtual;
    procedure DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    procedure DoUpdateKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    function GetDSAStateInternal(out ModalResult: Integer): Boolean;
    function GetOrgOwner: TComponent;
    function GetOrgShowModalPtr: Pointer;
    function GetStorage: TDSAStorage;
    procedure FormPatch;
    procedure FormUnPatch;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCheckControl(Value: TWinControl); virtual;
    procedure SetDialogID(Value: Integer); virtual;
    procedure SetOrgOwner(Value: TComponent);
    procedure SetOrgShowModalPtr(Value: Pointer);
    procedure UpdateDSAState; virtual;

    property OrgOwner: TComponent read GetOrgOwner write SetOrgOwner;
    property OrgShowModalPtr: Pointer read GetOrgShowModalPtr write SetOrgShowModalPtr;
    property Storage: TDSAStorage read GetStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetModalResult: Integer; virtual;
    function IsDSAChecked: Boolean; virtual;
    procedure Loaded; override;
  published
    property CheckControl: TWinControl read FCheckControl write SetCheckControl;
    property DialogID: Integer read FDialogID write SetDialogID;
    property IgnoreDSAChkMrkTxt: Boolean read FIgnoreDSAChkMrkTxt write FIgnoreDSAChkMrkTxt;
    property OnApplyKeys: TJvDSADataEvent read FOnApplyKeys write FOnApplyKeys;
    property OnUpdateKeys: TJvDSADataEvent read FOnUpdateKeys write FOnUpdateKeys;
  end;

implementation

uses
  Consts, ExtCtrls, StdCtrls, Math, TypInfo,
  JclRegistry, JclSysUtils;

{$IFNDEF DELPHI6_UP}
type
  PBoolean = ^Boolean;

const
  sLineBreak = #13#10;
{$ENDIF DELPHI6_UP}

resourcestring
  sDSARegKeyCreateError = 'Unable to create key %s';
  sDSADuplicateID = 'DSA dialog with ID ''%d'' is already assigned to another dialog name.';
  sDSADuplicateName = 'DSA dialog named ''%s'' is already assigned to another dialog ID.';
  sDSADialogIDNotFound = 'DSA dialog %d does not exist.';
  sDSADuplicateCTK_ID = 'CheckMarkText ID %d already registered.';
  sDSADialogIDNotStored = 'DSA dialog %d has not been stored.';
  sDSAKeyNotFound = 'Key %s does not exist.';
  sDSAKeyNoAccessAs = 'Key %s cannot be accessed as %s.';

  sDSAAccessBool = 'Boolean';
  sDSAAccessFloat = 'Float';
  sDSAAccessInt64 = 'Int64';
  sDSAAccessInt = 'Integer';
  sDSAAccessString = 'string';

  sDSActkShowText = 'Don''t show this dialog again';
  sDSActkAskText = 'Don''t ask me again';
  sDSActkWarnText = 'Don''t warn me again';

  sDSAStateValueName = 'DSA_State'; // do not localize
  sDSAStateLastResultName = 'LastResult'; // do not localize

  sCtrlHasNoCheckedProp = 'The specified control has no "Checked" property.';
  sCtrlHasNoCaptionProp = 'The specified control has no "Caption" property.';
  sDialogIDChangeOnlyInDesign = 'The dialog ID can only be changed at design time.';
  sOnlyAllowedOnForms = 'TJvDSADialog is only allowed on forms.';
  sAlreadyDSADialog = 'The form already has a TJvDSADialog component.';

//--------------------------------------------------------------------------------------------------
//  CheckMarkTexts
//--------------------------------------------------------------------------------------------------

var
  GCheckMarkTexts: TStrings;

function CheckMarkTexts: TStrings;
begin
  if GCheckMarkTexts = nil then
    GCheckMarkTexts := TStringList.Create;
  Result := GCheckMarkTexts;
end;

//--------------------------------------------------------------------------------------------------

function GetCheckMarkText(const ID: TDSACheckTextKind): string;
var
  Idx: Integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    Result := CheckMarkTexts[Idx]
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------
//  TDSAMessageForm
//--------------------------------------------------------------------------------------------------

type
  TDSAMessageForm = class(TForm)
  private
  protected
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure WriteToClipBoard(Text: String);
    function GetFormText: String;
  public
    constructor CreateNew(AOwner: TComponent); reintroduce;
    function IsDSAChecked: Boolean;
  end;

constructor TDSAMessageForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

procedure TDSAMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Word('C')) then
  begin
    SysUtils.Beep;
    WriteToClipBoard(GetFormText);
  end;
end;

procedure TDSAMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDSAMessageForm.WriteToClipBoard(Text: String);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if OpenClipBoard(0) then
  begin
    try
      Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Length(Text) + 1);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(PChar(Text)^, DataPtr^, Length(Text) + 1);
          EmptyClipBoard;
          SetClipboardData(CF_TEXT, Data);
        finally
          GlobalUnlock(Data);
        end;
      except
        GlobalFree(Data);
        raise;
      end;
    finally
      CloseClipBoard;
    end;
  end
  else
    raise Exception.Create(SCannotOpenClipboard);
end;

function TDSAMessageForm.GetFormText: String;
var
  DividerLine, ButtonCaptions: string;
  I: integer;
begin
  DividerLine := StringOfChar('-', 27) + sLineBreak;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TButton then
      ButtonCaptions := ButtonCaptions + TButton(Components[I]).Caption +
        StringOfChar(' ', 3);
  ButtonCaptions := StringReplace(ButtonCaptions,'&','', [rfReplaceAll]);
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TLabel) do
    Dec(I);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, sLineBreak,
    DividerLine, TLabel(Components[I]).Caption, sLineBreak, DividerLine, ButtonCaptions,
    sLineBreak, DividerLine]);
end;

function TDSAMessageForm.IsDSAChecked: Boolean;
var
  I: Integer;
begin
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TCustomCheckBox) do
    Dec(I);
  if (I > -1) then
    Result := TCheckBox(Components[I]).Checked
  else
    Result := False;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint32(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function CreateDSAMessageForm(const ACaption, Msg: string; const APicture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Integer;
  const CheckCaption: string; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  HelpButton: Integer = -1): TDSAMessageForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  ChkTextWidth: Integer;
  IconID: PChar;
  TempRect, TextRect: TRect;
  I: Integer;
  CenterParent: TComponent;
  CenterParLeft, CenterParTop, CenterParWidth, CenterParHeight: Integer;
begin
  case Center of
    dckScreen:
      CenterParent := Screen;
    dckMainForm:
      CenterParent := Application.MainForm;
    dckActiveForm:
      CenterParent := Screen.ActiveCustomForm;
    else
      CenterParent := nil;
  end;
  if CenterParent = nil then
    CenterParent := Screen;
  if CenterParent is TScreen then
  begin
    CenterParLeft := 0;
    CenterParTop := 0;
    CenterParWidth := TScreen(CenterParent).Width;
    CenterParHeight := TScreen(CenterParent).Height;
  end
  else
  begin
    with TWinControl(CenterParent) do
    begin
      CenterParLeft := Left;
      CenterParTop := Top;
      CenterParWidth := Width;
      CenterParHeight := Height;
    end;
  end;
  if HelpButton = High(Integer) then
    HelpButton := High(Buttons);
  Result := TDSAMessageForm.CreateNew(Screen.ActiveCustomForm);
  try
    with Result do
    begin
      BiDiMode := Application.BiDiMode;
      BorderStyle := bsDialog;
      Canvas.Font := Font;
      KeyPreview := True;
      OnKeyDown := CustomKeyDown;
      DialogUnits := GetAveCharSize(Canvas);
      HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
      VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
      HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
      VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
      for I := Low(Buttons) to High(Buttons) do
      begin
        TextRect := Rect(0,0,0,0);
        Windows.DrawText(Canvas.Handle, PChar(Buttons[I]), -1, TextRect, DT_CALCRECT or DT_LEFT or
          DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
        with TextRect do
          if (Right - Left + 8) > ButtonWidth then
            ButtonWidth := (Right - Left + 8);
      end;
      ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
      ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
      if (Screen.Width div 2) > (CenterParWidth + (2 * CenterParLeft)) then
        SetRect(TextRect, 0, 0, CenterParWidth + (2 * CenterParLeft), 0)
      else
        SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
      DrawText(Canvas.Handle, PChar(Msg), Length(Msg)+1, TextRect,
        DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
        DrawTextBiDiModeFlagsReadingOnly);
      IconTextWidth := TextRect.Right;
      IconTextHeight := TextRect.Bottom;
      if CheckCaption <> '' then
      begin
        SetRect(TempRect, 0, 0, Screen.Width div 2, 0);
        DrawText(Canvas.Handle, PChar(CheckCaption), Length(CheckCaption)+1, TempRect,
          DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
          DrawTextBiDiModeFlagsReadingOnly);
        ChkTextWidth := TempRect.Right;
      end
      else
        ChkTextWidth := 0;
      if APicture <> nil then
      begin
        Inc(IconTextWidth, APicture.Width + HorzSpacing);
        if IconTextHeight < APicture.Height then IconTextHeight := APicture.Height;
      end;
      ButtonCount := Length(Buttons);
      ButtonGroupWidth := 0;
      if ButtonCount <> 0 then
        ButtonGroupWidth := ButtonWidth * ButtonCount +
          ButtonSpacing * (ButtonCount - 1);
      ClientWidth := Max(17 + ChkTextWidth, Max(IconTextWidth, ButtonGroupWidth)) + HorzMargin * 2;
      ClientHeight := IconTextHeight + ButtonHeight + VertSpacing * 2 + VertMargin;
      if CheckCaption <> '' then
        Result.ClientHeight := Result.ClientHeight + VertMargin + 17 + 17;
      Left := (CenterParWidth div 2) - (Width div 2) + CenterParLeft;
      Top := (CenterParHeight div 2) - (Height div 2) + CenterParTop;
      if ACaption <> '' then
        Caption := ACaption
      else
        Caption := Application.Title;
      if APicture <> nil then
        with TImage.Create(Result) do
        begin
          Name := 'Image';
          Parent := Result;
          Picture.Assign(APicture);
          SetBounds(HorzMargin, VertMargin, APicture.Width, APicture.Height);
        end;
      with TLabel.Create(Result) do
      begin
        Name := 'Message';
        Parent := Result;
        WordWrap := True;
        Caption := Msg;
        BoundsRect := TextRect;
        BiDiMode := Result.BiDiMode;
        ALeft := IconTextWidth - TextRect.Right + HorzMargin;
        if UseRightToLeftAlignment then
          ALeft := Result.ClientWidth - ALeft - Width;
        SetBounds(ALeft, VertMargin,
          TextRect.Right, TextRect.Bottom);
      end;
      X := (ClientWidth - ButtonGroupWidth) div 2;
      for I := Low(Buttons) to High(Buttons) do
        with TButton.Create(Result) do
        begin
          Name := 'Button' + IntToStr(I);
          Parent := Result;
          Caption := Buttons[I];
          ModalResult := Results[I];
          if I = DefaultButton then
            Default := True;
          if I = CancelButton then
            Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing, ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if I = HelpButton then
            OnClick := HelpButtonClick;
        end;
      if CheckCaption <> '' then
        with TCheckBox.Create(Result) do
        begin
          Name := 'DontShowAgain';
          Parent := Result;
          BiDiMode := Result.BiDiMode;
          Caption := CheckCaption;
          SetBounds(HorzMargin, IconTextHeight + VertMargin + VertSpacing * 2 + ButtonHeight,
            Result.ClientWidth - 2 * HorzMargin, Height);
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------
//  TDSARegister
//--------------------------------------------------------------------------------------------------

type
  TAddResult = (arAdded, arExists, arDuplicateID, arDuplicateName);

  TDSARegister = class
  private
    FList: array of TDSARegItem;
  protected
    function AddNew: Integer;
    procedure Remove(const Index: Integer);
    function IndexOf(const ID: Integer): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function IndexOf(const Item: TDSARegItem): Integer; overload;
  public
    destructor Destroy; override;
    function Add(const Item: TDSARegItem): TAddResult; overload;
    function Add(const ID: Integer; const Name, Description: string; const Storage: TDSAStorage;
      const CheckTextKind: TDSACheckTextKind = ctkShow): TAddResult; overload;
    procedure Clear;
    procedure Delete(const Item: TDSARegItem); overload;
    procedure Delete(const ID: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Locate(const ID: Integer): TDSARegItem; overload;
    function Locate(const Name: string): TDSARegItem; overload;
  end;

const
  EmptyItem: TDSARegItem = (ID: High(Integer); Name: ''; Storage: nil);

var
  DSARegister: TDSARegister;

function TDSARegister.AddNew: Integer;
begin
  Result := Length(FList);
  SetLength(FList, Result + 1);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegister.Remove(const Index: Integer);
var
  I: Integer;
begin
  for I := Index + 1 to High(FList) do
  begin
    FList[I - 1].ID := FList[I].ID;
    FList[I - 1].Name := FList[I].Name;
    FList[I - 1].Description := FList[I].Description;
    FList[I - 1].ChkTextKind := FList[I].ChkTextKind;
    FList[I - 1].Storage := FList[I].Storage;
  end;
  SetLength(FList, High(FList));
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.IndexOf(const ID: Integer): Integer;
begin
  Result := High(FList);
  while (Result > -1) and (FList[Result].ID <> ID) do
    Dec(Result);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.IndexOf(const Name: string): Integer;
begin
  Result := High(FList);
  while (Result > -1) and not AnsiSameText(FList[Result].Name, Name) do
    Dec(Result);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.IndexOf(const Item: TDSARegItem): Integer;
begin
  Result := Indexof(Item.ID);
  if (Result > -1) and not AnsiSameText(FList[Result].Name, Item.Name) then
    Result := -1;
end;

//--------------------------------------------------------------------------------------------------

destructor TDSARegister.Destroy;
begin
  inherited Destroy;
  Clear;
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.Add(const Item: TDSARegItem): TAddResult;
var
  Idx: Integer;
begin
  if IndexOf(Item) > -1 then
    Result := arExists
  else if IndexOf(Item.ID) > -1 then
  begin
    Idx := IndexOf(Item.ID);
    if AnsiSameText(FList[Idx].Name, Item.Name) then
      Result := arExists
    else
      Result := arDuplicateID;
  end
  else if IndexOf(Item.Name) > -1 then
    Result := arDuplicateName
  else
  begin
    Idx := AddNew;
    FList[Idx].ID := Item.ID;
    FList[Idx].Name := Item.Name;
    FList[Idx].Description := Item.Description;
    FList[Idx].Storage := Item.Storage;
    FList[Idx].ChkTextKind := Item.ChkTextKind;
    Result := arAdded;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.Add(const ID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow): TAddResult;
var
  TmpItem: TDSARegItem;
begin
  TmpItem.ID := ID;
  TmpItem.Name := Name;
  TmpItem.Description := Description;
  TmpItem.Storage := Storage;
  TmpItem.ChkTextKind := CheckTextKind;
  Result := Add(TmpItem);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegister.Clear;
begin
  SetLength(FList, 0);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegister.Delete(const Item: TDSARegItem);
var
  Idx: Integer;
begin
  Idx := IndexOf(Item.ID);
  if (Idx > -1) and AnsiSameText(FList[Idx].Name, Item.Name) then
    Remove(Idx);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegister.Delete(const ID: Integer);
var
  Idx: Integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Remove(Idx);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegister.Delete(const Name: string);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Remove(Idx);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.Locate(const ID: Integer): TDSARegItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Result := FList[Idx]
  else
    Result := EmptyItem;
end;

//--------------------------------------------------------------------------------------------------

function TDSARegister.Locate(const Name: string): TDSARegItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Result := FList[Idx]
  else
    Result := EmptyItem;
end;

//--------------------------------------------------------------------------------------------------
// TDSAStorage
//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.BeginCustomRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomRead);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.BeginCustomWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomWrite);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.BeginRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssRead);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.BeginWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssWrite);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.EndCustomRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomRead then
    raise EJvDSADialog.Create('Cannot end custom read if not in custom read mode.');
  FStates.Pop;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.EndCustomWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomWrite then
    raise EJvDSADialog.Create('Cannot end custom write if not in custom write mode.');
  FStates.Pop;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.EndRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssRead then
    raise EJvDSADialog.Create('Cannot end read if not in read mode.');
  FStates.Pop;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.EndWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssWrite then
    raise EJvDSADialog.Create('Cannot end write if not in write mode.');
  FStates.Pop;
end;

//--------------------------------------------------------------------------------------------------

function TDSAStorage.IsKeyNameAllowed(const Key: string): Boolean;
begin
  if AnsiSameText(Key, sDSAStateValueName) or AnsiSameText(Key, sDSAStateLastResultName) then
    Result := Integer(FStates.Peek) in [Integer(ssRead), Integer(ssWrite)]
  else
    Result := Integer(FStates.Peek) in [Integer(ssCustomRead), Integer(ssCustomWrite)];
end;

//--------------------------------------------------------------------------------------------------

constructor TDSAStorage.Create;
begin
  inherited Create;
  FStates := TStack.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TDSAStorage.Destroy;
begin
  FStates.Free;
end;

//--------------------------------------------------------------------------------------------------

function TDSAStorage.GetState(const DSAInfo: TDSARegItem; out LastResult: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean;
begin
  BeginRead(DSAInfo);
  try
    LastResult := 0;
    Result := ReadBoolDef(DSAInfo, sDSAStateValueName, False);
    if Result then
    begin
      LastResult := ReadIntegerDef(DSAInfo, sDSAStateLastResultName, 0);
      if @OnCustomData <> nil then
      begin
        BeginCustomRead(DSAInfo);
        try
          OnCustomData(Self, DSAInfo);
        finally
          EndCustomRead(DSAInfo);
        end;
      end;
    end;
  finally
    EndRead(DSAInfo);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAStorage.SetState(const DSAInfo: TDSARegItem; const DontShowAgain: Boolean;
  const LastResult: Integer; const OnCustomData: TDSACustomData = nil);
begin
  BeginWrite(DSAInfo);
  try
    WriteBool(DSAInfo, sDSAStateValueName, DontShowAgain);
    if DontShowAgain then
    begin
      WriteInteger(DSAInfo, sDSAStateLastResultName, LastResult);
      if @OnCustomData <> nil then
      begin
        BeginCustomWrite(DSAInfo);
        try
          OnCustomData(Self, DSAInfo);
        finally
          EndCustomWrite(DSAInfo);
        end;
      end;
    end;
  finally
    EndWrite(DSAInfo);
  end;
end;

//--------------------------------------------------------------------------------------------------
// TDSARegStorage
//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.CreateKey(const DSAInfo: TDSARegItem);
begin
  if not (RegKeyExists(RootKey, Key + '\' + DSAInfo.Name) or (RegCreateKey(RootKey, Key + '\' +
      DSAInfo.Name, '') = ERROR_SUCCESS)) then
    raise EJvDSADialog.CreateFmt(sDSARegKeyCreateError, [Key + '\' + DSAInfo.Name]);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.GetCheckMarkTextSuffix: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.SetCheckMarkTextSuffix(Value: string);
begin
end;

//--------------------------------------------------------------------------------------------------

constructor TDSARegStorage.Create(const ARootKey: HKEY; const AKey: string);
begin
  inherited Create;
  FRootKey := ARootKey;
  FKey := AKey;
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
begin
  Result := RegReadBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  Result := RegReadBoolDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  RegReadBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(Extended));
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  if RegReadBinaryDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(Extended),
      0) = 0 then
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := RegReadDWORD(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Int64): Int64;
begin
  Result := RegReadDWORDDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := RegReadInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  Result := RegReadIntegerDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := RegReadString(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

//--------------------------------------------------------------------------------------------------

function TDSARegStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  Result := RegReadStringDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  CreateKey(DSAInfo);
  RegWriteBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
var
  Temp: Extended;
begin
  CreateKey(DSAInfo);
  Temp := Value;
  RegWriteBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Temp, SizeOf(Extended));
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  CreateKey(DSAInfo);
  RegWriteDWORD(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  CreateKey(DSAInfo);
  RegWriteInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSARegStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  CreateKey(DSAInfo);
  RegWriteString(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

//--------------------------------------------------------------------------------------------------
// TDSAQueueStorage
//--------------------------------------------------------------------------------------------------

{ constants}

const
  DSABool = 1;
  DSAFloat = 2;
  DSAInt64 = 3;
  DSAInt = 4;
  DSAString = 5;

  DSAKindTexts: array[DSABool .. DSAString] of string = (
    sDSAAccessBool, sDSAAccessFloat, sDSAAccessInt64, sDSAAccessInt, sDSAAccessString);

{ TDSAValues object }

type
  TDSAValues = class(TStringList)
    constructor Create;
  end;

constructor TDSAValues.Create;
begin
  inherited Create;
  Sorted := True;
end;

{ TDSAQueueStorage }

procedure TDSAQueueStorage.AddDSA(const DSAInfo: TDSARegItem);
begin
  if FindDSA(DSAInfo) < 0 then
    FList.AddObject(DSAInfo.Name, TDSAValues.Create);
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.DeleteDSA(const Index: Integer);
begin
  FList.Objects[Index].Free;
  FList.Delete(Index);
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.FindDSA(const DSAInfo: TDSARegItem): Integer;
begin
  Result := FList.IndexOf(DSAInfo.Name);
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.GetCheckMarkTextSuffix: string;
begin
  Result := FCheckMarkSuffix;
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.GetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
  const Kind: Integer): string;
var
  I: Integer;
  DSAKeys: TStrings;
begin
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(sDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(sDSAKeyNotFound, [Key]);
  if Integer(DSAKeys.Objects[I]) <> Kind then
    raise EJvDSADialog.CreateFmt(sDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
  Result := DSAKeys.Values[Key];
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): Boolean;
var
  I: Integer;
  DSAKeys: TStrings;
begin
  I := FindDSA(DSAInfo);
  Result := I > -1;
  if Result then
  begin
    DSAKeys := TStrings(FList.Objects[I]);
    Result := DSAKeys.IndexOfName(Key) > -1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.SetCheckMarkTextSuffix(Value: string);
begin
  if Value <> CheckMarkTextSuffix then
    FCheckMarkSuffix := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.SetDSAValue(const DSAInfo: TDSARegItem; const Key: string;
  const Kind: Integer; const Value: string);
var
  I: Integer;
  DSAKeys: TStrings;
begin
  AddDSA(DSAInfo);
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(sDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    DSAKeys.AddObject(Key + '=' + Value, TObject(Kind))
  else
  begin
    if Integer(DSAKeys.Objects[I]) <> Kind then
      raise EJvDSADialog.CreateFmt(sDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
    DSAKeys.Values[Key] := Value;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TDSAQueueStorage.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  TStringList(FList).Sorted := True;
  FCheckMarkSuffix := 'in the current queue';
end;

//--------------------------------------------------------------------------------------------------

destructor TDSAQueueStorage.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.Clear;
begin
  while FList.Count > 0 do
    DeleteDSA(FList.Count - 1);
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
var
  S: string;
begin
  S := GetDSAValue(DSAInfo, Key, DSABool);
  Result := AnsiSameText(S, 'True') or AnsiSameText(S, '1');
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadBool(DSAInfo, Key)
  else
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  Result := StrToFloat(StringReplace(GetDSAValue(DSAInfo, Key, DSAFloat), ThousandSeparator,
    DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadFloat(DSAInfo, Key)
  else
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := StrToInt64(GetDSAValue(DSAInfo, Key, DSAInt64));
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Int64): Int64;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInt64(DSAInfo, Key)
  else
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := StrToInt(GetDSAValue(DSAInfo, Key, DSAInt));
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInteger(DSAInfo, Key)
  else
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := GetDSAValue(DSAInfo, Key, DSAString);
end;

//--------------------------------------------------------------------------------------------------

function TDSAQueueStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadString(DSAInfo, Key)
  else
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  if Value then
    SetDSAValue(DSAInfo, Key, DSABool, '1')
  else
    SetDSAValue(DSAInfo, Key, DSABool, '0');
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
begin
  SetDSAValue(DSAInfo, Key, DSAFloat, FloatToStr(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  SetDSAValue(DSAInfo, Key, DSAInt64, IntToStr(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  SetDSAValue(DSAInfo, Key, DSAInt, IntToStr(Value));
end;

//--------------------------------------------------------------------------------------------------

procedure TDSAQueueStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  SetDSAValue(DSAInfo, Key, DSAString, Value);
end;

//--------------------------------------------------------------------------------------------------
// Helpers
//--------------------------------------------------------------------------------------------------

const
  Captions: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError,
    SMsgDlgInformation, SMsgDlgConfirm, '');
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp);
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);

//--------------------------------------------------------------------------------------------------

function DlgCaption(const DlgType: TMsgDlgType): string;
begin
  Result := Captions[DlgType];
end;

//--------------------------------------------------------------------------------------------------

function DlgPic(const DlgType: TMsgDlgType): TGraphic;
begin
  if IconIDs[DlgType] <> nil then
  begin
    Result := TIcon.Create;
    try
      TIcon(Result).Handle := LoadIcon(0, IconIDs[DlgType]);
    except
      Result.Free;
      raise;
    end;
  end
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function DlgButtonCaptions(const Buttons: TMsgDlgButtons): TDynStringArray;
var
  I: Integer;
  B: TMsgDlgBtn;
begin
  SetLength(Result, Ord(High(TMsgDlgBtn)) + 1);
  I := 0;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  if B in Buttons then
  begin
    Result[I] := ButtonCaptions[B];
    Inc(I);
  end;
  SetLength(Result, I);
end;

//--------------------------------------------------------------------------------------------------

function DlgButtonResults(const Buttons: TMsgDlgButtons): TDynIntegerArray;
var
  I: Integer;
  B: TMsgDlgBtn;
begin
  SetLength(Result, Ord(High(TMsgDlgBtn)) + 1);
  I := 0;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  if B in Buttons then
  begin
    Result[I] := ModalResults[B];
    Inc(I);
  end;
  SetLength(Result, I);
end;

//--------------------------------------------------------------------------------------------------

function ButtonIndex(const Results: array of Integer; const ResCode: Integer): Integer; overload;
begin
  Result := High(Results);
  while (Result > -1) and (Results[Result] <> ResCode) do
    Dec(Result);
end;
//--------------------------------------------------------------------------------------------------

function ButtonIndex(const Results: array of Integer; const Button: TMsgDlgBtn): Integer; overload;
begin
  Result := ButtonIndex(Results, Modalresults[Button]);
end;

//--------------------------------------------------------------------------------------------------
// MessageDlg replacements and extensions
//--------------------------------------------------------------------------------------------------

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind = dckScreen);
begin
  MessageDlg(Msg, mtCustom, [mbOk], 0, Center);
end;

//--------------------------------------------------------------------------------------------------

procedure ShowMessageFmt(const Msg: string; const Params: array of const;
  const Center: TDlgCenterKind = dckScreen);
begin
  MessageDlg(Format(Msg, Params), mtCustom, [mbOk], 0, Center);
end;

//--------------------------------------------------------------------------------------------------

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx, Center, DefaultButton,
      CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault;
  const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(Caption, Msg, TmpPic, Buttons, HelpCtx, Center, DefaultButton,
      CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function MessageDlg(const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  DefBtn: TMsgDlgBtn;
  CanBtn: TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOk in Buttons then
      DefBtn := mbOk
    else if mbYes in Buttons then
      DefBtn := mbYes
    else
      DefBtn := mbRetry;
  end
  else
    DefBtn := DefaultButton;
  if CancelButton = mbDefault then
  begin
    if mbCancel in Buttons then
      CanBtn := mbCancel
    else if mbNo in Buttons then
      CanBtn := mbNo
    else
      CanBtn := mbOk;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := MessageDlgEx(Caption, Msg, Picture, DlgButtonCaptions(Buttons),
    BtnResults, HelpCtx, Center, ButtonIndex(BtnResults, DefBtn), ButtonIndex(BtnResults, CanBtn),
    ButtonIndex(BtnResults, HelpButton));
end;

//--------------------------------------------------------------------------------------------------

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(DlgCaption(DlgType), Msg, TmpPic, Buttons, Results, HelpCtx, Center,
      DefaultButton, CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(Caption, Msg, TmpPic, Buttons, Results, HelpCtx, Center, DefaultButton,
      CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): TModalResult;
begin
  with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx, '', Center,
    DefaultButton, CancelButton, HelpButton) do
  try
    Result := ShowModal;
  finally
    Free;
  end;
end;

//--------------------------------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//--------------------------------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: Integer; const Msg: string;
  const Center: TDlgCenterKind = dckScreen);
begin
  DSAMessageDlg(DlgID, Msg, mtCustom, [mbOk], 0, Center);
end;

//--------------------------------------------------------------------------------------------------

procedure DSAShowMessageFmt(const DlgID: Integer; const Msg: string; const Params: array of const;
  const Center: TDlgCenterKind = dckScreen);
begin
  DSAMessageDlg(DlgID, Format(Msg, Params), mtCustom, [mbOk], 0, Center);
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlg(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx, Center,
      DefaultButton, CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string; const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, Caption, Msg, TmpPic, Buttons, HelpCtx, Center, DefaultButton,
      CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlg(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: TMsgDlgButtons; const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault;
  const HelpButton: TMsgDlgBtn = mbHelp): TModalResult;
var
  DefBtn: TMsgDlgBtn;
  CanBtn: TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOk in Buttons then
      DefBtn := mbOk
    else if mbYes in Buttons then
      DefBtn := mbYes
    else
      DefBtn := mbRetry;
  end
  else
    DefBtn := DefaultButton;
  if CancelButton = mbDefault then
  begin
    if mbCancel in Buttons then
      CanBtn := mbCancel
    else if mbNo in Buttons then
      CanBtn := mbNo
    else
      CanBtn := mbOk;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := DSAMessageDlgEx(DlgID, Caption, Msg, Picture, DlgButtonCaptions(Buttons),
    BtnResults, HelpCtx, Center, ButtonIndex(BtnResults, DefBtn), ButtonIndex(BtnResults, CanBtn),
    ButtonIndex(BtnResults, HelpButton));
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlgEx(const DlgID: Integer; const Msg: string; const DlgType: TMsgDlgType;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): Integer;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons, Results, HelpCtx,
      Center, DefaultButton, CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string;
  const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of Integer;
  const HelpCtx: Longint; const Center: TDlgCenterKind = dckScreen;
  const DefaultButton: Integer = 0; const CancelButton: Integer = 1;
  const HelpButton: Integer = -1): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, Caption, Msg, TmpPic, Buttons, Results, HelpCtx, Center,
      DefaultButton, CancelButton, HelpButton);
  finally
    TmpPic.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function DSAMessageDlgEx(const DlgID: Integer; const Caption, Msg: string; const Picture: TGraphic;
  const Buttons: array of string; const Results: array of Integer; const HelpCtx: Longint;
  const Center: TDlgCenterKind = dckScreen; const DefaultButton: Integer = 0;
  const CancelButton: Integer = 1; const HelpButton: Integer = -1): Integer;
var
  DSAItem: TDSARegItem;
  CheckCaption: string;
  Temp: string;
begin
  if not GetDSAState(DlgID, Result) then
  begin
    Result := High(Integer);
    DSAItem := LocateDSAReg(DlgID);
    CheckCaption := GetCheckMarkText(DSAItem.ChkTextKind);
    if CheckCaption = '' then
      CheckCaption := GetCheckMarkText(ctkShow);
    Temp := DSAItem.Storage.CheckMarkTextSuffix;
    if Temp <> '' then
      CheckCaption := CheckCaption + ' ' + Temp + '.'
    else
      CheckCaption := CheckCaption + '.';
    // Create and show dialog
    with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx,
      CheckCaption, Center, DefaultButton, CancelButton, HelpButton) do
    try
      Result := ShowModal;
      if IsDSAChecked then
        SetDSAState(DlgID, True, Result);
    finally
      Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// DSA registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: Integer; const Name, Description: string;
  const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
begin
  case DSARegister.Add(DlgID, Name, Description, Storage, CheckTextKind) of
    arDuplicateID:
      raise EJvDSADialog.CreateFmt(sDSADuplicateID, [DlgID]);
    arDuplicateName:
      raise EJvDSADialog.CreateFmt(sDSADuplicateName, [Name]);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure UnregisterDSA(const DlgID: Integer);
begin
  DSARegister.Delete(DlgID);
end;

//--------------------------------------------------------------------------------------------------

function LocateDSAReg(const DlgID: Integer): TDSARegItem;
begin
  Result := DSARegister.Locate(DlgID);
end;

//--------------------------------------------------------------------------------------------------
// DSA state setting/retrieving
//--------------------------------------------------------------------------------------------------

function GetDSAState(const DlgID: Integer): Boolean;
var
  Dummy: Integer;
begin
  Result := GetDSAState(DlgID, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function GetDSAState(const DlgID: Integer; out ResCode: Integer;
  const OnCustomData: TDSACustomData = nil): Boolean;
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    Result := Regitem.Storage.GetState(RegItem, ResCode, OnCustomData)
  else
    raise EJvDSADialog.CreateFmt(sDSADialogIDNotFound, [DlgID]);
end;

//--------------------------------------------------------------------------------------------------

procedure SetDSAState(const DlgID: Integer; const DontShowAgain: Boolean;
  const LastResult: Integer = mrNone; const OnCustomData: TDSACustomData = nil);
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    RegItem.Storage.SetState(RegItem, DontShowAgain, LastResult, OnCustomData)
  else
    raise EJvDSADialog.CreateFmt(sDSADialogIDNotFound, [DlgID]);
end;

//--------------------------------------------------------------------------------------------------
// Iterating the DSA registration
//--------------------------------------------------------------------------------------------------

function DSACount: Integer;
begin
  Result := Length(DSARegister.FList);
end;

//--------------------------------------------------------------------------------------------------

function DSAItem(const Index: Integer): TDSARegItem;
begin
  Result := DSARegister.FList[Index];
end;

//--------------------------------------------------------------------------------------------------
// DSA check box text registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSACheckMarkText(const ID: TDSACheckTextKind; const Text: string);
begin
  if CheckMarkTexts.IndexOfObject(TObject(ID)) < 0 then
    CheckMarkTexts.AddObject(Text, TObject(ID))
  else
    raise EJvDSADialog.CreateFmt(sDSADuplicateCTK_ID, [ID]);
end;

//--------------------------------------------------------------------------------------------------

procedure UnregisterDSACheckMarkText(const ID: TDSACheckTextKind);
var
  Idx: Integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    CheckMarkTexts.Delete(Idx);
end;

//--------------------------------------------------------------------------------------------------

function GetDSACheckMarkText(const ID: TDSACheckTextKind): string;
begin
  Result := GetCheckMarkText(ID);
end;

//--------------------------------------------------------------------------------------------------
// Standard DSA storage devices
//--------------------------------------------------------------------------------------------------

var
  GRegStore: TDSAStorage;

function DSARegStore: TDSARegStorage;
begin
  if GRegStore = nil then
    GRegStore := TDSARegStorage.Create(HKEY_CURRENT_USER, 'Software\' + Application.Title + '\DSA');
  Result := TDSARegStorage(GRegStore);
end;

//--------------------------------------------------------------------------------------------------

var
  GQueueStore: TDSAStorage;

function DSAQueueStore: TDSAQueueStorage;
begin
  if GQueueStore = nil then
    GQueueStore := TDSAQueueStorage.Create;
  Result := TDSAQueueStorage(GQueueStore);
end;

//--------------------------------------------------------------------------------------------------

{ ShowModal patch }

procedure CallShowModal(const Frm: TCustomForm); // Helper to get the VMT index of ShowModal
begin
  Frm.ShowModal;
end;

function FindShowModalVMT: Integer; //  Locate the VMT index of ShowModal
var
  Ptr: Pointer;
begin
  Ptr := @CallShowModal;
  if Ptr = nil then
  begin
    Result := -1;
    Exit;
  end
  else
  begin
    Inc(Integer(Ptr), 4);
    Result := PInteger(Ptr)^ div 4;
  end;
end;

procedure SetVirtualMethodInstance(Instance: TObject; const VMTIdx: Integer; const MethodPtr: Pointer);
// Set the virtual method pointer for an instance, nice addition for JCL?
var
  OldProt: Cardinal;
begin
  VirtualProtect(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer)), SizeOf(Pointer), PAGE_READWRITE, OldProt);
  try
    PInteger(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer))) ^ := Integer(MethodPtr);
  finally
    VirtualProtect(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer)), SizeOf(Pointer), OldProt, OldProt);
  end;
end;

type
  TShowModalMethod = function: Integer of object; // So we can call the original ShowModal method.

  TPatchedForm = class(TCustomForm) // To replace the original ShowModal method.
  public
    function ShowModal: Integer; override;
  end;

function TPatchedForm.ShowModal: Integer;
var
  I: Integer;
  JvDSADialog: TJvDSADialog;
  DSAItem: TDSARegItem;
  CheckCaption: string;
  Temp: string;
  ShowModalMethod: TShowModalMethod;
begin
  // retrieve the TJvDSADialog instance.
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TJvDSADialog) do
    Dec(I);
  if I = -1 then
    raise EJvDSADialog.Create('JvDSADialog patch error: JvDSADialog component not found.');
  JvDSADialog := Components[I] as TJvDSADialog;

  // Check the DSA state
  if not JvDSADialog.GetDSAStateInternal(Result) then
  begin
    if (JvDSADialog.CheckControl <> nil) and not JvDSADialog.IgnoreDSAChkMrkTxt then
    begin
      // Get DSA checkmark caption
      DSAItem := LocateDSAReg(JvDSADIalog.DialogID);
      CheckCaption := GetDSACheckMarkText(DSAItem.ChkTextKind);
      if CheckCaption = '' then
        CheckCaption := GetDSACheckMarkText(ctkShow);
      Temp := DSAItem.Storage.CheckMarkTextSuffix;
      if Temp <> '' then
        CheckCaption := CheckCaption + ' ' + Temp + '.'
      else
        CheckCaption := CheckCaption + '.';
      SetStrProp(JvDSADialog.CheckControl, 'Caption', CheckCaption);
    end;

    // Show the dialog by calling the original ShowModal method: setting up the method pointers.
    TMethod(ShowModalMethod).Data := Self;
    TMethod(ShowModalMethod).Code := JvDSADialog.GetOrgShowModalPtr;
    // Show the dialog by calling the original ShowModal method: make the actual call.
    Result := ShowModalMethod;
    // Update the DSA state in storage.
    JvDSADialog.UpdateDSAState;
  end
  else
    // The dialog is suppressed. Apply the saved state.
    JvDSADialog.ApplySavedState;
end;

{ TJvDSADialog }

procedure TJvDSADialog.ApplySavedState;
var
  ResCode: Integer;
begin
  GetDSAState(DialogID, ResCode, DoApplyKeys);
  TCustomForm(Owner).ModalResult := ResCode;
end;

procedure TJvDSADialog.DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem);
begin
  if @FOnApplyKeys <> nil then
    OnApplyKeys(Self, DSAInfo, Storage);
end;

procedure TJvDSADialog.DoUpdateKeys;
begin
  if @FOnUpdateKeys <> nil then
    OnUpdateKeys(Self, DSAInfo, Storage);
end;

function TJvDSADialog.GetDSAStateInternal(out ModalResult: Integer): Boolean;
begin
  Result := GetDSAState(DialogID, ModalResult);
end;

function TJvDSADialog.GetOrgOwner: TComponent;
begin
  Result := FOrgOwner;
end;

function TJvDSADialog.GetOrgShowModalPtr: Pointer;
begin
  Result := FOrgShowModalPtr;
end;

function TJvDSADialog.GetStorage: TDSAStorage;
begin
  Result := LocateDSAReg(DialogID).Storage;
end;

procedure TJvDSADialog.FormPatch;
var
  VMTIdx: Integer;
begin
  VMTIdx := FindShowModalVMT;
  SetOrgShowModalPtr(GetVirtualMethod(Owner.ClassType, VMTIdx));
  SetOrgOwner(Owner);
  SetVirtualMethodInstance(Owner, VMTIdx, @TPatchedForm.ShowModal);
end;

procedure TJvDSADialog.FormUnPatch;
var
  VMTIdx: Integer;
begin
  if GetOrgShowModalPtr <> nil then
  begin
    VMTIdx := FindShowModalVMT;
    SetVirtualMethodInstance(GetOrgOwner, VMTIdx, GetOrgShowModalPtr);
    SetOrgShowModalPtr(nil);
  end;
end;

procedure TJvDSADialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = CheckControl) then
    CheckControl := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvDSADialog.SetCheckControl(Value: TWinControl);
begin
  if Value <> CheckControl then
  begin
    if Value <> nil then
    begin
      if GetPropInfo(Value, 'Checked') = nil then
        raise EJvDSADialog.Create(sCtrlHasNoCheckedProp);
      if GetPropInfo(Value, 'Caption') = nil then
        raise EJvDSADialog.Create(sCtrlHasNoCaptionProp);
    end;
    FCheckControl := Value;
  end;
end;

procedure TJvDSADialog.SetDialogID(Value: Integer);
begin
  if Value <> DialogID then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in Owner.ComponentState) then
      raise EJvDSADialog.Create(sDialogIDChangeOnlyInDesign);
    FDialogID := Value;
  end;
end;

procedure TJvDSADialog.SetOrgOwner(Value: TComponent);
begin
  FOrgOwner := Value;
end;

procedure TJvDSADialog.SetOrgShowModalPtr(Value: Pointer);
begin
  FOrgShowModalPtr := Value;
end;

procedure TJvDSADialog.UpdateDSAState;
begin
  SetDSAState(DialogID, IsDSAChecked, TCustomForm(Owner).ModalResult, DoUpdateKeys);
end;

constructor TJvDSADialog.Create(AOwner: TComponent);
var
  I: Integer;
begin
  if AOwner is TCustomForm then
  begin
    I := AOwner.ComponentCount - 1;
    while (I > -1) and not (AOwner.Components[I] is TJvDSADialog) do
      Dec(I);
    if I > -1 then
      raise EJvDSADialog.Create(sAlreadyDSADialog);
    inherited Create(AOwner);
  end
  else
    raise EJvDSADialog.Create(sOnlyAllowedOnForms);
end;

destructor TJvDSADialog.Destroy;
begin
  FormUnPatch;
  inherited Destroy;
end;

function TJvDSADialog.GetModalResult: Integer;
begin
  Result := TCustomForm(Owner).ModalResult;
end;

function TJvDSADialog.IsDSAChecked: Boolean;
begin
  if CheckControl <> nil then
    Result := GetOrdProp(CheckControl, 'Checked') <> 0
  else
    Result := False;
end;

procedure TJvDSADialog.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    FormPatch;
end;

initialization
  DSARegister := TDSARegister.Create;
  RegisterDSACheckMarkText(ctkShow, sDSActkShowText);
  RegisterDSACheckMarkText(ctkAsk, sDSActkAskText);
  RegisterDSACheckMarkText(ctkWarn, sDSActkWarnText);

finalization
  DSARegister.Free;
  if GRegStore <> nil then
    FreeAndNil(GRegStore);
  if GQueueStore <> nil then
    FreeAndNil(GQueueStore);
  if GCheckMarkTexts <> nil then
    FreeAndNil(GCheckMarkTexts);
end.

