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
  Steve Magruder

Last Modified: 2003-01-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDSADialogs;

interface

uses
  Classes, Contnrs, Controls, StdCtrls, Dialogs, ExtCtrls, Forms, Graphics, SysUtils, Windows,
  JclBase, JvComponent, JvTypes, JvDynControlEngine;

type
  TDlgCenterKind = (dckScreen, dckMainForm, dckActiveForm);
//=== TDSAMessageForm ========================================================

  TDSAMessageForm = class (TForm)
  private
    FTimeout: integer;
    FTimer: TTimer;
    FCountdown: TLabel;
  protected
    procedure CustomKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CustomMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure CustomShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure WriteToClipBoard(Text: string);
    function GetFormText: string;
    function TimeoutUnit(Secs: integer): string;
    procedure CancelAutoClose;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    function IsDSAChecked: boolean;
    property Timeout: integer read FTimeout write FTimeout;
  end;

//--------------------------------------------------------------------------------------------------
// DSA storage and registration classes, types, constants and exceptions
//--------------------------------------------------------------------------------------------------

type
  TDSACheckTextKind = type integer;

const
  ctkShow = 0;
  ctkAsk  = 1;
  ctkWarn = 2;

type
  TDSAStorage = class
    ;

  TDSARegItem = record
    ID: integer;
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
    function IsKeyNameAllowed(const Key: string): boolean;

    function GetCheckMarkTextSuffix: string; virtual; abstract;
    procedure SetCheckMarkTextSuffix(Value: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function GetState(const DSAInfo: TDSARegItem; out LastResult: integer; const OnCustomData: TDSACustomData = nil): boolean; virtual;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): boolean; virtual; abstract;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: boolean): boolean; virtual; abstract;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): extended; virtual; abstract;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: extended): extended; virtual; abstract;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): int64; virtual; abstract;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: int64): int64; virtual; abstract;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): integer; virtual; abstract;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: integer): integer; virtual; abstract;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; virtual; abstract;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; virtual; abstract;
    procedure SetState(const DSAInfo: TDSARegItem; const DontShowAgain: boolean; const LastResult: integer; const OnCustomData: TDSACustomData = nil); virtual;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: boolean); virtual; abstract;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: extended); virtual; abstract;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: int64); virtual; abstract;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: integer); virtual; abstract;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); virtual; abstract;

    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix;
  end;

  TDSARegStorage = class (TDSAStorage)
  private
    FRootKey: HKEY;
    FKey: string;
  protected
    procedure CreateKey(const DSAInfo: TDSARegItem); virtual;
    function GetCheckMarkTextSuffix: string; override;
    procedure SetCheckMarkTextSuffix(Value: string); override;
  public
    constructor Create(const ARootKey: HKEY; const AKey: string);

    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: boolean): boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: extended): extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: int64): int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: integer): integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;

    property RootKey: HKEY read FRootKey write FRootKey;
    property Key: string read FKey write FKey;
  end;

  TDSAQueueStorage = class (TDSAStorage)
  private
    FList: TStrings;
    FCheckMarkSuffix: string;
  protected
    procedure AddDSA(const DSAInfo: TDSARegItem);
    procedure DeleteDSA(const Index: integer);
    function FindDSA(const DSAInfo: TDSARegItem): integer;
    function GetCheckMarkTextSuffix: string; override;
    function GetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: integer): string;
    function HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): boolean;
    procedure SetCheckMarkTextSuffix(Value: string); override;
    procedure SetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: boolean): boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: extended): extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: int64): int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: integer): integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;

    property CheckMarkTextSuffix: string read GetCheckMarkTextSuffix write SetCheckMarkTextSuffix;
  end;

const
  ssCustomRead: Pointer = @TDSAStorage.BeginCustomRead;
  ssCustomWrite: Pointer = @TDSAStorage.BeginCustomWrite;
  ssRead: Pointer  = @TDSAStorage.BeginRead;
  ssWrite: Pointer = @TDSAStorage.BeginWrite;

//--------------------------------------------------------------------------------------------------
// MessageDlg replacements and extensions
//--------------------------------------------------------------------------------------------------

// Additional values for DefaultButton, CancelButton and HelpButton parameters
const
  mbNone    = TMsgDlgBtn( -1);
  mbDefault = TMsgDlgBtn( -2);

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);
procedure ShowMessageFmt(const Msg: string; const Params: array of const; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlg(const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;

//--------------------------------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//--------------------------------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: integer; const Msg: string; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);
procedure DSAShowMessageFmt(const DlgID: integer; const Msg: string; const Params: array of const; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const ADynControlEngine: TJvDynControlEngine = nil);

function DSAMessageDlg(const DlgID: integer; const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlg(const DlgID: integer; const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlg(const DlgID: integer; const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: TMsgDlgBtn = mbDefault; const CancelButton: TMsgDlgBtn = mbDefault; const HelpButton: TMsgDlgBtn = mbHelp; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;

function DSAMessageDlgEx(const DlgID: integer; const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): integer; overload;
function DSAMessageDlgEx(const DlgID: integer; const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TModalResult; overload;
function DSAMessageDlgEx(const DlgID: integer; const Caption, Msg: string; const Picture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind = dckScreen; const Timeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; const HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): integer; overload;

//--------------------------------------------------------------------------------------------------
// Generic DSA dialog
//--------------------------------------------------------------------------------------------------
function CreateDSAMessageForm(const ACaption, Msg: string; const APicture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: integer; const CheckCaption: string; const Center: TDlgCenterKind = dckScreen; const ATimeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TDSAMessageForm;

//--------------------------------------------------------------------------------------------------
// DSA registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: integer; const Name, Description: string; const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
procedure UnregisterDSA(const DlgID: integer);
function LocateDSAReg(const DlgID: integer): TDSARegItem;

//--------------------------------------------------------------------------------------------------
// DSA state setting/retrieving
//--------------------------------------------------------------------------------------------------

function GetDSAState(const DlgID: integer): boolean; overload;
function GetDSAState(const DlgID: integer; out ResCode: integer; const OnCustomData: TDSACustomData = nil): boolean; overload;
procedure SetDSAState(const DlgID: integer; const DontShowAgain: boolean; const LastResult: integer = mrNone; const OnCustomData: TDSACustomData = nil);

//--------------------------------------------------------------------------------------------------
// Iterating the DSA registration
//--------------------------------------------------------------------------------------------------

function DSACount: integer;
function DSAItem(const Index: integer): TDSARegItem;

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
  EJvDSADialog = class (EJVCLException);

  TJvDSADataEvent = procedure(Sender: TObject; const DSAInfo: TDSARegItem; const Storage: TDSAStorage) of object;
  TJvDSAAutoCloseEvent = procedure(Sender: TObject; var Handled: boolean) of object;

  TJvDSADialog = class (TJvComponent)
  private
    FCheckControl: TWinControl;
    FDialogID: integer;
    FIgnoreDSAChkMrkTxt: boolean;
    FOnUpdateKeys: TJvDSADataEvent;
    FOnApplyKeys: TJvDSADataEvent;
    FOrgOwner: TComponent;
    FOrgShowModalPtr: Pointer;
    FTimeout: integer;
    FTimer: TTimer;
    FTimerCount: integer;
    FOnCountdown: TNotifyEvent;
    FOnAutoClose: TJvDSAAutoCloseEvent;
  protected
    procedure AutoClose;
    procedure AfterShow; virtual;
    procedure ApplySavedState; virtual;
    procedure BeforeShow; virtual;
    procedure DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    function DoAutoClose: boolean;
    procedure DoCountDown;
    procedure DoUpdateKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem); virtual;
    function GetDSAStateInternal(out ModalResult: integer): boolean;
    function GetOrgOwner: TComponent;
    function GetOrgShowModalPtr: Pointer;
    function GetStorage: TDSAStorage;
    procedure FormPatch;
    procedure FormUnPatch;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCheckControl(Value: TWinControl); virtual;
    procedure SetDialogID(Value: integer); virtual;
    procedure SetOrgOwner(Value: TComponent);
    procedure SetOrgShowModalPtr(Value: Pointer);
    procedure TimerEvent(Sender: TObject);
    procedure UpdateDSAState; virtual;

    property OrgOwner: TComponent read GetOrgOwner write SetOrgOwner;
    property OrgShowModalPtr: Pointer read GetOrgShowModalPtr write SetOrgShowModalPtr;
    property Storage: TDSAStorage read GetStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetModalResult: integer; virtual;
    function IsDSAChecked: boolean; virtual;
    procedure Loaded; override;
    procedure CancelCountdown; virtual;
    function SecondsLeft: integer;
  published
    property Timeout: integer read FTimeout write FTimeout;
    property CheckControl: TWinControl read FCheckControl write SetCheckControl;
    property DialogID: integer read FDialogID write SetDialogID;
    property IgnoreDSAChkMrkTxt: boolean read FIgnoreDSAChkMrkTxt write FIgnoreDSAChkMrkTxt;
    property OnApplyKeys: TJvDSADataEvent read FOnApplyKeys write FOnApplyKeys;
    property OnUpdateKeys: TJvDSADataEvent read FOnUpdateKeys write FOnUpdateKeys;
    property OnCountdown: TNotifyEvent read FOnCountdown write FOnCountdown;
    property OnAutoClose: TJvDSAAutoCloseEvent read FOnAutoClose write FOnAutoClose;
  end;

implementation

uses
  Consts, Math, TypInfo,
  JclRegistry, JclSysUtils,
  JvResources, JvDynControlEngine_Interface;

{$IFNDEF DELPHI6_UP}
type
  PBoolean = ^boolean;
{$ENDIF DELPHI6_UP}

const
  cDSAStateValueName      = 'DSA_State';  // do not localize
  cDSAStateLastResultName = 'LastResult'; // do not localize

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

function GetCheckMarkText(const ID: TDSACheckTextKind): string;
var
  Idx: integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    Result := CheckMarkTexts[Idx]
  else
    Result := '';
end;

constructor TDSAMessageForm.CreateNew(AOwner: TComponent; Dummy: integer);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner, Dummy);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled  := false;
  FTimer.Interval := 1000;
  FTimer.OnTimer  := TimerEvent;
end;

procedure TDSAMessageForm.CustomKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  CancelAutoClose;
  if (Shift = [ssCtrl]) and (Key = word('C')) then
  begin
    SysUtils.Beep;
    WriteToClipBoard(GetFormText);
  end;
end;

procedure TDSAMessageForm.CustomMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  CancelAutoClose;
end;

procedure TDSAMessageForm.CustomShow(Sender: TObject);
var
  I: integer;
begin
  if Timeout <> 0 then
    FTimer.Enabled := true;
  for I := 0 to ComponentCount - 1 do
  begin
    if (Components[I] is TButton) and (Components[I] as TButton).Default then
    begin
      (Components[I] as TButton).SetFocus;
      Break;
    end;
  end;
  FCountdown := TLabel(FindComponent('Countdown'));
end;

procedure TDSAMessageForm.HelpButtonClick(Sender: TObject);
begin
  CancelAutoClose;
  Application.HelpContext(HelpContext);
end;

procedure TDSAMessageForm.TimerEvent(Sender: TObject);
var
  I: integer;
begin
  if FTimer.Enabled then
  begin
    Dec(FTimeout);
    if FTimeout = 0 then
    begin
      FTimer.Enabled := false;
      for I := 0 to ComponentCount - 1 do
      begin
        if (Components[I] is TButton) and (Components[I] as TButton).Default then
        begin
          (Components[I] as TButton).Click;
          Exit;
        end;
      end;
      // No default button found; just close the form
      Close;
    end
    else if FCountdown <> nil then
      FCountdown.Caption := Format(RsCntdownText, [Timeout, TimeoutUnit(Timeout)]);
  end;
end;

procedure TDSAMessageForm.WriteToClipBoard(Text: string);
var
  Data:    THandle;
  DataPtr: Pointer;
begin
  if OpenClipBoard(0) then
  begin
    try
      Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Length(Text) + 1);
      try
        DataPtr := GlobalLock(Data);
        try
          Move(pchar(Text)^, DataPtr^, Length(Text) + 1);
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
      CloseClipboard;
    end;
  end
  else
    raise EJVCLException.Create(SCannotOpenClipboard);
end;

function TDSAMessageForm.GetFormText: string;
var
  DividerLine, ButtonCaptions: string;
  I: integer;
begin
  DividerLine := StringOfChar('-', 27) + CrLf;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TButton then
      ButtonCaptions := ButtonCaptions + TButton(Components[I]).Caption + StringOfChar(' ', 3);
  ButtonCaptions := StringReplace(ButtonCaptions, '&', '', [rfReplaceAll]);
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TLabel) do
    Dec(I);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, CrLf, DividerLine, TLabel(Components[I]).Caption, CrLf, DividerLine, ButtonCaptions, CrLf, DividerLine]);
end;

function TDSAMessageForm.TimeoutUnit(Secs: integer): string;
begin
  if Secs <> 1 then
    Result := RsCntdownSecsText
  else
    Result := RsCntdownSecText;
end;

procedure TDSAMessageForm.CancelAutoClose;
begin
  FTimer.Enabled := false;
  FreeAndNil(FCountdown);
end;

function TDSAMessageForm.IsDSAChecked: boolean;
var
  I: integer;
begin
  I := ComponentCount - 1;
  while (I > -1) and not (Components[I] is TCustomCheckBox) do
    Dec(I);
  if (I > -1) then
    Result := TCheckBox(Components[I]).Checked
  else
    Result := false;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I:      integer;
  Buffer: array [0..51] of char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint32(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function CreateDSAMessageForm(const ACaption, Msg: string; const APicture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: integer; const CheckCaption: string; const Center: TDlgCenterKind = dckScreen; const ATimeout: integer = 0; const DefaultButton: integer = 0; const CancelButton: integer = 1; HelpButton: integer = -1; const ADynControlEngine: TJvDynControlEngine = nil): TDSAMessageForm;
const
  mcHorzMargin    = 8;
  mcVertMargin    = 8;
  mcHorzSpacing   = 10;
  mcVertSpacing   = 10;
  mcButtonWidth   = 50;
  mcButtonHeight  = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth, ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth, IconTextWidth, IconTextHeight, X, ALeft: integer;
  ChkTextWidth: integer;
  TimeoutTextWidth: integer;
  IconID: pchar;
  TempRect, TextRect: TRect;
  I:      integer;
  CenterParent: TComponent;
  CenterParLeft, CenterParTop, CenterParWidth, CenterParHeight: integer;
  DynControlEngine: TJvDynControlEngine;
  CountDownlabel, MessageLabel: TControl;
  Image:  TWinControl;
  DynControlImage: IJvDynControlImage;
  DynControlLabel: IJvDynControlLabel;
  Panel:  TWinControl;
begin
  if Assigned(ADynControlEngine) then
    DynControlEngine := ADynControlEngine
  else
    DynControlEngine := DefaultDynControlEngine;
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
    CenterParLeft   := 0;
    CenterParTop    := 0;
    CenterParWidth  := TScreen(CenterParent).Width;
    CenterParHeight := TScreen(CenterParent).Height;
  end
  else
  begin
    with TWinControl(CenterParent) do
    begin
      CenterParLeft   := Left;
      CenterParTop    := Top;
      CenterParWidth  := Width;
      CenterParHeight := Height;
    end;
  end;
  if HelpButton = High(integer) then
    HelpButton := High(Buttons);
  Result := TDSAMessageForm.CreateNew(Screen.ActiveCustomForm);
  try
    with Result do
    begin
      BiDiMode    := Application.BiDiMode;
      BorderStyle := bsDialog;
      Canvas.Font := Font;
      KeyPreview  := true;
      OnKeyDown   := CustomKeyDown;
      OnShow      := CustomShow;
      OnMouseDown := CustomMouseDown;
      DialogUnits := GetAveCharSize(Canvas);
      HorzMargin  := MulDiv(mcHorzMargin, DialogUnits.X, 4);
      VertMargin  := MulDiv(mcVertMargin, DialogUnits.Y, 8);
      HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
      VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
      Timeout     := Abs(ATimeout);
      for I := Low(Buttons) to High(Buttons) do
      begin
        TextRect := Rect(0, 0, 0, 0);
        Windows.DrawText(Canvas.Handle, pchar(Buttons[I]), -1, TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
        with TextRect do
          if (Right - Left + 8) > ButtonWidth then
            ButtonWidth := (Right - Left + 8);
      end;
      ButtonHeight  := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
      ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
      if (Screen.Width div 2) > (CenterParWidth + (2 * CenterParLeft)) then
        SetRect(TextRect, 0, 0, CenterParWidth + (2 * CenterParLeft), 0)
      else
        SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
      DrawText(Canvas.Handle, pchar(Msg), Length(Msg) + 1, TextRect,
        DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
      IconTextWidth  := TextRect.Right;
      IconTextHeight := TextRect.Bottom;
      if CheckCaption <> '' then
      begin
        SetRect(TempRect, 0, 0, Screen.Width div 2, 0);
        DrawText(Canvas.Handle, pchar(CheckCaption), Length(CheckCaption) + 1, TempRect,
          DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
        ChkTextWidth := TempRect.Right;
      end
      else
        ChkTextWidth := 0;
      if ATimeout > 0 then
      begin
        SetRect(TempRect, 0, 0, Screen.Width div 2, 0);
        DrawText(Canvas.Handle, pchar(Format(RsCntdownText, [Timeout, TimeoutUnit(Timeout)])),
          Length(Format(RsCntdownText, [Timeout, TimeoutUnit(Timeout)])) + 1, TempRect,
          DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
        TimeoutTextWidth := TempRect.Right;
      end
      else
        TimeoutTextWidth := 0;
      if APicture <> nil then
      begin
        Inc(IconTextWidth, APicture.Width + HorzSpacing);
        if IconTextHeight < APicture.Height then
          IconTextHeight := APicture.Height;
      end;
      ButtonCount      := Length(Buttons);
      ButtonGroupWidth := 0;
      if ButtonCount <> 0 then
        ButtonGroupWidth := ButtonWidth * ButtonCount + ButtonSpacing * (ButtonCount - 1);
      ClientWidth := MAx(TimeoutTextWidth, Max(17 + ChkTextWidth, Max(IconTextWidth, ButtonGroupWidth))) + HorzMargin * 2;
      ClientHeight := IconTextHeight + ButtonHeight + VertSpacing * 2 + VertMargin;
      if CheckCaption <> '' then
        Result.ClientHeight := Result.ClientHeight + VertMargin + 17;
      if ATimeout > 0 then
        Result.ClientHeight := Result.CLientHeight + VertMargin + 13;
      Left := (CenterParWidth div 2) - (Width div 2) + CenterParLeft;
      Top := (CenterParHeight div 2) - (Height div 2) + CenterParTop;
      if ACaption <> '' then
        Caption := ACaption
      else
        Caption := Application.Title;
      Panel := DynControlEngine.CreatePanelControl(Result, Result, 'Panel', '', alClient);
      if APicture <> nil then
      begin
        Image := DynControlEngine.CreateImageControl(Result, Panel, 'Image');
        if Supports(Image, IJvDynControlImage, DynControlImage) then
        begin
          DynControlImage.ControlSetGraphic(APicture);
          DynControlImage.ControlSetCenter(true);
        end;
        Image.SetBounds(HorzMargin - 2, VertMargin - 2, APicture.Width + 4, APicture.Height + 4);
      end;
      MessageLabel := DynControlEngine.CreateLabelControl(Result, Panel, 'Message', Msg, nil);
 //      if Supports(MessageLabel, IJvDynControlLabel, DynControlLabel) then
 //        DynControlLabel.ControlSetWordWrap(True);
      with MessageLabel do
      begin
        BoundsRect := TextRect;
        BiDiMode   := Result.BiDiMode;
        ALeft      := IconTextWidth - TextRect.Right + HorzMargin;
        if UseRightToLeftAlignment then
          ALeft := Result.ClientWidth - ALeft - Width;
        SetBounds(ALeft, VertMargin,
          TextRect.Right, TextRect.Bottom);
      end;
      X := (ClientWidth - ButtonGroupWidth) div 2;
      for I := Low(Buttons) to High(Buttons) do
      begin
        with DynControlEngine.CreateButton(Result, Panel, 'Button' + IntToStr(I), Buttons[I], '', nil, false, false) do
        begin
          ModalResult := Results[I];
          if I = DefaultButton then
            Default := true;
          if I = CancelButton then
            Cancel := true;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing, ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if I = HelpButton then
            OnClick := HelpButtonClick;
        end;
      end;
      if CheckCaption <> '' then
        with DynControlEngine.CreateCheckboxControl(Result, Panel, 'DontShowAgain', CheckCaption) do
        begin
          BiDiMode := Result.BiDiMode;
          SetBounds(HorzMargin, IconTextHeight + VertMargin + VertSpacing * 2 + ButtonHeight,
            Result.ClientWidth - 2 * HorzMargin, Height);
        end;
      if ATimeout > 0 then
      begin
        CountDownlabel := DynControlEngine.CreateLabelControl(Result, Panel, 'Countdown', Format(RsCntdownText, [Timeout, TimeoutUnit(Timeout)]), nil);
        with CountDownlabel do
        begin
          BiDiMode := Result.BiDiMode;
          if CheckCaption = '' then
            SetBounds(HorzMargin, IconTextHeight + VertMargin + VertSpacing * 2 + ButtonHeight,
              Result.ClientWidth - 2 * HorzMargin, Height)
          else
            SetBounds(HorzMargin, IconTextHeight + 2 * VertMargin + VertSpacing * 2 + ButtonHeight + 17, Result.ClientWidth - 2 * HorzMargin, Height);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

//=== TDSARegister ===========================================================

type
  TAddResult = (arAdded, arExists, arDuplicateID, arDuplicateName);

  TDSARegister = class
  private
    FList: array of TDSARegItem;
  protected
    function AddNew: integer;
    procedure Remove(const Index: integer);
    function IndexOf(const ID: integer): integer; overload;
    function IndexOf(const Name: string): integer; overload;
    function IndexOf(const Item: TDSARegItem): integer; overload;
  public
    destructor Destroy; override;
    function Add(const Item: TDSARegItem): TAddResult; overload;
    function Add(const ID: integer; const Name, Description: string; const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow): TAddResult; overload;
    procedure Clear;
//    procedure Delete(const Item: TDSARegItem); overload;
    procedure Delete(const ID: integer); overload;
//    procedure Delete(const Name: string); overload;
    function Locate(const ID: integer): TDSARegItem; overload;
//    function Locate(const Name: string): TDSARegItem; overload;
  end;

const
  EmptyItem: TDSARegItem = (ID: High(integer); Name: ''; Storage: nil);

var
  DSARegister: TDSARegister;

destructor TDSARegister.Destroy;
begin
  inherited Destroy;
  Clear;
end;

function TDSARegister.AddNew: integer;
begin
  Result := Length(FList);
  SetLength(FList, Result + 1);
end;

procedure TDSARegister.Remove(const Index: integer);
var
  I: integer;
begin
  for I := Index + 1 to High(FList) do
  begin
    FList[I - 1].ID      := FList[I].ID;
    FList[I - 1].Name    := FList[I].Name;
    FList[I - 1].Description := FList[I].Description;
    FList[I - 1].ChkTextKind := FList[I].ChkTextKind;
    FList[I - 1].Storage := FList[I].Storage;
  end;
  SetLength(FList, High(FList));
end;

function TDSARegister.IndexOf(const ID: integer): integer;
begin
  Result := High(FList);
  while (Result > -1) and (FList[Result].ID <> ID) do
    Dec(Result);
end;

function TDSARegister.IndexOf(const Name: string): integer;
begin
  Result := High(FList);
  while (Result > -1) and not AnsiSameText(FList[Result].Name, Name) do
    Dec(Result);
end;

function TDSARegister.IndexOf(const Item: TDSARegItem): integer;
begin
  Result := IndexOf(Item.ID);
  if (Result > -1) and not AnsiSameText(FList[Result].Name, Item.Name) then
    Result := -1;
end;

function TDSARegister.Add(const Item: TDSARegItem): TAddResult;
var
  Idx: integer;
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
    Idx    := AddNew;
    FList[Idx].ID := Item.ID;
    FList[Idx].Name := Item.Name;
    FList[Idx].Description := Item.Description;
    FList[Idx].Storage := Item.Storage;
    FList[Idx].ChkTextKind := Item.ChkTextKind;
    Result := arAdded;
  end;
end;

function TDSARegister.Add(const ID: integer; const Name, Description: string; const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow): TAddResult;
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

procedure TDSARegister.Clear;
begin
  SetLength(FList, 0);
end;

(* make Delphi 5 compiler happy // andreas
procedure TDSARegister.Delete(const Item: TDSARegItem);
var
  Idx: Integer;
begin
  Idx := IndexOf(Item.ID);
  if (Idx > -1) and AnsiSameText(FList[Idx].Name, Item.Name) then
    Remove(Idx);
end;
*)

procedure TDSARegister.Delete(const ID: integer);
var
  Idx: integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Remove(Idx);
end;

(* make Delphi 5 compiler happy // andreas
procedure TDSARegister.Delete(const Name: string);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Remove(Idx);
end;
*)

function TDSARegister.Locate(const ID: integer): TDSARegItem;
var
  Idx: integer;
begin
  Idx := IndexOf(ID);
  if Idx > -1 then
    Result := FList[Idx]
  else
    Result := EmptyItem;
end;

(* make Delphi 5 compiler happy // andreas
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
*)

//=== TDSAStorage ============================================================

constructor TDSAStorage.Create;
begin
  inherited Create;
  FStates := TStack.Create;
end;

destructor TDSAStorage.Destroy;
begin
  FStates.Free;
  inherited Create;
end;

procedure TDSAStorage.BeginCustomRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomRead);
end;

procedure TDSAStorage.BeginCustomWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssCustomWrite);
end;

procedure TDSAStorage.BeginRead(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssRead);
end;

procedure TDSAStorage.BeginWrite(const DSAInfo: TDSARegItem);
begin
  FStates.Push(ssWrite);
end;

procedure TDSAStorage.EndCustomRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomRead then
    raise EJvDSADialog.Create(RsECannotEndCustomReadIfNotInCustomRea);
  FStates.Pop;
end;

procedure TDSAStorage.EndCustomWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssCustomWrite then
    raise EJvDSADialog.Create(RsECannotEndCustomWriteIfNotInCustomWr);
  FStates.Pop;
end;

procedure TDSAStorage.EndRead(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssRead then
    raise EJvDSADialog.Create(RsECannotEndReadIfNotInReadMode);
  FStates.Pop;
end;

procedure TDSAStorage.EndWrite(const DSAInfo: TDSARegItem);
begin
  if FStates.Peek <> ssWrite then
    raise EJvDSADialog.Create(RsECannotEndWriteIfNotInWriteMode);
  FStates.Pop;
end;

function TDSAStorage.IsKeyNameAllowed(const Key: string): boolean;
begin
  if AnsiSameText(Key, cDSAStateValueName) or AnsiSameText(Key, cDSAStateLastResultName) then
    Result := integer(FStates.Peek) in [integer(ssRead), integer(ssWrite)]
  else
    Result := integer(FStates.Peek) in [integer(ssCustomRead), integer(ssCustomWrite)];
end;

function TDSAStorage.GetState(const DSAInfo: TDSARegItem; out LastResult: integer; const OnCustomData: TDSACustomData = nil): boolean;
begin
  BeginRead(DSAInfo);
  try
    LastResult := 0;
    Result     := ReadBoolDef(DSAInfo, cDSAStateValueName, false);
    if Result then
    begin
      LastResult := ReadIntegerDef(DSAInfo, cDSAStateLastResultName, 0);
      if Assigned(OnCustomData) then
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

procedure TDSAStorage.SetState(const DSAInfo: TDSARegItem; const DontShowAgain: boolean; const LastResult: integer; const OnCustomData: TDSACustomData = nil);
begin
  BeginWrite(DSAInfo);
  try
    WriteBool(DSAInfo, cDSAStateValueName, DontShowAgain);
    if DontShowAgain then
    begin
      WriteInteger(DSAInfo, cDSAStateLastResultName, LastResult);
      if Assigned(OnCustomData) then
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

//=== TDSARegStorage =========================================================

constructor TDSARegStorage.Create(const ARootKey: HKEY; const AKey: string);
begin
  inherited Create;
  FRootKey := ARootKey;
  FKey     := AKey;
end;

procedure TDSARegStorage.CreateKey(const DSAInfo: TDSARegItem);
begin
  if not (RegKeyExists(RootKey, Key + '\' + DSAInfo.Name) or (RegCreateKey(RootKey, Key + '\' + DSAInfo.Name, '') = ERROR_SUCCESS)) then
    raise EJvDSADialog.CreateFmt(RsEDSARegKeyCreateError, [Key + '\' + DSAInfo.Name]);
end;

function TDSARegStorage.GetCheckMarkTextSuffix: string;
begin
  Result := '';
end;

procedure TDSARegStorage.SetCheckMarkTextSuffix(Value: string);
begin
end;

function TDSARegStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): boolean;
begin
  Result := RegReadBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: boolean): boolean;
begin
  Result := RegReadBoolDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): extended;
begin
  RegReadBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(extended));
end;

function TDSARegStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: extended): extended;
begin
  if RegReadBinaryDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Result, SizeOf(extended), 0) = 0 then
    Result := Default;
end;

function TDSARegStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): int64;
begin
  Result := RegReadDWORD(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: int64): int64;
begin
  Result := RegReadDWORDDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): integer;
begin
  Result := RegReadInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: integer): integer;
begin
  Result := RegReadIntegerDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

function TDSARegStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := RegReadString(RootKey, Self.Key + '\' + DSAInfo.Name, Key);
end;

function TDSARegStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string;
begin
  Result := RegReadStringDef(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Default);
end;

procedure TDSARegStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: boolean);
begin
  CreateKey(DSAInfo);
  RegWriteBool(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: extended);
var
  Temp: extended;
begin
  CreateKey(DSAInfo);
  Temp := Value;
  RegWriteBinary(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Temp, SizeOf(extended));
end;

procedure TDSARegStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: int64);
begin
  CreateKey(DSAInfo);
  RegWriteDWORD(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: integer);
begin
  CreateKey(DSAInfo);
  RegWriteInteger(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

procedure TDSARegStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string);
begin
  CreateKey(DSAInfo);
  RegWriteString(RootKey, Self.Key + '\' + DSAInfo.Name, Key, Value);
end;

//=== TDSAValues =============================================================

const
  DSABool   = 1;
  DSAFloat  = 2;
  DSAInt64  = 3;
  DSAInt    = 4;
  DSAString = 5;

  DSAKindTexts: array [DSABool..DSAString] of string = (
    RsDSAAccessBool, RsDSAAccessFloat, RsDSAAccessInt64, RsDSAAccessInt, RsDSAAccessString);

type
  TDSAValues = class (TStringList)
    constructor Create;
  end;

constructor TDSAValues.Create;
begin
  inherited Create;
  Sorted := true;
end;

//=== TDSAQueueStorage =======================================================

constructor TDSAQueueStorage.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  TStringList(FList).Sorted := true;
  FCheckMarkSuffix := RsInTheCurrentQueue;
end;

destructor TDSAQueueStorage.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDSAQueueStorage.AddDSA(const DSAInfo: TDSARegItem);
begin
  if FindDSA(DSAInfo) < 0 then
    FList.AddObject(DSAInfo.Name, TDSAValues.Create);
end;

procedure TDSAQueueStorage.DeleteDSA(const Index: integer);
begin
  FList.Objects[Index].Free;
  FList.Delete(Index);
end;

function TDSAQueueStorage.FindDSA(const DSAInfo: TDSARegItem): integer;
begin
  Result := FList.IndexOf(DSAInfo.Name);
end;

function TDSAQueueStorage.GetCheckMarkTextSuffix: string;
begin
  Result := FCheckMarkSuffix;
end;

function TDSAQueueStorage.GetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: integer): string;
var
  I: integer;
  DSAKeys: TStrings;
begin
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(RsEDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(RsEDSAKeyNotFound, [Key]);
  if integer(DSAKeys.Objects[I]) <> Kind then
    raise EJvDSADialog.CreateFmt(RsEDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
  Result := DSAKeys.Values[Key];
end;

function TDSAQueueStorage.HasDSAKey(const DSAInfo: TDSARegItem; const Key: string): boolean;
var
  I: integer;
  DSAKeys: TStrings;
begin
  I      := FindDSA(DSAInfo);
  Result := I > -1;
  if Result then
  begin
    DSAKeys := TStrings(FList.Objects[I]);
    Result  := DSAKeys.IndexOfName(Key) > -1;
  end;
end;

procedure TDSAQueueStorage.SetCheckMarkTextSuffix(Value: string);
begin
  if Value <> CheckMarkTextSuffix then
    FCheckMarkSuffix := Value;
end;

procedure TDSAQueueStorage.SetDSAValue(const DSAInfo: TDSARegItem; const Key: string; const Kind: integer; const Value: string);
var
  I: integer;
  DSAKeys: TStrings;
begin
  AddDSA(DSAInfo);
  I := FindDSA(DSAInfo);
  if I < 0 then
    raise EJvDSADialog.CreateFmt(RsEDSADialogIDNotStored, [DSAInfo.ID]);
  DSAKeys := TStrings(FList.Objects[I]);
  I := DSAKeys.IndexOfName(Key);
  if I < 0 then
    DSAKeys.AddObject(Key + '=' + Value, TObject(Kind))
  else
  begin
    if integer(DSAKeys.Objects[I]) <> Kind then
      raise EJvDSADialog.CreateFmt(RsEDSAKeyNoAccessAs, [Key, DSAKindTexts[Kind]]);
    DSAKeys.Values[Key] := Value;
  end;
end;

procedure TDSAQueueStorage.Clear;
begin
  while FList.Count > 0 do
    DeleteDSA(FList.Count - 1);
end;

function TDSAQueueStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): boolean;
var
  S: string;
begin
  S      := GetDSAValue(DSAInfo, Key, DSABool);
  Result := AnsiSameText(S, 'True') or AnsiSameText(S, '1');
end;

function TDSAQueueStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: boolean): boolean;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadBool(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): extended;
begin
  Result := StrToFloat(StringReplace(GetDSAValue(DSAInfo, Key, DSAFloat), ThousandSeparator, DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
end;

function TDSAQueueStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: extended): extended;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadFloat(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): int64;
begin
  Result := StrToInt64(GetDSAValue(DSAInfo, Key, DSAInt64));
end;

function TDSAQueueStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: int64): int64;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInt64(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): integer;
begin
  Result := StrToInt(GetDSAValue(DSAInfo, Key, DSAInt));
end;

function TDSAQueueStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: integer): integer;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadInteger(DSAInfo, Key)
  else
    Result := Default;
end;

function TDSAQueueStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := GetDSAValue(DSAInfo, Key, DSAString);
end;

function TDSAQueueStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string;
begin
  if HasDSAKey(DSAInfo, Key) then
    Result := ReadString(DSAInfo, Key)
  else
    Result := Default;
end;

procedure TDSAQueueStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: boolean);
begin
  if Value then
    SetDSAValue(DSAInfo, Key, DSABool, '1')
  else
    SetDSAValue(DSAInfo, Key, DSABool, '0');
end;

procedure TDSAQueueStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: extended);
begin
  SetDSAValue(DSAInfo, Key, DSAFloat, FloatToStr(Value));
end;

procedure TDSAQueueStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: int64);
begin
  SetDSAValue(DSAInfo, Key, DSAInt64, IntToStr(Value));
end;

procedure TDSAQueueStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: integer);
begin
  SetDSAValue(DSAInfo, Key, DSAInt, IntToStr(Value));
end;

procedure TDSAQueueStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string);
begin
  SetDSAValue(DSAInfo, Key, DSAString, Value);
end;

//--------------------------------------------------------------------------------------------------
// Helpers
//--------------------------------------------------------------------------------------------------

const
  Captions: array [TMsgDlgType] of string =
    (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');
  IconIDs: array [TMsgDlgType] of pchar =
    (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions: array [TMsgDlgBtn] of string =
    (SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp);
  ModalResults: array [TMsgDlgBtn] of integer =
    (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);

function DlgCaption(const DlgType: TMsgDlgType): string;
begin
  Result := Captions[DlgType];
end;

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

function DlgButtonCaptions(const Buttons: TMsgDlgButtons): TDynStringArray;
var
  I: integer;
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

function DlgButtonResults(const Buttons: TMsgDlgButtons): TDynIntegerArray;
var
  I: integer;
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

function ButtonIndex(const Results: array of integer; const ResCode: integer): integer; overload;
begin
  Result := High(Results);
  while (Result > -1) and (Results[Result] <> ResCode) do
    Dec(Result);
end;

function ButtonIndex(const Results: array of integer; const Button: TMsgDlgBtn): integer; overload;
begin
  Result := ButtonIndex(Results, Modalresults[Button]);
end;

//--------------------------------------------------------------------------------------------------
// MessageDlg replacements and extensions
//--------------------------------------------------------------------------------------------------

procedure ShowMessage(const Msg: string; const Center: TDlgCenterKind; const Timeout: integer; const ADynControlEngine: TJvDynControlEngine);
begin
  MessageDlg(Msg, mtCustom, [mbOK], 0, Center, Timeout);
end;

procedure ShowMessageFmt(const Msg: string; const Params: array of const; const Center: TDlgCenterKind; const Timeout: integer; const ADynControlEngine: TJvDynControlEngine);
begin
  MessageDlg(Format(Msg, Params), mtCustom, [mbOK], 0, Center, Timeout);
end;

function MessageDlg(const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlg(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlg(Caption, Msg, TmpPic, Buttons, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlg(const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  DefBtn:     TMsgDlgBtn;
  CanBtn:     TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOK in Buttons then
      DefBtn := mbOK
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
      CanBtn := mbOK;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := MessageDlgEx(Caption, Msg, Picture, DlgButtonCaptions(Buttons), BtnResults, HelpCtx, Center, Timeout, ButtonIndex(BtnResults, DefBtn), ButtonIndex(BtnResults, CanBtn), ButtonIndex(BtnResults, HelpButton), ADynControlEngine);
end;

function MessageDlgEx(const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: integer; const CancelButton: integer; const HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(DlgCaption(DlgType), Msg, TmpPic, Buttons, Results, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlgEx(const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: integer; const CancelButton: integer; const HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := MessageDlgEx(Caption, Msg, TmpPic, Buttons, Results, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function MessageDlgEx(const Caption, Msg: string; const Picture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: integer; const CancelButton: integer; const HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): TModalResult;
begin
  with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx, '', Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine) do
    try
      Result := ShowModal;
    finally
      Free;
    end;
end;

//--------------------------------------------------------------------------------------------------
// "Don't Show Again" (DSA) dialogs
//--------------------------------------------------------------------------------------------------

procedure DSAShowMessage(const DlgID: integer; const Msg: string; const Center: TDlgCenterKind; const Timeout: integer; const ADynControlEngine: TJvDynControlEngine);
begin
  DSAMessageDlg(DlgID, Msg, mtCustom, [mbOK], 0, Center, Timeout, mbDefault, mbDefault, mbHelp, ADynControlEngine);
end;

procedure DSAShowMessageFmt(const DlgID: integer; const Msg: string; const Params: array of const; const Center: TDlgCenterKind; const Timeout: integer; const ADynControlEngine: TJvDynControlEngine);
begin
  DSAMessageDlg(DlgID, Format(Msg, Params), mtCustom, [mbOK], 0, Center, Timeout, mbDefault, mbDefault, mbHelp, ADynControlEngine);
end;

function DSAMessageDlg(const DlgID: integer; const Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlg(const DlgID: integer; const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlg(DlgID, Caption, Msg, TmpPic, Buttons, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlg(const DlgID: integer; const Caption, Msg: string; const Picture: TGraphic; const Buttons: TMsgDlgButtons; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: TMsgDlgBtn; const CancelButton: TMsgDlgBtn; const HelpButton: TMsgDlgBtn; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  DefBtn:     TMsgDlgBtn;
  CanBtn:     TMsgDlgBtn;
  BtnResults: TDynIntegerArray;
begin
  if DefaultButton = mbDefault then
  begin
    if mbOK in Buttons then
      DefBtn := mbOK
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
      CanBtn := mbOK;
  end
  else
    CanBtn := CancelButton;
  BtnResults := DlgButtonResults(Buttons);
  Result := DSAMessageDlgEx(DlgID, Caption, Msg, Picture, DlgButtonCaptions(Buttons), BtnResults, HelpCtx, Center, Timeout, ButtonIndex(BtnResults, DefBtn), ButtonIndex(BtnResults, CanBtn), ButtonIndex(BtnResults, HelpButton), ADynControlEngine);
end;

function DSAMessageDlgEx(const DlgID: integer; const Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout: integer; const DefaultButton: integer; const CancelButton: integer; const HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): integer;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, DlgCaption(DlgType), Msg, TmpPic, Buttons, Results, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlgEx(const DlgID: integer; const Caption, Msg: string; const DlgType: TMsgDlgType; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout, DefaultButton, CancelButton, HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): TModalResult;
var
  TmpPic: TGraphic;
begin
  TmpPic := DlgPic(DlgType);
  try
    Result := DSAMessageDlgEx(DlgID, Caption, Msg, TmpPic, Buttons, Results, HelpCtx, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine);
  finally
    TmpPic.Free;
  end;
end;

function DSAMessageDlgEx(const DlgID: integer; const Caption, Msg: string; const Picture: TGraphic; const Buttons: array of string; const Results: array of integer; const HelpCtx: longint; const Center: TDlgCenterKind; const Timeout, DefaultButton, CancelButton, HelpButton: integer; const ADynControlEngine: TJvDynControlEngine): integer;
var
  DSAItem: TDSARegItem;
  CheckCaption: string;
  Temp: string;
begin
  if not GetDSAState(DlgID, Result) then
  begin
    Result  := High(integer);
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
    with CreateDSAMessageForm(Caption, Msg, Picture, Buttons, Results, HelpCtx, CheckCaption, Center, Timeout, DefaultButton, CancelButton, HelpButton, ADynControlEngine) do
      try
        Result := ShowModal;
        if IsDSAChecked then
          SetDSAState(DlgID, true, Result);
      finally
        Free;
      end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// DSA registration
//--------------------------------------------------------------------------------------------------

procedure RegisterDSA(const DlgID: integer; const Name, Description: string; const Storage: TDSAStorage; const CheckTextKind: TDSACheckTextKind = ctkShow);
begin
  case DSARegister.Add(DlgID, Name, Description, Storage, CheckTextKind) of
    arDuplicateID:
      raise EJvDSADialog.CreateFmt(RsEDSADuplicateID, [DlgID]);
    arDuplicateName:
      raise EJvDSADialog.CreateFmt(RsEDSADuplicateName, [Name]);
  end;
end;

procedure UnregisterDSA(const DlgID: integer);
begin
  DSARegister.Delete(DlgID);
end;

function LocateDSAReg(const DlgID: integer): TDSARegItem;
begin
  Result := DSARegister.Locate(DlgID);
end;

//--------------------------------------------------------------------------------------------------
// DSA state setting/retrieving
//--------------------------------------------------------------------------------------------------

function GetDSAState(const DlgID: integer): boolean;
var
  Dummy: integer;
begin
  Result := GetDSAState(DlgID, Dummy);
end;

function GetDSAState(const DlgID: integer; out ResCode: integer; const OnCustomData: TDSACustomData = nil): boolean;
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    Result := Regitem.Storage.GetState(RegItem, ResCode, OnCustomData)
  else
    raise EJvDSADialog.CreateFmt(RsEDSADialogIDNotFound, [DlgID]);
end;

procedure SetDSAState(const DlgID: integer; const DontShowAgain: boolean; const LastResult: integer = mrNone; const OnCustomData: TDSACustomData = nil);
var
  RegItem: TDSARegItem;
begin
  RegItem := DSARegister.Locate(DlgID);
  if RegItem.ID <> EmptyItem.ID then
    RegItem.Storage.SetState(RegItem, DontShowAgain, LastResult, OnCustomData)
  else
    raise EJvDSADialog.CreateFmt(RsEDSADialogIDNotFound, [DlgID]);
end;

//--------------------------------------------------------------------------------------------------
// Iterating the DSA registration
//--------------------------------------------------------------------------------------------------

function DSACount: integer;
begin
  Result := Length(DSARegister.FList);
end;

function DSAItem(const Index: integer): TDSARegItem;
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
    raise EJvDSADialog.CreateFmt(RsEDSADuplicateCTK_ID, [ID]);
end;

procedure UnregisterDSACheckMarkText(const ID: TDSACheckTextKind);
var
  Idx: integer;
begin
  Idx := CheckMarkTexts.IndexOfObject(TObject(ID));
  if Idx > -1 then
    CheckMarkTexts.Delete(Idx);
end;

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

var
  GQueueStore: TDSAStorage;

function DSAQueueStore: TDSAQueueStorage;
begin
  if GQueueStore = nil then
    GQueueStore := TDSAQueueStorage.Create;
  Result := TDSAQueueStorage(GQueueStore);
end;

{ ShowModal patch }

procedure CallShowModal(const Frm: TCustomForm); // Helper to get the VMT index of ShowModal
begin
  Frm.ShowModal;
end;

function FindShowModalVMT: integer; //  Locate the VMT index of ShowModal
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
    Inc(integer(Ptr), 4);
    Result := PInteger(Ptr)^ div 4;
  end;
end;

procedure SetVirtualMethodInstance(Instance: TObject; const VMTIdx: integer; const MethodPtr: Pointer);
// Set the virtual method pointer for an instance, nice addition for JCL?
var
  OldProt: cardinal;
begin
  VirtualProtect(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer)), SizeOf(Pointer), PAGE_READWRITE, OldProt);
  try
    PInteger(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer))) ^ := integer(MethodPtr);
  finally
    VirtualProtect(Pointer(PInteger(Instance)^ + VMTIdx * SizeOf(Pointer)), SizeOf(Pointer), OldProt, OldProt);
  end;
end;

//=== TPatchedForm ===========================================================

type
  TShowModalMethod = function: integer of object; // So we can call the original ShowModal method.

  TPatchedForm = class (TCustomForm) // To replace the orignal ShowModal method.
  public
    function ShowModal: integer; override;
  end;

function TPatchedForm.ShowModal: integer;
var
  I:    integer;
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
    raise EJvDSADialog.Create(RsEJvDSADialogPatchErrorJvDSADialogCom);
  JvDSADialog := Components[I] as TJvDSADialog;

  // Check the DSA state
  if not JvDSADialog.GetDSAStateInternal(Result) then
  begin
    if (JvDSADialog.CheckControl <> nil) and not JvDSADialog.IgnoreDSAChkMrkTxt then
    begin
      // Get DSA checkmark caption
      DSAItem      := LocateDSAReg(JvDSADIalog.DialogID);
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

    { Notify the JvDSADialog component that we are about to show the form (may initialize the
      auto-close timer) }
    JvDSADialog.BeforeShow;
    // Show the dialog by calling the original ShowModal method: setting up the method pointers.
    TMethod(ShowModalMethod).Data := Self;
    TMethod(ShowModalMethod).Code := JvDSADialog.GetOrgShowModalPtr;
    // Show the dialog by calling the original ShowModal method: make the actual call.
    Result := ShowModalMethod;
    { Notify the JvDSADialog component that we the form has closed (may clean up the
      auto-close timer) }
    JvDSADialog.AfterShow;
    // Update the DSA state in storage.
    JvDSADialog.UpdateDSAState;
  end
  else
    // The dialog is suppressed. Apply the saved state.
    JvDSADialog.ApplySavedState;
end;

//=== TJvDSADialog ===========================================================

constructor TJvDSADialog.Create(AOwner: TComponent);
var
  I: integer;
begin
  if AOwner is TCustomForm then
  begin
    I := AOwner.ComponentCount - 1;
    while (I > -1) and not (AOwner.Components[I] is TJvDSADialog) do
      Dec(I);
    if I > -1 then
      raise EJvDSADialog.Create(RsEAlreadyDSADialog);
    inherited Create(AOwner);
  end
  else
    raise EJvDSADialog.Create(RsEOnlyAllowedOnForms);
end;

destructor TJvDSADialog.Destroy;
begin
  FormUnPatch;
  inherited Destroy;
end;

procedure TJvDSADialog.AutoClose;
begin
  CancelCountdown;
  if not DoAutoClose then
    (Owner as TCustomForm).Close;
end;

procedure TJvDSADialog.AfterShow;
begin
  if FTimer <> nil then
    FreeAndNil(FTimer);
end;

procedure TJvDSADialog.ApplySavedState;
var
  ResCode: integer;
begin
  GetDSAState(DialogID, ResCode, DoApplyKeys);
  TCustomForm(Owner).ModalResult := ResCode;
end;

procedure TJvDSADialog.BeforeShow;
begin
  if FTimeout > 0 then
  begin
    FTimer      := TTimer.Create(Self);
    FTimer.Enabled := false;
    FTimer.Interval := 1000;
    FTimer.OnTimer := TimerEvent;
    FTimerCount := FTimeout;
  end;
end;

procedure TJvDSADialog.DoApplyKeys(const Storage: TDSAStorage; const DSAInfo: TDSARegItem);
begin
  if Assigned(FOnApplyKeys) then
    OnApplyKeys(Self, DSAInfo, Storage);
end;

function TJvDSADialog.DoAutoClose: boolean;
begin
  Result := false;
  if Assigned(FOnAutoClose) then
    FOnAutoClose(Self, Result);
end;

procedure TJvDSADialog.DoCountDown;
begin
  if Assigned(FOnCountdown) then
    OnCountdown(Self);
end;

procedure TJvDSADialog.DoUpdateKeys;
begin
  if Assigned(FOnUpdateKeys) then
    OnUpdateKeys(Self, DSAInfo, Storage);
end;

function TJvDSADialog.GetDSAStateInternal(out ModalResult: integer): boolean;
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
  VMTIdx: integer;
begin
  VMTIdx := FindShowModalVMT;
  SetOrgShowModalPtr(GetVirtualMethod(Owner.ClassType, VMTIdx));
  SetOrgOwner(Owner);
  SetVirtualMethodInstance(Owner, VMTIdx, @TPatchedForm.ShowModal);
end;

procedure TJvDSADialog.FormUnPatch;
var
  VMTIdx: integer;
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
        raise EJvDSADialog.Create(RsECtrlHasNoCheckedProp);
      if GetPropInfo(Value, 'Caption') = nil then
        raise EJvDSADialog.Create(RsECtrlHasNoCaptionProp);
    end;
    FCheckControl := Value;
  end;
end;

procedure TJvDSADialog.SetDialogID(Value: integer);
begin
  if Value <> DialogID then
  begin
    if not (csDesigning in ComponentState) and not (csLoading in Owner.ComponentState) then
      raise EJvDSADialog.Create(RsEDialogIDChangeOnlyInDesign);
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

procedure TJvDSADialog.TimerEvent(Sender: TObject);
begin
  Dec(FTimerCount);
  if FTimerCount = 0 then
    AutoClose
  else
    DoCountdown;
end;

procedure TJvDSADialog.UpdateDSAState;
begin
  SetDSAState(DialogID, IsDSAChecked, TCustomForm(Owner).ModalResult, DoUpdateKeys);
end;

function TJvDSADialog.GetModalResult: integer;
begin
  Result := TCustomForm(Owner).ModalResult;
end;

function TJvDSADialog.IsDSAChecked: boolean;
begin
  if CheckControl <> nil then
    Result := GetOrdProp(CheckControl, 'Checked') <> 0
  else
    Result := false;
end;

procedure TJvDSADialog.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    FormPatch;
end;

procedure TJvDSADialog.CancelCountdown;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := false;
    FreeAndNil(FTimer);
  end;
end;

function TJvDSADialog.SecondsLeft: integer;
begin
  if Timeout <> 0 then
    Result := FTimerCount
  else
    Result := 0;
end;

initialization
  DSARegister := TDSARegister.Create;
  RegisterDSACheckMarkText(ctkShow, RsDSActkShowText);
  RegisterDSACheckMarkText(ctkAsk, RsDSActkAskText);
  RegisterDSACheckMarkText(ctkWarn, RsDSActkWarnText);

finalization
  DSARegister.Free;
  if GRegStore <> nil then
    FreeAndNil(GRegStore);
  if GQueueStore <> nil then
    FreeAndNil(GQueueStore);
  if GCheckMarkTexts <> nil then
    FreeAndNil(GCheckMarkTexts);

end.
