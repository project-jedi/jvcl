{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExcptDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExceptionForm;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  Messages, ComObj,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  JvLabel, JvComponent;

type
  TJvErrorEvent = procedure(Error: Exception; var Msg: string) of object;

  TJvErrorDialog = class(TJvForm)
    BasicPanel: TPanel;
    ErrorText: TLabel;
    IconPanel: TPanel;
    IconImage: TImage;
    TopPanel: TPanel;
    RightPanel: TPanel;
    DetailsPanel: TPanel;
    MessageText: TMemo;
    ErrorAddress: TEdit;
    ErrorType: TEdit;
    ButtonPanel: TPanel;
    DetailsBtn: TButton;
    OKBtn: TButton;
    AddrLabel: TJvLabel;
    TypeLabel: TJvLabel;
    BottomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDetails: Boolean;
    FDetailsHeight: Integer;
    FExceptObj: Exception;
    FPrevOnException: TExceptionEvent;
    FOnErrorMsg: TJvErrorEvent;
    FHelpFile: string;
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
    procedure GetErrorMsg(var Msg: string);
    procedure ShowError;
    procedure SetShowDetails(Value: Boolean);
  public
    procedure ShowException(Sender: TObject; E: Exception);
    property OnErrorMsg: TJvErrorEvent read FOnErrorMsg write FOnErrorMsg;
  end;

procedure JvErrorIntercept;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Consts,
  JvJCLUtils, JvConsts, JvResources;

{$R *.dfm}

var
  JvErrorDialog: TJvErrorDialog = nil;

procedure JvErrorIntercept;
begin
  JvErrorDialog.Free;
  JvErrorDialog := TJvErrorDialog.Create(Application);
end;

procedure TJvErrorDialog.ShowException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.NormalizeTopMosts;
  try
    if Assigned(FPrevOnException) then
      FPrevOnException(Sender, E)
    else
    if (FExceptObj = nil) and not Application.Terminated then
    begin
      FExceptObj := E;
      try
        ShowModal;
      finally
        FExceptObj := nil;
      end;
    end
    else
    begin
      Application.ShowException(E);
    end;
  except
    { ignore any exceptions }
  end;
  Application.RestoreTopMosts;
end;

// (rom) i see no reason for assembler here

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX
        JE      @@1
        SUB     EAX, $1000
@@1:
end;

{$IFDEF MSWINDOWS}
{$IFNDEF RTL230_UP}
type
  INT_PTR = Integer;
{$ENDIF ~RTL230_UP}
procedure TJvErrorDialog.ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
var
  Info: TMemoryBasicInformation;
  Temp, ModName: array [0..MAX_PATH] of Char;
begin
  VirtualQuery(ExceptAddr, Info, SizeOf(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFileName(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    LogicalAddress := ConvertAddr(LogicalAddress);
  end
  else
    INT_PTR(LogicalAddress) := INT_PTR(LogicalAddress) - INT_PTR(Info.AllocationBase);
  StrLCopy(ModName, AnsiStrRScan(Temp, PathDelim) + 1, SizeOf(ModName) - 1);
  ModuleName := StrPas(ModName);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
procedure TJvErrorDialog.ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
var
  Temp, ModName: array [0..MAX_PATH] of Char;
begin
  GetModuleFileName(HInstance, Temp, SizeOf(Temp));
  LogicalAddress := ConvertAddr(LogicalAddress);
  StrLCopy(ModName, AnsiStrRScan(Temp, PathDelim) + 1, SizeOf(ModName) - 1);
  ModuleName := StrPas(ModName);
end;
{$ENDIF UNIX}

procedure TJvErrorDialog.ShowError;
var
  S, ModuleName: string;
  P: Pointer;
begin
  P := ExceptAddr;
  ModuleName := '';
  ErrorInfo(P, ModuleName);
  AddrLabel.Enabled := (P <> nil);
  ErrorAddress.Text := Format('%p', [ExceptAddr]);
  ErrorType.Text := FExceptObj.ClassName;
  TypeLabel.Enabled := ErrorType.Text <> '';
  S := Trim(FExceptObj.Message);
  if Pos(CrLf, S) = 0 then
    S := ReplaceStr(S, Lf, CrLf);
  if FExceptObj is EInOutError then
    S := Format(RsCodeError, [S, EInOutError(FExceptObj).ErrorCode])
  else
  if FExceptObj is EOleException then
  begin
    with EOleException(FExceptObj) do
      if (Source <> '') and (AnsiCompareText(S, Trim(Source)) <> 0) then
        S := S + CrLf + Trim(Source);
    S := Format(RsCodeError, [S, EOleException(FExceptObj).ErrorCode])
  end
  else
  if FExceptObj is EOleSysError then
    S := Format(RsCodeError, [S, EOleSysError(FExceptObj).ErrorCode])
  else
  if FExceptObj is EExternalException then
    S := Format(RsCodeError, [S,
      EExternalException(FExceptObj).ExceptionRecord^.ExceptionCode])
  else
  if FExceptObj is EOSError then
    S := Format(RsCodeError,
     [S, EOSError(FExceptObj).ErrorCode])
  else
    S := S + '.';
  MessageText.Text := Format(RsModuleError, [ModuleName, S]);
end;

procedure TJvErrorDialog.SetShowDetails(Value: Boolean);
begin
  DisableAlign;
  try
    if Value then
    begin
      DetailsPanel.Height := FDetailsHeight;
      ClientHeight := DetailsPanel.Height + BasicPanel.Height;
      DetailsBtn.Caption := RsDetailsLeftCaption;
      ShowError;
    end
    else
    begin
      ClientHeight := BasicPanel.Height;
      DetailsPanel.Height := 0;
      DetailsBtn.Caption := RsDetailsRightCaption;
    end;
    DetailsPanel.Enabled := Value;
    FDetails := Value;
  finally
    EnableAlign;
  end;
end;

procedure TJvErrorDialog.GetErrorMsg(var Msg: string);
var
  I: Integer;
begin
  I := Pos(CrLf, Msg);
  if I > 0 then
    System.Delete(Msg, I, MaxInt);
  if Assigned(FOnErrorMsg) then
  try
    FOnErrorMsg(FExceptObj, Msg);
  except
  end;
end;


procedure TJvErrorDialog.WMHelp(var Msg: TWMHelp);
var
  AppHelpFile: string;
begin
  AppHelpFile := Application.HelpFile;
  try
    if FHelpFile <> '' then
      Application.HelpFile := FHelpFile;
    inherited;
  finally
    Application.HelpFile := AppHelpFile;
  end;
end;


procedure TJvErrorDialog.FormCreate(Sender: TObject);
begin
  BorderIcons := [biSystemMenu, biHelp];
  FDetailsHeight := DetailsPanel.Height;
  Icon.Handle := LoadIcon(0, IDI_HAND);
  IconImage.Picture.Icon := Icon;
  { Load string resources }
  Caption := SMsgDlgError;
  OKBtn.Caption := SOKButton;
  { Set exception handler }
  FPrevOnException := Application.OnException;
  Application.OnException := ShowException;
end;

procedure TJvErrorDialog.FormDestroy(Sender: TObject);
begin
  Application.OnException := FPrevOnException;
end;

procedure TJvErrorDialog.FormShow(Sender: TObject);
var
  S: string;
  ExStyle: Longint;
begin
  if FExceptObj.HelpContext <> 0 then
    HelpContext := FExceptObj.HelpContext
  else
    HelpContext := THelpContext(0);
  if FExceptObj is EOleException then
    FHelpFile := EOleException(FExceptObj).HelpFile
  else
    FHelpFile := '';
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if HelpContext <> 0 then
    ExStyle := ExStyle or WS_EX_CONTEXTHELP
  else
    ExStyle := ExStyle and not WS_EX_CONTEXTHELP;
  SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
  S := Trim(FExceptObj.Message) + '.';
  GetErrorMsg(S);
  ErrorText.Caption := S;
  SetShowDetails(False);
  DetailsBtn.Enabled := True;
end;

procedure TJvErrorDialog.DetailsBtnClick(Sender: TObject);
begin
  SetShowDetails(not FDetails);
end;

procedure TJvErrorDialog.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
  Info: THelpInfo;

begin

  if (Key = VK_F1) and (HelpContext <> 0) then
  begin
    with Info do
    begin
      cbSize := SizeOf(THelpInfo);
      iContextType := HELPINFO_WINDOW;
      iCtrlId := 0;
      hItemHandle := Handle;
      dwContextId := HelpContext;
      GetCursorPos(MousePos);
    end;
    Perform(WM_HELP, 0, LPARAM(@Info));
  end;

end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
