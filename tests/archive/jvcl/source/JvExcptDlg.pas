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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvExcptDlg;


interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvxCtrls;

type
  TErrorEvent = procedure (Error: Exception; var Msg: string) of object;

  TJvErrorDialog = class(TForm)
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
    AddrLabel: TJvxLabel;
    TypeLabel: TJvxLabel;
    BottomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    Details: Boolean;
    DetailsHeight: Integer;
    ExceptObj: Exception;
    FPrevOnException: TExceptionEvent;
    FOnErrorMsg: TErrorEvent;
{$IFDEF WIN32}
    FHelpFile: string;
{$ENDIF}
    procedure GetErrorMsg(var Msg: string);
    procedure ShowError;
    procedure SetShowDetails(Value: Boolean);
{$IFDEF WIN32}
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
{$ENDIF}
  public
    procedure ShowException(Sender: TObject; E: Exception);
    property OnErrorMsg: TErrorEvent read FOnErrorMsg write FOnErrorMsg;
  end;

const
  ErrorDlgHelpCtx: THelpContext = 0;

var
  RxErrorDialog: TJvErrorDialog;

procedure RxErrorIntercept;

implementation

uses
{$IFDEF WIN32}
  Windows, {$IFDEF Delphi3_Up} ComObj, {$ELSE} OleAuto, {$ENDIF Delphi3_Up}
{$ELSE WIN32}
  WinProcs, WinTypes, ToolHelp, JvStr16,
{$ENDIF WIN32}
  Consts, JvCConst, JvStrUtils, JvVCLUtils;

{$R *.DFM}

{$IFDEF Delphi3_Up}
resourcestring
{$ELSE}
const
{$ENDIF}
  SCodeError = '%s.'#13#10'Error Code: %.8x (%1:d).';
  SModuleError = 'Exception in module %s.'#13#10'%s';

const
  CRLF = #13#10;

procedure RxErrorIntercept;
begin
  if RxErrorDialog <> nil then RxErrorDialog.Free;
  RxErrorDialog := TJvErrorDialog.Create(Application);
end;

{ TJvErrorDialog }

procedure TJvErrorDialog.ShowException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.NormalizeTopMosts;
  try
    if Assigned(FPrevOnException) then FPrevOnException(Sender, E)
    else if (ExceptObj = nil) and not Application.Terminated then begin
      ExceptObj := E;
      try
        ShowModal;
      finally
        ExceptObj := nil;
      end;
    end
    else begin
      if NewStyleControls then Application.ShowException(E)
      else MessageDlg(E.Message + '.', mtError, [mbOk], 0);
    end;
  except
    { ignore any exceptions }
  end;
  Application.RestoreTopMosts;
end;

{$IFDEF WIN32}

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX
        JE      @@1
        SUB     EAX, $1000
@@1:
end;

procedure TJvErrorDialog.ErrorInfo(var LogicalAddress: Pointer;
  var ModuleName: string);
var
  Info: TMemoryBasicInformation;
  Temp, ModName: array[0..MAX_PATH] of Char;
begin
  VirtualQuery(ExceptAddr, Info, SizeOf(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp,
    SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    LogicalAddress := ConvertAddr(LogicalAddress);
  end
  else Integer(LogicalAddress) := Integer(LogicalAddress) -
    Integer(Info.AllocationBase);
{$IFDEF Delphi3_Up}
  StrLCopy(ModName, AnsiStrRScan(Temp, '\') + 1, SizeOf(ModName) - 1);
{$ELSE}
  StrLCopy(ModName, StrRScan(Temp, '\') + 1, SizeOf(ModName) - 1);
{$ENDIF}
  ModuleName := StrPas(ModName);
end;

{$ELSE}

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        MOV     AX,Address.Word[0]
        MOV     DX,Address.Word[2]
        MOV     CX,DX
        OR      CX,AX
        JE      @@1
        CMP     DX,0FFFFH
        JE      @@1
        MOV     ES,DX
        MOV     DX,ES:Word[0]
@@1:
end;

procedure TJvErrorDialog.ErrorInfo(var LogicalAddress: Pointer;
  var ModuleName: string);
var
  GlobalEntry: TGlobalEntry;
  hMod: THandle;
  ModName: array[0..15] of Char;
  Buffer: array[0..255] of Char;
begin
  GlobalEntry.dwSize := SizeOf(GlobalEntry);
  if GlobalEntryHandle(@GlobalEntry, THandle(PtrRec(LogicalAddress).Seg)) then
    with GlobalEntry do begin
      hMod := hOwner;
      if wType in [GT_CODE, GT_DATA, GT_DGROUP] then
        PtrRec(LogicalAddress).Seg := wData;
    end
    else LogicalAddress := ConvertAddr(LogicalAddress);
  GetModuleFileName(hMod, Buffer, SizeOf(Buffer));
  StrLCopy(ModName, StrRScan(Buffer, '\') + 1, SizeOf(ModName) - 1);
  ModuleName := StrPas(ModName);
end;

{$ENDIF}

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
  ErrorType.Text := ExceptObj.ClassName;
  TypeLabel.Enabled := ErrorType.Text <> '';
  S := Trim(ExceptObj.Message);
  if Pos(CRLF, S) = 0 then
    S := ReplaceStr(S, #10, CRLF);
  if ExceptObj is EInOutError then
    S := Format(SCodeError, [S, EInOutError(ExceptObj).ErrorCode])
{$IFDEF WIN32}
  else if ExceptObj is EOleException then begin
    with EOleException(ExceptObj) do
      if (Source <> '') and (AnsiCompareText(S, Trim(Source)) <> 0) then
        S := S + CRLF + Trim(Source);
    S := Format(SCodeError, [S, EOleException(ExceptObj).ErrorCode])
  end
  else if ExceptObj is EOleSysError then
    S := Format(SCodeError, [S, EOleSysError(ExceptObj).ErrorCode])
  else if ExceptObj is EExternalException then
    S := Format(SCodeError, [S,
      EExternalException(ExceptObj).ExceptionRecord^.ExceptionCode])
{$ENDIF}
{$IFDEF Delphi3_Up}
  else if ExceptObj is {$IFDEF Delphi6_Up}EOSError{$ELSE}EWin32Error{$ENDIF} then
    S := Format(SCodeError, [S, {$IFDEF Delphi6_Up}EOSError{$ELSE}EWin32Error{$ENDIF}(ExceptObj).ErrorCode])
{$ENDIF}
  else S := S + '.';
  MessageText.Text := Format(SModuleError, [ModuleName, S]);
end;

procedure TJvErrorDialog.SetShowDetails(Value: Boolean);
begin
  DisableAlign;
  try
    if Value then begin
      DetailsPanel.Height := DetailsHeight;
      ClientHeight := DetailsPanel.Height + BasicPanel.Height;
      DetailsBtn.Caption := '<< &' + LoadStr(SDetails);
      ShowError;
    end
    else begin
      ClientHeight := BasicPanel.Height;
      DetailsPanel.Height := 0;
      DetailsBtn.Caption := '&' + LoadStr(SDetails) + ' >>';
    end;
    DetailsPanel.Enabled := Value;
    Details := Value;
  finally
    EnableAlign;
  end;
end;

procedure TJvErrorDialog.GetErrorMsg(var Msg: string);
var
  I: Integer;
begin
  I := Pos(CRLF, Msg);
  if I > 0 then System.Delete(Msg, I, MaxInt);
  if Assigned(FOnErrorMsg) then
    try
      FOnErrorMsg(ExceptObj, Msg);
    except
    end;
end;

{$IFDEF WIN32}
procedure TJvErrorDialog.WMHelp(var Message: TWMHelp);
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
{$ENDIF}

procedure TJvErrorDialog.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  BorderIcons := [biSystemMenu, biHelp];
{$ELSE}
  BorderIcons := [];
{$ENDIF}
  DetailsHeight := DetailsPanel.Height;
  Icon.Handle := LoadIcon(0, IDI_HAND);
  IconImage.Picture.Icon := Icon;
  { Load string resources }
  Caption := ResStr(SMsgDlgError);
  OKBtn.Caption := ResStr(SOKButton);
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
{$IFDEF WIN32}
  ExStyle: Longint;
{$ENDIF}
begin
  if ExceptObj.HelpContext <> 0 then
    HelpContext := ExceptObj.HelpContext
  else HelpContext := ErrorDlgHelpCtx;
{$IFDEF WIN32}
  if ExceptObj is EOleException then
    FHelpFile := EOleException(ExceptObj).HelpFile
  else FHelpFile := '';
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if (HelpContext <> 0) then
    ExStyle := ExStyle or WS_EX_CONTEXTHELP
  else
    ExStyle := ExStyle and not WS_EX_CONTEXTHELP;
  SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
{$ENDIF}
  S := Trim(ExceptObj.Message) + '.';
  GetErrorMsg(S);
  ErrorText.Caption := S;
  SetShowDetails(False);
  DetailsBtn.Enabled := True;
end;

procedure TJvErrorDialog.DetailsBtnClick(Sender: TObject);
begin
  SetShowDetails(not Details);
end;

procedure TJvErrorDialog.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{$IFDEF WIN32}
var
  Info: THelpInfo;
{$ENDIF}
begin
  if (Key = VK_F1) and (HelpContext <> 0) then begin
{$IFDEF WIN32}
    with Info do begin
      cbSize := SizeOf(THelpInfo);
      iContextType := HELPINFO_WINDOW;
      iCtrlId := 0;
      hItemHandle := Handle;
      dwContextId := HelpContext;
      GetCursorPos(MousePos);
    end;
    Perform(WM_HELP, 0, Longint(@Info));
{$ELSE}
    Application.HelpContext(HelpContext);
{$ENDIF}
  end;
end;

initialization
  RxErrorDialog := nil;
end.
