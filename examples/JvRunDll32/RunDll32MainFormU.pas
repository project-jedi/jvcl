{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$I jvcl.inc}

unit RunDll32MainFormU;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TRunDll32MainForm = class(TForm)
    edModule: TEdit;
    edFunc: TEdit;
    edCmdLine: TEdit;
    lblModule: TLabel;
    lblFunc: TLabel;
    lblCmdLine: TLabel;	
    chkWait: TCheckBox;
    btnBrowse: TButton;
    btnRun: TButton;
    Label1: TLabel;
    btnInfo: TButton;
    btnInternal: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure edModuleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInternalClick(Sender: TObject);
  end;

var
  RunDll32MainForm: TRunDll32MainForm;

implementation
uses
  JvJCLUtils, ShellAPI, InfoFrm;

{$R *.dfm}
{
RunDLL32.EXE shell32.dll,Control_FillCache_RunDLL
RunDLL32.EXE shell32.dll,Control_RunDLL

RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,3
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,4
RunDLL32.EXE shell32.dll,Control_RunDLL access.cpl,,5

RunDLL32.EXE shell32.dll,Control_RunDLL ports.cpl

RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL appwiz.cpl,,3

RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,-1
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,0
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,1
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,2
RunDLL32.EXE shell32.dll,Control_RunDLL desk.cpl,,3

RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL Image
RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL Infrared

RunDLL32.EXE shell32.dll,Control_RunDLL odbccp32.cpl

RunDLL32.EXE shell32.dll,Control_RunDLL joy.cpl
RunDLL32.EXE shell32.dll,Control_RunDLL sysdm.cpl @1
RunDLL32.EXE shell32.dll,Control_RunDLL findfast.cpl

RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL <DeviceID>
RunDLL32.EXE SysDM.cpl,InstallDevice_RunDLL,,0
RunDLL32.EXE devmgr.dll DeviceManager_Execute
RunDLL32.EXE SHELL32.DLL,OpenAs_RunDLL <FileName>
RunDLL32.EXE SHELL32.DLL,SHHelpShortcuts_RunDLL <FontsFolder>
RunDLL32.EXE shell32.dll,Control_RunDLL main.cpl @3

RunDLL32.EXE SYNCUI.DLL,Briefcase_Create
RunDLL32.EXE syncui.dll,Briefcase_Intro


RunDLL32.EXE USER.DLL,cascadechildwindows
RunDLL32.EXE USER.DLL,tilechildwindows

RunDLL32.EXE RNAUI.DLL,RnaWizard
RunDLL32.EXE RNAUI.DLL,RnaWizard @ 1
RunDLL32.EXE Rnaui.dll,RnaDial <ConnectionName>

RunDLL32.EXE DISKCOPY.DLL,DiskCopyRunDll
RunDLL32.EXE SHELL32.DLL,SHFormatDrive
RunDLL32.EXE USER.DLL,repaintscreen
}

function GetWinSysDir: string;
begin
  SetLength(Result,MAX_PATH+1);
  GetSystemDirectory(PChar(Result), MAX_PATH);
  Result := string(PChar(Result));
end;

function GetExpandedSysFilePath(const S: string): string;
begin
  if ExtractFilePath(S) = '' then
    Result := IncludeTrailingPathDelimiter(GetWinSysDir) + S
  else
    Result := S;
end;

function PipeToFile(Filename: string; Cmd: string; Append: boolean): boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Appnd: string;
  ComSpec: string;
  procedure GetComSpec;
  begin
    SetLength(ComSpec, MAX_PATH);
    FillChar(ComSpec[1], MAX_PATH, #0);
    if GetEnvironmentVariable('COMSPEC', PChar(ComSpec), MAX_PATH) = 0 then
      ComSpec := 'C:\command.com' // best guess ?
    else
      ComSpec := string(PChar(ComSpec));
  end;
begin
  GetComSpec;
  GetStartUpInfo(SI);
  SI.wShowWindow := SW_HIDE;
  if Append then
    Appnd := ' >>'
  else
    Appnd := ' >';
  Cmd := string(PChar(ComSpec)) + ' /C ' + Cmd + Appnd + Filename;
  Result := CreateProcess(nil, PChar(Cmd), nil, nil, false, 0, nil, nil, SI, PI);
  try
    if Result then
    begin
      WaitForInputIdle(PI.hProcess, INFINITE);
      Result := WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_FAILED;
    end;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
  if not Result then
    RaiseLastOSError;
end;

procedure TRunDll32MainForm.btnRunClick(Sender: TObject);
begin
  if not RunDll32(edModule.Text, edFunc.Text, edCmdLine.Text, chkWait.Checked) then
    RaiseLastOSError;
end;

procedure TRunDll32MainForm.btnBrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Modules|*.exe;*.dll;*.ocx;|All files|*.*';
    Filename := edModule.Text;
    if ExtractFilePath(Filename) = '' then
      InitialDir := GetWinSysDir
    else
      InitialDir := ExtractFilePath(Filename);
    if Execute then
      edModule.Text := Filename;
  finally
    Free;
  end;
end;

procedure TRunDll32MainForm.Label1Click(Sender: TObject);
begin
  OpenObject('http://www.dx21.com/scripting/rundll32/');
end;

procedure TRunDll32MainForm.btnInfoClick(Sender: TObject);
var tmp: string;
begin
  tmp := ChangeFileExt(Application.ExeName, '.dmp');
  // NOTE: this requires the $(DELPHI)\bin folder and the tdump.exe utility in the PATH to work!!!
  PipeToFile(tmp, Format('tdump.exe -ee -q %s', [GetExpandedSysFilePath(edModule.Text)]), false);
  TfrmInfo.View(tmp, ExtractFileName(edModule.Text));
end;

procedure TRunDll32MainForm.edModuleChange(Sender: TObject);
begin
  btnInfo.Enabled := FileExists(GetExpandedSysFilePath(edModule.Text));
end;

procedure TRunDll32MainForm.FormCreate(Sender: TObject);
begin
  edModuleChange(Sender);
end;

procedure TRunDll32MainForm.btnInternalClick(Sender: TObject);
var H:Hwnd;
begin
  if chkWait.Checked then
    H := Handle
  else
    H := GetDesktopWindow;
  RunDLL32Internal(H,edModule.Text,edFunc.Text,edCmdLine.Text,SW_SHOWDEFAULT);
end;

end.

