{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJVCLAboutForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Michael Beck [mbeck att bigfoot dott com]
Portions created by Michael Beck are Copyright (C) 2002 Michael Beck
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQJVCLAboutForm;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  JclWin32,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, IniFiles,
  
  
  QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QDialogs, QButtons, QWindows,
  
  JclSysInfo,
  JVCLXVer, JvQBaseDlg, JvQComponent;

type
  TJvJVCLAboutForm = class(TJvForm)
    Bevel1: TBevel;
    lblVersion: TLabel;
    pnlImage: TPanel;
    imgStarfield: TImage;
    btnOK: TButton;
    JvHotLink1: TLabel;
    JvHotLink4: TLabel;
    lblNews: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lblCopyRight: TLabel;
    lblRights: TLabel;
    imgProjectJEDI: TImage;
    MainPanel: TPanel;
    Bevel2: TBevel;
    lblVisitJedi: TLabel;
    lblMailingList: TLabel;
    lblNewsgroup: TLabel;
    lblJvHotLink2: TLabel;
    lblBugs: TLabel;
    lblBugsURL: TLabel;
    btnHelp: TSpeedButton;
    btnOptions: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Bevel3: TBevel;
    lblWindowsVersion: TLabel;
    Label4: TLabel;
    lblMemory: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnHelpClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenURLClick(Sender: TObject);
  private
    FHelpFile: string;
    FHelpDirectory: string;
  public
    procedure LoadOptions;
    procedure SaveOptions;
    class procedure Execute(StoreSettings: Boolean);
  end;

  TJvJVCLAboutComponent = class(TJvCommonDialogP)
  private
    FStoreSettings: Boolean;
  public
    procedure Execute; override;
  published
    property StoreSettings: Boolean read FStoreSettings write FStoreSettings default false;
  end;

implementation

uses
  JvQJVCLUtils, JvQJCLUtils;




{$R *.xfm}



const
  cOptions = 'Options';
  cBoundsLeft = 'Bounds.Left';
  cBoundsTop = 'Bounds.Top';
  cHelpFile = 'Help.File';
  cHelpDirectory = 'Help.Directory';
  {$IFDEF MSWINDOWS}
  cJVCLIni = '\JVCL.ini';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  cJVCLIni = '/.JVCL';
  {$ENDIF LINUX}

procedure TJvJVCLAboutForm.FormShow(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  VersionInfo: TOSVersionInfoEx;
{$ENDIF MSWINDOWS}
begin
  lblVersion.Caption := 'Version: ' + JVCL_VERSIONSTRING;
  {$IFDEF MSWINDOWS}
  FillChar(VersionInfo, SizeOf(TOSVersionInfoEx), #0);
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  JclWin32.GetVersionEx(@VersionInfo);
  if VersionInfo.wServicePackMajor = 0 then
    lblWindowsVersion.Caption := Format('%s (Build %u)',
      [GetWindowsVersionString, VersionInfo.dwBuildNumber])
  else
    lblWindowsVersion.Caption := Format('%s (Build %u: %s)',
      [GetWindowsVersionString, VersionInfo.dwBuildNumber, GetWindowsServicePackVersionString]);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  lblWindowsVersion.Caption := GetOSVersionString;
  Label4.Caption := 'Memory Available to Linux:';
  {$ENDIF LINUX}
  lblMemory.Caption := Format('%u KB', [GetTotalPhysicalMemory div 1024]);
  lblCopyRight.Caption := 'Copyright © Project JEDI, 1999 - ' + FormatDateTime('yyyy', Now);
//  LoadOptions;
  btnHelp.Enabled := FHelpFile <> '';
end;

procedure TJvJVCLAboutForm.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  
end;

procedure TJvJVCLAboutForm.btnHelpClick(Sender: TObject);
begin
  Exec(FHelpFile, '', FHelpDirectory);
  Close;
end;

procedure TJvJVCLAboutForm.btnOptionsClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FHelpFile := ExtractFileName(OpenDialog1.FileName);
    FHelpDirectory := ExtractFileDir(OpenDialog1.FileName);
//    SaveOptions;
    btnHelp.Enabled := FHelpFile <> '';
  end;
end;

procedure TJvJVCLAboutForm.LoadOptions;
var
  L, T: Integer;
begin
  {$IFDEF MSWINDOWS}
  with TIniFile.Create(ExtractFileDir(Application.ExeName) + cJVCLIni) do
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  with TIniFile.Create(GetEnvironmentVariable('HOME') + cJVCLIni) do
  {$ENDIF LINUX}
  try
    L := ReadInteger(cOptions, cBoundsLeft, -1);
    T := ReadInteger(cOptions, cBoundsTop, -1);

    FHelpFile := ReadString(cOptions, cHelpFile, '');
    FHelpDirectory := ReadString(cOptions, cHelpDirectory, '');
  finally
    Free;
  end;

  //make sure the form is positioned on screen ...
  //(ie make sure nobody's fiddled with the INI file!)
  if (L >= 0) and (T >= 0) and (L < Screen.Width) and (T < Screen.Height) then
  begin
    Left := L;
    Top := T;
  end;
end;

procedure TJvJVCLAboutForm.SaveOptions;
begin
  {$IFDEF MSWINDOWS}
  with TIniFile.Create(ExtractFileDir(Application.ExeName) + cJVCLIni) do
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  with TIniFile.Create(GetEnvironmentVariable('HOME') + cJVCLIni) do
  {$ENDIF LINUX}
  try
    if WindowState = wsNormal then
    begin
      WriteInteger(cOptions, cBoundsLeft, Left);
      WriteInteger(cOptions, cBoundsTop, Top);
    end;
    WriteString(cOptions, cHelpFile, FHelpFile);
    WriteString(cOptions, cHelpDirectory, FHelpDirectory);
  finally
    Free;
  end;
end;

procedure TJvJVCLAboutForm.FormDestroy(Sender: TObject);
begin
//  SaveOptions;
end;

procedure TJvJVCLAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TJvJVCLAboutComponent.Execute;
begin
  TJvJVCLAboutForm.Execute(StoreSettings);
end;

class procedure TJvJVCLAboutForm.Execute(StoreSettings: Boolean);
begin
  with Self.Create(Application) do
  try
    if StoreSettings then
      LoadOptions;
    // (rom) used as component outside the IDE the buttons are not useful
    btnHelp.Visible := StoreSettings;
    btnOptions.Visible := StoreSettings;
    ShowModal;
    if StoreSettings then
      SaveOptions;
  finally
    Free;
  end;
end;

procedure TJvJVCLAboutForm.OpenURLClick(Sender: TObject);
begin
  OpenObject((Sender as TLabel).Caption);
end;

end.

