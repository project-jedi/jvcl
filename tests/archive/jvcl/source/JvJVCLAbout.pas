{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJVCLAbout.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Michael Beck [mbeck@bigfoot.com]
Portions created by Michael Beck are Copyright (C) 2002 Michael Beck
All Rights Reserved.

Contributor(s):

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvJVCLAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, IniFiles, StdCtrls, ExtCtrls,
  JclSysInfo, JclWin32, JVCLVer, JvBaseDlg, Dialogs, jpeg, Buttons;

type
  TJvJVCLAboutForm = class(TForm)
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
  JvFunctions;

{$R *.dfm}

const
  cOptions = 'Options';
  cBoundsLeft = 'Bounds.Left';
  cBoundsTop = 'Bounds.Top';
  cHelpFile = 'Help.File';
  cHelpDirectory = 'Help.Directory';
  cJVCLIni = '\JVCL.ini';

procedure TJvJVCLAboutForm.FormShow(Sender: TObject);
var
  VersionInfo: TOSVersionInfoEx;
begin
  FillChar(VersionInfo, SizeOf(TOSVersionInfoEx), #0);
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  JclWin32.GetVersionEx(@VersionInfo);
  lblVersion.Caption := 'Version: ' + JVCL_VERSIONSTRING;
  if VersionInfo.wServicePackMajor = 0 then
    lblWindowsVersion.Caption := Format('%s (Build %u)',
      [GetWindowsVersionString, VersionInfo.dwBuildNumber])
  else
    lblWindowsVersion.Caption := Format('%s (Build %u: %s)',
      [GetWindowsVersionString, VersionInfo.dwBuildNumber, GetWindowsServicePackVersionString]);
  lblMemory.Caption := Format('%u KB', [GetTotalPhysicalMemory div 1024]);
  lblCopyRight.Caption := 'Copyright © Project JEDI, 1999 - ' + FormatDateTime('yyyy', Now);
//  LoadOptions;
  btnHelp.Enabled := FHelpFile <> '';
end;

procedure TJvJVCLAboutForm.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, SC_MOVE + 2, 0);
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
  l, t: Integer;
begin
  with TIniFile.Create(ExtractFileDir(Application.ExeName) + cJVCLIni) do
  try
    l := ReadInteger(cOptions, cBoundsLeft, -1);
    t := ReadInteger(cOptions, cBoundsTop, -1);

    FHelpFile := ReadString(cOptions, cHelpFile, '');
    FHelpDirectory := ReadString(cOptions, cHelpDirectory, '');
  finally
    Free;
  end;

  //make sure the form is positioned on screen ...
  //(ie make sure nobody's fiddled with the INI file!)
  if (l >= 0) and (t >= 0) and (l < Screen.Width) and (t < Screen.Height) then
  begin
    Left := l;
    Top := t;
  end;
end;

procedure TJvJVCLAboutForm.SaveOptions;
begin
  with TIniFile.Create(ExtractFileDir(Application.ExeName) + cJVCLIni) do
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
  with self.Create(Application) do
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

end.

