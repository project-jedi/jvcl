{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCL3Install.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JVCL3Install;

interface

uses
  Windows, ShellAPI, SysUtils, Classes, Core, Graphics, Controls, Forms,
  StdCtrls, Dialogs,
  DelphiData, JVCLData, Utils,
  JvConsts;

type
  TInstallType = (itFreshInstall, itUpdate, itUninstall);

  TInstaller = class(TInterfacedObject, IInstaller)
  private
    FWelcomePage: IWelcomePage;
    FPackageInstaller: IPackageInstaller;

    FInstallType: TInstallType;
    FData: TJVCLData;
    FSelTargets: TList;

    function GetJclDir: WideString;
    procedure SetJCLDir(const Value: WideString);
    function GetJVCLDir: string;
    function GetSelTargetCount: Integer;
    function GetSelTargets(Index: Integer): TTargetConfig;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(APackageInstaller: IPackageInstaller);
    function InstallerName: WideString;
    function FirstPage: IInstallerPage;
    function AutoInstall: Boolean;

    function CanInstall: Boolean;
    procedure Finish;

    procedure SelTargetsClear;
    procedure SelTargetsAdd(TargetConfig: TTargetConfig);

    property PackageInstaller: IPackageInstaller read FPackageInstaller;
    property InstallType: TInstallType read FInstallType write FInstallType;
    property JCLDir: WideString read GetJclDir write SetJCLDir;
    property JVCLDir: string read GetJVCLDir;

    property SelTargetCount: Integer read GetSelTargetCount;
    property SelTargets[Index: Integer]: TTargetConfig read GetSelTargets;

    property Data: TJVCLData read FData;
  public
    procedure DoHomepageClick(Sender: TObject);
  end;

  { Base class for all installer pages }
  TInstallerPage = class(TInterfacedObject, IInstallerPage)
  private
    FInstaller: TInstaller;
    FNext: IInstallerPage;
  public
    constructor Create(AInstaller: TInstaller);
    procedure Init; virtual;
    property Installer: TInstaller read FInstaller;
  public
    { IInstallerPage }
    procedure Title(var Title, SubTitle: WideString); virtual;
    function CanNext: Boolean; virtual;
    function CanPrev: Boolean; virtual;
    function NextPage: IInstallerPage; virtual; abstract;
    procedure Action; virtual;
  end;

  { Welcome page that shows a text field and the install types }
  TWelcomePage = class(TInstallerPage, IWelcomePage, IUserDefinedPage)
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title, SubTitle: WideString); override;
  public
    { IWelcomePage }
    function Text: WideString;
    procedure Options(Options: TStrings; var HorzOrientation: THorzOrientation);
    procedure SetSelectedOption(Index: Integer);
    function GetSelectedOption: Integer;
    procedure SetupRadioButton(Index: Integer; Control: TRadioButton);
    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl;
  end;

implementation

uses
  PgIDESelection, CmdLineUtils,
  {$IFDEF USE_DXGETTEXT}
  JvGnugettext,
  {$ENDIF USE_DXGETTEXT}
  InstallerConsts;

var
  WelcomeText: string;

{ TInstaller }

function TInstaller.InstallerName: WideString;
begin
  Result := RsInstallerName;
end;

function TInstaller.FirstPage: IInstallerPage;
begin
  if not Assigned(FWelcomePage) then
    FWelcomePage := TWelcomePage.Create(Self);
  Result := FWelcomePage;
end;

procedure TInstaller.Init(APackageInstaller: IPackageInstaller);
begin
  FPackageInstaller := APackageInstaller;

  InstallType := itFreshInstall;
  if Data.IsJVCLInstalledAnywhere(3) then
    InstallType := itUpdate
  else
    CmdOptions.AutoUpdate := False; // auto update not possible
end;

constructor TInstaller.Create;
begin
  inherited Create;
  FData := TJVCLData.Create;
  FSelTargets := TList.Create;
end;

destructor TInstaller.Destroy;
begin
  FSelTargets.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TInstaller.Finish;
begin
  // do nothing
end;

function TInstaller.GetJclDir: WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Data.Targets.Count - 1 do
    if DirectoryExists(Data.TargetConfig[i].JCLDir) then
    begin
      Result := Data.TargetConfig[i].JCLDir;
      Exit;
    end;
  Result := Format(sJclRootDirFromJVCLDir, [ExtractFileDir(JVCLDir)]);
  if not DirectoryExists(Result) then
    Result := ''
  else
    Result := Format(sJclRootDirName, [ExtractFileDir(JVCLDir)]);
end;

procedure TInstaller.SetJCLDir(const Value: WideString);
var
  I: Integer;
begin
  for I := 0 to Data.Targets.Count - 1 do
    Data.TargetConfig[I].JCLDir := Value;
end;

function TInstaller.GetJVCLDir: string;
begin
  Result := Data.JVCLDir;
end;

function TInstaller.CanInstall: Boolean;
begin
  Result := False;
  if Data.Targets.Count = 0 then
    MessageDlg(RsNoDelphiBcbInstalled, mtInformation, [mbOk], 0)
  else if (FindWindow('TAppBuilder', nil) <> 0) and (not CmdOptions.IgnoreIDE) then // do not localize
    MessageDlg(RsDelphiBcbRunning, mtInformation, [mbOk], 0)
  else
    Result := True;
end;

procedure TInstaller.DoHomepageClick(Sender: TObject);
var
  S: string;
  ps: Integer;
begin
  S := TWinControl(Sender).Hint;
  ps := Pos('|', S);
  if ps <> 0 then
    Delete(S, 1, ps);
  ShellExecute(Application.Handle, 'open', PChar(S), nil, nil, SW_SHOWNORMAL); // do not localize
end;

function TInstaller.GetSelTargetCount: Integer;
begin
  Result := FSelTargets.Count;
end;

function TInstaller.GetSelTargets(Index: Integer): TTargetConfig;
begin
  Result := TTargetConfig(FSelTargets[Index]);
end;

procedure TInstaller.SelTargetsAdd(TargetConfig: TTargetConfig);
begin
  FSelTargets.Add(TargetConfig);
end;

procedure TInstaller.SelTargetsClear;
begin
  FSelTargets.Clear;
end;

function TInstaller.AutoInstall: Boolean;
begin
  Result := CmdOptions.AutoUpdate;
end;

{ TInstallerPage }

constructor TInstallerPage.Create(AInstaller: TInstaller);
begin
  inherited Create;
  FInstaller := AInstaller;
  Init;
end;

function TInstallerPage.CanNext: Boolean;
begin
  Result := True;
end;

function TInstallerPage.CanPrev: Boolean;
begin
  Result := True;
end;

procedure TInstallerPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsInstallerTitle;
  SubTitle := '';
end;

procedure TInstallerPage.Init;
begin
  // do nothing
end;

procedure TInstallerPage.Action;
begin
 // do nothing
end;

{ TWelcomePage }

function TWelcomePage.GetSelectedOption: Integer;
begin
  Result := Integer(Installer.InstallType);
end;

function TWelcomePage.NextPage: IInstallerPage;
begin
  if FNext = nil then
    FNext := TIDESelectionPage.Create(Installer);
  Result := FNext;
end;

procedure TWelcomePage.Options(Options: TStrings; var HorzOrientation: THorzOrientation);
begin
  Options.Add(RsInstallMode);

  if Installer.Data.IsJVCLInstalledAnywhere(3) then
    Options.Add(RsUpdateMode)
  else
    Options.Add('');

  Options.Add('');
  if Installer.Data.IsJVCLInstalledAnywhere(1) then
    Options.Add(RsUninstallMode)
  else
    Options.Add('');
end;

procedure TWelcomePage.SetSelectedOption(Index: Integer);
begin
  case Index of
    0: Installer.InstallType := itFreshInstall;
    1: Installer.InstallType := itUpdate;
    3: Installer.InstallType := itUninstall;
  end;
  PackageInstaller.UpdatePages;
end;

procedure TWelcomePage.SetupRadioButton(Index: Integer; Control: TRadioButton);
begin
end;

function TWelcomePage.Text: WideString;
var
  Lines: TStrings;
  Filename: string;
begin
  if WelcomeText = '' then
  begin
    Lines := TStringList.Create;
    try
      Filename := Format(sWelcomeFilename, [Installer.JVCLDir]);
      if FileExists(Filename) then
      begin
        Lines.LoadFromFile(Filename);
        WelcomeText := Trim(Lines.Text);
//        Delete(WelcomeText, Length(WelcomeText) - 1, 2);
      end;
      if WelcomeText = '' then
        WelcomeText := LoadLongResString(@SWelcomeText); // just in case the welcome text gets longer than 1024 characters

      {$IFDEF USE_DXGETTEXT}
      WelcomeText := dgettext('JVCLInstall', WelcomeText);
      {$ENDIF USE_DXGETTEXT}
    finally
      Lines.Free;
    end;
  end;
  Result := WelcomeText;
end;

function TWelcomePage.SetupPage(Client: TWinControl): TWinControl;
var
  Btn: TButton;
  Canvas: TControlCanvas;
begin
  Result := nil;
  Btn := TButton.Create(Client);

  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := Client;
    Btn.Width := Canvas.TextWidth(RsShowMPL) + 16;
  finally
    Canvas.Free;
  end;

  Btn.Left := Client.Width - Btn.Width - 8;
  Btn.Top := TWinControl(Client.FindComponent('piPageMemo')).BoundsRect.Bottom + 8; // do not localize
  Btn.Parent := Client;
  Btn.Caption := RsShowMPL;
  Btn.Hint := '|' + Installer.JVCLDir + PathDelim + sMPLFile;
  Btn.OnClick := Installer.DoHomepageClick;
end;

procedure TWelcomePage.Title(var Title, SubTitle: WideString);
begin
  inherited Title(Title, SubTitle);
  SubTitle := RsWelcomePageSubTitle;
end;

initialization
  PackageInstaller := TPackageInstaller.Create(TInstaller.Create);

end.
