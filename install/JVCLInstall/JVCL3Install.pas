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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JVCL3Install;

interface

uses
  Windows, ShellAPI, SysUtils, Classes, Core, Graphics, Controls, Forms,
  StdCtrls, Dialogs,
  DelphiData, JVCLData, Utils;

type
  TInstallType = (itFreshInstall, itUpdate, itUninstall);

  TInstallerOptions = class(TObject)
  private
    FIgnoreIDE: Boolean;
  public
    constructor Create;

    property IgnoreIDE: Boolean read FIgnoreIDE;
  end;

  TInstaller = class(TInterfacedObject, IInstaller)
  private
    FWelcomePage: IWelcomePage;
    FPackageInstaller: IPackageInstaller;

    FInstallType: TInstallType;
    FData: TJVCLData;
    FSelTargets: TList;
    FInstallerOptions: TInstallerOptions;

    function GetJclDir: string;
    procedure SetJCLDir(const Value: string);
    function GetJVCLDir: string;
    function GetSelTargetCount: Integer;
    function GetSelTargets(Index: Integer): TTargetConfig;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(APackageInstaller: IPackageInstaller);
    function InstallerName: WideString;
    function FirstPage: IInstallerPage;

    function CanInstall: Boolean;
    procedure Finish;

    procedure SelTargetsClear;
    procedure SelTargetsAdd(TargetConfig: TTargetConfig);

    property PackageInstaller: IPackageInstaller read FPackageInstaller;
    property InstallType: TInstallType read FInstallType write FInstallType;
    property JCLDir: string read GetJclDir write SetJCLDir;
    property JVCLDir: string read GetJVCLDir;

    property SelTargetCount: Integer read GetSelTargetCount;
    property SelTargets[Index: Integer]: TTargetConfig read GetSelTargets;

    property Data: TJVCLData read FData;
    property InstallerOptions: TInstallerOptions read FInstallerOptions;
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
  TWelcomePage = class(TInstallerPage, IWelcomePage)
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
  end;

implementation

uses
  PgIDESelection;

resourcestring
  SWelcomeText =
    'The JEDI Visual Component Library (JVCL) consists of a large collection (currently ca. 500)'#10 +
    'visual and non-visual components which can be instantly reused in your Delphi, Kylix and'#10 +
    'C++ Builder projects.'#10 +
    ''#10 +
    'The library is built upon code donated from the JEDI community. It is reformatted to achieve'#10 +
    'a common look-and-feel, tested, documented and merged into the library. The library is grouped'#10 +
    'into several categories such as Enhanced Standard, Visual, Non-Visual, Data Aware and many,'#10 +
    'many more. The library is released to the public under the terms of the Mozilla Public License'#10 +
    '(MPL) and as such can be freely used in both freeware, shareware, open source and commercial'#10 +
    'projects.'#10 +
    ''#10 +
    'Source code files included in the JVCL have a header which explicitly states the license (as'#10 +
    'is required). However, unless noted otherwise, all files, including those without an MPL'#10 +
    'header, are subject to the MPL license.';

var
  WelcomeText: string;
  
{ TInstaller }

function TInstaller.InstallerName: WideString;
begin
  Result := 'JVCL 3  Installation';
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
    InstallType := itUpdate;
end;

constructor TInstaller.Create;
begin
  inherited Create;
  FData := TJVCLData.Create;
  FSelTargets := TList.Create;
  FInstallerOptions := TInstallerOptions.Create;
end;

destructor TInstaller.Destroy;
begin
  FSelTargets.Free;
  FInstallerOptions.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TInstaller.Finish;
begin
  // do nothing
end;

function TInstaller.GetJclDir: string;
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
  Result := ExtractFilePath(JVCLDir) + 'Jcl\Source\Common';
  if not DirectoryExists(Result) then
    Result := ''
  else
    Result := ExtractFilePath(JVCLDir) + 'Jcl';
end;

procedure TInstaller.SetJCLDir(const Value: string);
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
    MessageDlg('No Delphi or BCB is installed. The installer terminates.', mtInformation, [mbOk], 0)
  else if (FindWindow('TAppBuilder', nil) <> 0) and (not InstallerOptions.IgnoreIDE) then
    MessageDlg('Delphi or BCB is running. Terminate the IDE and restart the installer.', mtInformation, [mbOk], 0)
  else
    Result := True;
end;

procedure TInstaller.DoHomepageClick(Sender: TObject);
var
  S: string;
  ps: Integer;
begin
  S := TLabel(Sender).Hint;
  ps := Pos('|', S);
  if ps <> 0 then
    Delete(S, 1, ps);
  ShellExecute(Application.Handle, 'open', PChar(S), nil, nil, SW_SHOWNORMAL);
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
  Title := 'JVCL 3 Installation';
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
  Options.Add('New installation / Upgrade from an older version|');

  if Installer.Data.IsJVCLInstalledAnywhere(3) then
    Options.Add('Compile already installed packages / Update IDE|')
  else
    Options.Add('');

  Options.Add('');

{  if Installer.Data.IsJVCLInstalledAnywhere(1) then
    Options.Add('Uninstall JVCL 3')
  else}
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
begin
  if WelcomeText = '' then
  begin
    Lines := TStringList.Create;
    try
      if FileExists(Installer.JVCLDir + '\Install\JVCLInstall\welcome.txt') then
      begin
        Lines.LoadFromFile(Installer.JVCLDir + '\Install\JVCLInstall\welcome.txt');
        WelcomeText := Lines.Text;
        Delete(WelcomeText, Length(WelcomeText) - 1, 2);
      end
      else
        WelcomeText := SWelcomeText;
    finally
      Lines.Free;
    end;
  end;
  Result := SWelcomeText;
end;

procedure TWelcomePage.Title(var Title, SubTitle: WideString);
begin
  inherited Title(Title, SubTitle);
  SubTitle := 'Welcome to the JVCL 3 installation program.';
end;

{ TInstallerOptions }

constructor TInstallerOptions.Create;
var
  i: Integer;
  S: string;
begin
  for i := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S[1] = '-' then
    begin
      if StartsWith('--ignore-ide', S, True) then
        FIgnoreIDE := True;
    end;
  end;
end;

initialization
  PackageInstaller := TPackageInstaller.Create(TInstaller.Create);

end.
