{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Core.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit Core;

interface

uses
  SysUtils, Classes, Contnrs, Controls, StdCtrls, ExtCtrls, ImgList,
  Windows, ShellAPI, CommCtrl;

type
  THorzOrientation = (hoDefault, hoLeft, hoCenter, hoRight);
  IInstallerPage = interface;
  IInstaller = interface;
  IPackageInstaller = interface;

  { IInstallerPage supports all methods that are necessary for a installer page.
    There is no page builder for an IInstallerPage implementation. So you have
    to implement pages that are derived from IInstallerPage. }
  IInstallerPage = interface
    ['{B3B1AA03-CBA3-448B-B89C-99D7F79181D8}']
    procedure Title(var Title, SubTitle: WideString);
      { Title returns the title and subtitle of the page. No Title and no
        SubTitle means no header. }
    function NextPage: IInstallerPage;
      { NextPage returns the next page that should be displayed. All options are
        garanteed to be transitted before calling NextPage. The installer can
        go back to another page by it's page cache. Return nil if this is
        the very last page. }
    function CanNext: Boolean;
      { CanNext must return False if the next page is available }
    function CanPrev: Boolean;
      { CanPrev must return False if the installer should not allow going
        back. }

    procedure Init;
      { Init is called through PackageInstaller.RebuildPage and by the
        constructor of TInstallerPage. }

    procedure Action;
      { Action is called when the page is visible. This allows auto-action
        pages. }
  end;

  { IMultiChoosePage represents a page that contains only CheckBoxes. }
  IMultiChoosePage = interface(IInstallerPage)
    ['{D331311B-19C2-4B66-942C-A47406C127A8}']
    procedure CheckBoxes(CheckBoxes: TStrings; var HorzOrientation: THorzOrientation);
      { returns a list of check boxes. Format: "Caption|Hint" }
    procedure SetCheckBox(Index: Integer; Value: Boolean);
      { called by the installer to set the check box state. Index is from the
        CheckBoxes list. Return True if the page order has changed due to the
        changes. }
    function GetCheckBox(Index: Integer): Boolean;
      { called by the installer to obtain the check box state. Index is from the
        CheckBoxes list. }
    procedure SetupCheckBox(Index: Integer; Control: TCheckBox);
      { The installer calls SetupCheckBox for every check box. }
  end;

  ISingleChoosePage = interface(IInstallerPage)
    ['{976AE67C-0C9D-4EB3-9250-94FB283EB5E7}']
    procedure Options(Options: TStrings; var HorzOrientation: THorzOrientation);
      { returns a list of radio buttons. Format: "Caption|Hint" }

    procedure SetSelectedOption(Index: Integer);
      { called by the installer to set the active radio button. Return True if
        the page order has changed due to the changes.}
    function GetSelectedOption: Integer;
      { called by the installer to obtain the active radio button. }

    procedure SetupRadioButton(Index: Integer; Control: TRadioButton);
      { The installer calls SetupRadioButton for every radio button. }
  end;

  { IWelcomePage represents a page that has a memo field (2/3 of the page's
    height and an grouped options. If IMultiChoosePage is supported then also
    checkboxes are available. }
  IWelcomePage = interface(ISingleChoosePage)
    ['{9088CC78-0F61-48EC-ABF1-41869EC581CF}']
    function Text: WideString;
      { Text returns the text for the Memo on the page. No text means no Memo}
  end;

  IUserDefinedPage = interface(IInstallerPage)
    ['{20941184-2F88-4054-9420-AFE7309168E7}']
    function SetupPage(Client: TWinControl): TWinControl;
      { The installer calls SetupPage when the page's content should be created.
        Returns the control that should be the active control. }
  end;

  ISummaryPage = interface(IInstallerPage)
    ['{972C7061-59DE-4A6A-8B74-D94E14CAF856}']
    procedure GetSummary(Actions, Comments: TStrings);
      { GetSummary filles the list pair Actions/Comments with the summary
        information. For example:
          Actions.Add('Install');
          Comments.Add('Delphi Package JvCore'); }
  end;

  IInstallPage = interface(IInstallerPage)
    ['{5DFD17E9-1DCE-444F-9B6F-FC27B79DFBFF}']
    procedure Abort;
      { The package installer calls Abort when the installation process should
        be aborted. }
  end;

  IUninstallPage = interface(IInstallerPage)
    ['{921DE9FC-F450-4130-A0C4-311558E6777A}']
  end;

  IInstaller = interface
    ['{DC4BBDB8-E879-4BC8-A4F6-44A9737CA46E}']
    procedure Init(APackageInstaller: IPackageInstaller);
      { called when the package installer is initializing the installer. }

    function InstallerName: WideString;
      { Returns the name of the installer. This name is used for the form's
        caption. }

    function FirstPage: IInstallerPage;
      { Returns the first installer page. From this page the installer will
        browse through all pages. }

    function CanInstall: Boolean;
      { If there is no installation possible the installer should return False.
        The package installer does not display any message box. This is the job
        if this method. }

    procedure Finish;
      { Is called when the finish button is pressed. }

    function AutoInstall: Boolean;
      { Return True if the package installer should step through all pages
        automatically until it reaches a IInstallPage or IUninstallPage. }
  end;

  IPackageInstaller = interface
    ['{7285179D-BBF1-4FF3-85DD-27CD5E76A6B2}']
    procedure UpdatePages;
      { updates the next page and triggers OnPagesChanged. }
    procedure RebuildPage;
      { rebuilds the current page and triggers OnPageRecreate. }
  end;

  TPackageInstaller = class(TComponent, IPackageInstaller)
  private
    FInstaller: IInstaller;
    FPrevPages: TInterfaceList;
    FUpdateLock: Integer;
    FOnPagesChanged: TNotifyEvent;
    FOnPageRecreate: TNotifyEvent;
    FOnUpdateNavigation: TNotifyEvent;

    function GetPage: IInstallerPage;
    function GetNextPage: IInstallerPage;
    function GetPrevPage: IInstallerPage;
  public
    { IPackageInstaller }
    procedure UpdatePages;
    procedure RebuildPage;
  public
    constructor Create(AInstaller: IInstaller); reintroduce;
    destructor Destroy; override;

    procedure GoToNextPage;
    procedure GoToPrevPage;
    procedure GoToFirstPage;

    function CanNextPage: Boolean;
    function CanPrevPage: Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Installer: IInstaller read FInstaller;
    property Page: IInstallerPage read GetPage;
    property PrevPage: IInstallerPage read GetPrevPage;
    property NextPage: IInstallerPage read GetNextPage;

    property OnPagesChanged: TNotifyEvent read FOnPagesChanged write FOnPagesChanged;
    property OnPageRecreate: TNotifyEvent read FOnPageRecreate write FOnPageRecreate;
    property OnUpdateNavigation: TNotifyEvent read FOnUpdateNavigation write FOnUpdateNavigation;
  end;

var
  PackageInstaller: TPackageInstaller;

procedure AddIconFileToImageList(ImageList: TImageList; const Filename: string);

implementation

procedure AddIconFileToImageList(ImageList: TImageList; const Filename: string);
var
  FileInfo: TShFileInfo;
begin
  if FileExists(Filename) then
  begin
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    SHGetFileInfo(PChar(Filename), 0, FileInfo, SizeOf(FileInfo),
      SHGFI_ICON or SHGFI_SMALLICON);
    if FileInfo.hIcon <> 0 then
    begin
      ImageList_AddIcon(ImageList.Handle, FileInfo.hIcon);
      DestroyIcon(FileInfo.hIcon);
    end;
  end;
end;

{ TPackageInstaller }

procedure TPackageInstaller.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TPackageInstaller.EndUpdate;
begin
  Dec(FUpdateLock);
end;

function TPackageInstaller.CanNextPage: Boolean;
begin
  Result := (Page <> nil) and Page.CanNext and (Page.NextPage <> nil);
end;

function TPackageInstaller.CanPrevPage: Boolean;
begin
//  Result := (FPrevPages.Count > 1) and
//            (IInstallerPage(FPrevPages[FPrevPages.Count - 1 - 1]).CanPrev);
  Result := (FPrevPages.Count > 1) and Page.CanPrev;
end;

constructor TPackageInstaller.Create(AInstaller: IInstaller);
begin
  inherited Create(nil);
  FInstaller := AInstaller;
  FInstaller.Init(Self);
  FPrevPages := TInterfaceList.Create;
end;

destructor TPackageInstaller.Destroy;
begin
  FPrevPages.Free;
  FInstaller := nil;
  inherited Destroy;
end;

function TPackageInstaller.GetNextPage: IInstallerPage;
begin
  if CanNextPage then
    Result := Page.NextPage;
end;

function TPackageInstaller.GetPage: IInstallerPage;
begin
  if FPrevPages.Count = 0 then
    FPrevPages.Add(Installer.FirstPage);
  Result := IInstallerPage(FPrevPages[FPrevPages.Count - 1]);
end;

function TPackageInstaller.GetPrevPage: IInstallerPage;
begin
  if CanPrevPage then
    Result := IInstallerPage(FPrevPages[FPrevPages.Count - 1 - 1]);
end;

procedure TPackageInstaller.GoToNextPage;
begin
  if CanNextPage then
    FPrevPages.Add(Page.NextPage);
end;

procedure TPackageInstaller.GoToPrevPage;
begin
  if CanPrevPage then
    FPrevPages.Delete(FPrevPages.Count - 1);
end;

procedure TPackageInstaller.UpdatePages;
begin
  if FUpdateLock = 0 then
  begin
    if Assigned(FOnPagesChanged) then
      FOnPagesChanged(Self);
    if Assigned(FOnUpdateNavigation) then
      FOnUpdateNavigation(Self);
  end;
end;

procedure TPackageInstaller.RebuildPage;
begin
  if FUpdateLock = 0 then
  begin
    if Assigned(FOnPageRecreate) then
      FOnPageRecreate(Self);
    if Assigned(FOnUpdateNavigation) then
      FOnUpdateNavigation(Self);
  end;
end;

procedure TPackageInstaller.GoToFirstPage;
begin
  FPrevPages.Clear;
end;

initialization

finalization
  FreeAndNil(PackageInstaller);

end.

