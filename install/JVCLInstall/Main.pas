{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Main.pas, released on 2004-03-29.

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

{
  command line arguments:
    --help              Shows the help screen with all options
}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList,
  Dialogs, JvExControls, JvComponent, jpeg, ExtCtrls, StdCtrls, JvWizard,
  JvWizardRouteMapList,
  Core,
  ShellAPI, HtHint, JvWizardRouteMapSteps;

type
  TFormMain = class(TForm)
    JvWizard: TJvWizard;
    ImageList: TImageList;
    PanelLogo: TPanel;
    ImageLogo: TImage;
    JvWizardRouteMapList: TJvWizardRouteMapList;
    LblHomepage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvWizardActivePageChanging(Sender: TObject;
      var ToPage: TJvWizardCustomPage);
    procedure JvWizardFinishButtonClick(Sender: TObject);
    procedure JvWizardCancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure JvWizardActivePageChanged(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure JvWizardRouteMapNodes1Displaying(Sender: TObject;
      const Page: TJvWizardCustomPage; var AllowDisplay: Boolean);
    procedure LblHomepageClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FAppStartFailed: Boolean;
    FFinished: Boolean;
    function BuildPages(Inst: IInstallerPage): TWinControl;
  protected
    procedure DoPagesChanged(Sender: TObject);
    procedure DoPageRecreate(Sender: TObject);
    procedure DoUpdateNavigation(Sender: TObject);
    procedure DoPageShow(Sender: TObject);

    procedure RecreatePage(var Msg: TMessage); message WM_USER + 1;
  public
    property AppStartFailed: Boolean read FAppStartFailed write FAppStartFailed;
    property Finished: Boolean read FFinished write FFinished;
  end;

var
  FormMain: TFormMain;

implementation

uses
  InstallerConsts, PageBuilder, JvResources, Utils;

(* // Main.pas  - see InstallerConsts.pas
resourcestring
  RsBtnInstall = '&Install';
  RsBtnUninstall = '&Uninstall';
  RsNoPackageInstaller = 'Application error. No PackageInstaller created.';
  RsCancelInstallation = 'Do you really want to cancel the installation?';

  RsJediHomepage = 'http://projectjedi.sourceforge.net';
  RsJVCLHomepage = 'http://jvcl.sourceforge.net';
*)

{$R *.dfm}
{$R WinXP.res}

procedure TFormMain.DoPagesChanged(Sender: TObject);
begin
  BuildPages(PackageInstaller.Page.NextPage);
end;

procedure TFormMain.DoPageRecreate(Sender: TObject);
begin
  // Could be called by an event handler of a component that will be destroyed.
  PostMessage(Handle, WM_USER + 1, 0, Integer(Sender));
end;

procedure TFormMain.RecreatePage(var Msg: TMessage);
begin
  if JvWizard.ActivePage <> nil then
  begin
    DestroyPage(TJvWizardInteriorPage(JvWizard.ActivePage));
    PackageInstaller.Page.Init;
    PreparePage(TJvWizardInteriorPage(JvWizard.ActivePage), PackageInstaller.Page);
    DoPageShow(JvWizard.ActivePage);
    DoPagesChanged(TObject(Msg.LParam));
  end;
end;

procedure TFormMain.DoPageShow(Sender: TObject);
var
  Ctrl: TWinControl;
begin
  PackageInstaller.BeginUpdate;
  try
    Ctrl := CreatePage(Sender as TJvWizardInteriorPage, PackageInstaller.Page);
    while (Ctrl <> nil) and (not Ctrl.Enabled) do
      Ctrl := FindNextControl(Ctrl, True, False, False);
    ActiveControl := Ctrl;
    DoUpdateNavigation(Sender);
  finally
    PackageInstaller.EndUpdate;
  end;

  if Supports(PackageInstaller.Page.NextPage, IInstallPage) then
    JvWizard.ButtonNext.Caption := RsBtnInstall
  else if Supports(PackageInstaller.Page.NextPage, IUninstallPage) then
    JvWizard.ButtonNext.Caption := RsBtnUninstall
  else
    JvWizard.ButtonNext.Caption := RsNextButtonCaption;
end;

function TFormMain.BuildPages(Inst: IInstallerPage): TWinControl;
var
  Page: TJvWizardInteriorPage;
  Index: Integer;
begin
  PackageInstaller.BeginUpdate;
  try
    Result := nil;
   // remove old pages
    Index := JvWizard.ActivePageIndex;
    while Index + 1 < JvWizard.PageCount do
    begin
     // First remove ActionList from the frame (bug in VCL).
      DestroyPage(JvWizard.WizardPages[JvWizard.PageCount - 1] as TJvWizardInteriorPage);
      JvWizard.WizardPages[JvWizard.PageCount - 1].Free;
    end;

    Page := nil;
    while Inst <> nil do
    begin
      Page := TJvWizardInteriorPage.Create(JvWizard);
      Page.Wizard := JvWizard;
      Page.Header.Height := 65;
      PreparePage(Page, Inst);
      Page.OnPage := DoPageShow;

      Inst := Inst.NextPage; // create next page
    end;
    if Page <> nil then
    begin
      Page.VisibleButtons := Page.VisibleButtons + [bkFinish] - [bkNext];
      Page.EnableButton(bkCancel, False);
    end;
  finally
    PackageInstaller.EndUpdate;
  end;
end;

procedure TFormMain.DoUpdateNavigation(Sender: TObject);
begin
  if JvWizard.ActivePage <> nil then
  begin
    JvWizard.ActivePage.EnableButton(bkBack, PackageInstaller.CanPrevPage);
    JvWizard.ActivePage.EnableButton(bkNext, PackageInstaller.CanNextPage);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.HintHidePause := MaxInt;
  HintWindowClass := THtHintWindow;
  Application.ShowHint := False;
  Application.ShowHint := True;

  if PackageInstaller = nil then
  begin
    MessageDlg(RsNoPackageInstaller, mtError, [mbOk], 0);
    AppStartFailed := True;
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  PackageInstaller.OnPagesChanged := DoPagesChanged;
  PackageInstaller.OnPageRecreate := DoPageRecreate;
  PackageInstaller.OnUpdateNavigation := DoUpdateNavigation;

  Caption := PackageInstaller.Installer.InstallerName;
  Application.Title := Caption;

  if not PackageInstaller.Installer.CanInstall then
  begin
    Application.ShowMainForm := False;
    Application.Terminate;
    Exit;
  end;

 // create welcome page
  BuildPages(PackageInstaller.Page);
  JvWizard.SelectFirstPage;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if AppStartFailed then
    Exit; // do not free any memory that is not allocated
end;

procedure TFormMain.JvWizardActivePageChanging(Sender: TObject;
  var ToPage: TJvWizardCustomPage);
var
  Index: Integer;
begin
  Index := JvWizard.ActivePageIndex;
  if (ToPage = nil) or (Index = -1) then
    Exit;
  if Index < ToPage.PageIndex then
  begin
    while Index < ToPage.PageIndex do
    begin
      if not PackageInstaller.CanNextPage then
      begin
        ToPage := JvWizard.WizardPages[Index];
        Break;
      end;
      PackageInstaller.GoToNextPage;
      Inc(Index);
    end;
  end
  else
  begin
    while Index > ToPage.PageIndex do
    begin
      if not PackageInstaller.CanPrevPage then
      begin
        ToPage := JvWizard.WizardPages[Index];
        Break;
      end;
      PackageInstaller.GoToPrevPage;
      Dec(Index);
    end;
  end;
end;

procedure TFormMain.JvWizardFinishButtonClick(Sender: TObject);
begin
  Finished := True;
  PackageInstaller.Installer.Finish;
  Close;
end;

procedure TFormMain.JvWizardCancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  InstallPage: IInstallPage;
begin
  if not Finished then
  begin
    CanClose := MessageDlg(RsCancelInstallation,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    if CanClose and Supports(PackageInstaller.Page, IInstallPage, InstallPage) and
       (JvWizard.ActivePage.EnabledButtons = []) then
    begin
      CanClose := False;
      InstallPage.Abort;
    end;
  end;
end;

procedure TFormMain.JvWizardActivePageChanged(Sender: TObject);
var
  Buttons: TJvWizardButtonSet;
begin
  PanelLogo.BringToFront;
  if PackageInstaller.Page <> nil then
  begin
    Buttons := JvWizard.ActivePage.EnabledButtons;
    JvWizard.ActivePage.EnabledButtons := [];
    try
      PackageInstaller.Page.Action;
    finally
      JvWizard.ActivePage.EnabledButtons := Buttons;
    end;
  end;
end;

procedure TFormMain.ImageLogoClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(RsJediHomepage), // do not localize
    nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.JvWizardRouteMapNodes1Displaying(Sender: TObject;
  const Page: TJvWizardCustomPage; var AllowDisplay: Boolean);
begin
  AllowDisplay := Page.PageIndex < Page.Wizard.PageCount - 1;
end;

procedure TFormMain.LblHomepageClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(RsJVCLHomepage), // do not localize
    nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.FormPaint(Sender: TObject);
var
  Page: IInstallerPage;
begin
  if Tag <> 0 then
    Exit;
  Tag := 1;

 // auto installation 
  if PackageInstaller.Installer.AutoInstall then
  begin
    while True do
    begin
      Page := PackageInstaller.Page.NextPage;
      if (Page = nil) then
        Break;
      if Supports(Page, IInstallPage) or Supports(Page, IUninstallPage) then
      begin
        JvWizard.SelectNextPage;
        Break;
      end;
      Page := nil;
      JvWizard.SelectNextPage;
    end;
  end;
end;

end.
