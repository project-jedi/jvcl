{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmPackageSelections.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit FrmPackageSelection;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, ImgList, ExtCtrls, Menus, ActnList,
  ShellAPI, CommCtrl,
  JVCL3Install, JVCLData, PackageInformation, PackageUtils, HtHint;

type
  TFramePackageSelection = class(TFrame)
    CheckListBoxPackages: TCheckListBox;
    ComboBoxDisplayMode: TComboBox;
    ImageListPackages: TImageList;
    LblTarget: TLabel;
    ListViewTargetIDEs: TListView;
    LblIDEs: TLabel;
    ImageListTargets: TImageList;
    CheckListBoxFrameworks: TCheckListBox;
    PopupMenuPackages: TPopupMenu;
    MenuInstallAll: TMenuItem;
    MenuInstallNone: TMenuItem;
    ActionList: TActionList;
    ActionInstallAll: TAction;
    ActionInstallNone: TAction;
    LblFrameworks: TLabel;
    BtnReset: TButton;
    ActionResetPackages: TAction;
    LblShowMode: TLabel;
    N2: TMenuItem;
    ResetPackages1: TMenuItem;
    TimerHint: TTimer;
    procedure ListViewTargetIDEsSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure CheckListBoxFrameworksClick(Sender: TObject);
    procedure ComboBoxDisplayModeChange(Sender: TObject);
    procedure CheckListBoxFrameworksClickCheck(Sender: TObject);
    procedure CheckListBoxPackagesClickCheck(Sender: TObject);
    procedure CheckListBoxPackagesDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ActionInstallAllUpdate(Sender: TObject);
    procedure ActionInstallAllExecute(Sender: TObject);
    procedure CheckListBoxPackagesMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerHintTimer(Sender: TObject);
  private
    function GetSelProjectGroup: TProjectGroup;
    function GetSelTargetConfig: TTargetConfig;
  private
    FInitializing: Boolean;
    FInstaller: TInstaller;
    procedure Init;
  protected
    property Installer: TInstaller read FInstaller;
  private
    FOrgWndProc: TWndMethod;
    procedure HookWndProc(var Msg: TMessage);
  private
    FInSelection: Integer;

    procedure SelectTargetIde(TargetConfig: TTargetConfig);
    procedure SelectPackageList(ProjectGroup: TProjectGroup);

    procedure UpdatePackageState;
    function GetStateOfPackage(Pkg: TPackageTarget): TCheckBoxState;

    property SelTargetConfig: TTargetConfig read GetSelTargetConfig;
    property SelProjectGroup: TProjectGroup read GetSelProjectGroup;
  public
    constructor Create(AOwner: TComponent); override;

    class function Build(Installer: TInstaller; Client: TWinControl): TFramePackageSelection;
  end;

implementation

uses
  InstallerConsts, Core, Intf, Utils;

{$R *.dfm}

{ TFramePackageSelection }

class function TFramePackageSelection.Build(Installer: TInstaller;
  Client: TWinControl): TFramePackageSelection;
begin
  Result := TFramePackageSelection.Create(Client);
  Installer.PackageInstaller.Translate(Result);
  Result.FInstaller := Installer;
  Result.ComboBoxDisplayMode.ItemIndex := 0;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFramePackageSelection.Init;
var
  I: Integer;
  ListItem: TListItem;
begin
  FInitializing := True;
  try
    TimerHint.Interval := Application.HintPause;

   // fill target IDEs list
    ImageListTargets.Clear;
    ListViewTargetIDEs.Items.BeginUpdate;
    try
      ListViewTargetIDEs.Items.Clear;
      for I := 0 to Installer.SelTargetCount - 1 do
      begin
        with Installer.SelTargets[I] do
        begin
          if InstallJVCL then
          begin
            ListItem := ListViewTargetIDEs.Items.Add;
            ListItem.Caption := Target.DisplayName;
            ListItem.Data := Installer.SelTargets[I];

            AddIconFileToImageList(ImageListTargets, Target.Executable);
            ListItem.ImageIndex := ImageListTargets.Count - 1;
          end;
        end;
      end;

      if ListViewTargetIDEs.Items.Count > 0 then
        SelectTargetIde(ListViewTargetIDEs.Items[0].Data)
      else
        SelectTargetIde(nil);

    finally
      ListViewTargetIDEs.Items.EndUpdate;
    end;
  finally
    FInitializing := False;
  end;
end;

function TFramePackageSelection.GetSelProjectGroup: TProjectGroup;
begin
  with CheckListBoxFrameworks do
  begin
    if ItemIndex <> -1 then
      Result := TProjectGroup(Items.Objects[ItemIndex])
    else
      Result := nil;
  end;
end;

function TFramePackageSelection.GetSelTargetConfig: TTargetConfig;
begin
  if ListViewTargetIDEs.Selected <> nil then
    Result := ListViewTargetIDEs.Selected.Data
  else
    Result := nil;
end;

procedure TFramePackageSelection.ListViewTargetIDEsSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if (Item <> nil) and (Selected) then
    SelectTargetIde(Item.Data)
  else
    SelectTargetIde(nil);
end;

procedure TFramePackageSelection.SelectTargetIde(TargetConfig: TTargetConfig);
var
  Kind: TPackageGroupKind;
  Group: TProjectGroup;
  Idx: Integer;
  Item: TListItem;
begin
  if FInSelection <> 0 then
    Exit;

  Inc(FInSelection);
  CheckListBoxFrameworks.Items.BeginUpdate;
  try
   // clear sub targets
    CheckListBoxFrameworks.Items.Clear;

    if TargetConfig <> nil then
    begin
      LblTarget.Caption := TargetConfig.Target.DisplayName;

      Item := ListViewTargetIDEs.FindData(0, TargetConfig, True, False);
      if not Item.Selected then
        Item.Selected := True;

      CheckListBoxFrameworks.ItemIndex := -1;
      for Kind := pkFirst to pkLast do
      begin
       // find first selected item
        Group := TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, Kind];
        if Group <> nil then
        begin
          with CheckListBoxFrameworks do
          begin
            Idx := Items.AddObject(PackageGroupKindToStr[Kind], Group);
            Checked[Idx] := Kind in TargetConfig.InstallMode;
            if Checked[Idx] and (ItemIndex = -1) then
              ItemIndex := Idx;
          end;
        end;
      end;

    end
    else
      LblTarget.Caption := RsSelectTargetIDE;
  finally
    CheckListBoxFrameworks.Items.EndUpdate;
    Dec(FInSelection);
  end;
 // select package list
  CheckListBoxFrameworksClick(CheckListBoxFrameworks);
end;

procedure TFramePackageSelection.CheckListBoxFrameworksClick(Sender: TObject);
begin
  SelectPackageList(SelProjectGroup);
end;

procedure TFramePackageSelection.SelectPackageList(ProjectGroup: TProjectGroup);
var
  I, Idx: Integer;
  DisplayMode: Integer;
begin
  if FInSelection <> 0 then
    Exit;

  DisplayMode := ComboBoxDisplayMode.ItemIndex;
  ActionResetPackages.Visible := (SelTargetConfig <> nil) and
                                 (SelTargetConfig.InstalledJVCLVersion = 3) and
                                 (DisplayMode <> -1);

  CheckListBoxPackages.Items.BeginUpdate;
  try
    CheckListBoxPackages.Items.Clear;
    if ProjectGroup <> nil then
    begin
      Idx := CheckListBoxFrameworks.Items.IndexOfObject(ProjectGroup);
      LblTarget.Caption := TTargetConfig(ListViewTargetIDEs.Selected.Data).Target.DisplayName +
        ' - ' + CheckListBoxFrameworks.Items[Idx];
      for I := 0 to ProjectGroup.Count - 1 do
      begin
        case DisplayMode of
          0: // designtime
            if not ProjectTypeIsDesign(ProjectGroup.Packages[I].Info.ProjectType) then
              Continue;
          1: // runtime
            if ProjectTypeIsDesign(ProjectGroup.Packages[I].Info.ProjectType) then
              Continue;
          2: // both
            ;
        end;
        CheckListBoxPackages.Items.AddObject(ProjectGroup.Packages[I].Info.DisplayName,
          ProjectGroup.Packages[I]);
      end;
    end
    else
      LblTarget.Caption := RsSelectTargetIDE;

    UpdatePackageState;
    // If DesignTime packages should be shown but there are none (e.g. in x64), then switch to
    // runtime packages.
    if (CheckListBoxPackages.Items.Count = 0) and (DisplayMode = 0) then
    begin
      ComboBoxDisplayMode.ItemIndex := 1; // show runtime pages
      SelectPackageList(ProjectGroup);
    end;
  finally
    CheckListBoxPackages.Items.EndUpdate;
  end;
end;

procedure TFramePackageSelection.ComboBoxDisplayModeChange(Sender: TObject);
begin
  CheckListBoxFrameworksClick(CheckListBoxFrameworks);
end;

procedure TFramePackageSelection.CheckListBoxFrameworksClickCheck(Sender: TObject);
var
  I: Integer;
  Kind: TPackageGroupKind;
  NewInstallMode: TInstallMode;
  S: string;
begin
  Assert(SelTargetConfig <> nil);

  NewInstallMode := [];
  for I := 0 to CheckListBoxFrameworks.Items.Count - 1 do
  begin
    S := CheckListBoxFrameworks.Items[I];
    for Kind := pkFirst to pkLast do
    begin
      if S = PackageGroupKindToStr[Kind] then
      begin
        if CheckListBoxFrameworks.Checked[I] then
          Include(NewInstallMode, Kind)
        else
          Exclude(NewInstallMode, Kind);
        Break;
      end;
    end;
  end;

  SelTargetConfig.InstallMode := NewInstallMode;

 // update, InstallMode uses a write-method
  for I := 0 to CheckListBoxFrameworks.Items.Count - 1 do
  begin
    S := CheckListBoxFrameworks.Items[I];
    for Kind := pkFirst to pkLast do
    begin
      if S = PackageGroupKindToStr[Kind] then
      begin
        CheckListBoxFrameworks.Checked[I] := Kind in SelTargetConfig.InstallMode;
        Break;
      end;
    end;
  end;
end;

procedure TFramePackageSelection.CheckListBoxPackagesClickCheck(Sender: TObject);
var
  I: Integer;
  Pkg: TPackageTarget;
begin
  with CheckListBoxPackages do
  begin
    Pkg := nil;
   // get the one that has changed
    for I := 0 to Items.Count - 1 do
    begin
      if State[I] <> GetStateOfPackage(TPackageTarget(Items.Objects[I])) then
      begin
        Pkg := TPackageTarget(Items.Objects[I]);
        Break;
      end;
    end;

    if Pkg <> nil then
    begin
      if ProjectTypeIsDesign(Pkg.Info.ProjectType) then
      begin
       // designtime package can be installed
        if State[I] = cbGrayed then
          State[I] := cbChecked;
        Pkg.Install := Checked[I];
        Pkg.Compile := Checked[I];
        if ComboBoxDisplayMode.ItemIndex in [0, 1] then // designtime, runtime
        begin
          Pkg := Pkg.FindRuntimePackage;
          if Pkg <> nil then
          begin
            Pkg.Install := False;
            Pkg.Compile := State[I] <> cbUnchecked;
          end;
        end;
      end
      else
      begin
       // runtime packages can only be compiled but not installed
        if State[I] = cbChecked then
          State[I] := cbUnchecked;
        Pkg.Install := False;
        Pkg.Compile := State[I] <> cbUnchecked;
      end;
    end;

  end;

  UpdatePackageState;
end;

procedure TFramePackageSelection.UpdatePackageState;
var
  I: Integer;
begin
  if CheckListBoxFrameworks.ItemIndex <> -1 then
  begin
    with CheckListBoxPackages do
     // update from dependencies
      for I := 0 to Items.Count - 1 do
      begin
        State[I] := GetStateOfPackage(TPackageTarget(Items.Objects[I]));
        ItemEnabled[I] := CheckListBoxFrameworks.Checked[CheckListBoxFrameworks.ItemIndex];
      end;
  end;
end;

function TFramePackageSelection.GetStateOfPackage(Pkg: TPackageTarget): TCheckBoxState;
begin
  if ProjectTypeIsDesign(Pkg.Info.ProjectType) then
  begin
    Pkg.Install := Pkg.Compile;
    if Pkg.Install then
      Result := cbChecked
    else
      Result := cbUnchecked;
  end
  else
  begin
    Pkg.Install := False;
    if Pkg.Compile then
      Result := cbGrayed
    else
      Result := cbUnchecked;
  end;
end;

procedure TFramePackageSelection.CheckListBoxPackagesDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Pkg: TPackageTarget;
  ImgIndex: Integer;
  Canvas: TCanvas;
  R: TRect;
begin
  Canvas := TCheckListBox(Control).Canvas;
  Pkg := TPackageTarget(CheckListBoxPackages.Items.Objects[Index]);
  if ProjectTypeIsDesign(Pkg.Info.ProjectType) then
  begin
    ImgIndex := 0;
    if Pkg.Info.RequiresDB then
      Inc(ImgIndex);
  end
  else
    ImgIndex := 2;

  if (Index mod 2 = 1) and ([odSelected, odFocused] * State = []) then
    Canvas.Brush.Color := RGB(245, 245, 245);
  Canvas.FillRect(Rect);

  ImageListPackages.Draw(Canvas, Rect.Left + 1, Rect.Top + 1, ImgIndex);
  Inc(Rect.Left, ImageListPackages.Width + 2);

  R := Rect;
  R.Right := R.Left + 120;
  Canvas.TextRect(R, R.Left, R.Top + 2, Pkg.Info.DisplayName);

  Inc(Rect.Left, 130);
  Canvas.TextRect(Rect, Rect.Left, Rect.Top + 2, Pkg.Info.Description);

  Canvas.Pen.Color := RGB(235, 235, 235);
  Canvas.MoveTo(Rect.Left - 5, Rect.Top);
  Canvas.LineTo(Rect.Left - 5, Rect.Bottom);
end;

procedure TFramePackageSelection.ActionInstallAllUpdate(Sender: TObject);
begin
  if Sender = ActionInstallAll then
    ActionInstallAll.Enabled := (CheckListBoxPackages.Items.Count > 0) and
                                (SelProjectGroup <> nil)
  else if Sender = ActionInstallNone then
    ActionInstallNone.Enabled := (CheckListBoxPackages.Items.Count > 0) and
                                (SelProjectGroup <> nil)
  ;
end;

procedure TFramePackageSelection.ActionInstallAllExecute(Sender: TObject);
var
  I: Integer;
  Pkg: TPackageTarget;
begin
  if (Sender = ActionInstallAll) or (Sender = ActionInstallNone) then
  begin
    for I := 0 to CheckListBoxPackages.Items.Count - 1 do
    begin
      Pkg := TPackageTarget(CheckListBoxPackages.Items.Objects[I]);
      if Sender = ActionInstallAll then
      begin
        Pkg.Install := True
      end
      else if Sender = ActionInstallNone then
      begin
        Pkg.Compile := False;
        if ComboBoxDisplayMode.ItemIndex in [0, 1] then // designtime, runtime
        begin
          Pkg := Pkg.FindRuntimePackage;
          if Pkg <> nil then
            Pkg.Compile := False;
        end;
      end;
    end;
  end
  else if Sender = ActionResetPackages then
    SelTargetConfig.ResetPackagesSettings(SelProjectGroup);
  UpdatePackageState;
end;

procedure TFramePackageSelection.CheckListBoxPackagesMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  S: string;
  Lines: TStrings;
  Pkg, RuntimePkg: TPackageTarget;
  I: Integer;
begin
  if TimerHint.Tag <> 1 then
    Exit;

  Index := CheckListBoxPackages.ItemAtPos(Point(X, Y), True);
  if Index = -1 then
    S := ''
  else
  begin
    Pkg := TPackageTarget(CheckListBoxPackages.Items.Objects[Index]);

    Lines := TStringList.Create;
    try
      if (Pkg.RequireCount > 0) or (Pkg.ContainCount > 0) then
      begin
        Lines.Add('<b><c:red>' + Pkg.Info.DisplayName + '</b><c:black>');
        Lines.Add('<i>' + WordWrapString(Pkg.Info.Description) + '</i>');
        Lines.Add('');
      end;

      RuntimePkg := Pkg.FindRuntimePackage;
      if RuntimePkg <> nil then
        Pkg := RuntimePkg;

      if Pkg.RequireCount > 0 then
      begin
        Lines.Add('<b>' + RsPkgInfoRequires + '</b>');
        S := '';
        for I := 0 to Pkg.RequireCount - 1 do
          S := S + Pkg.Requires[I].GetBplName(SelProjectGroup) + ', ';
        Delete(S, Length(S) - 1, 2);
        Lines.Add(WordWrapString(S));
        Lines.Add('');
      end;

      if Pkg.ContainCount > 0 then
      begin
        Lines.Add('<b>' + RsPkgInfoContains + '</b>');
        S := '';
        for I := 0 to Pkg.ContainCount - 1 do
          S := S + ExtractFileName(Pkg.Contains[I].Name) + ', ';
        Delete(S, Length(S) - 1, 2);
        Lines.Add(WordWrapString(S));
      end;

      S := Lines.Text;
    finally
      Lines.Free;
    end;
  end;

  if (CheckListBoxPackages.Hint <> S) and (CheckListBoxPackages.Hint <> '') then
  begin
    Application.CancelHint;
    CheckListBoxPackages.Hint := S;
    TimerHint.Tag := 0;
    TimerHint.Enabled := True;
  end
  else
  begin
    if CheckListBoxPackages.Hint <> S then
    begin
      CheckListBoxPackages.Hint := S;
      X := CheckListBoxPackages.BoundsRect.Right - 250;
      Application.ActivateHint(CheckListBoxPackages.ClientToScreen(Point(X, Y)));
    end;
  end;
end;

procedure TFramePackageSelection.TimerHintTimer(Sender: TObject);
var
  Pt: TPoint;
begin
  TimerHint.Enabled := False;
  TimerHint.Tag := 1;
  Pt := CheckListBoxPackages.ScreenToClient(Mouse.CursorPos);
  CheckListBoxPackagesMouseMove(CheckListBoxPackages, [], Pt.X, Pt.Y);
end;

procedure TFramePackageSelection.HookWndProc(var Msg: TMessage);
begin
  FOrgWndProc(Msg);
  case Msg.Msg of
    CM_MOUSEENTER:
      begin
        TimerHint.Enabled := True;
      end;
    CM_MOUSELEAVE:
      begin
        TimerHint.Tag := 0;
        TimerHint.Enabled := False;
      end;
  end;
end;

constructor TFramePackageSelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrgWndProc := CheckListBoxPackages.WindowProc;
  CheckListBoxPackages.WindowProc := HookWndProc;

  LblFrameworks.Visible := False;
  CheckListBoxFrameworks.Visible := False;
  {$IFDEF COMPILER12_UP}
  CheckListBoxPackages.ParentDoubleBuffered := False;
  CheckListBoxFrameworks.ParentDoubleBuffered := False;
  {$ENDIF COMPILER12_UP}
  {$IFDEF COMPILER11_UP}
  CheckListBoxPackages.DoubleBuffered := False;
  CheckListBoxFrameworks.DoubleBuffered := False;
  {$ENDIF COMPILER11_UP}
end;

end.
