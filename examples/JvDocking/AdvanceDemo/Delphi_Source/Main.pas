{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, ComCtrls, ToolWin, JvDockControlForm, JvDockTree,
  JvDockVCStyle, JvDockDelphiStyle, JvDockVIDStyle, JvDockVSNetStyle, JvDockVIDVCStyle, 
  JvDockSupportClass, ActnList
  {$IFDEF USEJVCL}
  , JvComponent, JvAppStorage, JvAppRegistryStorage, JvAppIniStorage, JvAppXmlStorage
  {$ENDIF}
  {$IFDEF VER150}, XPMan{$ENDIF};

type
  TRunTimeForm = class(TForm)
  private
    FMenuItem: TMenuItem;
    FDockClient: TJvDockClient;
    procedure SetMenuItem(AMenuItem: TMenuItem);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property DockClient: TJvDockClient read FDockClient;
  end;

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    DockForm_Menu: TMenuItem;
    DelphiStyle: TMenuItem;
    VCStyle: TMenuItem;
    VIDStyle: TMenuItem;
    ToolBar1: TToolBar;
    btnDelphi: TToolButton;
    btnVC: TToolButton;
    btnVID: TToolButton;
    ShowWindow_Menu: TMenuItem;
    DockInfo_Menu: TMenuItem;
    SaveToIniFile: TMenuItem;
    LoadFromIniFile: TMenuItem;
    SaveToReg: TMenuItem;
    LoadFromReg: TMenuItem;
    N24: TMenuItem;
    DockOption_Menu: TMenuItem;
    TopDocked: TMenuItem;
    BottomDocked: TMenuItem;
    LeftDocked: TMenuItem;
    RightDocked: TMenuItem;
    AllDocked: TMenuItem;
    N31: TMenuItem;
    lbDockServer1: TJvDockServer;
    JvDockDelphiStyle1: TJvDockDelphiStyle;
    JvDockVCStyle1: TJvDockVCStyle;
    JvDockVIDStyle1: TJvDockVIDStyle;
    StatusBar1: TStatusBar;
    btnVSNet: TToolButton;
    VSNETStyle: TMenuItem;
    PopupMenu2: TPopupMenu;
    ClientDockorFloat: TMenuItem;
    ClientHide: TMenuItem;
    ClientTopDocked: TMenuItem;
    ClientBottomDocked: TMenuItem;
    ClientLeftDocked: TMenuItem;
    ClientRightDocked: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    ClientEachOtherDocked: TMenuItem;
    ClientAllDocked: TMenuItem;
    Memo1: TMemo;
    JvDockVSNetStyle1: TJvDockVSNetStyle;
    N1: TMenuItem;
    SaveToXmlFile: TMenuItem;
    LoadFromXmlFile: TMenuItem;
    ToolButton1: TToolButton;
    JvDockVIDVCStyle1: TJvDockVIDVCStyle;
    ToolButton2: TToolButton;
    ServerStyle_Menu: TMenuItem;
    ServerDelphiStyle: TMenuItem;
    ServerVisualCStyle: TMenuItem;
    ServerVisualInterDevStyle: TMenuItem;
    ServerVisualStudionetStyle: TMenuItem;
    ActionList1: TActionList;
    actServerStyleDelphi: TAction;
    actServerStyleVC: TAction;
    actServerStyleVID: TAction;
    actServerStyleVSNet: TAction;
    actServerStyleVIDVC: TAction;
    VisualInterDevCStyle1: TMenuItem;
    actNewWindowDelphiStyle: TAction;
    actNewWindowVCStyle: TAction;
    actNewWindowVIDStyle: TAction;
    actNewWindowVSNetStyle: TAction;
    actNewWindowVIDVCStyle: TAction;
    VIDVCStyle1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure SaveToIniFileClick(Sender: TObject);
    procedure LoadFromIniFileClick(Sender: TObject);
    procedure SaveToRegClick(Sender: TObject);
    procedure LoadFromRegClick(Sender: TObject);
    procedure TopDockedClick(Sender: TObject);
    procedure BottomDockedClick(Sender: TObject);
    procedure LeftDockedClick(Sender: TObject);
    procedure RightDockedClick(Sender: TObject);
    procedure AllDockedClick(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure ClientTopDockedClick(Sender: TObject);
    procedure ClientBottomDockedClick(Sender: TObject);
    procedure ClientLeftDockedClick(Sender: TObject);
    procedure ClientRightDockedClick(Sender: TObject);
    procedure ClientEachOtherDockedClick(Sender: TObject);
    procedure ClientAllDockedClick(Sender: TObject);
    procedure ClientDockorFloatClick(Sender: TObject);
    procedure ClientHideClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveToXmlFileClick(Sender: TObject);
    procedure LoadFromXmlFileClick(Sender: TObject);
    procedure CloseAllClick(Sender: TObject);
    procedure actServerStyleExecute(Sender: TObject);
    procedure actServerStyleUpdate(Sender: TObject);
    procedure NewWindowExecute(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF USEJVCL}
    FJvAppRegistryStorage: TJvAppRegistryStorage;
    FJvAppIniFileStorage: TJvAppIniFileStorage;
    FJvAppXmlStorage: TJvAppXmlFileStorage;
    {$ENDIF}
    FFormCount: array[0..4] of Integer;
    procedure AddRunTimeItemToShowDockMenu(AForm: TRunTimeForm);
    procedure ShowDockWindowMenuClick(Sender: TObject);

    {$IFDEF USEJVCL}
    procedure LoadFromAppStorage(AppStorage: TJvCustomAppStorage);
    procedure SaveToAppStorage(AppStorage: TJvCustomAppStorage);

    procedure SaveFormsToAppStorage(AppStorage: TJvCustomAppStorage);
    procedure LoadFormsFromAppStorage(AppStorage: TJvCustomAppStorage);

    procedure FreeRunTimeForms;
    {$ENDIF}

    function GetFormCount(AStyle: TJvDockBasicStyle): Integer;
    procedure SetFormCount(AStyle: TJvDockBasicStyle;
      const Value: Integer);

    procedure ConstructRunTimeForm(AStyle: TJvDockBasicStyle; const AName: string);
  public
    function ActionToStyle(AAction: TAction): TJvDockBasicStyle;
    function IDToStyle(const ID: Integer): TJvDockBasicStyle;
    function StyleToStr(AStyle: TJvDockBasicStyle): string;
    function StyleToID(AStyle: TJvDockBasicStyle): Integer;

    procedure UpdateCaption;

    property FormCount[AStyle: TJvDockBasicStyle]: Integer read GetFormCount write SetFormCount;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

const
  // Used as value of the tags of the actions, for example actServerStyleVIDVC.Tag = 4
  cDelphiStyleID = 0;
  cVisualCStyleID = 1;
  cVisualInterDevStyleID = 2;
  cVisualStudioNetStyleID = 3;
  cVisualInterDevVisualCStyleID = 4;

  cStyleStr: array[0..4] of string = (
    'Delphi Style',
    'Visual C++ Style',
    'Visual InterDev Style',
    'Visual Studio.Net Style',
    'Visual InterDev C++ Style'
    );

//=== Local procedures =======================================================

function CreateUniqueName: string;
var
  I: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Screen.FormCount - 1 do
      if CompareText(AName, Screen.Forms[I].Name) = 0 then
        Exit;
    Result := True;
  end;

begin
  for I := 1 to MaxInt do
  begin
    Result := Format('RunTimeForm_%d', [I]);
    if IsUnique(Result) then
      Exit;
  end;
end;

//=== { TMainForm } ==========================================================

function TMainForm.ActionToStyle(AAction: TAction): TJvDockBasicStyle;
begin
  Result := IDToStyle(AAction.Tag);
end;

procedure TMainForm.actServerStyleExecute(Sender: TObject);
begin
  if Sender is TAction then
  begin
    lbDockServer1.DockStyle := ActionToStyle(TAction(Sender));
    UpdateCaption;
  end;
end;

procedure TMainForm.actServerStyleUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked :=
      lbDockServer1.DockStyle = ActionToStyle(TAction(Sender));
end;

procedure TMainForm.AddRunTimeItemToShowDockMenu(AForm: TRunTimeForm);
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := NewItem(AForm.Caption, 0, True, True,
    ShowDockWindowMenuClick, 0, '');
  ShowWindow_Menu.Add(AMenuItem);
  AMenuItem.Tag := Integer(AForm);
  AForm.MenuItem := AMenuItem;
end;

procedure TMainForm.AllDockedClick(Sender: TObject);
begin
  AllDocked.Checked := not AllDocked.Checked;
  lbDockServer1.EnableDock := AllDocked.Checked;
end;

procedure TMainForm.BottomDockedClick(Sender: TObject);
begin
  BottomDocked.Checked := not BottomDocked.Checked;
  lbDockServer1.BottomDock := BottomDocked.Checked;
end;

procedure TMainForm.ClientAllDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientAllDocked.Checked := not ClientAllDocked.Checked;
      DockClient.EnableDock := ClientAllDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientBottomDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientBottomDocked.Checked := not ClientBottomDocked.Checked;
      DockClient.BottomDock := ClientBottomDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientDockorFloatClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
      DockClient.RestoreChild;
  end;
end;

procedure TMainForm.ClientEachOtherDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientEachOtherDocked.Checked := not ClientEachOtherDocked.Checked;
      DockClient.EachOtherDock := ClientEachOtherDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientHideClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
      DockClient.HideParentForm;
  end;
end;

procedure TMainForm.ClientLeftDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientLeftDocked.Checked := not ClientLeftDocked.Checked;
      DockClient.LeftDock := ClientLeftDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientRightDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientRightDocked.Checked := not ClientRightDocked.Checked;
      DockClient.RightDock := ClientRightDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientTopDockedClick(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientTopDocked.Checked := not ClientTopDocked.Checked;
      DockClient.TopDock := ClientTopDocked.Checked;
    end;
  end;
end;

procedure TMainForm.CloseAllClick(Sender: TObject);
var
  aMenuItem: TMenuItem;
  Frm: TForm;
  DockClient: TJvDockClient;
  I: Integer;
begin
  for I := ShowWindow_Menu.Count -1 downto 0 do
  begin
    aMenuItem := ShowWindow_Menu.Items[I];
    Frm := TForm(aMenuItem.Tag);
    DockClient := FindDockClient(Frm);
    if DockClient <> nil then
      DoFloatForm(Frm);

    { It should also work with Frm.Free, but that gives problems, needs to be
      fixed }
    Frm.Release;

    { A TRunTimeForm form implicitly destroys its attached MenuItem }
    if not (Frm is TRunTimeForm) then
      ShowWindow_Menu.Delete(I);

    { Without the next call, not all helper forms will be destroyed. This needs
      to be fixed }
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.ConstructRunTimeForm(AStyle: TJvDockBasicStyle; const AName: string);
var
  Frm: TRunTimeForm;
  LStyle: Integer;
begin
  Frm := TRunTimeForm.Create(Application);
  Frm.Visible := True;
  if AName = '' then
    Frm.Name := CreateUniqueName
  else
    Frm.Name := AName;
  Frm.Caption := StyleToStr(AStyle) + ' _ ' + IntToStr(FormCount[AStyle]);
  FormCount[AStyle] := FormCount[AStyle] + 1;
  Frm.DockClient.DockStyle := AStyle;

  LStyle := StyleToID(AStyle);
  Frm.DockClient.DirectDrag :=
    (LStyle = cVisualCStyleID) or (LStyle = cDelphiStyleID);
  Frm.DockClient.EachOtherDock :=
    (LStyle = cVisualStudioNetStyleID) or
    (LStyle = cVisualInterDevVisualCStyleID) or
    (LStyle = cVisualInterDevStyleID);

  AddRunTimeItemToShowDockMenu(Frm);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TopDocked.Checked := lbDockServer1.TopDock;
  BottomDocked.Checked := lbDockServer1.BottomDock;
  LeftDocked.Checked := lbDockServer1.LeftDock;
  RightDocked.Checked := lbDockServer1.RightDock;
  AllDocked.Checked := lbDockServer1.EnableDock;
  Memo1.WordWrap := True;
  UpdateCaption;
  {$IFDEF USEJVCL}
  FJvAppRegistryStorage := TJvAppRegistryStorage.Create(self);
  FJvAppRegistryStorage.Path := '\Software\JVCL\Examples\JvDocking\AdvancePro';
  FJvAppRegistryStorage.AutoFlush := True;
  FJvAppRegistryStorage.AutoReload := True;
  FJvAppIniFileStorage := TJvAppIniFileStorage.Create(self);
  FJvAppIniFileStorage.FileName := 'DockInfo.ini';
  FJvAppIniFileStorage.AutoFlush := True;
  FJvAppIniFileStorage.AutoReload := True;
  FJvAppXmlStorage := TJvAppXmlFileStorage.Create(self);
  FJvAppXMLStorage.FileName := 'DockInfo.xml';
  FJvAppXMLStorage.AutoFlush := True;
  FJvAppXMLStorage.AutoReload := True;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  { Prevent last Flush by setting Path/FileName to '' }
  FJvAppRegistryStorage.Path := '';
  FreeAndNil(FJvAppRegistryStorage);
  FJvAppIniFileStorage.FileName := '';
  FreeAndNil(FJvAppIniFileStorage);
  FJvAppXmlStorage.FileName := '';
  FreeAndNil(FJvAppXmlStorage);
  {$ENDIF}
end;

{$IFDEF USEJVCL}
procedure TMainForm.FreeRunTimeForms;
var
  I: Integer;
  Frm: TForm;
  ADockClient: TJvDockClient;
begin
  for I := Screen.FormCount - 1 downto 0 do
    if Screen.Forms[I] is TRunTimeForm then
    begin
      Frm := Screen.Forms[I];
      ADockClient := FindDockClient(Frm);
      if ADockClient <> nil then
        DoFloatForm(Frm);
      Frm.Free;
    end;
end;
{$ENDIF USEJVCL}

function TMainForm.GetFormCount(AStyle: TJvDockBasicStyle): Integer;
begin
  Result := FFormCount[StyleToID(AStyle)];
end;

function TMainForm.IDToStyle(const ID: Integer): TJvDockBasicStyle;
begin
  case ID of
    cDelphiStyleID: Result := JvDockDelphiStyle1;
    cVisualCStyleID: Result := JvDockVCStyle1;
    cVisualInterDevStyleID: Result := JvDockVIDStyle1;
    cVisualStudioNetStyleID: Result := JvDockVSNetStyle1;
    cVisualInterDevVisualCStyleID: Result := JvDockVIDVCStyle1;
  else
    Result := nil;
  end;
end;

procedure TMainForm.LeftDockedClick(Sender: TObject);
begin
  LeftDocked.Checked := not LeftDocked.Checked;
  lbDockServer1.LeftDock := LeftDocked.Checked;
end;

{$IFDEF USEJVCL}

procedure TMainForm.LoadFormsFromAppStorage(
  AppStorage: TJvCustomAppStorage);
var
  I: Integer;
  OldPath: string;
  Count: Integer;
  APath: string;

  FrmName: string;
  StyleID: Integer;
begin
  OldPath := AppStorage.Path;
  AppStorage.Path := AppStorage.ConcatPaths([OldPath, 'ExtraInfo']);
  try
    { Read and set the dock style of the dockserver component }
    StyleID := AppStorage.ReadInteger('ServerStyle');
    lbDockServer1.DockStyle := IDToStyle(StyleID);
    UpdateCaption;

    { Read the name and dock style of the forms and create the forms }
    Count := AppStorage.ReadInteger('Count');
    for I := 0 to Count - 1 do
    begin
      APath := Format('Item%d', [I]);
      FrmName := AppStorage.ReadString(AppStorage.ConcatPaths([APath, 'Name']));
      StyleID := AppStorage.ReadInteger(AppStorage.ConcatPaths([APath, 'StyleID']));

      ConstructRunTimeForm(IDToStyle(StyleID), FrmName);
    end;
  finally
    AppStorage.Path := OldPath;
  end;
end;

procedure TMainForm.LoadFromAppStorage(AppStorage: TJvCustomAppStorage);
begin
  FreeRunTimeForms;

  AppStorage.BeginUpdate;
  try
    LoadFormsFromAppStorage(AppStorage);
    LoadDockTreeFromAppStorage(AppStorage);
  finally
    AppStorage.EndUpdate;
  end;
end;

{$ENDIF USEJVCL}

procedure TMainForm.LoadFromIniFileClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  LoadFromAppStorage(FJvAppIniFileStorage);
  {$ELSE}
  LoadDockTreeFromFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
  {$ENDIF}
end;

procedure TMainForm.LoadFromRegClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  LoadFromAppStorage(FJvAppRegistryStorage);
  {$ELSE}
  LoadDockTreeFromReg(HKEY_CURRENT_USER, '\Software\DockInfo');
  {$ENDIF}
end;

procedure TMainForm.LoadFromXmlFileClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  LoadFromAppStorage(FJvAppXmlStorage);
  {$ELSE}
  ShowMessage('Not supported unless USEJVCL is defined');
  {$ENDIF}
end;

procedure TMainForm.NewWindowExecute(Sender: TObject);
begin
  if Sender is TAction then
    ConstructRunTimeForm(ActionToStyle(TAction(Sender)), '');
end;

procedure TMainForm.PopupMenu2Popup(Sender: TObject);
var
  DockClient: TJvDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientTopDocked.Checked := DockClient.TopDock;
      ClientBottomDocked.Checked := DockClient.BottomDock;
      ClientLeftDocked.Checked := DockClient.LeftDock;
      ClientRightDocked.Checked := DockClient.RightDock;
      ClientEachOtherDocked.Checked := DockClient.EachOtherDock;
      ClientAllDocked.Checked := DockClient.EnableDock;
      if DockClient.DockState = JvDockState_Floating then
        ClientDockorFloat.Caption := 'Dock'
      else
        ClientDockorFloat.Caption := 'Float';
    end;
  end;
end;

procedure TMainForm.RightDockedClick(Sender: TObject);
begin
  RightDocked.Checked := not RightDocked.Checked;
  lbDockServer1.RightDock := RightDocked.Checked;
end;

{$IFDEF USEJVCL}

procedure TMainForm.SaveFormsToAppStorage(AppStorage: TJvCustomAppStorage);
var
  I: Integer;
  OldPath: string;
  APath: string;
  Count: Integer;

  Frm: TForm;
  FrmDockClient: TJvDockClient;
begin
  Count := 0;
  OldPath := AppStorage.Path;
  AppStorage.Path := AppStorage.ConcatPaths([OldPath, 'ExtraInfo']);
  try
    AppStorage.WriteInteger('ServerStyle', StyleToID(lbDockServer1.DockStyle));
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I] is TRunTimeForm then
      begin
        Frm := Screen.Forms[I];
        FrmDockClient := FindDockClient(Frm);
        if Assigned(FrmDockClient) then
        begin
          APath := Format('Item%d', [Count]);
          AppStorage.WriteString(AppStorage.ConcatPaths([APath, 'Name']), Frm.Name);
          AppStorage.WriteInteger(AppStorage.ConcatPaths([APath, 'StyleID']),
            StyleToID(FrmDockClient.DockStyle));
          Inc(Count);
        end;
      end;
    AppStorage.WriteInteger('Count', Count);
  finally
    AppStorage.Path := OldPath;
  end;
end;

procedure TMainForm.SaveToAppStorage(AppStorage: TJvCustomAppStorage);
begin
  AppStorage.BeginUpdate;
  try
    SaveDockTreeToAppStorage(AppStorage);
    { SaveDockTreeToAppStorage clears the storage, so we save the forms after the
      SaveDockTreeToAppStorage call }
    SaveFormsToAppStorage(AppStorage);
  finally
    AppStorage.EndUpdate;
  end;
end;

{$ENDIF USEJVCL}

procedure TMainForm.SaveToIniFileClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  SaveToAppStorage(FJvAppIniFileStorage);
  {$ELSE}
  SaveDockTreeToFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
  {$ENDIF}
end;

procedure TMainForm.SaveToRegClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  SaveToAppStorage(FJvAppRegistryStorage);
  {$ELSE}
  SaveDockTreeToReg(HKEY_CURRENT_USER, '\Software\DockInfo');
  {$ENDIF}
end;

procedure TMainForm.SaveToXmlFileClick(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  SaveToAppStorage(FJvAppXmlStorage);
  {$ELSE}
  ShowMessage('Not supported unless USEJVCL is defined');
  {$ENDIF}
end;

procedure TMainForm.SetFormCount(AStyle: TJvDockBasicStyle;
  const Value: Integer);
begin
  FFormCount[StyleToID(AStyle)] := Value;
end;

procedure TMainForm.ShowDockWindowMenuClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  Frm: TForm;
begin
  MenuItem := TMenuItem(Sender);
  Frm := TForm(MenuItem.Tag);
  if MenuItem.Checked then
  begin
    if GetFormVisible(Frm) then
    begin
      HideDockForm(Frm);
      MenuItem.Checked := False;
    end
    else
      ShowDockForm(Frm);
  end
  else
  begin
    ShowDockForm(Frm);
    MenuItem.Checked := True;
  end;
end;

function TMainForm.StyleToID(AStyle: TJvDockBasicStyle): Integer;
begin
  if AStyle is TJvDockVSNetStyle then
    Result := cVisualStudioNetStyleID
  else
  if AStyle is TJvDockVIDVCStyle then
    Result := cVisualInterDevVisualCStyleID
  else
  if AStyle is TJvDockVIDStyle then
    Result := cVisualInterDevStyleID
  else
  if AStyle is TJvDockVCStyle then
    Result := cVisualCStyleID
  else
  if AStyle is TJvDockDelphiStyle then
    Result := cDelphiStyleID
  else
    raise Exception.Create('Unknown style');
end;

function TMainForm.StyleToStr(AStyle: TJvDockBasicStyle): string;
begin
  Result := cStyleStr[StyleToID(AStyle)];
end;

procedure TMainForm.TopDockedClick(Sender: TObject);
begin
  TopDocked.Checked := not TopDocked.Checked;
  lbDockServer1.TopDock := TopDocked.Checked;
end;

procedure TMainForm.UpdateCaption;
begin
  Caption := 'Main Window (docking is set to ' + lbDockServer1.DockStyle.ClassName + ')';
end;

//=== { TRunTimeForm } =======================================================

constructor TRunTimeForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  Width := 186;
  Height := 188;
  BorderStyle := bsSizeToolWin;
  DockSite := True;
  DragKind := dkDock;
  DragMode := dmAutomatic;
  Font.Name := 'MS Shell Dlg 2';
  FormStyle := fsStayOnTop;
  Position := poDefaultPosOnly;
  //  Visible := F;
  with TMemo.Create(Self) do
  begin
    Align := alClient;
    BorderStyle := bsNone;
  end;
  FDockClient := TJvDockClient.Create(Self);
  with FDockClient do
  begin
    OnFormShow := lbDockClient1FormShow;
    OnFormHide := lbDockClient1FormHide;
    NCPopupMenu := MainForm.PopupMenu2;
    DirectDrag := True;
    ShowHint := True;
    EnableCloseButton := True;
    EachOtherDock := False;
  end
end;

destructor TRunTimeForm.Destroy;
begin
  MenuItem.Free;
  inherited Destroy;
end;

procedure TRunTimeForm.lbDockClient1FormHide(Sender: TObject);
begin
  if Assigned(MenuItem) then
    MenuItem.Checked := False;
end;

procedure TRunTimeForm.lbDockClient1FormShow(Sender: TObject);
begin
  if Assigned(MenuItem) then
    MenuItem.Checked := True;
end;

procedure TRunTimeForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = MenuItem) and (Operation = opRemove) then
    MenuItem := nil;
end;

procedure TRunTimeForm.SetMenuItem(AMenuItem: TMenuItem);
begin
  if FMenuItem <> nil then
    RemoveFreeNotification(Self);
  FMenuItem := AMenuItem;
  if FMenuItem <> nil then
    FreeNotification(Self);
end;

end.
