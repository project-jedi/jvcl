{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, JvQXPCore, JvQXPBar, JvQXPContainer, QImgList, QActnList, QExtCtrls,
  QStdCtrls, QComCtrls, JvQExControls, JvQComponent, JvQXPCheckCtrls,
  JvQXPButtons;

resourcestring
  SClickEvent =
    '  You clicked on the action "%s"...';

type
  TProcControl = procedure (Control:TControl) of object;
  TfrmMain = class(TForm)
    acConnectAdministrator: TAction;
    acConnectLocalServer: TAction;
    acConnectRemoteServer: TAction;
    aclWinXPBar: TActionList;
    acSettingsDatabase: TAction;
    acSettingsDownloads: TAction;
    acSettingsStatistics: TAction;
    acSettingsUsers: TAction;
    acSynchronizeUnknown: TAction;
    acSynchronizeWeb: TAction;
    btnCollapseAll: TJvXPButton;
    btnExpandAll: TJvXPButton;
    cntDetails: TJvXPContainer;
    cntWinXPBar: TJvXPContainer;
    imlWinXPBar: TImageList;
    lbWelcome: TLabel;
    sbxWinXPBar: TScrollBox;
    spltMain: TSplitter;
    btnToogleEnableMode: TJvXPButton;
    btnToggleVisibleMode: TJvXPButton;
    ilOldButtons: TImageList;
    chkGrouped: TJvXPCheckbox;
    ilWhiteButtons: TImageList;
    ilRedButtons: TImageList;
    ilBlackButtons: TImageList;
    acGettingStarted: TAction;
    acHelp: TAction;
    acHowDoI: TAction;
    acCommonQuestions: TAction;
    JvXPContainer1: TJvXPContainer;
    dxWinXPBar4: TJvXPBar;
    JvXPBar1: TJvXPBar;
    dxWinXPBar3: TJvXPBar;
    dxWinXPBar2: TJvXPBar;
    dxWinXPBar1: TJvXPBar;
    tvSelfView: TTreeView;
    StatusBar1: TStatusBar;
    ilMSN: TImageList;
    ilMSN2: TImageList;
    ilFB: TImageList;
    procedure acConnectRemoteServerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnToogleEnableModeClick(Sender: TObject);
    procedure btnToggleVisibleModeClick(Sender: TObject);
    procedure chkGroupedClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoGrouped(Control:TControl);
    procedure DoExpandAll(Control:TControl);
    procedure DoCollapseAll(Control:TControl);
    procedure DoEnableToggle(Control:TControl);
    procedure DoVisibleToggle(Control:TControl);
    procedure IterateControls(Proc:TProcControl);
    procedure BuildStructure;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.xfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cntDetails.Align := alClient;
  BuildStructure;
end;

procedure TfrmMain.acConnectRemoteServerExecute(Sender: TObject);
begin
  with TAction(Sender) do
    StatusBar1.Panels[0].Text :=  Format(SClickEvent, [Name]);
end;

procedure TfrmMain.btnCollapseAllClick(Sender: TObject);
begin
  IterateControls(DoCollapseAll);
end;


procedure TfrmMain.btnExpandAllClick(Sender: TObject);
begin
  IterateControls(DoExpandAll);
end;

procedure TfrmMain.btnToogleEnableModeClick(Sender: TObject);
begin
  IterateControls(DoEnableToggle);
end;

procedure TfrmMain.btnToggleVisibleModeClick(Sender: TObject);
begin
  IterateControls(DoVisibleToggle);
end;

procedure TfrmMain.chkGroupedClick(Sender: TObject);
begin
  IterateControls(DoGrouped);
  btnExpandAll.Enabled := not chkGrouped.Checked;
end;

procedure TfrmMain.DoGrouped(Control: TControl);
begin
  if Control is TJvXPBar then
    TJvXPBar(Control).Grouped := chkGrouped.Checked;
end;

procedure TfrmMain.DoExpandAll(Control: TControl);
begin
  if Control is TJvXPBar then
    TJvXPBar(COntrol).Collapsed := false;
end;

procedure TfrmMain.IterateControls(Proc: TProcControl);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TControl then
      Proc(TControl(Components[i]));
end;

procedure TfrmMain.DoCollapseAll(Control: TControl);
begin
  if Control is TJvXPBar then
    TJvXPBar(COntrol).Collapsed := true;
end;

procedure TfrmMain.DoEnableToggle(Control: TControl);
var i:integer;
begin
  if Control is TJvXPBar then
    for i := 0 to TJvXPBar(Control).Items.Count - 1 do
      if Odd(i) then
        TJvXPBar(Control).Items[i].Enabled := not TJvXPBar(Control).Items[i].Enabled;
end;

procedure TfrmMain.DoVisibleToggle(Control: TControl);
var i:integer;
begin
  if Control is TJvXPBar then
    for i := 0 to TJvXPBar(Control).Items.Count - 1 do
      if Odd(i) then
        TJvXPBar(Control).Items[i].Visible := not TJvXPBar(Control).Items[i].Visible;
end;

procedure TfrmMain.BuildStructure;
var
  i,j:integer;
  Parent, Child:TTreeNode;
  ABar:TJvXPBar;
begin
  tvSelfView.Items.Clear;
  for i := ComponentCount - 1 downto 0 do
    if Components[i] is TJvXPBar then
    begin
      ABar := TJvXPBar(Components[i]);
      Parent := tvSelfView.Items.AddChild(nil,ABar.Caption);
      if ABar.ControlCount = 0 then
      for j := 0 to ABar.Items.Count - 1 do
      begin
        Child := tvSelfView.Items.AddChild(Parent,ABar.Items[j].Caption);
        Child.ImageIndex := ABar.Items[j].ImageIndex;
        Child.SelectedIndex := Child.ImageIndex;
      end;
    end;
  tvSelfView.FullExpand;
end;

end.

