unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, JvComponent, JvTrayIcon, ExtCtrls;

type
  TfrmMain = class(TForm)
    JvTrayIcon1: TJvTrayIcon;
    GroupBox1: TGroupBox;
    chkActive: TCheckBox;
    Label1: TLabel;
    edHint: TEdit;
    chkSnap: TCheckBox;
    chkTaskBar: TCheckBox;
    chkTaskList: TCheckBox;
    chkAutoHide: TCheckBox;
    chkRestoreClick: TCheckBox;
    chkRestoreDblClick: TCheckBox;
    chkMinClick: TCheckBox;
    chkMinDblClick: TCheckBox;
    popTrayIcon: TPopupMenu;
    mnuShowHide: TMenuItem;
    chkPopUp: TCheckBox;
    btnUpdate: TButton;
    chkDropDown: TCheckBox;
    chkAutoRestore: TCheckBox;
    RestoreTimer: TTimer;
    procedure btnUpdateClick(Sender: TObject);
    procedure mnuShowHideClick(Sender: TObject);
    procedure chkRestoreClickClick(Sender: TObject);
    procedure chkRestoreDblClickClick(Sender: TObject);
    procedure chkMinClickClick(Sender: TObject);
    procedure chkMinDblClickClick(Sender: TObject);
    procedure RestoreTimerTimer(Sender: TObject);
    procedure chkAutoRestoreClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnUpdateClick(Sender: TObject);
var Options:TTrayVisibilities;
begin
  with JvTrayIcon1 do
  begin
    Active := false;
    Hint := edHint.Text;
    Snap := chkSnap.Checked;
    if chkPopUp.Checked then
      PopUpMenu := popTrayIcon
    else
      PopUpMenu := nil;
    if chkDropDown.Checked then
      DropDownMenu := popTrayIcon
    else
      DropDownMenu := nil;
    Options := [];
    if chkTaskBar.Checked then
      Include(Options,tvVisibleTaskBar);
    if chkTaskList.Checked then
      Include(Options,tvVisibleTaskList);
    if chkAutohide.Checked then
      Include(Options,tvAutoHide);
    if chkRestoreClick.Checked and chkRestoreClick.Enabled then
      Include(Options,tvRestoreClick);
    if chkRestoreDblClick.Checked and chkRestoreDblClick.Enabled then
      Include(Options,tvRestoreDbClick);
    if chkMinClick.Checked and chkMinClick.Enabled then
      Include(Options,tvMinimizeClick);
    if chkMinDblClick.Checked and chkMinDblClick.Enabled then
      Include(Options,tvMinimizeDbClick);
    Visibility := Options;
    Active := chkActive.Checked;
  end;
end;

procedure TfrmMain.mnuShowHideClick(Sender: TObject);
begin
  if IsWindowVisible(Handle) then
    JvTrayIcon1.HideApplication
  else
    JvTrayIcon1.ShowApplication;
end;

procedure TfrmMain.chkRestoreClickClick(Sender: TObject);
begin
  chkRestoreDblClick.Enabled := not chkRestoreClick.Checked;
end;

procedure TfrmMain.chkRestoreDblClickClick(Sender: TObject);
begin
  chkRestoreClick.Enabled := not chkRestoreDblClick.Checked;
end;

procedure TfrmMain.chkMinClickClick(Sender: TObject);
begin
  chkMinDblClick.Enabled := not chkMinClick.Checked;
end;

procedure TfrmMain.chkMinDblClickClick(Sender: TObject);
begin
  chkMinClick.Enabled := not chkMinDblClick.Checked;
end;

procedure TfrmMain.RestoreTimerTimer(Sender: TObject);
begin
  if not IsWindowVisible(Handle) then
    JvTrayIcon1.ShowApplication;
end;

procedure TfrmMain.chkAutoRestoreClick(Sender: TObject);
begin
  RestoreTimer.Enabled := not chkAutoRestore.Checked;
end;

end.
