unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, JvToolBar, Menus, JvMenus, ExtCtrls,
  StdCtrls, JvListBox, JvCtrls, JvCoolBar, JvSplitter, JvStatusBar,
  JvComCtrls, JvControlBar, ImgList, ActnList, FileWrapper, AppEvnts,
  JvComponent, JvMRUList, JvFormPlacement, JvAppStorage, JvAppRegistryStorage,
  JvMRUManager, JvExComCtrls, JvExExtCtrls, JvExStdCtrls;

type
  TfrmMain = class(TForm)
    jlbItems: TJvListBox;
    pnlItems: TPanel;
    jmnMain: TJvMainMenu;
    jtbMain: TJvToolBar;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    sepBeforeMRU: TMenuItem;
    mnuExit: TMenuItem;
    mnuSave: TMenuItem;
    jspVertSplitter: TJvSplitter;
    pnlEdit: TPanel;
    jsbStatus: TJvStatusBar;
    jpcEdit: TJvPageControl;
    tshElements: TTabSheet;
    tshRaw: TTabSheet;
    memRaw: TMemo;
    jvcControls: TJvControlBar;
    pnlFileInfo: TPanel;
    ledPackage: TLabeledEdit;
    mnuNavigate: TMenuItem;
    mnuNextItem: TMenuItem;
    mnuPrevItem: TMenuItem;
    lblStatus: TLabel;
    cmbStatus: TComboBox;
    jtbButtons: TJvToolBar;
    aclActions: TActionList;
    imlImages: TImageList;
    actExit: TAction;
    actOpen: TAction;
    actSave: TAction;
    actNextItem: TAction;
    actPrevItem: TAction;
    imlDisabledImages: TImageList;
    tbtNextItem: TToolButton;
    tbtPrevItem: TToolButton;
    tbtOpen: TToolButton;
    tbtSave: TToolButton;
    tbtExit: TToolButton;
    ToolButton6: TToolButton;
    odlOpen: TOpenDialog;
    lblSummary: TLabel;
    memSummary: TMemo;
    memDescription: TMemo;
    lblDescription: TLabel;
    lblAuthor: TLabel;
    memAuthor: TMemo;
    aevAppEvents: TApplicationEvents;
    shCaptionHider: TShape;
    mnuView: TMenuItem;
    mnuToolBar: TMenuItem;
    mnuFileInfo: TMenuItem;
    actToolBar: TAction;
    actFileInfo: TAction;
    mnuSaveAs: TMenuItem;
    actSaveAs: TAction;
    sdlSave: TSaveDialog;
    lblReturnValue: TLabel;
    memReturnValue: TMemo;
    memParameters: TMemo;
    lblParameters: TLabel;
    memSeeAlso: TMemo;
    lblSeeAlso: TLabel;
    lblJVCLInfo: TLabel;
    memJVCLInfo: TMemo;
    lblPreText: TLabel;
    memPreText: TMemo;
    jvmRecentFiles: TJvMRUList;
    sepAfterMRU: TMenuItem;
    JvAppRegistryStorage: TJvAppRegistryStorage;
    JvFormStorage: TJvFormStorage;
    actRaw: TAction;
    actElements: TAction;
    mnuRawText: TMenuItem;
    mnuElements: TMenuItem;
    procedure actExitExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure jlbItemsClick(Sender: TObject);
    procedure memSummaryChange(Sender: TObject);
    procedure memDescriptionChange(Sender: TObject);
    procedure memAuthorChange(Sender: TObject);
    procedure actNextItemUpdate(Sender: TObject);
    procedure actNextItemExecute(Sender: TObject);
    procedure actPrevItemUpdate(Sender: TObject);
    procedure actPrevItemExecute(Sender: TObject);
    procedure aevAppEventsHint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actFileInfoUpdate(Sender: TObject);
    procedure actFileInfoExecute(Sender: TObject);
    procedure actToolBarExecute(Sender: TObject);
    procedure actToolBarUpdate(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure memReturnValueChange(Sender: TObject);
    procedure memParametersChange(Sender: TObject);
    procedure memSeeAlsoChange(Sender: TObject);
    procedure memJVCLInfoChange(Sender: TObject);
    procedure memPreTextChange(Sender: TObject);
    procedure tshRawShow(Sender: TObject);
    procedure actRawExecute(Sender: TObject);
    procedure actElementsExecute(Sender: TObject);
    procedure cmbStatusChange(Sender: TObject);
    procedure ledPackageChange(Sender: TObject);
  private
    { Private declarations }
    FFileWrapper : TFileWrapper;

    procedure updateItem;
    procedure updateMRUMenu(Sender : TObject);

    procedure recentFileClick(Sender : TObject);
    procedure openFile(FileName : string);
  public
    { Public declarations }
    destructor Destroy; override;
    constructor Create(AOwner : TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

uses FileItem;

{$R *.dfm}

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if odlOpen.Execute then
  begin
    openFile(odlOpen.FileName);
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  FFileWrapper.Save;
end;

destructor TfrmMain.Destroy;
begin
  FFileWrapper.Free;
  inherited;
end;

procedure TfrmMain.jlbItemsClick(Sender: TObject);
begin
  updateItem;
end;

procedure TfrmMain.updateItem;
var
  curItem : TFileItem;
begin
  if jlbItems.ItemIndex < 0 then
    jlbItems.ItemIndex := 0;
  if jlbItems.ItemIndex >= FFileWrapper.Items.Count then
    jlbItems.ItemIndex := FFileWrapper.Items.Count-1 ;

  if jlbItems.ItemIndex >= 0 then
  begin
    curItem := FFileWrapper.Items[jlbItems.ItemIndex];

    // raw text first
    memRaw.Text := curItem.GetRawText;

    // then the elements
    memSummary.Text := curItem.Summary;
    memAuthor.Text := curItem.Author;
    memDescription.Text := curItem.Description;
    memParameters.Text := curItem.Parameters;
    memReturnValue.Text := curItem.ReturnValue;
    memSeeAlso.Text := curItem.SeeAlsoAsString;
    memJVCLInfo.Text := curItem.JVCLInfo;
    memPreText.Text := curItem.PreText;
  end;
end;

procedure TfrmMain.memSummaryChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].Summary := memSummary.Text;
end;

procedure TfrmMain.memDescriptionChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].Description := memDescription.Text;
end;

procedure TfrmMain.memAuthorChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].Author := memAuthor.Text;
end;

procedure TfrmMain.actNextItemUpdate(Sender: TObject);
begin
  actNextItem.Enabled := (jlbItems.ItemIndex < jlbItems.Count-1) and
                         (jlbItems.ItemIndex > -1);
end;

procedure TfrmMain.actNextItemExecute(Sender: TObject);
begin
  jlbItems.ItemIndex := jlbItems.ItemIndex + 1;
  updateItem;
end;

procedure TfrmMain.actPrevItemUpdate(Sender: TObject);
begin
  actPrevItem.Enabled := jlbItems.ItemIndex > 0;
end;

procedure TfrmMain.actPrevItemExecute(Sender: TObject);
begin
  jlbItems.ItemIndex := jlbItems.ItemIndex - 1;
  updateItem;
end;

procedure TfrmMain.aevAppEventsHint(Sender: TObject);
var longHint : string;
begin
  if Sender is TApplication then
  begin
    longHint := GetLongHint((Sender as TApplication).Hint);
    jsbStatus.Panels[0].Text := longHint;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    case Key of
      VK_UP:
        if actPrevItem.Enabled then
        begin
          actPrevItem.Execute;
          Key := 0;
        end;
      VK_DOWN:
        if actNextItem.Enabled then
        begin
          actNextItem.Execute;
          Key := 0;
        end;
{      VK_RIGHT:
        begin
          jpcEdit.ActivePageIndex := 1;
          Key := 0;
        end;
      VK_LEFT:
        begin
          jpcEdit.ActivePageIndex := 0;
          Key := 0;
        end;}
    end;
  end;
end;

procedure TfrmMain.actFileInfoUpdate(Sender: TObject);
begin
  actFileInfo.Checked := pnlFileInfo.Visible;
end;

procedure TfrmMain.actFileInfoExecute(Sender: TObject);
begin
  pnlFileInfo.Visible := actFileInfo.Checked;
end;

procedure TfrmMain.actToolBarUpdate(Sender: TObject);
begin
  actToolBar.Checked := jtbButtons.Visible;
end;

procedure TfrmMain.actToolBarExecute(Sender: TObject);
begin
  jtbButtons.Visible := actToolBar.Checked;
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
begin
  if sdlSave.Execute then
  begin
    FFileWrapper.SaveAs(sdlSave.FileName);
  end;
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FFileWrapper <> nil;
end;

procedure TfrmMain.memReturnValueChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].ReturnValue := memReturnValue.Text;
end;

procedure TfrmMain.memParametersChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].Parameters := memParameters.Text;
end;

procedure TfrmMain.memSeeAlsoChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].SeeAlsoAsString := memSeeAlso.Text;
end;

procedure TfrmMain.memJVCLInfoChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].JVCLInfo := memJVCLInfo.Text;
end;

procedure TfrmMain.memPreTextChange(Sender: TObject);
begin
  FFileWrapper.Items[jlbItems.ItemIndex].PreText := memPreText.Text;
end;

procedure TfrmMain.tshRawShow(Sender: TObject);
begin
  if (jlbItems.ItemIndex <> -1) then
    memRaw.Text := FFileWrapper.Items[jlbItems.ItemIndex].GetRawText;
end;

procedure TfrmMain.updateMRUMenu(Sender : TObject);
var
  mruStartIndex : Integer;
  mruEndIndex : Integer;
  i : Integer;
  parentItem : TMenuItem;
  menuItem : TMenuItem;
begin
  parentItem := (Sender as TMenuItem).Parent;
  mruStartIndex := parentItem.IndexOf(sepBeforeMRU)+1;
  mruEndIndex := parentItem.IndexOf(sepAfterMRU);

  if mruEndIndex = mruStartIndex then
    parentItem.Items[mruEndIndex].Visible := False
  else
    parentItem.Items[mruEndIndex].Visible := True;
  // start by removing all items
  for i := 0 to mruEndIndex-mruStartIndex-1 do
  begin
    parentItem.Items[mruStartIndex].Free;
  end;

  // then enumerate all existing items
  for i := jvmRecentFiles.GetItemsCount-1 downto 0 do
  begin
    menuItem := TMenuItem.Create(self);
    jvmRecentFiles.GetItem(i);
    menuItem.Caption := jvmRecentFiles.ItemDataAsPChar;
    menuItem.OnClick := recentFileClick;
    menuItem.Tag := i;
    parentItem.Insert(mruStartIndex, menuItem);
  end;
  if jvmRecentFiles.GetItemsCount > 0 then
    sepAfterMRU.Visible := True;
    //parentItem.Items[mruEndIndex].Visible := True;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;

  jvmRecentFiles.Open;
  
  updateMRUMenu(mnuOpen);
end;

procedure TfrmMain.recentFileClick(Sender: TObject);
begin
  jvmRecentFiles.GetItem((Sender as TComponent).Tag);
  openFile(jvmRecentFiles.ItemDataAsPChar);
end;

procedure TfrmMain.openFile(FileName: string);
var
  i : Integer;
begin
  FFileWrapper.Free;
  FFileWrapper := TFileWrapper.Create(FileName);
  cmbStatus.Text := FFileWrapper.Status;
  ledPackage.Text := FFileWrapper.Package;

  // update caption
  Caption := 'DtxEdit - '+FileName;

  // update item list
  jlbItems.Items.Clear;
  for i := 0 to FFileWrapper.Items.Count - 1 do
  begin
    jlbItems.Items.Add(FFileWrapper.Items[i].Name);
  end;

  // activate editing
  jlbItems.ItemIndex := 0;
  tshElements.Enabled := True;
  tshRaw.Enabled := True;
  updateItem;

  // keep track of recent files
  jvmRecentFiles.AddString(FileName);
  updateMRUMenu(mnuOpen);
end;

procedure TfrmMain.actRawExecute(Sender: TObject);
begin
  jpcEdit.ActivePage := tshRaw;
end;

procedure TfrmMain.actElementsExecute(Sender: TObject);
begin
  jpcEdit.ActivePage := tshElements;
end;

procedure TfrmMain.cmbStatusChange(Sender: TObject);
begin
  FFileWrapper.Status := cmbStatus.Text;
end;

procedure TfrmMain.ledPackageChange(Sender: TObject);
begin
  FFileWrapper.Package :=  ledPackage.Text;
end;

end.
