unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, JvToolBar, Menus, JvMenus, ExtCtrls,
  StdCtrls, JvListBox, JvCtrls, JvCoolBar, JvSplitter, JvStatusBar,
  JvComCtrls, JvControlBar, ImgList, ActnList, FileWrapper;

type
  TfrmMain = class(TForm)
    jlbItems: TJvListBox;
    pnlItems: TPanel;
    jmnMain: TJvMainMenu;
    jtbMain: TJvToolBar;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    mnuSave: TMenuItem;
    jspVertSplitter: TJvSplitter;
    pnlEdit: TPanel;
    JvStatusBar1: TJvStatusBar;
    jpcEdit: TJvPageControl;
    tshElements: TTabSheet;
    tshRaw: TTabSheet;
    memRaw: TMemo;
    jvcControls: TJvControlBar;
    pnlControls: TPanel;
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
    procedure actExitExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure jlbItemsClick(Sender: TObject);
    procedure memSummaryChange(Sender: TObject);
    procedure memDescriptionChange(Sender: TObject);
    procedure memAuthorChange(Sender: TObject);
  private
    { Private declarations }
    FFileWrapper : TFileWrapper;

    procedure updateItem;
  public
    { Public declarations }
    destructor Destroy; override;
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
var
  i : Integer;
begin
  if odlOpen.Execute then
  begin
    FFileWrapper.Free;
    FFileWrapper := TFileWrapper.Create(odlOpen.FileName);
    cmbStatus.Text := FFileWrapper.Status;
    ledPackage.Text := FFileWrapper.Package;

    // update caption
    Caption := 'DtxEdit - '+odlOpen.FileName;

    // update item list
    jlbItems.Items.Clear;
    for i := 0 to FFileWrapper.Items.Count - 1 do
    begin
      jlbItems.Items.Add(FFileWrapper.Items[i].Name);
    end;

    // activate pages
    tshElements.Enabled := True;
    tshRaw.Enabled := True;
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  //
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
  curItem := FFileWrapper.Items[jlbItems.ItemIndex];

  // raw text first
  memRaw.Text := curItem.GetRawText;

  // then the elements
  memSummary.Text := curItem.Summary;
  memDescription.Text := curItem.Description;
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

end.
