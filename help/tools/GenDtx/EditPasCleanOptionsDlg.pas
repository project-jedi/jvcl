unit EditPasCleanOptionsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, JvToolEdit, Mask, ExtCtrls, JvComponent,
  JvAppStorage, JvAppRegistryStorage;

type
  TEditPasCleanSwitch = (epcsDoCapitalization, epcsSortImplementation);
  TEditPasCleanSwitches = set of TEditPasCleanSwitch;

  TEditPasCleanOptions = record
    RCapitalizationFile: string;
    ROutputToSameDir: Boolean;
    ROutputDirectory: string;
    RSwitches: TEditPasCleanSwitches;
  end;

  TfrmEditPasCleanOptions = class(TForm)
    Panel1: TPanel;
    chbCapitalization: TCheckBox;
    Label1: TLabel;
    chbSortImplementationSection: TCheckBox;
    Panel2: TPanel;
    rbtOutputToSameDir: TRadioButton;
    rbtOutputToOtherDir: TRadioButton;
    Label2: TLabel;
    edtCapitalizationFile: TJvFilenameEdit;
    edtOutputDirectory: TJvDirectoryEdit;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    actCancel: TAction;
    actOK: TAction;
    JvAppRegistryStore1: TJvAppRegistryStorage;
    procedure actCancelExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure chbCapitalizationClick(Sender: TObject);
    procedure rbtOutputToOtherDirClick(Sender: TObject);
    procedure rbtOutputToSameDirClick(Sender: TObject);
  private
    procedure Init;
    procedure UpdateEnabled;
    procedure LoadOptions(var Options: TEditPasCleanOptions);
    procedure SaveOptions(const Options: TEditPasCleanOptions);
    procedure CtrlsToData(var Options: TEditPasCleanOptions);
    procedure DataToCtrls(const Options: TEditPasCleanOptions);
  public
    class function Execute(var Options: TEditPasCleanOptions): Boolean;
  end;

implementation

{$R *.dfm}
//=== TfrmEditPasCleanOptions ================================================

procedure TfrmEditPasCleanOptions.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditPasCleanOptions.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmEditPasCleanOptions.chbCapitalizationClick(Sender: TObject);
begin
  UpdateEnabled;
end;

procedure TfrmEditPasCleanOptions.CtrlsToData(
  var Options: TEditPasCleanOptions);
begin
  with Options do
  begin
    RCapitalizationFile := edtCapitalizationFile.FileName;
    ROutputDirectory := edtOutputDirectory.Text;
    ROutputToSameDir := rbtOutputToSameDir.Checked;
    RSwitches := [];
    if chbCapitalization.Checked then
      Include(RSwitches, epcsDoCapitalization);
    if chbSortImplementationSection.Checked then
      Include(RSwitches, epcsSortImplementation);
  end;
end;

procedure TfrmEditPasCleanOptions.DataToCtrls(
  const Options: TEditPasCleanOptions);
begin
  with Options do
  begin
    edtCapitalizationFile.Text := RCapitalizationFile;
    edtOutputDirectory.Text := ROutputDirectory;
    rbtOutputToSameDir.Checked := ROutputToSameDir;
    rbtOutputToOtherDir.Checked := not ROutputToSameDir;
    chbCapitalization.Checked := epcsDoCapitalization in RSwitches;
    chbSortImplementationSection.Checked := epcsSortImplementation in RSwitches;
  end;
end;

class function TfrmEditPasCleanOptions.Execute(var Options: TEditPasCleanOptions): Boolean;
begin
  with TfrmEditPasCleanOptions.Create(Application) do
  try
    Init;
    Result := ShowModal = mrOk;
    if Result then
    begin
      CtrlsToData(Options);
      SaveOptions(Options);
    end;
  finally
    Free;
  end;
end;

procedure TfrmEditPasCleanOptions.Init;
var
  Options: TEditPasCleanOptions;
begin
  LoadOptions(Options);
  DataToCtrls(Options);

  UpdateEnabled;
end;

procedure TfrmEditPasCleanOptions.LoadOptions(
  var Options: TEditPasCleanOptions);
begin
  with Options, JvAppRegistryStore1 do
  begin
    RSwitches := [];
    if Boolean(ReadInteger('PasCleanOptions\DoCapitalization', Integer(True))) then
      Include(RSwitches, epcsDoCapitalization);
    if Boolean(ReadInteger('PasCleanOptions\SortImplementationSection', Integer(True))) then
      Include(RSwitches, epcsSortImplementation);
    RCapitalizationFile := ReadString('PasCleanOptions\CapitalizationFile');
    ROutputToSameDir := Boolean(ReadInteger('PasCleanOptions\OuputToSameDir', Integer(False)));
    ROutputDirectory := ReadString('PasCleanOptions\OutputDirectory');
  end;
end;

procedure TfrmEditPasCleanOptions.rbtOutputToOtherDirClick(
  Sender: TObject);
begin
  UpdateEnabled;
end;

procedure TfrmEditPasCleanOptions.rbtOutputToSameDirClick(Sender: TObject);
begin
  UpdateEnabled;
end;

procedure TfrmEditPasCleanOptions.SaveOptions(
  const Options: TEditPasCleanOptions);
begin
  with Options, JvAppRegistryStore1 do
  begin
    WriteInteger('PasCleanOptions\DoCapitalization', Integer(epcsDoCapitalization in RSwitches));
    WriteString('PasCleanOptions\CapitalizationFile', RCapitalizationFile);
    WriteInteger('PasCleanOptions\SortImplementationSection', Integer(epcsSortImplementation in RSwitches));
    WriteInteger('PasCleanOptions\OuputToSameDir', Integer(ROutputToSameDir));
    WriteString('PasCleanOptions\OutputDirectory', ROutputDirectory);
  end;
end;

procedure TfrmEditPasCleanOptions.UpdateEnabled;
begin
  edtCapitalizationFile.Enabled := chbCapitalization.Checked;
  edtOutputDirectory.Enabled := rbtOutputToOtherDir.Checked;
end;

end.
