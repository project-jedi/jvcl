unit ModelsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, JvToolEdit, Mask, Grids, JvStringGrid,
  JvSimpleXml, JvExGrids, JvExMask;

type
  TfrmModels = class(TForm)
    btnOk: TBitBtn;
    bttCancel: TBitBtn;
    lblSelectModel: TLabel;
    cmbModels: TComboBox;
    grpProperties: TGroupBox;
    lblName: TLabel;
    edtName: TEdit;
    lblPrefix: TLabel;
    lblFormat: TLabel;
    lblPackageLocation: TLabel;
    lblIncFile: TLabel;
    edtPrefix: TEdit;
    edtFormat: TEdit;
    jfeIncFile: TJvFilenameEdit;
    jdePackages: TJvDirectoryEdit;
    stgTargets: TJvStringGrid;
    lblTargets: TLabel;
    lblAliases: TLabel;
    stgAliases: TJvStringGrid;
    btnAdd: TButton;
    btnDelete: TButton;
    procedure stgTargetsGetCellAlignment(Sender: TJvStringGrid; AColumn,
      ARow: Integer; State: TGridDrawState; var CellAlignment: TAlignment);
    procedure stgTargetsExitCell(Sender: TJvStringGrid; AColumn,
      ARow: Integer; const EditText: String);
    procedure cmbModelsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    xml : TJvSimpleXML;
    filename : string;
    FEditIndex: integer;

    procedure SaveModel;
    procedure LoadModel;
    procedure RefreshModelList;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadModels(XmlFileName : string);

    property EditIndex : integer read FEditIndex write FEditIndex;
  end;

var
  frmModels: TfrmModels;

implementation

{$R *.dfm}

{ TfrmModels }

procedure TfrmModels.LoadModels(XmlFileName : string);
begin
  self.filename := XmlFileName;
  // read XML file
  xml.LoadFromFile(XmlFileName);

  RefreshModelList;

  btnDelete.Enabled := cmbModels.Items.Count > 1;
end;

constructor TfrmModels.Create(AOwner: TComponent);
begin
  inherited;
  xml := TJvSimpleXml.Create(nil);
  FEditIndex := -1;

  with stgTargets do
  begin
    Cells[0, 0] := 'name';
    Cells[1, 0] := 'dir';
    Cells[2, 0] := 'pname';
    Cells[3, 0] := 'pdir';
    Cells[4, 0] := 'env';
    Cells[5, 0] := 'ver';

    ColWidths[0] := 40;
    ColWidths[1] := 40;
    ColWidths[2] := 40;
    ColWidths[3] := 40;
    ColWidths[4] := 30;
    ColWidths[5] := 30;
  end;

  with stgAliases do
  begin
    Cells[0, 0] := 'name';
    Cells[1, 0] := 'value';

    ColWidths[0] := 60;
    ColWidths[1] := 130;
  end;

end;

procedure TfrmModels.stgTargetsGetCellAlignment(Sender: TJvStringGrid;
  AColumn, ARow: Integer; State: TGridDrawState;
  var CellAlignment: TAlignment);
begin
  if ARow = 0 then
    CellAlignment := taCenter
  else
    CellAlignment := taLeftJustify;
end;

procedure TfrmModels.stgTargetsExitCell(Sender: TJvStringGrid; AColumn,
  ARow: Integer; const EditText: String);
begin
  if AColumn = 0 then
  begin
    if (Sender.RowCount > 2) and
       (Sender.Cells[0, ARow] = '') and
       (ARow < Sender.RowCount-1) then
      Sender.RemoveRow(ARow);
  end;
  if (Sender.RowCount <=1) or
     (Sender.Cells[0, Sender.RowCount-1] <> '') then
  begin
    Sender.InsertRow(Sender.RowCount);
    Sender.FixedRows := 1;
  end;
end;

destructor TfrmModels.Destroy;
begin
  xml.Free;
  inherited;
end;

procedure TfrmModels.cmbModelsClick(Sender: TObject);
begin
  SaveModel;
  LoadModel;
end;

procedure TfrmModels.LoadModel;
var
  model : TJvSimpleXmlElem;
  target : TJvSimpleXmlElem;
  alias : TJvSimpleXmlElem;
  row : TStrings;
  i : integer;
begin
  model := xml.root.Items.ItemNamed['Models'].Items[0];
  for i := 0 to xml.root.Items.ItemNamed['Models'].Items.Count - 1 do
  begin
    if xml.root.Items.ItemNamed['Models'].Items[i].Properties.ItemNamed['Name'].value = cmbModels.Text then
      model := xml.root.Items.ItemNamed['Models'].Items[i];
  end;

  // generic properties
  edtFormat.Text := model.Properties.ItemNamed['format'].value;
  edtName.Text := model.Properties.ItemNamed['name'].value;
  edtPrefix.Text := model.Properties.ItemNamed['prefix'].value;
  jdePackages.Text := model.Properties.ItemNamed['packages'].value;
  jfeIncFile.Text := model.Properties.ItemNamed['incfile'].value;

  // targets
  stgTargets.RowCount := 2;
  stgTargets.Rows[1].Text := '';
  for i := 0 to model.Items.ItemNamed['targets'].Items.count - 1 do
  begin
    target := model.Items.ItemNamed['targets'].Items[i];
    row := stgTargets.Rows[stgTargets.RowCount-1];
    row[0] := target.properties.ItemNamed['name'].value;
    if Assigned(target.properties.ItemNamed['dir']) then
      row[1] := target.properties.ItemNamed['dir'].value;
    if Assigned(target.properties.ItemNamed['pname']) then
      row[2] := target.properties.ItemNamed['pname'].value;
    if Assigned(target.properties.ItemNamed['pdir']) then
      row[3] := target.properties.ItemNamed['pdir'].value;
    if Assigned(target.properties.ItemNamed['env']) then
      row[4] := target.properties.ItemNamed['env'].value;
    if Assigned(target.properties.ItemNamed['ver']) then
      row[5] := target.properties.ItemNamed['ver'].value;

    stgTargets.InsertRow(stgTargets.RowCount);
  end;

  // aliases
  stgAliases.RowCount := 2;
  stgAliases.Rows[1].Text := '';
  for i := 0 to model.Items.ItemNamed['aliases'].Items.count - 1 do
  begin
    alias := model.Items.ItemNamed['aliases'].Items[i];
    row := stgAliases.Rows[stgAliases.RowCount-1];
    row[0] := alias.properties.ItemNamed['name'].value;
    row[1] := alias.properties.ItemNamed['value'].value;

    stgAliases.InsertRow(stgAliases.RowCount);
  end;
end;

procedure TfrmModels.SaveModel;
var
  model : TJvSimpleXmlElem;
  target : TJvSimpleXmlElem;
  alias : TJvSimpleXmlElem;
  row : TStrings;
  i : integer;
begin
  if edtName.Text <> '' then
  begin
    model := xml.root.Items.ItemNamed['Models'].Items[0];
    for i := 0 to xml.root.Items.ItemNamed['Models'].Items.Count - 1 do
    begin
      if xml.root.Items.ItemNamed['Models'].Items[i].Properties.ItemNamed['Name'].value = edtName.Text then
        model := xml.root.Items.ItemNamed['Models'].Items[i];
    end;

    // generic properties
    model.Properties.ItemNamed['format'].value := edtFormat.Text;
    model.Properties.ItemNamed['name'].value := edtName.Text;
    model.Properties.ItemNamed['prefix'].value := edtPrefix.Text;
    model.Properties.ItemNamed['packages'].value := jdePackages.Text;
    model.Properties.ItemNamed['incfile'].value := jfeIncFile.Text;

    // targets
    model.Items.ItemNamed['targets'].Items.Clear;
    for i := 1 to stgTargets.RowCount-2 do
    begin
      target := model.Items.ItemNamed['targets'].Items.Add('target');

      row := stgTargets.Rows[i];

      target.properties.Add('name', row[0]);
      if row[1] <> '' then
        target.properties.Add('dir', row[1]);
      if row[2] <> '' then
        target.properties.Add('pname', row[2]);
      if row[3] <> '' then
        target.properties.Add('pdir', row[3]);
      if row[4] <> '' then
        target.properties.Add('env', row[4]);
      if row[5] <> '' then
        target.properties.Add('ver', row[5]);
    end;

    // aliases
    model.Items.ItemNamed['aliases'].Items.Clear;
    for i := 1 to stgAliases.RowCount-2 do
    begin
      alias := model.Items.ItemNamed['aliases'].Items.Add('alias');

      row := stgAliases.Rows[i];

      alias.properties.Add('name', row[0]);
      alias.properties.Add('value', row[1]);
    end;
  end;
end;

procedure TfrmModels.btnDeleteClick(Sender: TObject);
begin
  if Application.MessageBox('Are you sure you want to delete this model?', 'Delete confirmation', MB_ICONQUESTION+MB_YESNO) = MRYES then
  begin
    xml.root.Items.ItemNamed['Models'].Items.Delete(cmbModels.Text);
    RefreshModelList;
  end;
end;

procedure TfrmModels.btnAddClick(Sender: TObject);
var
  ModelName : string;
  Model : TJvSimpleXmlElem;
begin
  ModelName := InputBox('Adding a model', 'Please indicate the name of the model to add', '');
  if ModelName <> '' then
  begin
    model := xml.root.Items.ItemNamed['Models'].Items.Add('Model');
    model.Properties.Add('Name', ModelName);
    model.Properties.Add('format', '');
    model.Properties.Add('prefix', '');
    model.Properties.Add('packages', '');
    model.Properties.Add('incfile', '');
    model.Items.Add('targets');
    model.Items.Add('aliases');
    RefreshModelList;
  end;
end;

procedure TfrmModels.btnOkClick(Sender: TObject);
begin
  xml.SaveToFile(filename);
  ModalResult := mrOk;
end;

procedure TfrmModels.RefreshModelList;
var
  models : TJvSimpleXMLElem;
  i : integer;
  savedIndex : integer;
begin
  with xml do
  begin
    models := root.Items.ItemNamed['Models'];
    savedIndex := cmbModels.ItemIndex;
    cmbModels.Items.Clear;
    if Assigned(models) then
    begin
      for i := 0 to models.Items.Count - 1 do
        cmbModels.Items.Add(models.Items[i].Properties.ItemNamed['Name'].Value);
    end;
    cmbModels.ItemIndex := savedIndex;
  end;
end;

procedure TfrmModels.FormShow(Sender: TObject);
begin
  cmbModels.ItemIndex := EditIndex;
  LoadModel;
end;

end.
