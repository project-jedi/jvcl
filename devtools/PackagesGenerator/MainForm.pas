unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, ComCtrls, JvToolBar, ToolWin, JvCoolBar,
  JvStatusBar, ExtCtrls, JvSplitter, StdCtrls, JvListBox, JvCtrls,
  JvControlBar, ImgList, ActnList, JvComponent, JvBaseDlg, JvBrowseFolder,
  Mask, JvToolEdit, AppEvnts, Grids, JvGrids, JvFormPlacement, JvAppStore,
  JvAppIniStore, JvStringGrid;

type
  TfrmMain = class(TForm)
    jmmMain: TJvMainMenu;
    mnuFile: TMenuItem;
    jsbStatus: TJvStatusBar;
    jtbMenus: TJvToolBar;
    mnuExit: TMenuItem;
    mnuNavigation: TMenuItem;
    pnlList: TPanel;
    jspLeft: TJvSplitter;
    jlbList: TJvListBox;
    jcbMain: TJvControlBar;
    jtbTools: TJvToolBar;
    tbtOpen: TToolButton;
    tbtSave: TToolButton;
    aclActions: TActionList;
    imlActive: TImageList;
    imlDisabled: TImageList;
    actExit: TAction;
    actSave: TAction;
    actNew: TAction;
    actPrevPackage: TAction;
    actNextPackage: TAction;
    tbtExit: TToolButton;
    ToolButton4: TToolButton;
    tbtPrevPackage: TToolButton;
    tbtNextPackage: TToolButton;
    N1: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuPreviousPackage: TMenuItem;
    mnuNextPackage: TMenuItem;
    jbfFolder: TJvBrowseForFolderDialog;
    pnlEdit: TPanel;
    pnlPackagesLocation: TPanel;
    jdePackagesLocation: TJvDirectoryEdit;
    Label1: TLabel;
    aevEvents: TApplicationEvents;
    ledName: TLabeledEdit;
    rbtRuntime: TRadioButton;
    rbtDesign: TRadioButton;
    ledDescription: TLabeledEdit;
    lblDependencies: TLabel;
    jsgDependencies: TJvStringGrid;
    jsgFiles: TJvStringGrid;
    lblFiles: TLabel;
    odlAddFiles: TOpenDialog;
    ledC5PFlags: TLabeledEdit;
    ledC6PFlags: TLabeledEdit;
    actSaveAll: TAction;
    actAddFiles: TAction;
    ToolButton1: TToolButton;
    actGenerate: TAction;
    tbtGenerate: TToolButton;
    jpmGridPopup: TJvPopupMenu;
    mnuView: TMenuItem;
    mnuKnown: TMenuItem;
    N2: TMenuItem;
    actMainToolbar: TAction;
    actLocation: TAction;
    actKnown: TAction;
    mnuMainToolbar: TMenuItem;
    mnuLocationBar: TMenuItem;
    mnuUp: TMenuItem;
    mnuDown: TMenuItem;
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure aevEventsHint(Sender: TObject);
    procedure jsgDependenciesGetCellAlignment(Sender: TJvStringGrid;
      AColumn, ARow: Integer; State: TGridDrawState;
      var CellAlignment: TAlignment);
    procedure jsgDependenciesExitCell(Sender: TJvStringGrid; AColumn,
      ARow: Integer; const EditText: String);
    procedure actAddFilesExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure ledC6PFlagsChange(Sender: TObject);
    procedure ledC5PFlagsChange(Sender: TObject);
    procedure jsgFilesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure jsgDependenciesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure ledDescriptionChange(Sender: TObject);
    procedure ledNameChange(Sender: TObject);
    procedure rbtDesignClick(Sender: TObject);
    procedure rbtRuntimeClick(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actPrevPackageUpdate(Sender: TObject);
    procedure actNextPackageUpdate(Sender: TObject);
    procedure jlbListClick(Sender: TObject);
    procedure actGenerateExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actPrevPackageExecute(Sender: TObject);
    procedure actNextPackageExecute(Sender: TObject);
    procedure actMainToolbarUpdate(Sender: TObject);
    procedure actLocationUpdate(Sender: TObject);
    procedure actMainToolbarExecute(Sender: TObject);
    procedure actLocationExecute(Sender: TObject);
    procedure actKnownExecute(Sender: TObject);
    procedure mnuUpClick(Sender: TObject);
    procedure mnuDownClick(Sender: TObject);
  private
    { Private declarations }
    Changed : Boolean; // true if current file has changed

    procedure LoadPackagesList;
    procedure LoadPackage;
    procedure MoveLine(sg : TStringGrid; direction : Integer);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileUtils, JvSimpleXml, JclFileUtils, JclStrings, TargetDialog,
  GenerateUtils, KnownTagsForm;
{$R *.dfm}

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  // empty everything
  ledName.Text := '';
  ledDescription.Text := '';
  rbtDesign.Checked := False;
  rbtRuntime.Checked := True;
  ledC5PFlags.Text := '';
  ledC6PFlags.Text := '';
  jsgDependencies.RowCount := 2;
  jsgDependencies.Rows[1].Text := '';
  jsgFiles.RowCount := 2;
  jsgFiles.Rows[1].Text := '';
end;

procedure TfrmMain.aevEventsHint(Sender: TObject);
begin
  jsbStatus.Panels[0].Text:= GetLongHint(Application.Hint);
end;

constructor TfrmMain.Create(AOwner: TComponent);
var i : integer;
begin
  inherited;
  with jsgDependencies do
  begin
    Cells[0, 0] := 'Name';
    Cells[1,  0] := 'D5';
    Cells[2,  0] := 'D5s';
    Cells[3,  0] := 'D6';
    Cells[4,  0] := 'D6p';
    Cells[5,  0] := 'D7';
    Cells[6,  0] := 'D7p';
    Cells[7,  0] := 'C5';
    Cells[8,  0] := 'C6';
    Cells[9,  0] := 'C6p';
    Cells[10, 0] := 'K2';
    Cells[11, 0] := 'K3';
    Cells[12, 0] := 'Design';
    Cells[13, 0] := 'Condition';
    ColWidths[0] := 120;
    for i := 1 to 11 do
    begin
      if Length(Cells[i,0]) = 2 then
        ColWidths[i] := 18
      else
        ColWidths[i] := 23;
    end;
    ColWidths[12] := 40;
    ColWidths[13] := 146;
  end;

  with jsgFiles do
  begin
    Cells[0,  0] := 'Name';
    Cells[1,  0] := 'D5';
    Cells[2,  0] := 'D5s';
    Cells[3,  0] := 'D6';
    Cells[4,  0] := 'D6p';
    Cells[5,  0] := 'D7';
    Cells[6,  0] := 'D7p';
    Cells[7,  0] := 'C5';
    Cells[8,  0] := 'C6';
    Cells[9,  0] := 'C6p';
    Cells[10, 0] := 'K2';
    Cells[11, 0] := 'K3';
    Cells[12, 0] := 'Form name';
    Cells[13, 0] := 'Condition';
    ColWidths[0] := 120;
    for i := 1 to 11 do
    begin
      if Length(Cells[i,0]) = 2 then
        ColWidths[i] := 18
      else
        ColWidths[i] := 23;
    end;
    ColWidths[12] := 86;
    ColWidths[13] := 100;
  end;

  // Load the list of packages
  LoadPackagesList;
end;

procedure TfrmMain.jsgDependenciesGetCellAlignment(Sender: TJvStringGrid;
  AColumn, ARow: Integer; State: TGridDrawState;
  var CellAlignment: TAlignment);
begin
  if (ARow > 0) and
     ((AColumn = 0) or
      (AColumn >= 12)) then
    CellAlignment := taLeftJustify
  else
    CellAlignment := taCenter;
end;

procedure TfrmMain.jsgDependenciesExitCell(Sender: TJvStringGrid; AColumn,
  ARow: Integer; const EditText: String);
var
  row : TStrings;
  ColIndex : Integer;
begin
  if AColumn = 0 then
  begin
    if (Sender.RowCount > 2) and
       (Sender.Cells[0, ARow] = '') and
       (ARow < Sender.RowCount-1) then
      Sender.RemoveRow(ARow);

    if (Sender.RowCount > 1) and
       (Sender.Cells[0, Sender.RowCount-1] <> '') then
    begin
      Sender.InsertRow(Sender.RowCount);
      row := Sender.Rows[ARow];
      for ColIndex := 1 to 11 do
        row[ColIndex] := 'y';
      if rbtDesign.Checked and (Sender = jsgDependencies) then
        row[8] := 'y';
    end; 
  end;
end;

procedure TfrmMain.actAddFilesExecute(Sender: TObject);
var
  i : Integer;
  ColIndex : Integer;
  Name : string;
  PackagesDir : string;
  FormName : string;
  FormType : string;
  Dir : string;
  row : TStrings;
  dfm : textfile;
  pas : textfile;
  line : string;
begin
  if odlAddFiles.Execute then
  begin
    if PathIsAbsolute(jdePackagesLocation.Text) then
      PackagesDir := jdePackagesLocation.Text
    else
      PackagesDir := PathNoInsideRelative(StrEnsureSuffix('\', extractfilepath(Application.exename))+jdePackagesLocation.Text);
    for i := 0 to odlAddFiles.Files.Count-1 do
    begin
      row := jsgFiles.InsertRow(jsgFiles.RowCount-1);
      Name := odlAddFiles.Files[i];
      Dir := GetRelativePath(PackagesDir, ExtractFilePath(Name));
      row[0] := '..\' + StrEnsureSuffix('\', Dir) + ExtractFileName(Name);
      for ColIndex := 1 to 11 do
        row[ColIndex] := 'y';

      // try to find if there is a dfm associated with the file
      // if there is one, open it and read the first line to get the
      // name of the form inside it
      if FileExists(ChangeFileExt(Name, '.dfm')) then
      begin
        AssignFile(dfm, ChangeFileExt(Name, '.dfm'));
        Reset(dfm);
        ReadLn(dfm, line);
        CloseFile(dfm);
        FormName := Copy(line, Pos(' ', line)+1, Pos(':', line)-Pos(' ', line)-1);
        FormType := '';
        // open the pas file and look for the declaration of the
        // class associated with that form to get its base type
        AssignFile(pas, Name);
        Reset(pas);
        while (FormType = '') and not Eof(pas) do
        begin
          ReadLn(pas, line);
          line := Trim(line);
          if Copy(line, 2, Length(FormName)+9) = FormName + ' = class(' then
          begin
            FormType := Copy(line, Pos('class(', line)+6, Length(line));
            FormType := Copy(FormType, 1, Length(FormType)-1);
          end;
        end;
        CloseFile(pas);

        // if the form type is TForm or TJvForm then ignore it
        // else include it in the row
        if (FormType = 'TForm') or
           (FormType = 'TJvForm') then
          row[12] := FormName
        else
          row[12] := FormName + ': ' + FormType;
      end;
    end;
    Changed := True;
  end;
end;

procedure TfrmMain.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := (ledName.Text <> '') and Changed;
end;

procedure TfrmMain.ledC6PFlagsChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.ledC5PFlagsChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.jsgFilesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  Changed := True;
end;

procedure TfrmMain.jsgDependenciesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  Changed := True;
end;

procedure TfrmMain.ledDescriptionChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.ledNameChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.rbtDesignClick(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.rbtRuntimeClick(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.LoadPackagesList;
begin
  EnumeratePackages(jdePackagesLocation.Text, jlbList.Items);
  jlbList.ItemIndex := 0;
  LoadPackage;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
var
  xml : TJvSimpleXml;
  i : Integer;
  j : Integer;
  FileName : string;
  propname : string;
  row : TStrings;
  rootNode : TJvSimpleXmlElemClassic;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  filesNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
begin
  if PathIsAbsolute(jdePackagesLocation.Text) then
    FileName := jdePackagesLocation.Text
  else
    FileName := PathNoInsideRelative(StrEnsureSuffix('\', ExtractFilePath(Application.ExeName)) + jdePackagesLocation.Text);
  FileName := FileName + '\xml\' + ledName.Text;
  if rbtDesign.Checked then
    FileName := FileName + '-D.xml'
  else
    FileName := FileName + '-R.xml';
  xml := TJvSimpleXml.Create(nil);
  try
    with xml do
    begin
      Options := [sxoAutoCreate, sxoAutoIndent];
      IndentString := '  ';

      // create root node
      Root.Name := 'Package';
      rootNode := xml.Root;
      rootNode.Properties.Add('Name', ledName.Text);
      rootNode.Properties.Add('Design', rbtDesign.Checked);

      // add description, and PFLAGS
      rootNode.Items.Add('Description', ledDescription.Text);
      rootNode.Items.Add('C5PFlags', ledC5PFlags.Text);
      rootNode.Items.Add('C6PFlags', ledC6PFlags.Text);

      // add required packages
      requiredNode := rootNode.Items.Add('Requires');
      for i := 1 to jsgDependencies.RowCount-2 do
      begin
        row := jsgDependencies.Rows[i];
        packageNode := requiredNode.Items.Add('Package');
        for j := 0 to row.Count - 1 do
        begin
          packageNode.Properties.Add(jsgDependencies.Rows[0][j], row[j]);
        end;
      end;

      // add files
      filesNode := rootNode.Items.Add('Contains');
      for i := 1 to jsgFiles.RowCount-2 do
      begin
        row := jsgFiles.Rows[i];
        fileNode := filesNode.Items.Add('File');
        for j := 0 to row.Count - 1 do
        begin
          propname := jsgFiles.Rows[0][j];
          StrReplace(propname, ' ', '', [rfReplaceAll]);
          fileNode.Properties.Add(propname, row[j]);
        end;
      end;
    end;
    // save into file
    xml.SaveToFile(fileName);
  finally
    xml.Free;
  end;

  // reload package list
  i := jlbList.ItemIndex;
  LoadPackagesList;
  jlbList.ItemIndex := i;
  LoadPackage;
end;

procedure TfrmMain.actPrevPackageUpdate(Sender: TObject);
begin
  actPrevPackage.Enabled := jlbList.ItemIndex > 0;
end;

procedure TfrmMain.actNextPackageUpdate(Sender: TObject);
begin
  actNextPackage.Enabled := (jlbList.ItemIndex > -1) and
                            (jlbList.ItemIndex < jlbList.Count-1);
end;

procedure TfrmMain.LoadPackage;
var
  xml : TJvSimpleXml;
  i : Integer;
  j : Integer;
  xmlFileName : string;
  propname : string;
  row : TStrings;
  rootNode : TJvSimpleXmlElemClassic;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  filesNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
begin
  if PathIsAbsolute(jdePackagesLocation.Text) then
    xmlFileName := jdePackagesLocation.Text
  else
    xmlFileName := PathNoInsideRelative(StrEnsureSuffix('\', ExtractFilePath(Application.ExeName)) + jdePackagesLocation.Text);
  xmlFileName := xmlFileName + '\xml\';
  if rbtDesign.Checked then
    xmlFileName := xmlFileName + jlbList.Items[jlbList.ItemIndex] + '.xml'
  else
    xmlFileName := xmlFileName + jlbList.Items[jlbList.ItemIndex] + '.xml';
  xml := TJvSimpleXml.Create(nil);
  try
    with xml do
    begin
      Options := [sxoAutoCreate];

      LoadFromFile(xmlFileName);

      // read root node
      rootNode := xml.Root;
      ledName.Text      := rootNode.Properties.ItemNamed['Name'].Value;
      rbtDesign.Checked := rootNode.Properties.ItemNamed['Design'].BoolValue;
      rbtRuntime.Checked := not rbtDesign.Checked;

      // read description, and PFLAGS
      ledDescription.Text := rootNode.Items.ItemNamed['Description'].Value;
      ledC5PFlags.Text    := rootNode.Items.ItemNamed['C5PFlags'].Value;
      ledC6PFlags.Text    := rootNode.Items.ItemNamed['C6PFlags'].Value;

      // read required packages
      jsgDependencies.RowCount := 2;
      jsgDependencies.Rows[1].Text := '';
      requiredNode := rootNode.Items.ItemNamed['Requires'];
      for i := 0 to requiredNode.Items.Count - 1 do
      begin
        row := jsgDependencies.InsertRow(jsgDependencies.RowCount-1);
        packageNode := requiredNode.Items[i];
        for j := 0 to row.Count - 1 do
        begin
          row[j] := packageNode.Properties.ItemNamed[jsgDependencies.Rows[0][j]].Value;
        end;
      end;

      // read files
      jsgFiles.RowCount := 2;
      jsgFiles.Rows[1].Text := '';
      filesNode := rootNode.Items.ItemNamed['Contains'];
      for i := 0 to filesNode.Items.Count - 1 do
      begin
        row := jsgFiles.InsertRow(jsgFiles.RowCount-1);
        fileNode := filesNode.Items[i];
        for j := 0 to row.Count - 1 do
        begin
          propname := jsgFiles.Rows[0][j];
          StrReplace(propname, ' ', '', [rfReplaceAll]);
          row[j] := fileNode.Properties.ItemNamed[propname].Value;
        end;
      end;
    end;
    Changed := False;
  finally
    xml.Free;
  end;
end;

procedure TfrmMain.jlbListClick(Sender: TObject);
begin
  LoadPackage;
end;

procedure TfrmMain.actGenerateExecute(Sender: TObject);
var
  path : string;
  targets : TStringList;
  i : Integer;
begin
  if PathIsAbsolute(jdePackagesLocation.Text) then
    path := jdePackagesLocation.Text
  else
    path := PathNoInsideRelative(StrEnsureSuffix('\', ExtractFilePath(Application.ExeName)) + jdePackagesLocation.Text);

  frmTargets.Path := path;
  if frmTargets.ShowModal = mrOk then
  begin
    targets := TStringList.Create;
    try
      for i := 0 to frmTargets.clbBuilds.Items.Count - 1 do
      begin
        with frmTargets.clbBuilds do
        begin
          if Checked[i] then
            targets.Add(Items[i]);
        end;
      end;

      Generate(jlbList.Items, targets, path);
    finally
      targets.Free;
    end;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    case Key of
      VK_UP:
        begin
          if actPrevPackage.Enabled then
            actPrevPackage.Execute;
          Key := 0;
        end;
      VK_DOWN:
        begin
          if actNextPackage.Enabled then
            actNextPackage.Execute;
          Key := 0;
        end;
    end;
  end;
end;

procedure TfrmMain.actPrevPackageExecute(Sender: TObject);
begin
  jlbList.ItemIndex := jlbList.ItemIndex - 1;
  LoadPackage;
end;

procedure TfrmMain.actNextPackageExecute(Sender: TObject);
begin
  jlbList.ItemIndex := jlbList.ItemIndex + 1;
  LoadPackage;
end;

procedure TfrmMain.actMainToolbarUpdate(Sender: TObject);
begin
  actMainToolbar.Checked := jtbTools.Visible;
end;

procedure TfrmMain.actLocationUpdate(Sender: TObject);
begin
  actLocation.Checked := pnlPackagesLocation.Visible;
end;

procedure TfrmMain.actMainToolbarExecute(Sender: TObject);
begin
  jtbTools.Visible := actMainToolbar.Checked;
end;

procedure TfrmMain.actLocationExecute(Sender: TObject);
begin
  pnlPackagesLocation.Visible := actLocation.Checked;
end;

procedure TfrmMain.actKnownExecute(Sender: TObject);
begin
  frmKnownTags.ShowModal;
end;

procedure TfrmMain.MoveLine(sg : TStringGrid; direction : Integer);
var
  tmpRow : TStrings;
  RowIndex : Integer;
begin
  RowIndex := sg.Row;
  if not ((RowIndex + direction = 0) or
          (RowIndex + direction = sg.RowCount - 1)) then
  begin
    tmpRow := TStringList.Create;
    try
      tmpRow.Assign(sg.Rows[RowIndex]);
      sg.Rows[RowIndex] := sg.Rows[RowIndex + direction];
      sg.Rows[RowIndex + direction] := tmpRow;
      Changed := True;
    finally
      tmpRow.Free;
    end;
  end;
end;

procedure TfrmMain.mnuUpClick(Sender: TObject);
begin
  if ActiveControl is TInPlaceEdit then
  begin
    MoveLine((ActiveControl as TInPlaceEdit).Parent as TStringGrid, -1);
  end;
end;

procedure TfrmMain.mnuDownClick(Sender: TObject);
begin
  if ActiveControl is TInPlaceEdit then
  begin
    MoveLine((ActiveControl as TInPlaceEdit).Parent as TStringGrid, +1);
  end;
end;

end.
