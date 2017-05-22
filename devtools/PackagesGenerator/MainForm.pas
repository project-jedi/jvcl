unit MainForm;

{$I jvcl.inc}

interface    

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, ComCtrls, JvToolBar, ToolWin, JvCoolBar,
  JvStatusBar, ExtCtrls, JvSplitter, StdCtrls, JvListBox, JvCtrls,
  JvControlBar, ImgList, ActnList, JvComponent, JvBaseDlg, JvBrowseFolder,
  Mask, JvToolEdit, AppEvnts, Grids, JvGrids, JvFormPlacement, JvAppStorage,
  JvStringGrid, JvAppXMLStorage, JvExGrids, JvExComCtrls, JvExExtCtrls,
  JvExStdCtrls, JvComponentBase,
  PackageGenerator;

type
  TfrmMain = class(TForm)
    jmmMain: TJvMainMenu;
    mnuFile: TMenuItem;
    jsbStatus: TJvStatusBar;
    jtbMenus: TJvToolBar;
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
    tbtSep: TToolButton;
    tbtPrevPackage: TToolButton;
    tbtNextPackage: TToolButton;
    mnuGenPackages: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuPreviousPackage: TMenuItem;
    mnuNextPackage: TMenuItem;
    jbfFolder: TJvBrowseForFolderDialog;
    pnlEdit: TPanel;
    lblModel: TLabel;
    aevEvents: TApplicationEvents;
    ledName: TEdit;
    ledDescription: TEdit;
    lblDependencies: TLabel;
    jsgDependencies: TJvStringGrid;
    jsgFiles: TJvStringGrid;
    lblFiles: TLabel;
    odlAddFiles: TOpenDialog;
    actSaveAll: TAction;
    actAddFiles: TAction;
    tbtAddFiles: TToolButton;
    actGenerate: TAction;
    tbtGenerate: TToolButton;
    jpmDepPopup: TJvPopupMenu;
    mnuView: TMenuItem;
    actMainToolbar: TAction;
    actKnown: TAction;
    mnuMainToolbar: TMenuItem;
    mnuUpD: TMenuItem;
    mnuDownD: TMenuItem;
    pnlDepAndFiles: TPanel;
    sptDepAndFiles: TSplitter;
    lblName: TLabel;
    lblDescription: TLabel;
    jfsStore: TJvFormStorage;
    mnuHelp: TMenuItem;
    mnuKnown: TMenuItem;
    mnuAbout: TMenuItem;
    N2: TMenuItem;
    mnuAddFiles: TMenuItem;
    N3: TMenuItem;
    mnuExit: TMenuItem;
    actOptions: TAction;
    mnuParameters: TMenuItem;
    jpmFilesPopup: TJvPopupMenu;
    actUp: TAction;
    actDown: TAction;
    mnuUpF: TMenuItem;
    mnuDownF: TMenuItem;
    mnuAddFilesP: TMenuItem;
    actDelete: TAction;
    jpmList: TJvPopupMenu;
    mnuDeletePackageP: TMenuItem;
    jaxStore: TJvAppXMLFileStorage;
    pnlOptions: TPanel;
    shHideOptions: TShape;
    mnuDeletePackage: TMenuItem;
    cmbModel: TComboBox;
    btnEditModel: TButton;
    btnCLXDescription: TButton;
    lblGUID: TLabel;
    btnGenerateGUID: TButton;
    edtGUID: TEdit;
    ComboBoxType: TComboBox;
    sptFilesAndProps: TSplitter;
    jsgProperties: TJvStringGrid;
    lblProperties: TLabel;
    procedure ComboBoxTypeSelect(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure aevEventsHint(Sender: TObject);
    procedure jsgStringGridGetCellAlignment(Sender: TJvStringGrid;
      AColumn, ARow: Integer; State: TGridDrawState;
      var CellAlignment: TAlignment);
    procedure jsgStringGridExitCell(Sender: TJvStringGrid; AColumn,
      ARow: Integer; const EditText: String);
    procedure actAddFilesExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure ledC6PFlagsChange(Sender: TObject);
    procedure jsgDependenciesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure ledDescriptionChange(Sender: TObject);
    procedure ledNameChange(Sender: TObject);
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
    procedure actMainToolbarExecute(Sender: TObject);
    procedure actKnownExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure jlbListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sptDepAndFilesMoved(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure jsgDependenciesGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure FormShow(Sender: TObject);
    procedure actUpExecute(Sender: TObject);
    procedure actDownExecute(Sender: TObject);
    procedure actUpUpdate(Sender: TObject);
    procedure actDownUpdate(Sender: TObject);
    procedure jdePackagesLocationChange(Sender: TObject);
    procedure jlbListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actDeleteExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actOptionsUpdate(Sender: TObject);
    procedure btnEditModelClick(Sender: TObject);
    procedure cmbModelClick(Sender: TObject);
    procedure btnCLXDescriptionClick(Sender: TObject);
    procedure edtGUIDChange(Sender: TObject);
    procedure btnGenerateGUIDClick(Sender: TObject);
    procedure edtCompilerDefinesChange(Sender: TObject);
    procedure sptFilesAndPropsMoved(Sender: TObject);
    procedure jsgPropertiesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure jsgFilesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure jsgFilesGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure jsgPropertiesGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
  private
    { Private declarations }
    FPackageGenerator: TPackageGenerator;
    Changed : Boolean; // true if current file has changed

    ConfigLoadedOk : Boolean; // true if the config loaded ok

    FOrgValueDep: string; // original value of current column (dependencies list)
    FOrgValueFiles: string; // original value of current column (files list)
    FOrgValueProps: string; // original value of current column (properties list)
    FValidOrgDep: Boolean; // True if FOrgValueDep is officially set
    FValidOrgFiles: Boolean; // True if FOrgValueFiles is officially set
    FValidOrgProps: Boolean; // True if FOrgValueProps is officially set

    FClxDescription: string; // The CLX description of the active package

    procedure LoadPackagesList;
    procedure LoadPackage;
    procedure ClearAll;
    procedure MoveLine(sg : TStringGrid; direction : Integer);
    function IsOkToChange : Boolean;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileUtils, JvSimpleXml, JclFileUtils, JclStrings, TargetDialog,
  GenerateUtils, KnownTagsForm, FormTypeDialog, ShellApi,
  GenerationMessagesForm, ActiveX, PackageInformation,
  ModelsForm;

{$R *.dfm}

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  ClearAll;
end;

procedure TfrmMain.aevEventsHint(Sender: TObject);
begin
  jsbStatus.Panels[0].Text:= GetLongHint(Application.Hint);
end;

procedure TfrmMain.ComboBoxTypeSelect(Sender: TObject);
begin
  Changed := True;
end;

constructor TfrmMain.Create(AOwner: TComponent);
var
  ErrMsg : string;
  i: Integer;
begin
  inherited Create(AOwner);
  FPackageGenerator := TPackageGenerator.Create;

  jaxStore.FileName := StrEnsureSuffix(DirDelimiter, ExtractFilePath(Application.exename)) + 'pgEdit.xml';
//  if cmbModel.ItemIndex >-1 then
//    ConfigLoadedOk := LoadConfig(jaxStore.FileName, cmbModel.Items[cmbModel.ItemIndex], ErrMsg)
//  else
    ConfigLoadedOk := FPackageGenerator.LoadConfig(jaxStore.FileName, '', ErrMsg);

  if not ConfigLoadedOk then
  begin
    Application.MessageBox(PChar('Error loading configuration:'#13#10+
                                 #13#10+
                                 ErrMsg),
                           'Error loading configuration',
                           MB_ICONERROR);
    Application.Terminate;
    Exit;
  end;

  with jsgDependencies do
  begin
    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Targets';
    Cells[2, 0] := 'Condition';
    ColWidths[0] := 120;
    ColWidths[1] := 150;
    ColWidths[2] := 70;
  end;

  with jsgFiles do
  begin
    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Targets';
    Cells[2, 0] := 'Form name';
    Cells[3, 0] := 'Condition';
    ColWidths[0] := 230;
    ColWidths[1] := 50;
    ColWidths[2] := 170;
    ColWidths[3] := 70;
  end;

  with jsgProperties do
  begin
    RowCount := Length(KnownPackageProperties) + 1;
    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Value';
    ColWidths[0] := 130;
    ColWidths[1] := 350;
    for i := Low(KnownPackageProperties) to High(KnownPackageProperties) do
      Cells[0, i + 1] := KnownPackageProperties[i];
  end;

  jtbMenus.AutoSize := true;
end;

destructor TfrmMain.Destroy;
begin
  FPackageGenerator.Free;
  inherited Destroy;
end;

procedure TfrmMain.jsgStringGridGetCellAlignment(Sender: TJvStringGrid;
  AColumn, ARow: Integer; State: TGridDrawState;
  var CellAlignment: TAlignment);
begin
  if (ARow > 0) then
    CellAlignment := taLeftJustify
  else
    CellAlignment := taCenter;
end;

procedure TfrmMain.jsgStringGridExitCell(Sender: TJvStringGrid; AColumn,
  ARow: Integer; const EditText: String);
var
  row : TStrings;
begin
  if AColumn = 0 then
  begin
    if (Sender.RowCount > 2) and
       (Sender.Cells[0, ARow] = '') and
       (ARow < Sender.RowCount-1) then
      Sender.RemoveRow(ARow);
  end;
  if (Sender.RowCount > 1) and
     (Sender.Cells[0, Sender.RowCount-1] <> '') then
  begin
    Sender.InsertRow(Sender.RowCount);
    if Sender <> jsgProperties then
    begin
      row := Sender.Rows[ARow];
      row[1] := 'all';
      Changed := True;
    end;
  end;
  if (ARow > 0) and
     (((Sender = jsgDependencies) and FValidOrgDep and (FOrgValueDep <> EditText)) or
      ((Sender = jsgFiles) and FValidOrgFiles and (FOrgValueFiles <> EditText)) or
      ((Sender = jsgProperties) and FValidOrgProps and (FOrgValueProps <> EditText))) then
    Changed := True;
end;

procedure TfrmMain.actAddFilesExecute(Sender: TObject);
var
  i : Integer;
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
    if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
      PackagesDir := FPackageGenerator.PackagesLocation
    else
      PackagesDir := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);
    for i := 0 to odlAddFiles.Files.Count-1 do
    begin
      row := jsgFiles.InsertRow(jsgFiles.RowCount-1);
      Name := odlAddFiles.Files[i];
      Dir := GetRelativePath(PackagesDir, ExtractFilePath(Name));
      row[0] := '..' + DirDelimiter + StrEnsureSuffix(DirDelimiter, Dir) + ExtractFileName(Name);
      row[1] := 'all';

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

        frmFormType.EnsureCorrectType(FormType, ChangeFileExt(row[0], '.dfm'), line);

        // if the form type is TForm or TJvForm then ignore it
        // else include it in the row
        if (FormType = 'TForm') or
           (FormType = 'TJvForm') or
           (FormType = '') then
          row[2] := FormName
        else
          row[2] := FormName + ': ' + FormType;
      end;
    end;
    odlAddFiles.InitialDir := '..' + DirDelimiter + Dir;
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

procedure TfrmMain.jsgDependenciesGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
begin
  FOrgValueDep := Value;
  FValidOrgDep := True;
end;

procedure TfrmMain.jsgDependenciesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  FOrgValueDep := jsgDependencies.Cells[ACol, ARow];
end;

procedure TfrmMain.jsgFilesGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  FOrgValueFiles := Value;
  FValidOrgFiles := True;
end;

procedure TfrmMain.jsgFilesSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FOrgValueFiles := jsgFiles.Cells[ACol, ARow];
end;

procedure TfrmMain.jsgPropertiesGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  FOrgValueProps := Value;
  FValidOrgProps := True;
end;

procedure TfrmMain.jsgPropertiesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  FOrgValueProps := jsgProperties.Cells[ACol, ARow];
end;

procedure TfrmMain.ledDescriptionChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.ledNameChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.edtCompilerDefinesChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.edtGUIDChange(Sender: TObject);
begin
  Changed := True;
end;

procedure TfrmMain.LoadPackagesList;
var
  path : string;
begin
  if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
    path := FPackageGenerator.PackagesLocation
  else
    path := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);

  EnumeratePackages(path, jlbList.Items);
  path := StrEnsureSuffix(DirDelimiter, path);
{  if FileExists(path+'Default-D.xml') then
    jlbList.Items.Add('Default-D');
  if FileExists(path+'Default-R.xml') then
    jlbList.Items.Add('Default-R'}
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
  propertiesNode, propertyNode : TJvSimpleXmlElem;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  filesNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
  ProjectType: TProjectType;
begin
  if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
    FileName := FPackageGenerator.PackagesLocation
  else
    FileName := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);

  ProjectType := TProjectType(ComboBoxType.ItemIndex);
  FileName := Format('%s%sxml%s%s-%s.xml',[FileName, DirDelimiter,
    DirDelimiter, ledName.Text, ProjectTypeToChar(ProjectType)]);

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
      case ProjectType of
        ptPackageRun:
          rootNode.Properties.Add('Design', False);
        ptPackageDesign:
          rootNode.Properties.Add('Design', True);
        else
          rootNode.Properties.Add('Type', ProjectTypeToChar(ProjectType));
      end;


      // add description, PFLAGS and libs
      rootNode.Items.Add('Description', ledDescription.Text);
      if FClxDescription <> '' then
        rootNode.Items.Add('ClxDescription', FClxDescription);
      rootNode.Items.Add('GUID', edtGUID.Text);

      // add properties
      propertiesNode := rootNode.Items.Add('Properties');
      for i := 1 to jsgProperties.RowCount - 2 do
      begin
        propertyNode := propertiesNode.Items.Add('Property');
        propertyNode.Properties.Add('Name', jsgProperties.Cells[0,i]);
        propertyNode.Properties.Add('Value', jsgProperties.Cells[1,i]);
      end;

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
                            (jlbList.ItemIndex < jlbList.Items.Count-1);
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
  propertiesNode, propertyNode : TJvSimpleXMLElem;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  filesNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
  savedChanged : Boolean;
begin
  savedChanged := Changed;
  ClearAll;
  Changed := savedChanged;

  if jlbList.ItemIndex < 0 then
    Exit;

  if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
    xmlFileName := FPackageGenerator.PackagesLocation
  else
    xmlFileName := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);

  xmlFileName := Format('%s%sxml%s%s.xml', [xmlFileName, DirDelimiter,
    DirDelimiter, jlbList.Items[jlbList.ItemIndex]]);

  xml := TJvSimpleXml.Create(nil);
  try
    with xml do
    begin
      Options := [sxoAutoCreate];

      LoadFromFile(xmlFileName);

      // read root node
      rootNode := xml.Root;
      ledName.Text      := rootNode.Properties.ItemNamed['Name'].Value;

      xml.Options := xml.Options - [sxoAutoCreate];
      if Assigned(rootNode.Properties.ItemNamed['Design']) then
      begin
        if rootNode.Properties.ItemNamed['Design'].BoolValue then
          ComboBoxType.ItemIndex := Integer(ptPackageDesign)
        else
          ComboBoxType.ItemIndex := Integer(ptPackageRun);
      end
      else
        ComboBoxType.ItemIndex := Integer(CharToProjectType(rootNode.Properties.ItemNamed['Type'].Value[1]));
      xml.Options := xml.Options + [sxoAutoCreate];

      // read description, PFLAGS, GUID, and libs
      ledDescription.Text := rootNode.Items.ItemNamed['Description'].Value;
      //ledC6PFlags.Text    := rootNode.Items.ItemNamed['C6PFlags'].Value;
      if Assigned(rootNode.Items.ItemNamed['GUID']) then
        edtGUID.Text := rootNode.Items.ItemNamed['GUID'].Value;

      for i := Low(KnownPackageProperties) to High(KnownPackageProperties) do
        if Assigned(rootNode.Items.ItemNamed[KnownPackageProperties[i]]) then
      begin
        j := jsgProperties.RowCount;
        if jsgProperties.Cells[0,j-1] <> '' then
          jsgProperties.RowCount := j + 1
        else
          Dec(j);
        jsgProperties.Cells[0,j] := KnownPackageProperties[i];
        jsgProperties.Cells[1,j] := rootNode.Items.ItemNamed[KnownPackageProperties[i]].Value;
      end;

      propertiesNode := rootNode.Items.ItemNamed['Properties'];
      if Assigned(propertiesNode) then
        for i := 0 to propertiesNode.Items.Count - 1 do
      begin
        j := jsgProperties.RowCount;
        if jsgProperties.Cells[0,j-1] <> '' then
          jsgProperties.RowCount := j + 1
        else
          Dec(j);
        propertyNode := propertiesNode.Items.Item[i];
        jsgProperties.Cells[0,j] := propertyNode.Properties.ItemNamed['Name'].Value;
        jsgProperties.Cells[1,j] := propertyNode.Properties.ItemNamed['Value'].Value;
      end;

      // read required packages
      requiredNode := rootNode.Items.ItemNamed['Requires'];
      jsgDependencies.RowCount := requiredNode.Items.Count + 2;
      jsgDependencies.Rows[jsgDependencies.RowCount-1].Text := '';
      jsgDependencies.FixedRows := 1;
      for i := 0 to requiredNode.Items.Count - 1 do
      begin
        row := jsgDependencies.Rows[i+1];
        packageNode := requiredNode.Items[i];
        for j := 0 to row.Count - 1 do
        begin
          row[j] := packageNode.Properties.ItemNamed[jsgDependencies.Rows[0][j]].Value;
        end;
      end;

      // read files
      filesNode := rootNode.Items.ItemNamed['Contains'];
      jsgFiles.RowCount := filesNode.Items.Count + 2;
      jsgFiles.FixedRows := 1;
      jsgFiles.Rows[jsgFiles.RowCount-1].Text := '';
      for i := 0 to filesNode.Items.Count - 1 do
      begin
        row := jsgFiles.Rows[i+1];
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
  ErrMsg : string;
begin
  if IsOkToChange then
  begin
    if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
      path := FPackageGenerator.PackagesLocation
    else
      path := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);

    targets := TStringList.Create;
    try
      FPackageGenerator.TargetList.EnumerateTargets(targets);
      frmTargets.Targets := targets;
    finally
      targets.Free;
    end;
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

        frmGenMessages.Show;
        // clean the package cache first, files might have changed on disk
        ClearXmlFileCache;

        FPackageGenerator.Generate(jlbList.Items, targets, AddMessage, jaxStore.FileName, cmbModel.Items[cmbModel.ItemIndex], ErrMsg);
      finally
        targets.Free;
      end;
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
  if IsOkToChange then
  begin
    jlbList.ItemIndex := jlbList.ItemIndex - 1;
    LoadPackage;
  end;
end;

procedure TfrmMain.actNextPackageExecute(Sender: TObject);
begin
  if IsOkToChange then
  begin
    jlbList.ItemIndex := jlbList.ItemIndex + 1;
    LoadPackage;
  end;
end;

procedure TfrmMain.actMainToolbarUpdate(Sender: TObject);
begin
  actMainToolbar.Checked := jtbTools.Visible;
end;

procedure TfrmMain.actMainToolbarExecute(Sender: TObject);
begin
  jtbTools.Visible := actMainToolbar.Checked;
end;

procedure TfrmMain.actKnownExecute(Sender: TObject);
begin
  frmKnownTags.ShowModal;
end;

function TfrmMain.IsOkToChange: Boolean;
begin
  if Changed then
  begin
    Result := True;
    case Application.MessageBox(
          'The values for this package have changed.'#13#10 + 
          'Do you want to save before the next action ?',
          'Package has changed',
          MB_ICONQUESTION or MB_YESNOCANCEL) of
      MRYES:
        actSave.Execute;
      MRCANCEL:
        Result := False;
    end;
  end
  else
    Result := True;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := IsOkToChange;
end;

procedure TfrmMain.jlbListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    IsOkToChange;
end;

procedure TfrmMain.sptDepAndFilesMoved(Sender: TObject);
begin
  lblDependencies.Top := pnlDepAndFiles.Top +
                         (jsgDependencies.Height - lblDependencies.Height) div 2;
  lblFiles.Top := pnlDepAndFiles.Top +
                  jsgFiles.Top +
                  (jsgFiles.Height - lblFiles.Height) div 2;
end;

procedure TfrmMain.sptFilesAndPropsMoved(Sender: TObject);
begin
  lblFiles.Top := pnlDepAndFiles.Top +
                  jsgFiles.Top +
                  (jsgFiles.Height - lblFiles.Height) div 2;
  lblProperties.Top := pnlDepAndFiles.Top +
                       jsgProperties.Top +
                       (jsgProperties.Height - lblProperties.Height) div 2;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  ShowMessage(
    'JEDI Package Generator'#13#10+
    #13#10+
    '(c) 2003-2005 Olivier Sannier for the JEDI group');
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  ErrMsg : string;
begin
  if not Application.Terminated then
  begin
    // force the models to be loaded in appropriate form and
    // load the names in the combo box
    frmModels.LoadModels(jaxStore.FileName, ConfigLoadedOk);
    cmbModel.Items.Assign(frmModels.cmbModels.Items);

    jfsStore.RestoreFormPlacement;

    if cmbModel.ItemIndex >-1 then
      ConfigLoadedOk := FPackageGenerator.LoadConfig(jaxStore.FileName, cmbModel.Items[cmbModel.ItemIndex], ErrMsg)
    else
      ConfigLoadedOk := FPackageGenerator.LoadConfig(jaxStore.FileName, '', ErrMsg);
      
    if not ConfigLoadedOk then
    begin
      Application.MessageBox(PChar('Error loading configuration:'#13#10+
                                   #13#10+
                                   ErrMsg),
                             'Error loading configuration',
                             MB_ICONERROR);
      Application.Terminate;
      Exit;
    end;

    // Load the list of packages
    LoadPackagesList;
  end;
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
      sg.Row := RowIndex + direction;
    finally
      tmpRow.Free;
    end;
  end;
end;

procedure TfrmMain.actUpExecute(Sender: TObject);
begin
  if ActiveControl is TInPlaceEdit then
  begin
    MoveLine((ActiveControl as TInPlaceEdit).Parent as TStringGrid, -1);
  end;
end;

procedure TfrmMain.actDownExecute(Sender: TObject);
begin
  if ActiveControl is TInPlaceEdit then
  begin
    MoveLine((ActiveControl as TInPlaceEdit).Parent as TStringGrid, +1);
  end;
end;

procedure TfrmMain.actUpUpdate(Sender: TObject);
var
  curRow : Integer;
  sg : TStringGrid;
begin
  if ActiveControl is TInPlaceEdit then
  begin
    sg := ((ActiveControl as TInPlaceEdit).Parent as TStringGrid);
    curRow := sg.Row ;
    actUp.Enabled := (curRow > 1) and (curRow < sg.RowCount-1);
  end;
end;

procedure TfrmMain.actDownUpdate(Sender: TObject);
var
  sg : TStringGrid;
begin
  if ActiveControl is TInPlaceEdit then
  begin
    sg := ((ActiveControl as TInPlaceEdit).Parent as TStringGrid);
    actDown.Enabled := sg.Row < sg.RowCount-2;
  end;
end;

procedure TfrmMain.jdePackagesLocationChange(Sender: TObject);
begin
  LoadPackagesList;
end;

procedure TfrmMain.ClearAll;
begin
  // empty everything
  ledName.Text := '';
  ledDescription.Text := '';
  ComboBoxType.ItemIndex := Integer(ptPackageRun);
  edtGUID.Text := '';
  //ledC6PFlags.Text := '';
  //edtCompilerDefines.Text := '';
  jsgDependencies.Rows[1].Text := '';
  jsgDependencies.RowCount := 2;
  jsgFiles.Rows[1].Text := '';
  jsgFiles.RowCount := 2;
  jsgProperties.Rows[1].Text := '';
  jsgProperties.RowCount := 2;
end;

procedure TfrmMain.jlbListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      actDelete.Execute;
  end;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
var
  path : string;
begin
  if (jlbList.ItemIndex > -1) and
     (Application.MessageBox(PChar(
        'You are about to delete '+jlbList.Items[jlbList.ItemIndex]+#13#10+
        'Are you sure you want to do that ?'),
        'Deleting a package',
        MB_ICONQUESTION or MB_YESNO) = MRYES) then
  begin
    if PathIsAbsolute(FPackageGenerator.PackagesLocation) then
      path := FPackageGenerator.PackagesLocation
    else
      path := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FPackageGenerator.StartupDir) + FPackageGenerator.PackagesLocation);

    path := StrEnsureSuffix(DirDelimiter, path) + 'xml' + DirDelimiter + jlbList.Items[jlbList.ItemIndex] + '.xml';
    if not DeleteFile(path) then
      Application.MessageBox(PChar('Unable to delete ' + path),
                             'Error',
                             MB_ICONERROR);
    LoadPackagesList;
  end;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  pnlOptions.Visible := not pnlOptions.Visible;
end;

procedure TfrmMain.actOptionsUpdate(Sender: TObject);
begin
  actOptions.Checked := pnlOptions.Visible;
end;

procedure TfrmMain.btnEditModelClick(Sender: TObject);
begin
  frmModels.EditIndex := cmbModel.ItemIndex;
  if frmModels.ShowModal = mrOk then
  begin
    cmbModel.ItemIndex := frmModels.cmbModels.ItemIndex;
    LoadPackagesList;
    jaxStore.Reload;
  end;
end;

procedure TfrmMain.cmbModelClick(Sender: TObject);
var
  ErrMsg : string;
begin
  if not FPackageGenerator.LoadConfig(jaxStore.FileName, cmbModel.Items[cmbModel.ItemIndex], ErrMsg) then
    Application.MessageBox(PChar(ErrMsg), 'Error loading configuration', MB_ICONERROR)
  else
    LoadPackagesList;
end;

procedure TfrmMain.btnCLXDescriptionClick(Sender: TObject);
begin
  if (FClxDescription = '') and (cmbModel.Items[cmbModel.ItemIndex] = 'JVCL') then
  begin
    FClxDescription := ledDescription.Text;
    StrReplace(FClxDescription, 'JVCL ', 'JVCLX ', [rfReplaceAll]);
  end;
  if InputQuery('CLX Description',
                'Please indicate the CLX Description',
                FClxDescription) then
    Changed := True;
end;

procedure TfrmMain.btnGenerateGUIDClick(Sender: TObject);
var
  GeneratedGUID : TGUID;
begin
  // Generate a GUID for the package
  CoCreateGUID(GeneratedGUID);      
  edtGUID.Text := GUIDToString(GeneratedGUID);
end;

end.
