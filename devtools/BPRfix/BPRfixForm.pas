unit BPRfixForm;

interface                  

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvSimpleXml, StdCtrls, ComCtrls, Menus, ImgList, JvComponent,
  JvBrowseFolder, JvBaseDlg, ExtCtrls;

type
  TFormMain = class(TForm)
    JvSimpleXMLFiles: TJvSimpleXML;
    ListViewFiles: TListView;
    MemoFile: TMemo;
    Label1: TLabel;
    PopupMenuFiles: TPopupMenu;
    MenuItemEdit: TMenuItem;
    MenuItemAddFile: TMenuItem;
    MenuItemRemoveFile: TMenuItem;
    MenuItemCheck: TMenuItem;
    MenuItemCheckAll: TMenuItem;
    MenuItemScan: TMenuItem;
    ButtonCheckAll: TButton;
    ButtonReload: TButton;
    ButtonSave: TButton;
    ImageListIcons: TImageList;
    ButtonFix: TButton;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    OpenDialogAddFiles: TOpenDialog;
    JvBrowseForFolderDialogScan: TJvBrowseForFolderDialog;
    PanelLeft: TPanel;
    Splitter1: TSplitter;
    PanelRight: TPanel;
    CheckBoxQuietMode: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAddFileClick(Sender: TObject);
    procedure MenuItemRemoveFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemScanClick(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonReloadClick(Sender: TObject);
    procedure PopupMenuFilesPopup(Sender: TObject);
    procedure ListViewFilesClick(Sender: TObject);
    procedure ButtonCheckAllClick(Sender: TObject);
    procedure MenuItemCheckClick(Sender: TObject);
    procedure ButtonFixClick(Sender: TObject);
  private
  public
    CurrentFileName: string;
    procedure AddFile(const FileName: string);
    procedure FileFound(const FileName: string);
    function CloseCurrentFile: Boolean;
    procedure CheckFile(FileIndex: Integer; FixFile: Boolean);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  IniFiles,
  JclFileUtils,
  BPRfixScanForm;

resourcestring
  RsFileNotChecked = 'Not checked';
  RsFileOK         = 'Passed';
  RsFileError      = 'Error';
  RsFileInvalid    = 'Invalid';
  RsFileFixed      = 'Fixed';

procedure TFormMain.FormCreate(Sender: TObject);
var
  Index: Integer;
  XMLElement: TJvSimpleXMLElem;
  FileName: string;
begin
  JvSimpleXMLFiles.FileName := 'FileList.xml';
  JvSimpleXMLFiles.Root.Name := 'FileList';
  with JvSimpleXMLFiles.Root.Items do
    for Index := 0 to Count-1 do
  begin
    XMLElement := Item[Index];
    if (XMLElement.Name = 'File') then
    begin
      FileName := XMLElement.Properties.ItemNamed['Name'].Value;
      if (FileName <> '') then
        with ListViewFiles.Items.Add do
      begin
        Caption := FileName;
        ImageIndex := 0;
        SubItems.Add(RsFileNotChecked);
      end;
    end;
  end;
  ButtonCheckAll.Enabled := ListViewFiles.Items.Count > 0;
end;

procedure TFormMain.MenuItemAddFileClick(Sender: TObject);
var
  Index: Integer;
begin
  if OpenDialogAddFiles.Execute then
    for Index := 0 to OpenDialogAddFiles.Files.Count-1 do
      AddFile(OpenDialogAddFiles.Files.Strings[Index]);
  ButtonCheckAll.Enabled := ListViewFiles.Items.Count > 0;
end;

procedure TFormMain.MenuItemRemoveFileClick(Sender: TObject);
var
  Index: Integer;
  FileName: string;
  RemoveOK: Boolean;
begin
  FileName := ListViewFiles.Items.Item[ListViewFiles.ItemIndex].Caption;
  RemoveOK := True;
  if CompareText(CurrentFileName,FileName) = 0 then
    RemoveOK := CloseCurrentFile;
  if RemoveOK then
  begin
    with JvSimpleXMLFiles.Root.Items do
      for Index := Count-1 downto 0 do
        if Item[Index].Properties.ItemNamed['Name'].Value = FileName then
          Delete(Index);
    for Index := ListViewFiles.Items.Count-1 downto 0 do
      if ListViewFiles.Items.Item[Index].Caption = FileName then
        ListViewFiles.Items.Delete(Index);
  end;
  ButtonCheckAll.Enabled := ListViewFiles.Items.Count > 0;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  JvSimpleXMLFiles.SaveToFile(JvSimpleXMLFiles.FileName);
end;

procedure TFormMain.MenuItemScanClick(Sender: TObject);
var
  FileEnumerator: IJclFileEnumerator;
  ScanForm: TFormScan;
begin
  if JvBrowseForFolderDialogScan.Execute then
  begin
    ScanForm := TFormScan.Create(Self);
    try
      FileEnumerator := TJclFileEnumerator.Create;
      FileEnumerator.RootDirectory := JvBrowseForFolderDialogScan.Directory;
      FileEnumerator.SetOptions([fsIncludeSubDirectories,fsIncludeHiddenSubDirectories]);
      FileEnumerator.CaseSensitiveSearch := False;
      FileEnumerator.FileMask := '*.bp*';
      FileEnumerator.OnEnterDirectory := ScanForm.ChangeDirectory;
      FileEnumerator.OnTerminateTask := ScanForm.TaskFinished;
      FileEnumerator.ForEach(FileFound);
      ScanForm.ShowModal;
    finally
      FileEnumerator.StopAllTasks;
      ScanForm.Free;
    end;
  end;
end;

procedure TFormMain.FileFound(const FileName: string);
var
  FileExtension: string;
begin
  FileExtension := ExtractFileExt(FileName);
  if   (CompareText(FileExtension,'.bpr') = 0)
    or (CompareText(FileExtension,'.bpk') = 0) then
      AddFile(FileName);
end;

procedure TFormMain.AddFile(const FileName: string);
var
  NewFileName: string;
  Index: Integer;
begin
  NewFileName := ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir),FileName);
  for Index := 0 to ListViewFiles.Items.Count-1 do
    if CompareText(ListViewFiles.Items.Item[Index].Caption,NewFileName) = 0 then
      Exit;
  with ListViewFiles.Items.Add do
  begin
    Caption := NewFileName;
    ImageIndex := 0;
    SubItems.Add(RsFileNotChecked);
  end;
  JvSimpleXMLFiles.Root.Items.Add('File').Properties.Add('Name',NewFileName);
end;

function TFormMain.CloseCurrentFile: Boolean;
var
  CloseOK: TModalResult;
begin
  CloseOK := mrOk;
  if MemoFile.Modified then
  begin
    CloseOK := MessageDlg('The file is modified, save changes ?',mtConfirmation,[mbYes,mbNo,mbCancel],0);
    if CloseOK = mrYes then
      MemoFile.Lines.SaveToFile(CurrentFileName);
  end;

  Result := CloseOK <> mrCancel;
  if Result then
  begin
    MemoFile.Lines.Clear;
    MemoFile.Modified := False;
    ButtonReload.Enabled := False;
    ButtonSave.Enabled := False;
    ButtonFix.Enabled := False;
    CurrentFileName := '';
  end;
end;

procedure TFormMain.MenuItemEditClick(Sender: TObject);
var
  ChangeOK: Boolean;
begin
  if (ListViewFiles.ItemIndex >= 0) and (ListViewFiles.ItemIndex < ListViewFiles.Items.Count) then
  begin
    ChangeOK := True;
    if CurrentFileName <> '' then
      ChangeOK := CloseCurrentFile;
    if ChangeOK then
    begin
      CurrentFileName := ListViewFiles.Items.Item[ListViewFiles.ItemIndex].Caption;
      MemoFile.Lines.LoadFromFile(CurrentFileName);
      MemoFile.Modified := False;
      ButtonReload.Enabled := True;
      ButtonSave.Enabled := True;
      ButtonFix.Enabled := True;
    end;
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  MemoFile.Lines.SaveToFile(CurrentFileName);
  MemoFile.Modified := False;
end;

procedure TFormMain.ButtonReloadClick(Sender: TObject);
begin
  MemoFile.Lines.LoadFromFile(CurrentFileName);
  MemoFile.Modified := False;
end;

procedure TFormMain.PopupMenuFilesPopup(Sender: TObject);
begin
  with ListViewFiles do
  begin
    MenuItemEdit.Enabled := (ItemIndex >= 0) and (ItemIndex < Items.Count);
    MenuItemCheck.Enabled := MenuItemEdit.Enabled;
    MenuItemRemoveFile.Enabled := MenuItemEdit.Enabled;
    MenuItemCheckAll.Enabled := Items.Count > 0;
  end;
end;

procedure TFormMain.ListViewFilesClick(Sender: TObject);
begin
  MenuItemEdit.Click;
end;

procedure TFormMain.ButtonCheckAllClick(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to ListViewFiles.Items.Count-1 do
    CheckFile(Index, False);
end;

procedure TFormMain.MenuItemCheckClick(Sender: TObject);
begin
  CheckFile(ListViewFiles.ItemIndex, False);
end;

procedure TFormMain.ButtonFixClick(Sender: TObject);
begin
  CheckFile(ListViewFiles.ItemIndex, True);
end;

procedure TFormMain.CheckFile(FileIndex: Integer; FixFile: Boolean);
var
  ErrorFound: Boolean;
  function IsPackage(FileName: string): Boolean;
  begin
    Result := CompareText(ExtractFileExt(FileName),'.bpk') = 0;
  end;
  procedure ShowError(ErrorMessage: string);
  begin
    if not CheckBoxQuietMode.Checked then
      ShowMessage(ErrorMessage);
    ErrorFound := True;
  end;
  function CheckOutputNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Target: string;
  begin
    Result := True;
    if not IsPackage(XMLNode.SimpleXML.FileName) then
    begin
      Target := XMLNode.Properties.ItemNamed['value'].Value;
      Result := Pos('\',Target) <> 0;
      if (not Result) then
      begin
        ShowError('The output directory is not set');
        FixFile := False;
      end;
    end;
  end;
  function CheckObjFilesNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Lines: TStrings;
    Index: Integer;
  begin
    Result := True;
    if CompareText(ExtractFileName(XMLNode.SimpleXML.FileName),'template.bpk') = 0 then
      Exit;
    Lines := TStringList.Create;
    try
      Lines.Delimiter := ' ';
      Lines.DelimitedText := XMLNode.Properties.ItemNamed['value'].Value;
      for Index := 0 to Lines.Count-1 do
        if    (Pos('\',Lines.Strings[Index]) = 0)
          and (Pos('/',Lines.Strings[Index]) = 0) then
      begin
        ShowError('The output directory for libraries is not set');
        Result := False;
        Break;
      end;
    finally
      Lines.Free;
    end;
  end;
  function CheckEmptyNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Index: Integer;
  begin
    Result := True;
    if (XMLNode.Items.Count > 0) then
    begin
      ShowError('The '+XMLNode.Name+' can not have any items');
      Result := FixFile;
      if Result then
        for Index := XMLNode.Items.Count-1 downto 0 do
          XMLNode.Items.Delete(Index)
      else
        Exit;
    end;
    for Index := XMLNode.Properties.Count-1 downto 0 do
      if CompareText(XMLNode.Properties.Item[Index].Name,'value') <> 0 then
    begin
      ShowError('The '+XMLNode.Name+' can not have a property named '+XMLNode.Properties.Item[Index].Name);
      Result := FixFile;
      if Result then
        XMLNode.Properties.Delete(Index)
      else
        Exit;
    end;
  end;
  function CheckEmptyValue(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    if (XMLNode.Properties.ItemNamed['value'].Value <> '') then
    begin
      ShowError('The '+XMLNode.Name+' section is not empty');
      Result := FixFile;
      if (Result) then
        XMLNode.Properties.ItemNamed['value'].Value := '';
    end else Result := True;
    Result := Result and CheckEmptyNode(XMLNode);
  end;
  function CheckLibrariesNode(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    Result := CheckEmptyValue(XMLNode);
  end;
  function CheckSpareLibsNode(XMLNode: TJvSimpleXMLElem): Boolean;
  {var
    Packages, SpareLibs: TStringList;
    Index: Integer;
    LibName: string;}
  begin
    {Result := True;
    if IsPackage(XMLNode.SimpleXML.FileName) then
    begin
      Packages := TStringList.Create;
      SpareLibs := TStringList.Create;
      try
        Packages.Delimiter := ' ';
        Packages.DelimitedText := XMLNode.Parent.Items.ItemNamed['PACKAGES'].Properties.ItemNamed['value'].Value;
        SpareLibs.Delimiter := ' ';
        SpareLibs.DelimitedText := XMLNode.Properties.ItemNamed['value'].Value;
        for Index := 0 to Packages.Count-1 do
        begin
          LibName := ChangeFileExt(Packages.Strings[Index],'.lib');
          Packages.Strings[Index] := LibName;
          if (SpareLibs.IndexOf(LibName) = -1) then
          begin
            ShowError('The library '+LibName+' is missing in the libraries list');
            Result := FixFile;
            if Result then
              SpareLibs.Add(LibName)
            else Break;
          end;
        end;
        if Result then
          for Index := 0 to SpareLibs.Count-1 do
        begin
          LibName := SpareLibs.Strings[Index];
          if (Packages.IndexOf(LibName) = -1) then
          begin
            ShowError('The library '+LibName+' is not is not in the libraries list');
            Result := FixFile;
            if Result then
              Packages.Add(ChangeFileExt(LibName,'.bpi'))
            else Break;
          end;
        end;
      finally
        Packages.Free;
        SpareLibs.Free;
      end;
    end
    else} Result := CheckEmptyValue(XMLNode);
  end;
  function CheckPackagesNode(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    if IsPackage(XMLNode.SimpleXML.FileName) then
      Result := True
    else
      Result := CheckEmptyValue(XMLNode);
  end;
  function CheckSysDefinesNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Defines: TStringList;
    Index: Integer;
    Package: Boolean;
  begin
    Result := True;
    Defines := TStringList.Create;
    try
      Defines.Delimiter := ';';
      Defines.DelimitedText := XMLNode.Properties.ItemNamed['value'].Value;

      Package := IsPackage(XMLNode.SimpleXML.FileName);
      Index := Defines.IndexOf('_RTLDLL');
      if (Index <> -1) and (not Package) then
      begin
        ShowError('The project uses dynamic RTL');
        Result := FixFile;
        if Result then
          Defines.Delete(Index)
        else Exit;
      end;
      if (Index = -1) and (Package) then
      begin
        ShowError('The package doesn''t use dynamic RTL');
        Result := FixFile;
        if Result then
          Defines.Add('_RTLDLL')
        else Exit;
      end;

      if Result then
      begin
        Index := Defines.IndexOf('USEPACKAGES');
        if (Index <> -1) and (not Package) then
        begin
          ShowError('The project uses run-time packages');
          Result := FixFile;
          if Result then
            Defines.Delete(Index)
          else Exit;
        end;
        if (Index = -1) and (Package) then
        begin
          ShowError('The package doesn''t use run-time packages');
          Result := FixFile;
          if Result then
            Defines.Add('USEPACKAGES')
          else Exit;
        end;
      end;
      Result := Result and CheckEmptyNode(XMLNode);
      XMLNode.Properties.ItemNamed['value'].Value := Defines.DelimitedText;
    finally
      Defines.Free;
    end;
  end;
  function CheckPathesValue(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Pathes: TStringList;
    Index: Integer;
  begin
    Result := True;
    Pathes := TStringList.Create;
    try
      Pathes.Delimiter := ';';
      Pathes.QuoteChar := '"';
      Pathes.DelimitedText := XMLNode.Properties.ItemNamed['value'].Value;
      for Index := Pathes.Count-1 downto 0 do
        if Pos(':',Pathes.Strings[Index]) > 0 then
      begin
        ShowError('The section '+XMLNode.Name+' contains absolute pathes');
        Result := FixFile;
        if Result then
          Pathes.Delete(Index)
        else Exit;
      end;
      XMLNode.Properties.ItemNamed['value'].Value := Pathes.DelimitedText;
      Result := Result and CheckEmptyNode(XMLNode);
    finally
      Pathes.Free;
    end;
  end;
  function CheckIncludePathNode(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    Result := CheckPathesValue(XMLNode);
  end;
  function CheckLibPathNode(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    Result := CheckPathesValue(XMLNode);
  end;
  function CheckFileListNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Index: Integer;
    SubNode: TJvSimpleXMLElem;
    FileName: string;
  begin
    Result := True;
    for Index := 0 to XMLNode.Items.Count-1 do
    begin
      SubNode := XMLNode.Items.Item[Index];
      FileName := SubNode.Properties.ItemNamed['FILENAME'].Value;
      if Pos(':',FileName) > 0 then
      begin
        ShowError('The section '+XMLNode.Name+' contains absolute file names');
        Result := FixFile;
        if Result then
          SubNode.Properties.ItemNamed['FILENAME'].Value := ExtractFileName(FileName)
        else
          Exit;
      end;
    end;
  end;
  function CheckBuildToolsNode(XMLNode: TJvSimpleXMLElem): Boolean;
  begin
    Result := CheckEmptyNode(XMLNode);
  end;
  function CheckIdeOptionsNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    IniFile: TMemIniFile;
    Lines: TStringList;
    Index: Integer;
  const
    UselessSections: array [0..13] of string = ( 'Version Info Keys',
      'HistoryLists\hlIncludePath', 'HistoryLists\hlLibraryPath',
      'HistoryLists\hlDebugSourcePath', 'HistoryLists\hlConditionals',
      'HistoryLists\hlIntOutputDir', 'HistoryLists\hlFinalOutputDir',
      'Debugging', 'Parameters', 'Compiler', 'Excluded Packages',
      'Language', 'CORBA', 'HistoryLists\hIBPIOutputDir' );
  begin
    Result := True;
    IniFile := TMemIniFile.Create('');
    Lines := TStringList.Create;
    try
      Lines.Text := XMLNode.Value;
      IniFile.SetStrings(Lines);
      Lines.Clear;
      IniFile.ReadSections(Lines);
      for Index := Low(UseLessSections) to High(UseLessSections) do
        if Lines.IndexOf(UselessSections[Index]) <> -1 then
      begin
        ShowError('The section '+UselessSections[Index]+' is useless');
        Result := FixFile;
        if Result then
          IniFile.EraseSection(UselessSections[Index])
        else Exit;
      end;
      if Result then
      begin
        Lines.Clear;
        IniFile.GetStrings(Lines);
        XMLNode.Value := Lines.Text;
      end;
    finally
      Lines.Free;
      IniFile.Free;
    end;
  end;
  function CheckMacrosNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Index: Integer;
    IdentIndex: Integer;
  const
    CheckNodes: array [0..7] of TIdentMapEntry =
      ( (Value:0; Name:'LIBRARIES'), (Value:1; Name:'SPARELIBS'),
        (Value:2; Name:'PACKAGES'), (Value:3; Name:'SYSDEFINES'),
        (Value:4; Name:'INCLUDEPATH'), (Value:5; Name:'LIBPATH'),
        (Value:6; Name:'PROJECT'), (Value:7; Name:'OBJFILES') );
  begin
    Result := True;
    for Index := 0 to XMLNode.Items.Count-1 do
    begin
      if IdentToInt(XMLNode.Items.Item[Index].Name,IdentIndex,CheckNodes) then
        case IdentIndex of
          0 : Result := CheckLibrariesNode(XMLNode.Items.Item[Index]);
          1 : Result := CheckSpareLibsNode(XMLNode.Items.Item[Index]);
          2 : Result := CheckPackagesNode(XMLNode.Items.Item[Index]);
          3 : Result := CheckSysDefinesNode(XMLNode.Items.Item[Index]);
          4 : Result := CheckIncludePathNode(XMLNode.Items.Item[Index]);
          5 : Result := CheckLibPathNode(XMLNode.Items.Item[Index]);
          6 : Result := CheckOutputNode(XMLNode.Items.Item[Index]);
          7 : Result := CheckObjFilesNode(XMLNode.Items.Item[Index]);
        end;
      if (not FixFile) and (not Result) then
        Exit;
    end;
  end;
  function CheckProjectNode(XMLNode: TJvSimpleXMLElem): Boolean;
  var
    Index: Integer;
    IdentIndex: Integer;
  const
    CheckNodes: array [0..3] of TIdentMapEntry =
      ( (Value:0; Name:'MACROS'), (Value:1; Name:'FILELIST'),
        (Value:2; Name:'BUILDTOOLS'), (Value:3; Name:'IDEOPTIONS') );
  begin
    Result := True;
    for Index := 0 to XMLNode.Items.Count-1 do
    begin
      if IdentToInt(XMLNode.Items.Item[Index].Name,IdentIndex,CheckNodes) then
        case IdentIndex of
          0 : Result := CheckMacrosNode(XMLNode.Items.Item[Index]);
          1 : Result := CheckFileListNode(XMLNode.Items.Item[Index]);
          2 : Result := CheckBuildToolsNode(XMLNode.Items.Item[Index]);
          3 : Result := CheckIdeOptionsNode(XMLNode.Items.Item[Index]);
        end;
      if (not FixFile) and (not Result) then
        Exit;
    end;
  end;
var
  XMLFile: TJvSimpleXML;
  FileValid: Boolean;
begin
  if (FileIndex < 0) and (FileIndex >= ListViewFiles.Items.Count) then
    Exit;
  ErrorFound := False;
  XMLFile := TJvSimpleXML.Create(Self);
  with ListViewFiles.Items.Item[FileIndex] do
  try
    XMLFile.Options := XMLFile.Options + [sxoAutoEncodeEntity,sxoAutoEncodeValue];
    try
      XMLFile.FileName := Caption;
    except
      ImageIndex := 2;
      SubItems.Strings[0] := RsFileError;
      FreeAndNil(XMLFile);
    end;
    if Assigned(XMLFile) then
    begin
      FileValid := (XMLFile.Root.Name = 'PROJECT') and CheckProjectNode(XMLFile.Root);
      if (FileValid) then
      begin
        if (ErrorFound) then
        begin
          ImageIndex := 4;
          SubItems.Strings[0] := RsFileFixed;
        end else
        begin
          ImageIndex := 1;
          SubItems.Strings[0] := RsFileOK;
        end;
        if FixFile then
        begin
          XMLFile.SaveToFile(Caption);
          MemoFile.Lines.LoadFromFile(Caption);
        end;
      end else
      begin
        ImageIndex := 3;
        SubItems.Strings[0] := RsFileInvalid;
      end;
    end;
  finally
    XMLFile.Free;
  end;
end;

end.












