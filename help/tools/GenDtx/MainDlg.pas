unit MainDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, MainCtrl, Settings, JvComponent, JvProgressComponent,
  ComCtrls, ToolWin, ExtCtrls, Menus, Buttons, JvFormPlacement,
  JvAppStorage, JvAppRegistryStorage, JvComponentBase;

const
  CM_CheckDirectories = WM_APP + 1;

type
  TfrmMain = class(TForm)
    ActionList1: TActionList;
    actIncludeAll: TAction;
    actExcludeAll: TAction;
    actInclude: TAction;
    actExclude: TAction;
    actSettings: TAction;
    actGenerateDtxFiles: TAction;
    actAddToIgnoreUnitList: TAction;
    actAddToCompletedList: TAction;
    actUnitStatus: TAction;
    Panel1: TPanel;
    lsbSource: TListBox;
    btnInclude: TButton;
    btnIncludeAll: TButton;
    btnExclude: TButton;
    btnExcludeAll: TButton;
    lsbDest: TListBox;
    Button2: TButton;
    Button3: TButton;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    actShowCompleted: TAction;
    actShowIgnored: TAction;
    actShowOther: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    actRefresh: TAction;
    ToolButton9: TToolButton;
    actShowGenerated: TAction;
    actCheckDtxFiles: TAction;
    actClearGeneratedDtxDir: TAction;
    actClearMessages: TAction;
    actSaveMessages: TAction;
    actGeneratePackageList: TAction;
    actGenerateRegisteredClasses: TAction;
    actCheckPasFiles: TAction;
    actCheckCasePasFiles: TAction;
    MainMenu1: TMainMenu;
    View1: TMenuItem;
    ShowCompleted1: TMenuItem;
    ShowIgnored1: TMenuItem;
    ShowGenerated1: TMenuItem;
    ShowOther1: TMenuItem;
    N1: TMenuItem;
    Refresh1: TMenuItem;
    Check: TMenuItem;
    CheckDtxfiles1: TMenuItem;
    Checkpasfiles1: TMenuItem;
    Checkcasepasfiles1: TMenuItem;
    Generate1: TMenuItem;
    GenerateDtxfiles1: TMenuItem;
    GeneratePackage1: TMenuItem;
    GenerateRegisteredClasses1: TMenuItem;
    Options1: TMenuItem;
    Settings1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    lsbMessages: TListBox;
    Button4: TButton;
    Button5: TButton;
    Label3: TLabel;
    actDirectories: TAction;
    Directories1: TMenuItem;
    N2: TMenuItem;
    UnitStatus1: TMenuItem;
    N3: TMenuItem;
    ClearGenerateddtxDir1: TMenuItem;
    actCheckCasePasFilesAll: TAction;
    CheckCasinginPasFilesAll1: TMenuItem;
    actCheckDuplicateTypes: TAction;
    CheckDuplicateTypes1: TMenuItem;
    actGenerateList: TAction;
    GenerateList1: TMenuItem;
    Button1: TButton;
    actCopyToClipboard: TAction;
    actAddToIgnoreTokenList: TAction;
    actGenerateClassStructure: TAction;
    GenerateClassStructure1: TMenuItem;
    actSortPas: TAction;
    SortImplPas1: TMenuItem;
    JvAppRegistryStore1: TJvAppRegistryStorage;
    JvFormStorage1: TJvFormStorage;
    actCheckDtxFilesDialog: TAction;
    CheckDtxFilesDialog1: TMenuItem;
    procedure actAddToCompletedListExecute(Sender: TObject);
    procedure actAddToIgnoreUnitListExecute(Sender: TObject);
    procedure actCheckCasePasFilesExecute(Sender: TObject);
    procedure actCheckCasePasFilesUpdate(Sender: TObject);
    procedure actCheckDtxFilesExecute(Sender: TObject);
    procedure actCheckDtxFilesUpdate(Sender: TObject);
    procedure actCheckPasFilesExecute(Sender: TObject);
    procedure actCheckPasFilesUpdate(Sender: TObject);
    procedure actClearGeneratedDtxDirExecute(Sender: TObject);
    procedure actClearMessagesExecute(Sender: TObject);
    procedure actDirectoriesExecute(Sender: TObject);
    procedure actExcludeAllExecute(Sender: TObject);
    procedure actExcludeAllUpdate(Sender: TObject);
    procedure actExcludeExecute(Sender: TObject);
    procedure actExcludeUpdate(Sender: TObject);
    procedure actGenerateDtxFilesExecute(Sender: TObject);
    procedure actGenerateDtxFilesUpdate(Sender: TObject);
    procedure actGeneratePackageListExecute(Sender: TObject);
    procedure actGeneratePackageListUpdate(Sender: TObject);
    procedure actGenerateRegisteredClassesExecute(Sender: TObject);
    procedure actGenerateRegisteredClassesUpdate(Sender: TObject);
    procedure actIncludeAllExecute(Sender: TObject);
    procedure actIncludeAllUpdate(Sender: TObject);
    procedure actIncludeExecute(Sender: TObject);
    procedure actIncludeUpdate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actSaveMessagesExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowCompletedExecute(Sender: TObject);
    procedure actShowCompletedUpdate(Sender: TObject);
    procedure actShowGeneratedExecute(Sender: TObject);
    procedure actShowGeneratedUpdate(Sender: TObject);
    procedure actShowIgnoredExecute(Sender: TObject);
    procedure actShowIgnoredUpdate(Sender: TObject);
    procedure actShowOtherExecute(Sender: TObject);
    procedure actShowOtherUpdate(Sender: TObject);
    procedure actUnitStatusExecute(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectedProcessFilesAvailable(Sender: TObject);
    procedure actCheckCasePasFilesAllExecute(Sender: TObject);
    procedure actCheckDuplicateTypesUpdate(Sender: TObject);
    procedure actCheckDuplicateTypesExecute(Sender: TObject);
    procedure actGenerateListExecute(Sender: TObject);
    procedure actGenerateListUpdate(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actGenerateClassStructureExecute(Sender: TObject);
    procedure actGenerateClassStructureUpdate(Sender: TObject);
    procedure actSortPasUpdate(Sender: TObject);
    procedure actSortPasExecute(Sender: TObject);
    procedure JvFormStorage1RestorePlacement(Sender: TObject);
    procedure JvFormStorage1SavePlacement(Sender: TObject);
    procedure actCheckDtxFilesDialogExecute(Sender: TObject);
    procedure lsbSourceDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lsbDestDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FMainCtrl: TMainCtrl;
    function ProcessFilesAvailable: Boolean;
  protected
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure MoveSelectedToCompletedList(List: TCustomListBox);
    procedure MoveSelectedToIgnoredList(List: TCustomListBox);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure CMCheckDirectories(var Msg: TMessage); message CM_CheckDirectories;

    procedure LoadOptions;
    procedure SaveOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JclFileUtils, JvDSADialogs,
  SettingsDlg, DirectoriesDlg, DelphiParser, UnitStatusDlg, ClipBrd;

{$R *.dfm}
const
  { TODO: default values uitbreiden }
{ TODO: %code toevoegen }
{ TODO: %value toevoegen }
{ TODO: %scope toevoegen voor properties/methods }
{ TODO: Set -> Enumeration; Set zelf toevoegen }

  CGenerateDtxFilesID = 1;
  CGeneratePackageListID = 2;

  CRegisteredClassesID = 3;

  CCheckDtxFilesID = 4;
  CCheckPasFilesID = 5;
  CCheckCasingInPasFilesID = 6;

  //=== Local procedures =======================================================

function DelTreeEx(const Path: string): Boolean;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin
  Result := True;
  Files := TStringList.Create;
  try
    LPath := PathRemoveSeparator(Path);
    BuildFileList(LPath + '\*.dtx', faAnyFile, Files);
    for I := 0 to Files.Count - 1 do
    begin
      FileName := LPath + '\' + Files[I];
      PartialResult := True;
      // If the current file is itself a directory then recursively delete it
      Attr := GetFileAttributes(PChar(FileName));
      if (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        {PartialResult := DelTreeEx(FileName)}
      else
      begin
        if PartialResult then
        begin
          // Set attributes to normal in case it's a readonly file
          PartialResult := SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
          if PartialResult then
            PartialResult := DeleteFile(FileName);
        end;
      end;
      if not PartialResult then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

//=== TfrmMain ===============================================================

procedure TfrmMain.actAddToCompletedListExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelectedToCompletedList(lsbDest);
  SetItem(lsbDest, Index);
end;

procedure TfrmMain.actAddToIgnoreUnitListExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelectedToIgnoredList(lsbDest);
  SetItem(lsbDest, Index);
end;

procedure TfrmMain.actCheckCasePasFilesAllExecute(Sender: TObject);
begin
  FMainCtrl.CheckCasingPasFiles(True, True);
end;

procedure TfrmMain.actCheckCasePasFilesExecute(Sender: TObject);
begin
  FMainCtrl.CheckCasingPasFiles(False, True);
end;

procedure TfrmMain.actCheckCasePasFilesUpdate(Sender: TObject);
begin
  { RunTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and (TSettings.Instance.RunTimePasDir > '');
end;

procedure TfrmMain.actCheckDtxFilesDialogExecute(Sender: TObject);
begin
  FMainCtrl.CheckDtxFiles(True);
end;

procedure TfrmMain.actCheckDtxFilesExecute(Sender: TObject);
begin
  FMainCtrl.CheckDtxFiles(False);
end;

procedure TfrmMain.actCheckDtxFilesUpdate(Sender: TObject);
begin
  { RunTimePasDir, RealDtxDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and
      (TSettings.Instance.RunTimePasDir > '') and (TSettings.Instance.RealDtxDir > '');
end;

procedure TfrmMain.actCheckDuplicateTypesExecute(Sender: TObject);
begin
  FMainCtrl.CheckDuplicateTypes;
end;

procedure TfrmMain.actCheckDuplicateTypesUpdate(Sender: TObject);
begin
  { RunTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and (TSettings.Instance.RunTimePasDir > '');
end;

procedure TfrmMain.actCheckPasFilesExecute(Sender: TObject);
begin
  FMainCtrl.CheckPasFiles;
end;

procedure TfrmMain.actCheckPasFilesUpdate(Sender: TObject);
begin
  { RunTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and (TSettings.Instance.RunTimePasDir > '');
end;

procedure TfrmMain.actClearGeneratedDtxDirExecute(Sender: TObject);
var
  Cursor: TCursor;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if not DelTreeEx(TSettings.Instance.GeneratedDtxDir) then
      RaiseLastOSError;
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TfrmMain.actClearMessagesExecute(Sender: TObject);
begin
  lsbMessages.Clear;
end;

procedure TfrmMain.actCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := lsbMessages.Items.Text;
end;

procedure TfrmMain.actDirectoriesExecute(Sender: TObject);
begin
  TfrmDirectories.Execute;
end;

procedure TfrmMain.actExcludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to lsbDest.Items.Count - 1 do
    lsbSource.Items.AddObject(lsbDest.Items[I], lsbDest.Items.Objects[I]);
  lsbDest.Items.Clear;
  SetItem(lsbDest, 0);
end;

procedure TfrmMain.actExcludeAllUpdate(Sender: TObject);
begin
  actExcludeAll.Enabled := lsbDest.Items.Count > 0;
end;

procedure TfrmMain.actExcludeExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbDest);
  MoveSelected(lsbDest, lsbSource.Items);
  SetItem(lsbDest, Index);
end;

procedure TfrmMain.actExcludeUpdate(Sender: TObject);
begin
  actExclude.Enabled := lsbDest.SelCount > 0;
end;

procedure TfrmMain.actGenerateClassStructureExecute(Sender: TObject);
begin
  FMainCtrl.GenerateClassStructure;
end;

procedure TfrmMain.actGenerateClassStructureUpdate(Sender: TObject);
begin
  { RunTimePasDir or DelphiDir }
  if Sender is TAction then
    TAction(Sender).Enabled :=
      (TSettings.Instance.RunTimePasDir > '') or
      (TSettings.Instance.DelphiRootSourceDir > '');
end;

procedure TfrmMain.actGenerateDtxFilesExecute(Sender: TObject);
begin
  FMainCtrl.GenerateDtxFiles;
end;

procedure TfrmMain.actGenerateDtxFilesUpdate(Sender: TObject);
begin
  { GeneratedDtxDir, RunTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and
      (TSettings.Instance.GeneratedDtxDir > '') and (TSettings.Instance.RunTimePasDir > '');
end;

procedure TfrmMain.actGenerateListExecute(Sender: TObject);
begin
  FMainCtrl.GenerateList;
end;

procedure TfrmMain.actGenerateListUpdate(Sender: TObject);
begin
  { RunTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable and (TSettings.Instance.RunTimePasDir > '');
end;

procedure TfrmMain.actGeneratePackageListExecute(Sender: TObject);
begin
  FMainCtrl.GeneratePackageList;
end;

procedure TfrmMain.actGeneratePackageListUpdate(Sender: TObject);
begin
  { PackageDir }
  if Sender is TAction then
    TAction(Sender).Enabled := TSettings.Instance.PackageDir > '';
end;

procedure TfrmMain.actGenerateRegisteredClassesExecute(Sender: TObject);
begin
  FMainCtrl.GenerateRegisteredClassesList;
end;

procedure TfrmMain.actGenerateRegisteredClassesUpdate(Sender: TObject);
begin
  { DesignTimePasDir }
  if Sender is TAction then
    TAction(Sender).Enabled := TSettings.Instance.DesignTimePasDir > '';
end;

procedure TfrmMain.actIncludeAllExecute(Sender: TObject);
var
  I: Integer;
begin
  lsbDest.Items.BeginUpdate;
  try
    for I := 0 to lsbSource.Items.Count - 1 do
      lsbDest.Items.AddObject(lsbSource.Items[I],
        lsbSource.Items.Objects[I]);
  finally
    lsbDest.Items.EndUpdate;
  end;
  lsbSource.Items.Clear;
  SetItem(lsbSource, 0);
end;

procedure TfrmMain.actIncludeAllUpdate(Sender: TObject);
begin
  actIncludeAll.Enabled := lsbSource.Items.Count > 0;
end;

procedure TfrmMain.actIncludeExecute(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(lsbSource);
  MoveSelected(lsbSource, lsbDest.Items);
  SetItem(lsbSource, Index);
end;

procedure TfrmMain.actIncludeUpdate(Sender: TObject);
begin
  actInclude.Enabled := lsbSource.SelCount > 0;
end;

procedure TfrmMain.actRefreshExecute(Sender: TObject);
begin
  FMainCtrl.RefreshFiles;
end;

procedure TfrmMain.actSaveMessagesExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Application) do
  try
    DefaultExt := 'txt';
    Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';

    if Execute then
      lsbMessages.Items.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  TfrmSettings.Execute;
end;

procedure TfrmMain.actShowCompletedExecute(Sender: TObject);
begin
  FMainCtrl.ShowCompletedFiles := not FMainCtrl.ShowCompletedFiles;
end;

procedure TfrmMain.actShowCompletedUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowCompletedFiles;
end;

procedure TfrmMain.actShowGeneratedExecute(Sender: TObject);
begin
  FMainCtrl.ShowGeneratedFiles := not FMainCtrl.ShowGeneratedFiles;
end;

procedure TfrmMain.actShowGeneratedUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowGeneratedFiles;
end;

procedure TfrmMain.actShowIgnoredExecute(Sender: TObject);
begin
  FMainCtrl.ShowIgnoredFiles := not FMainCtrl.ShowIgnoredFiles;
end;

procedure TfrmMain.actShowIgnoredUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowIgnoredFiles;
end;

procedure TfrmMain.actShowOtherExecute(Sender: TObject);
begin
  FMainCtrl.ShowOtherFiles := not FMainCtrl.ShowOtherFiles;
end;

procedure TfrmMain.actShowOtherUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := FMainCtrl.ShowOtherFiles
end;

procedure TfrmMain.actSortPasExecute(Sender: TObject);
begin
  FMainCtrl.SortPasFiles;
  //FMainCtrl.RecapitalizeFiles;
end;

procedure TfrmMain.actSortPasUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := ProcessFilesAvailable;
end;

procedure TfrmMain.actUnitStatusExecute(Sender: TObject);
begin
  TfrmUnitStatus.Execute(FMainCtrl);
end;

procedure TfrmMain.CMCheckDirectories(var Msg: TMessage);
begin
  with TSettings.Instance do
    if (RunTimePasDir = '') or (DesignTimePasDir = '') or
      (PackageDir = '') or (RealDtxDir = '') or (GeneratedDtxDir = '') or
      (DelphiRootSourceDir = '') then

      actDirectories.Execute;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainCtrl := TMainCtrl.Create;
end;

destructor TfrmMain.Destroy;
begin
  FMainCtrl.Free;

  inherited Destroy;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMainCtrl.SkipList := lsbSource.Items;
  FMainCtrl.ProcessList := lsbDest.Items;
  FMainCtrl.MessagesList := lsbMessages.Items;

  FMainCtrl.RefreshFiles;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, CM_CheckDirectories, 0, 0);
end;

function TfrmMain.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then
      Exit;
  Result := LB_ERR;
end;

procedure TfrmMain.JvFormStorage1RestorePlacement(Sender: TObject);
begin
  LoadOptions;
end;

procedure TfrmMain.JvFormStorage1SavePlacement(Sender: TObject);
begin
  SaveOptions;
end;

procedure TfrmMain.LoadOptions;
begin
  with JvAppRegistryStore1 do
  begin
    FMainCtrl.ShowCompletedFiles := ReadBoolean('Main\ShowCompletedFiles', False);
    FMainCtrl.ShowIgnoredFiles := ReadBoolean('Main\ShowIgnoredFiles', False);
    FMainCtrl.ShowGeneratedFiles := ReadBoolean('Main\ShowGeneratedFiles', True);
    FMainCtrl.ShowOtherFiles := ReadBoolean('Main\ShowOtherFiles', False);
  end;
end;

procedure TfrmMain.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    List.Items.BeginUpdate;
    try
      for I := List.Items.Count - 1 downto 0 do
        if List.Selected[I] then
        begin
          Items.AddObject(List.Items[I], List.Items.Objects[I]);
          List.Items.Delete(I);
        end;
    finally
      List.Items.EndUpdate;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TfrmMain.MoveSelectedToCompletedList(List: TCustomListBox);
var
  I: Integer;
begin
  with List do
  begin
    Items.BeginUpdate;
    try
      for I := Items.Count - 1 downto 0 do
        if Selected[I] then
          FMainCtrl.AddToCompletedList(Items[I]);
    finally
      Items.EndUpdate;
    end;
  end;

  TSettings.Instance.SaveUnitStatus(usCompleted);
end;

procedure TfrmMain.MoveSelectedToIgnoredList(List: TCustomListBox);
var
  I: Integer;
begin
  with List do
  begin
    Items.BeginUpdate;
    try
      for I := Items.Count - 1 downto 0 do
        if Selected[I] then
          FMainCtrl.AddToIgnoreList(Items[I]);
    finally
      Items.EndUpdate;
    end;
  end;

  TSettings.Instance.SaveUnitStatus(usIgnored);
end;

function TfrmMain.ProcessFilesAvailable: Boolean;
begin
  Result := lsbDest.Count > 0;
end;

procedure TfrmMain.SaveOptions;
begin
  with JvAppRegistryStore1 do
  begin
    WriteBoolean('Main\ShowCompletedFiles', FMainCtrl.ShowCompletedFiles);
    WriteBoolean('Main\ShowIgnoredFiles', FMainCtrl.ShowIgnoredFiles);
    WriteBoolean('Main\ShowGeneratedFiles', FMainCtrl.ShowGeneratedFiles);
    WriteBoolean('Main\ShowOtherFiles', FMainCtrl.ShowOtherFiles);
  end;
end;

procedure TfrmMain.SelectedProcessFilesAvailable(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbDest.SelCount > 0;
end;

procedure TfrmMain.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then
      Index := 0
    else
      if Index > MaxIndex then
      Index := MaxIndex;
    Selected[Index] := True;
  end;
end;

const
  CFileStatusColors: array[TFileStatus] of TColor = (clGreen, clRed, clBlack);

procedure TfrmMain.lsbSourceDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
  begin
    Canvas.FillRect(Rect);
    Canvas.Font.Color := CFileStatusColors[TFileStatus(Items.Objects[Index])];
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index])
  end;
end;

procedure TfrmMain.lsbDestDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
  begin
    Canvas.FillRect(Rect);
    Canvas.Font.Color := CFileStatusColors[TFileStatus(Items.Objects[Index])];
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index])
  end;
end;

end.

