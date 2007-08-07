unit HelpBuildData;

interface

uses
  Windows, Classes, JVCLHelpUtils, ContNrs;

const
  HelpBuildType_SyncOnlineHelp: TGUID = '{687636C9-D401-43B6-9965-90ADC0855B68}';
  HelpBuildType_GenerateZipForUpdatingOnlineHelp: TGUID = '{334E351A-FDD8-4916-8594-657153CFF2AF}';
  HelpBuildType_CheckDtx: TGUID = '{ACF16F68-DD26-42AF-9BC9-945AE25C9839}';
  HelpBuildType_CheckDtxJCL: TGUID = '{1312A172-1E45-45F9-B3C8-F45F15DB6DBE}';
  HelpBuildType_BuildHelp: TGUID = '{8C4D6A95-7D7F-4ECC-993E-CDFA388F6E49}';
  HelpBuildType_BuildReferenceHelp: TGUID = '{12A688AB-7E49-4989-A1C5-2A5576F238E9}';
  HelpBuildType_BuildPackageHelp: TGUID = '{B28EFBF7-750D-4810-88D8-03E308698EB2}';
  HelpBuildType_Diagnose: TGUID = '{F6B9333B-CD40-4511-B235-87B72258121D}';
  HelpBuildType_PostProcessHelpOutput: TGUID = '{CAFCF478-78F7-44FC-B440-BECC1B6A6E2A}';

  HelpBuildTask_UnzipOnlineHelpZipFile: TGUID = '{C116953F-DF50-48E6-B905-A098E8692D76}';
  HelpBuildTask_DetermineMinDateForZippedFiles: TGUID = '{DF7DC7F9-53C6-4DBB-B1AF-AD19934FD5F0}';
  HelpBuildTask_GenerateImages: TGUID = '{7E852676-3694-4B4D-A115-EEBD6477846F}';
  HelpBuildTask_GenerateRegisteredClassesFile: TGUID = '{2BBB2F94-AB65-474F-838D-F645AB9C0F4D}';
  HelpBuildTask_GenerateGroupsFile: TGUID = '{0E524B8A-1B4C-4238-ACBB-9761DA813EBD}';
  HelpBuildTask_GeneratePackageList: TGUID = '{7E2C8B08-5962-463A-828B-0B673015BA7D}';
  HelpBuildTask_GenerateDonatorsDtxFile: TGUID = '{B562FF98-EFE1-403F-ABBB-FD3643BCFFC5}';
  HelpBuildTask_GenerateFuncRefDtxFile: TGUID = '{215A58D4-DD0F-4F09-A498-950D8D4571D1}';
  HelpBuildTask_GenerateStrippedPasFiles: TGUID = '{61A607E2-DD30-460F-AC4D-C341599EDCED}';
  HelpBuildTask_GenerateFormattedDtxFiles: TGUID = '{45E8B8BB-CCD3-4E37-87D8-8AD4178CBAD7}';
  HelpBuildTask_PreProcessOnlineDtxFiles: TGUID = '{676DA52B-8FDF-43A1-928C-F62F519E358B}';
  HelpBuildTask_GeneratePackagesDtxFile: TGUID = '{A7BFC951-E799-4B4C-8664-AE6620D055C8}';
  HelpBuildTask_GenerateHelpDoxFile: TGUID = '{C2AFD0EF-2AF5-441E-B894-57CB298455DB}';
  HelpBuildTask_GenerateReferenceDoxFile: TGUID = '{E4DA26D1-2C3E-44C7-8420-9E085F794E36}';
  HelpBuildTask_GeneratePackagesDoxFile: TGUID = '{28F14377-8E0B-4B95-9CB2-FBB443A48EBC}';
  // build dox file using dmcc.exe -> output html format
  HelpBuildTask_BuildHelpDoxFile_HTML: TGUID = '{9E12EF83-8398-49B5-A652-44FE5DF2CCA3}';
  // build dox file using dmcc.exe -> output winhelp format
  HelpBuildTask_BuildHelpDoxFile_WinHelp: TGUID = '{B8E2989E-49B9-4B5C-8916-9A4E18514FE8}';
  HelpBuildTask_BuildReferenceDoxFile_PDF: TGUID = '{3D3371C1-88B0-45B4-8E3A-EA34B0234DF2}';
  HelpBuildTask_BuildPackagesDoxFile_PDF: TGUID = '{E2360FB3-CD8F-4C01-98D7-FAEE66DBEAEC}';
  HelpBuildTask_PostProcessWInHelpOutput: TGUID = '{06325FAC-2AD7-49E9-942E-8E088BBF83C7}';
  HelpBuildTask_PostProcessHtmlHelpOutput: TGUID = '{839C74CF-340A-46E2-9B17-FBC1ECFD8112}';
  // build winhelp using xx
  HelpBuildTask_CompileWinHelp: TGUID = '{5363F3AA-31DD-452D-B196-C80AD692E082}';
  // build html using xx
  HelpBuildTask_CompileHtmlHelp: TGUID = '{DACCFC10-A838-4F78-9181-457E29016340}';
  HelpBuildTask_GenerateOnlineHelpZipFiles: TGUID = '{1EC4BD93-61D7-4F66-89DA-090DE07DA05F}';
  HelpBuildTask_GenerateDtxWordsFile: TGUID = '{5CF479F0-C615-4CC9-8FBA-0F59CE0D0443}';
  HelpBuildTask_GenerateDtxIdentifiersFile: TGUID = '{442EE22B-AAA0-45EF-917B-712D501E7ABF}';
  HelpBuildTask_GenerateDtxInvalidWordsFile: TGUID = '{54DE8DF8-7E2C-46AF-9E98-BAF8CDDD35AD}';
  HelpBuildTask_GenerateDtxDuplicateIdentifiersFile: TGUID = '{B631261A-5B78-4DFD-BD13-575B9E9855D2}';
  HelpBuildTask_ZipWinHelp: TGUID = '{13D60A6C-BB10-45DE-8D3E-62FDF80F2C81}';
  HelpBuildTask_ZipHtmlHelp: TGUID = '{59695E92-9DB2-4C35-B186-E967071E10CA}';
  HelpBuildTask_ZipPackagesPDF: TGUID = '{AEA62954-2A0E-4081-8FB6-853BFD2F0CB6}';
  HelpBuildTask_ZipReferencePDF: TGUID = '{83778CC2-A92F-4F4B-9235-BE4FAAD50C55}';

type
  THelpBuildData = class
  private
    FHelpDir: string;
    FJVCLxxDir: string;
    FHelpBuildType: TGUID;
    FTasks: TGUIDList;
    FHelpBuildTypesTasks: TGUIDList;
    FMinPercComplete: Integer;
    FMaxFilesInDox: Integer;
    FHHCFileName: string;
    FHCRTFFileName: string;
    FOnlineHelpZipFileName: string;
    FMaxFilesInZip: Integer;
    FMinDateForZippedFiles: TDateTime;
    FWZUnzipFileName: string;
    FWZZipFileName: string;
    FDtxInvalidWordsFileName: string;
    FDtxWordsFileName: string;
    FDictionaryFileName: string;
    FDtxIdentifiersFileName: string;
    FIgnoreWordsContainingNumbers: Boolean;
    FNotWhiteSpaceStr: string;
    FDtxDuplicateIdentifiersFileName: string;
    FDtxSelectionKind: TDtxSelectionKind;
    FSpecificDtxFiles: TStringList;
    FDoxName: string;
    FJVCLMajorVersion: string;
    FJVCLMinorVersion: string;
    FLoading: Boolean;
    FDoxTitle: string;
    FDMCCFileName: string;
    function GetDonatorsDtxFileName: string;
    function GetDoxFileName: string;
    function GetFuncRefDtxFileName: string;
    function GetGenDtxDir: string;
    function GetGenPasDir: string;
    function GetGroupListFileName: string;
    function GetGroupsTreeFileName: string;
    function GetHelpGeneratedIncludesDir: string;
    function GetHelpGenericDir: string;
    function GetHelpImagesCompDir: string;
    function GetHelpInfoDir: string;
    function GetHelpOutputDir: string;
    function GetHtmlHelpOutputDir: string;
    function GetJVCLxxDesignDir: string;
    function GetJVCLxxImagesDir: string;
    function GetJVCLxxRunDir: string;
    function GetMustCreateGenDtxDir: Boolean;
    function GetMustCreateGenPasDir: Boolean;
    function GetMustCreateGenZipDir: Boolean;
    function GetOnlineHelpDtxDir: string;
    function GetGenZipDir: string;
    //    function GetOptionalTasks: THelpBuildTasks;
    function GetPackageDescFileName: string;
    function GetPackageDir: string;
    function GetPackageListFileName: string;
    function GetPackagesDtxFileName: string;
    function GetRegisteredClassesFileName: string;
    function GetRootDir: string;
    //    function GetTask(const Index: THelpBuildTask): Boolean;
    function GetTemplateDoxFileName: string;
    function GetToolsDir: string;
    function GetWinHelpOutputDir: string;
    procedure SetHelpBuildType(const Value: TGUID);
    //    procedure SetTask(const Index: THelpBuildTask; const Value: Boolean);
    procedure SetToDefaultTasks;
    function GetSpecificDtxFiles: TStrings;
    procedure SetSpecificDtxFiles(Value: TStrings);
    procedure SetHelpDir(const Value: string);
    procedure SetJVCLxxDir(const Value: string);
    procedure SetJVCLMajorVersion(const Value: string);
    procedure SetJVCLMinorVersion(const Value: string);
    function GetTaskGUID(const AGUID: TGUID): Boolean;
    procedure SetTaskGUID(const AGUID: TGUID; const Value: Boolean);
    function GetHelpBuildType(const Index: Integer): TGUID;
    function GetHelpBuildTypesTasks(const Index: Integer): TGUIDList;
    function GetIsOptionalTask(const AGUID: TGUID): Boolean;
    function GetReferenceDoxFileName: string;
    function GetPackagesDoxFileName: string;
    function GetPDFHelpOutputDir: string;
  protected
    procedure VersionChanged;

    property HelpBuildTypes[const Index: Integer]: TGUID read GetHelpBuildType;
    property HelpBuildTypesTasks[const Index: Integer]: TGUIDList read GetHelpBuildTypesTasks;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    { Directories }
    property HelpDir: string read FHelpDir write SetHelpDir;
    property RootDir: string read GetRootDir;
    property ToolsDir: string read GetToolsDir;
    property GenDtxDir: string read GetGenDtxDir;
    property GenPasDir: string read GetGenPasDir;
    property OnlineHelpDtxDir: string read GetOnlineHelpDtxDir;
    property JVCLxxDir: string read FJVCLxxDir write SetJVCLxxDir;
    property JVCLxxRunDir: string read GetJVCLxxRunDir;
    property JVCLxxDesignDir: string read GetJVCLxxDesignDir;
    property PackageDir: string read GetPackageDir;
    property HelpGeneratedIncludesDir: string read GetHelpGeneratedIncludesDir;
    property HelpInfoDir: string read GetHelpInfoDir;
    property HelpGenericDir: string read GetHelpGenericDir;
    property HelpOutputDir: string read GetHelpOutputdir;
    property WinHelpOutputDir: string read GetWinHelpOutputDir;
    property HtmlHelpOutputDir: string read GetHtmlHelpOutputDir;
    property PDFHelpOutputDir: string read GetPDFHelpOutputDir;
    property HelpImagesCompDir: string read GetHelpImagesCompDir;
    property JVCL3ImagesDir: string read GetJVCLxxImagesDir;
    property GenZipDir: string read GetGenZipDir;

    property MustCreateGenDtxDir: Boolean read GetMustCreateGenDtxDir;
    property MustCreateGenPasDir: Boolean read GetMustCreateGenPasDir;
    property MustCreateGenZipDir: Boolean read GetMustCreateGenZipDir;

    { FileNames }
    // list with "packagename=filename" where pas file "filename" is in package "packagename"
    property PackageListFileName: string read GetPackageListFileName;
    property GroupListFileName: string read GetGroupListFileName;
    property RegisteredClassesFileName: string read GetRegisteredClassesFileName;
    property DonatorsDtxFileName: string read GetDonatorsDtxFileName;
    property FuncRefDtxFileName: string read GetFuncRefDtxFileName;
    property PackagesDtxFileName: string read GetPackagesDtxFileName;
    property PackageDescFileName: string read GetPackageDescFileName;
    property GroupsTreeFileName: string read GetGroupsTreeFileName;
    property TemplateDoxFileName: string read GetTemplateDoxFileName;
    property DoxFileName: string read GetDoxFileName;
    property ReferenceDoxFileName: string read GetReferenceDoxFileName;
    property PackagesDoxFileName: string read GetPackagesDoxFileName;
    property DoxName: string read FDoxName write FDoxName;
    property DoxTitle: string read FDoxTitle write FDoxTitle;
    property JVCLMajorVersion: string read FJVCLMajorVersion write SetJVCLMajorVersion;
    property JVCLMinorVersion: string read FJVCLMinorVersion write SetJVCLMinorVersion;
    property OnlineHelpZipFileName: string read FOnlineHelpZipFileName write FOnlineHelpZipFileName;
    // HTML help compiler
    property HHCFileName: string read FHHCFileName write FHHCFileName;
    // Windows help compiler
    property HCRTFFileName: string read FHCRTFFileName write FHCRTFFileName;
    // Winzip zipper 'wzzip'
    property WZZipFileName: string read FWZZipFileName write FWZZipFileName;
    // Winzip unzipper 'wzunzip'
    property WZUnzipFileName: string read FWZUnzipFileName write FWZUnzipFileName;
    //
    property DMCCFileName: string read FDMCCFileName write FDMCCFileName;

    property DtxWordsFileName: string read FDtxWordsFileName write FDtxWordsFileName;
    property DtxIdentifiersFileName: string read FDtxIdentifiersFileName write FDtxIdentifiersFileName;
    property DictionaryFileName: string read FDictionaryFileName write FDictionaryFileName;
    property DtxInvalidWordsFileName: string read FDtxInvalidWordsFileName write FDtxInvalidWordsFileName;
    property DtxDuplicateIdentifiersFileName: string read FDtxDuplicateIdentifiersFileName write
      FDtxDuplicateIdentifiersFileName;

    { Options }
    property HelpBuildType: TGUID read FHelpBuildType write SetHelpBuildType;
    //    property OptionalTasks: THelpBuildTasks read GetOptionalTasks;
    //    property Task[const Index: THelpBuildTask]: Boolean read GetTask write SetTask;
    property MinPercComplete: Integer read FMinPercComplete write FMinPercComplete;
    property MaxFilesInDox: Integer read FMaxFilesInDox write FMaxFilesInDox;
    property SpecificDtxFiles: TStrings read GetSpecificDtxFiles write SetSpecificDtxFiles;
    property DtxSelectionKind: TDtxSelectionKind read FDtxSelectionKind write FDtxSelectionKind;
    property MinDateForZippedFiles: TDateTime read FMinDateForZippedFiles write FMinDateForZippedFiles;
    property MaxFilesInZip: Integer read FMaxFilesInZip write FMaxFilesInZip;
    property NotWhiteSpaceStr: string read FNotWhiteSpaceStr write FNotWhiteSpaceStr;
    property IgnoreWordsContainingNumbers: Boolean read FIgnoreWordsContainingNumbers write
      FIgnoreWordsContainingNumbers;

    property IsOptionalTask[const AGUID: TGUID]: Boolean read GetIsOptionalTask;
    property Task[const AGUID: TGUID]: Boolean read GetTaskGUID write SetTaskGUID;

    //    property GenerateImages: Boolean index flGenerateImages read GetTask write SetTask;
    //    property GenerateRegisteredClassesFile: Boolean index flGenerateRegisteredClassesFile read GetTask write SetTask;
    //    property GenerateGroupsFile: Boolean index flGenerateGroupsFile read GetTask write SetTask;
    //    property GenerateStrippedPasFiles: Boolean index flGenerateStrippedPasFiles read GetTask write SetTask;
    //    property GeneratePackageList: Boolean index flGeneratePackageList read GetTask write SetTask;
    //    property GenerateDonatorsDtxFile: Boolean index flGenerateDonatorsDtxFile read GetTask write SetTask;
    //    property GenerateFormattedDtxFiles: Boolean index flGenerateFormattedDtxFiles read GetTask write SetTask;
    //    property GenerateFuncRefDtxFile: Boolean index flGenerateFuncRefDtxFile read GetTask write SetTask;
    //    property GeneratePackagesDtxFile: Boolean index flGeneratePackagesDtxFile read GetTask write SetTask;
    //    property GenerateDoxFile: Boolean index flGenerateDoxFile read GetTask write SetTask;
    //    property BuildDoxFile: Boolean index flBuildDoxFile read GetTask write SetTask;
    //    property PostProcessWinHelpOutput: Boolean index flPostProcessWinHelpOutput read GetTask write SetTask;
    //    property PostProcessHtmlHelpOutput: Boolean index flPostProcessHtmlHelpOutput read GetTask write SetTask;
    //    property BuildWinHelp: Boolean index flBuildWinHelp read GetTask write SetTask;
    //    property BuildHtmlHelp: Boolean index flBuildHtmlHelp read GetTask write SetTask;
    //    property UnzipOnlineHelpZipFile: Boolean index flUnzipOnlineHelpZipFile read GetTask write SetTask;
    //    property GenerateOnlineHelpZipFiles: Boolean index flGenerateOnlineHelpZipFiles read GetTask write SetTask;
    //    property DetermineMinDateForZippedFiles: Boolean index flDetermineMinDateForZippedFiles read GetTask write SetTask;
    //    property GenerateDtxWordsFile: Boolean index flGenerateDtxWordsFile read GetTask write SetTask;
    //    property GenerateDtxInvalidWordsFile: Boolean index flGenerateDtxInvalidWordsFile read GetTask write SetTask;
    //    property GenerateDtxIdentifiersFile: Boolean index flGenerateDtxIdentifiersFile read GetTask write SetTask;
    //    property GenerateDtxDuplicateIdentifiersFile: Boolean index flGenerateDtxDuplicateIdentifiersFile read GetTask write
    //      SetTask;
  end;

function AllHelpBuildTypes: TGUIDList;
function AllHelpBuildTasks(const AHelpBuildType: TGUID): TGUIDList;
function HelpBuildNiceStr(const AGUID: TGUID): string;
procedure UpdateTasks(const AHelpBuildType: TGUID; AHelpBuildTasks: TGuidList);

implementation

uses
  SysUtils, Forms, JvAppStorage, JvAppRegistryStorage, JvTypes;

//=== { THelpBuildData } =====================================================

constructor THelpBuildData.Create;
var
  I: Integer;
  ATasks: TGUIDList;
begin
  inherited Create;
  HelpDir := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)));
  JVCLxxDir := RootDir + 'JVCL3\';
  JVCLMajorVersion := '3';
  JVCLMinorVersion := '30';
  MaxFilesInDox := 10;
  MinPercComplete := 50;
  DtxSelectionKind := dskPercentage;
  MinDateForZippedFiles := 0;
  MaxFilesInZip := 10;
  WZZipFileName := 'C:\Progra~1\WinZip\wzzip';
  WZUnzipFileName := 'C:\Progra~1\WinZip\wzunzip';
  DMCCFileName := '';
  IgnoreWordsContainingNumbers := True;
  NotWhiteSpaceStr := '-_’''';

  FSpecificDtxFiles := TStringList.Create;
  FSpecificDtxFiles.Duplicates := dupIgnore;
  FSpecificDtxFiles.Sorted := True;

  FHelpBuildTypesTasks := TGUIDList.Create(True);
  for I := 0 to AllHelpBuildTypes.Count - 1 do
  begin
    ATasks := TGUIDList.Create(False);
    ATasks.Duplicates := dupIgnore;
    ATasks.Sorted := True;
    FHelpBuildTypesTasks.AddObject(AllHelpBuildTypes[i], ATasks);
  end;

  Load;
end;

function THelpBuildData.GetDonatorsDtxFileName: string;
begin
  Result := HelpGeneratedIncludesDir + 'JVCL.Donators.dtx';
end;

function THelpBuildData.GetDoxFileName: string;
begin
  Result := HelpDir + ChangeFileExt(DoxName, '.dox');
end;

//function THelpBuildData.GetTask(const Index: THelpBuildTask): Boolean;
//begin
//  Result := Index in FTasks;
//end;

function THelpBuildData.GetFuncRefDtxFileName: string;
begin
  Result := HelpGeneratedIncludesDir + 'JVCL.FuncRef.dtx';
end;

function THelpBuildData.GetGenDtxDir: string;
begin
  Result := HelpDir + 'Gen\Dtx\';
end;

function THelpBuildData.GetGenPasDir: string;
begin
  Result := HelpDir + 'Gen\pas\';
end;

function THelpBuildData.GetGroupListFileName: string;
begin
  Result := ToolsDir + 'GroupList.txt';
end;

function THelpBuildData.GetGroupsTreeFileName: string;
begin
  Result := HelpGenericDir + 'JVCL.Groups.tree';
end;

function THelpBuildData.GetHelpGeneratedIncludesDir: string;
begin
  Result := HelpDir + 'generated includes\';
end;

function THelpBuildData.GetHelpGenericDir: string;
begin
  Result := HelpDir + 'generic\';
end;

function THelpBuildData.GetHelpImagesCompDir: string;
begin
  Result := HelpDir + 'images\comp\';
end;

function THelpBuildData.GetHelpInfoDir: string;
begin
  Result := HelpDir + 'info\';
end;

function THelpBuildData.GetHelpOutputDir: string;
begin
  Result := HelpDir + 'output\';
end;

function THelpBuildData.GetHtmlHelpOutputDir: string;
begin
  Result := HelpOutputDir + 'HTML\';
end;

function THelpBuildData.GetJVCLxxDesignDir: string;
begin
  Result := JVCLxxDir + 'design\';
end;

//function THelpBuildData.GetJVCLxxDir: string;
//begin
//  { TODO : !! property }
////  Result := RootDir + 'JVCL3\';
//  Result := 'C:\temp\jvcl310\jvcl\';
//end;

function THelpBuildData.GetJVCLxxImagesDir: string;
begin
  Result := JVCLxxDir + 'images\';
end;

function THelpBuildData.GetJVCLxxRunDir: string;
begin
  Result := JVCLxxDir + 'run\';
end;

function THelpBuildData.GetMustCreateGenDtxDir: Boolean;
begin
  Result := Task[HelpBuildTask_GenerateFormattedDtxFiles] and not DirectoryExists(GenDtxDir);
end;

function THelpBuildData.GetMustCreateGenPasDir: Boolean;
begin
  Result := Task[HelpBuildTask_GenerateStrippedPasFiles] and
    not DirectoryExists(GenPasDir);
end;

function THelpBuildData.GetOnlineHelpDtxDir: string;
begin
  Result := HelpDir + 'Gen\Online\';
end;

//function THelpBuildData.GetOptionalTasks: THelpBuildTasks;
//begin
//  Result := cOptionalTasks[HelpBuildType];
//end;

function THelpBuildData.GetPackageDescFileName: string;
begin
  Result := ToolsDir + 'Package descripions.txt';
end;

function THelpBuildData.GetPackageDir: string;
begin
  Result := JVCLxxDir + 'packages\D7\';
end;

function THelpBuildData.GetPackageListFileName: string;
begin
  Result := ToolsDir + 'Files In Packages.txt';
end;

function THelpBuildData.GetPackagesDtxFileName: string;
begin
  Result := HelpGeneratedIncludesDir + 'JVCL.Packages.dtx';
end;

function THelpBuildData.GetRegisteredClassesFileName: string;
begin
  Result := ToolsDir + 'RegisteredClasses.txt';
end;

function THelpBuildData.GetRootDir: string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(HelpDir));
end;

function THelpBuildData.GetTemplateDoxFileName: string;
begin
  Result := HelpDir + 'JVCL3_FullHelpTemplate.dox';
end;

function THelpBuildData.GetToolsDir: string;
begin
  Result := HelpDir + 'tools\';
end;

function THelpBuildData.GetWinHelpOutputDir: string;
begin
  Result := HelpOutputDir + 'WinHELP\';
end;

procedure WriteGUID(Storage: TJvCustomAppStorage; const Path: string; const Value: TGUID);
begin
  Storage.WriteString(Path, GUIDToString(Value));
end;

function CLSIDFromString(psz: PWideChar; out clsid: TGUID): HResult; stdcall;
  external 'ole32.dll' name 'CLSIDFromString';

function TryStringToGUID(const S: string; out AGUID: TGUID): Boolean;
begin
  Result := Succeeded(CLSIDFromString(PWideChar(WideString(S)), AGUID));
end;

function ReadGUID(Storage: TJvCustomAppStorage; const Path: string; const Default: TGUID): TGUID;
var
  S: string;
begin
  S := Storage.ReadString(Path);
  if (S = '') or not TryStringToGUID(S, Result) then
    Result := Default
end;

procedure WriteGUIDList(Storage: TJvCustomAppStorage; const Path: string; const SL: TGUIDList);
var
  SS: TStringList;
  I: Integer;
begin
  SS := TStringList.Create;
  try
    for I := 0 to SL.Count - 1 do
      SS.Add(GUIDToString(SL[i]));
    Storage.WriteStringList(Path, SS);
  finally
    SS.Free;
  end;
end;

function ReadGUIDList(Storage: TJvCustomAppStorage; const Path: string; const SL: TGUIDList): Integer;
var
  SS: TStringList;
  I: Integer;
  AGUID: TGUID;
begin
  SS := TStringList.Create;
  try
    Result := Storage.ReadStringList(Path, SS, True);
    for I := 0 to SS.Count - 1 do
      if TryStringToGUID(SS[i], AGUID) then
        SL.Add(AGUID);
  finally
    SS.Free;
  end;
end;

procedure THelpBuildData.Load;
var
  DefDtxSelectionKind: TDtxSelectionKind;
  ADtxSelectionKind: TDtxSelectionKind;
  Storage: TJvAppRegistryStorage;
  I: Integer;
  AIndex: Integer;
begin
  if FLoading then
    Exit;

  FLoading := True;
  try
    Storage := TJvAppRegistryStorage.Create(nil);
    with Storage do
    try
      RegRoot := hkCurrentUser;
      Root := 'Software\JVCL\DtxTool\';

      HelpDir := ReadString('HelpDir', HelpDir);
      JVCLxxDir := ReadString('JVCLxxDir', JVCLxxDir);
      JVCLMajorVersion := ReadString('JVCLMajorVersion', JVCLMajorVersion);
      JVCLMinorVersion := ReadString('JVCLMinorVersion', JVCLMinorVersion);
      DoxName := ReadString('DoxName', DoxName);
      DoxTitle := ReadString('DoxTitle', DoxTitle);

      FHelpBuildType := ReadGUID(Storage, 'HelpBuildType', HelpBuildType_CheckDtx);
      AIndex := FHelpBuildTypesTasks.IndexOf(FHelpBuildType);
      if AIndex >= 0 then
        FTasks := HelpBuildTypesTasks[AIndex]
      else
        FTasks := nil;
      for I := 0 to FHelpBuildTypesTasks.Count - 1 do
      begin
        ReadGUIDList(Storage,
          Storage.ConcatPaths([GUIDToString(HelpBuildTypes[i]), 'Tasks']),
          HelpBuildTypesTasks[i]
          );
        UpdateTasks(HelpBuildTypes[i], HelpBuildTypesTasks[i]);
        //        FTasks := (FTasks * cOptionalTasks[FHelpBuildType]) + cDefaultTasks[FHelpBuildType];
      end;
      OnlineHelpZipFileName := ReadString('OnlineHelpZipFileName');

      MinPercComplete := ReadInteger('MinPercComplete', MinPercComplete);
      MaxFilesInDox := ReadInteger('MaxFilesInDox', MaxFilesInDox);
      if MaxFilesInDox = MaxInt then
      begin
        MaxFilesInDox := 10;
        DtxSelectionKind := dskPercentage;
      end;
      DefDtxSelectionKind := dskPercentage;
      ReadEnumeration('DtxSelectionKind', TypeInfo(TDtxSelectionKind), DefDtxSelectionKind, ADtxSelectionKind);
      FDtxSelectionKind := ADtxSelectionKind;
      ReadStringList('SpecificDtxFiles', FSpecificDtxFiles, True);
      MinDateForZippedFiles := ReadDateTime('MinDateForZippedFiles', MinDateForZippedFiles);
      MaxFilesInZip := ReadInteger('MaxFilesInZip', MaxFilesInZip);
      IgnoreWordsContainingNumbers := ReadBoolean('IgnoreWordsContainingNumbers', IgnoreWordsContainingNumbers);
      NotWhiteSpaceStr := ReadString('NotWhiteSpaceStr', NotWhiteSpaceStr);

      HHCFileName := ReadString('HHCFileName', HHCFileName);
      HCRTFFileName := ReadString('HCRTFFileName', HCRTFFileName);
      WZZipFileName := ReadString('WZZipFileName', WZZipFileName);
      WZUnzipFileName := ReadString('WZUnzipFileName', WZUnzipFileName);
      DMCCFileName := ReadString('DMCCFileName', DMCCFileName);

      DtxWordsFileName := ReadString('DtxWordsFileName', DtxWordsFileName);
      DtxIdentifiersFileName := ReadString('DtxIdentifiersFileName', DtxIdentifiersFileName);
      DictionaryFileName := ReadString('DictionaryFileName', DictionaryFileName);
      DtxInvalidWordsFileName := ReadString('DtxInvalidWordsFileName', DtxInvalidWordsFileName);
      DtxDuplicateIdentifiersFileName := ReadString('DtxDuplicateIdentifiersFileName',
        DtxDuplicateIdentifiersFileName);
    finally
      Free;
    end;
  finally
    FLoading := False;
  end;
end;

procedure THelpBuildData.Save;
var
  Storage: TJvAppRegistryStorage;
  I: Integer;
begin
  Storage := TJvAppRegistryStorage.Create(nil);
  with Storage do
  try
    RegRoot := hkCurrentUser;
    Root := 'Software\JVCL\DtxTool\';

    WriteString('HelpDir', HelpDir);
    WriteString('JVCLxxDir', JVCLxxDir);
    WriteString('JVCLMajorVersion', JVCLMajorVersion);
    WriteString('JVCLMinorVersion', JVCLMinorVersion);
    WriteString('DoxName', DoxName);
    WriteString('DoxTitle', DoxTitle);

    WriteGUID(Storage, 'HelpBuildType', FHelpBuildType);
    for I := 0 to FHelpBuildTypesTasks.Count - 1 do
    begin
      WriteGUIDList(Storage,
        Storage.ConcatPaths([GUIDToString(HelpBuildTypes[i]), 'Tasks']),
        HelpBuildTypesTasks[i]
        );
    end;

    //    WriteEnumeration('HelpBuildType', TypeInfo(THelpBuildType), FHelpBuildType);
    //
    //    WriteSet('Tasks', TypeInfo(THelpBuildTasks), FTasks);
    WriteString('OnlineHelpZipFileName', OnlineHelpZipFileName);

    WriteInteger('MinPercComplete', MinPercComplete);
    WriteInteger('MaxFilesInDox', MaxFilesInDox);
    WriteEnumeration('DtxSelectionKind', TypeInfo(TDtxSelectionKind), FDtxSelectionKind);
    WriteStringList('SpecificDtxFiles', FSpecificDtxFiles);
    WriteDateTime('MinDateForZippedFiles', MinDateForZippedFiles);
    WriteInteger('MaxFilesInZip', MaxFilesInZip);

    WriteBoolean('IgnoreWordsContainingNumbers', IgnoreWordsContainingNumbers);
    WriteString('NotWhiteSpaceStr', NotWhiteSpaceStr);

    WriteString('HHCFileName', HHCFileName);
    WriteString('HCRTFFileName', HCRTFFileName);
    WriteString('WZZipFileName', WZZipFileName);
    WriteString('WZUnzipFileName', WZUnzipFileName);
    WriteString('DMCCFileName', DMCCFileName);

    WriteString('DtxWordsFileName', DtxWordsFileName);
    WriteString('DtxIdentifiersFileName', DtxIdentifiersFileName);
    WriteString('DictionaryFileName', DictionaryFileName);
    WriteString('DtxInvalidWordsFileName', DtxInvalidWordsFileName);
    WriteString('DtxDuplicateIdentifiersFileName', DtxDuplicateIdentifiersFileName);
  finally
    Free;
  end;
end;

//procedure THelpBuildData.SetTask(const Index: THelpBuildTask;
//  const Value: Boolean);
//begin
//  if Value then
//    Include(FTasks, Index)
//  else
//    Exclude(FTasks, Index);
//end;

procedure THelpBuildData.SetHelpBuildType(const Value: TGUID);
var
  AIndex: Integer;
begin
  if not IsEqualGUID(Value, FHelpBuildType) then
  begin
    FHelpBuildType := Value;
    AIndex := FHelpBuildTypesTasks.IndexOf(FHelpBuildType);
    if AIndex >= 0 then
      FTasks := HelpBuildTypesTasks[AIndex]
    else
      FTasks := nil;
    SetToDefaultTasks;
  end;
end;

procedure THelpBuildData.SetToDefaultTasks;
begin
  //  FTasks := cDefaultTasks[FHelpBuildType];
end;

function THelpBuildData.GetGenZipDir: string;
begin
  Result := HelpDir + 'Gen\OnlineZip\';
end;

function THelpBuildData.GetMustCreateGenZipDir: Boolean;
begin
  Result :=
    (Task[HelpBuildTask_GenerateOnlineHelpZipFiles] or
     Task[HelpBuildTask_ZipWinHelp] or
     Task[HelpBuildTask_ZipHtmlHelp] or
     Task[HelpBuildTask_ZipPackagesPDF] or
     Task[HelpBuildTask_ZipReferencePDF]
    ) and
    not DirectoryExists(GenZipDir);
end;

function THelpBuildData.GetSpecificDtxFiles: TStrings;
begin
  Result := FSpecificDtxFiles;
end;

procedure THelpBuildData.SetSpecificDtxFiles(Value: TStrings);
var
  I: Integer;
begin
  FSpecificDtxFiles.BeginUpdate;
  try
    FSpecificDtxFiles.Clear;
    for I := 0 to Value.Count - 1 do
      FSpecificDtxFiles.Add(ExtractFileName(ChangeFileExt(Value[i], '')));
  finally
    FSpecificDtxFiles.EndUpdate;
  end;
end;

destructor THelpBuildData.Destroy;
begin
  FHelpBuildTypesTasks.Free;
  FSpecificDtxFiles.Free;
  inherited Destroy;
end;

procedure THelpBuildData.SetHelpDir(const Value: string);
begin
  FHelpDir := IncludeTrailingPathDelimiter(Value);
end;

procedure THelpBuildData.SetJVCLxxDir(const Value: string);
begin
  FJVCLxxDir := IncludeTrailingPathDelimiter(Value);
end;

procedure THelpBuildData.SetJVCLMajorVersion(const Value: string);
begin
  if FJVCLMajorVersion <> Value then
  begin
    FJVCLMajorVersion := Value;
    VersionChanged;
  end;
end;

procedure THelpBuildData.SetJVCLMinorVersion(const Value: string);
begin
  if FJVCLMinorVersion <> Value then
  begin
    FJVCLMinorVersion := Value;
    VersionChanged;
  end;
end;

procedure THelpBuildData.VersionChanged;
begin
  if FLoading then
    Exit;

  if JVCLMinorVersion = '' then
  begin
    DoxTitle := Format('JEDI-VCL %s help system', [JVCLMajorVersion]);
    DoxName := Format('JVCL%s', [JVCLMajorVersion]);
  end
  else
  begin
    DoxTitle := Format('JEDI-VCL %s.%s help system', [JVCLMajorVersion, JVCLMinorVersion]);
    DoxName := Format('JVCL%s%s', [JVCLMajorVersion, JVCLMinorVersion]);
  end;
end;

type
  //  THelpBuildType = (hbSyncOnlineHelp, hbGenerateZipForUpdatingOnlineHelp,
  //    hbCheckDtx, hbBuildHelp, hbDiagnose, hbPostProcessHelpOutput);

  THelpBuildTask = (
    flUnzipOnlineHelpZipFile, flDetermineMinDateForZippedFiles,
    flGenerateImages, flGenerateRegisteredClassesFile,
    flGenerateGroupsFile, flGeneratePackageList, flGenerateDonatorsDtxFile,
    flGenerateFuncRefDtxFile, flGenerateStrippedPasFiles, flGenerateFormattedDtxFiles,
    flGeneratePackagesDtxFile,
    flGenerateDoxFile, flBuildDoxFile, flPostProcessWInHelpOutput, flPostProcessHtmlHelpOutput,
    flBuildWinHelp, flBuildHtmlHelp,
    flGenerateOnlineHelpZipFiles,
    flGenerateDtxWordsFile, flGenerateDtxIdentifiersFile,
    flGenerateDtxInvalidWordsFile,
    flGenerateDtxDuplicateIdentifiersFile
    );
  THelpBuildTasks = set of THelpBuildTask;

  //const
  //  cHelpBuildTypeNiceStr: array[THelpBuildType] of string = (
  //    'Sync online help',
  //    'Generate zips for updating online help',
  //    'Check dtx files',
  //    'Build help',
  //    'Diagnose',
  //    'Post process help output'
  //    );
  //  cHelpBuildTaskStr: array[THelpBuildTask] of string = (
  //    'Unzip online help zip file',
  //    'Determine new min. date for zip files',
  //    'Generate images', 'Generate reg. classes file',
  //    'Generate groups file', 'Generate package list', 'Generate donators file',
  //    'Generate func. ref file', 'Generate stripped pas files',
  //    'Generate formatted dtx files', 'Generate packages file',
  //    'Generate dox file', 'Build help file', 'Post process Windows help output',
  //    'Post process HTML help output',
  //    'Build Windows Help',
  //    'Build HTML Help',
  //    'Generate zip files for updating the online help',
  //    'Generate file with all words in the dtx files',
  //    'Generate file with all identifiers in the dtx files',
  //    'Generate file with all invalid words in the dtx files',
  //    'Generate file with duplicate identifiers words in the dtx files'
  //    );
  //  cDefaultTasks: array[THelpBuildType] of THelpBuildTasks = (
  //    [// Sync online help
  //    flUnzipOnlineHelpZipFile, flDetermineMinDateForZippedFiles,
  //      flGenerateRegisteredClassesFile, flGenerateStrippedPasFiles,
  //      flGenerateFormattedDtxFiles
  //      ],
  //      [// Generate zips for updating online help
  //    flGenerateOnlineHelpZipFiles
  //      ],
  //      [// Check dtx files
  //    flGenerateRegisteredClassesFile,
  //      flGenerateGroupsFile, flGeneratePackageList,
  //      flGenerateStrippedPasFiles, flGenerateFormattedDtxFiles
  //      ],
  //      [// Build help
  //    flGenerateImages, flGenerateDonatorsDtxFile,
  //      flGenerateFuncRefDtxFile,
  //      flGenerateDoxFile, flBuildDoxFile, flGenerateStrippedPasFiles,
  //      flGenerateFormattedDtxFiles, flGeneratePackagesDtxFile
  //      ],
  //      [// Diagnose
  //    flGenerateDtxWordsFile, flGenerateDtxIdentifiersFile,
  //      flGenerateDtxInvalidWordsFile, flGenerateDtxDuplicateIdentifiersFile
  //      ],
  //      [// Post process help output
  //    flPostProcessWinHelpOutput, flPostProcessHTMLHelpOutput]
  //      );
  //
  //  cOptionalTasks: array[THelpBuildType] of THelpBuildTasks = (
  //    [// Sync online help
  //    flUnzipOnlineHelpZipFile, flDetermineMinDateForZippedFiles,
  //      flGenerateRegisteredClassesFile, flGenerateStrippedPasFiles
  //      ],
  //      [// Generate zips for updating online help
  //
  //    ],
  //      [// Check dtx files
  //    flGenerateRegisteredClassesFile,
  //      flGenerateGroupsFile, flGeneratePackageList,
  //      flGenerateStrippedPasFiles
  //      ],
  //      [// Build help
  //    flGenerateImages, flGenerateDonatorsDtxFile,
  //      flGenerateFuncRefDtxFile, flGenerateStrippedPasFiles,
  //      flGenerateFormattedDtxFiles, flGeneratePackagesDtxFile
  //      ],
  //      [// Diagnose
  //    flGenerateDtxWordsFile, flGenerateDtxIdentifiersFile,
  //      flGenerateDtxInvalidWordsFile, flGenerateDtxDuplicateIdentifiersFile
  //      ],
  //      [// Post process help output
  //    flPostProcessWinHelpOutput, flPostProcessHTMLHelpOutput,
  //      flBuildWinHelp, flBuildHtmlHelp]
  //      );

function THelpBuildData.GetTaskGUID(const AGUID: TGUID): Boolean;
begin
  Result := Assigned(FTasks) and (FTasks.IndexOf(AGUID) >= 0);
end;

procedure THelpBuildData.SetTaskGUID(const AGUID: TGUID;
  const Value: Boolean);
begin
  if Assigned(FTasks) then
  begin
    if Value then
      FTasks.Add(AGUID)
    else
      FTasks.Remove(AGUID);
  end;
end;

function THelpBuildData.GetHelpBuildType(const Index: Integer): TGUID;
begin
  Result := FHelpBuildTypesTasks[Index];
end;

function THelpBuildData.GetHelpBuildTypesTasks(
  const Index: Integer): TGUIDList;
begin
  Result := TGUIDList(FHelpBuildTypesTasks.Objects[Index]);
end;

type
  THelpBuildGuidManager = class
  private
    FHelpBuildTypes: TGUIDList;
    FNiceStrs: TStringList;
    FNiceStrGuids: TGUIDList;

    function GetTasks(const AHelpBuildType: TGUID): TGUIDList;

    procedure AddGuidNiceStr(const AGUID: TGUID; const ANiceStr: string);
    procedure AddHelpBuildType(const AGUID: TGUID; const ANiceStr: string);
    procedure AddHelpBuildTask(const AGUID: TGUID; const ANiceStr: string);
    procedure AddOption(const AHelpBuildType, AHelpBuildTask: TGUID);
    procedure AddAlways(const AHelpBuildType, AHelpBuildTask: TGUID);

    procedure RegisterLinks;
    procedure RegisterHelpBuildTypes;
    procedure RegisterHelpBuildTasks;
  protected
    property Tasks[const AHelpBuildType: TGUID]: TGUIDList read GetTasks;
  public
    constructor Create;
    destructor Destroy; override;

    function HelpBuildNiceStr(const AGUID: TGUID): string;
    function IsOptionalTask(const AHelpBuildType, AHelpBuildTask: TGUID): Boolean;
    procedure UpdateTasks(const AHelpBuildType: TGUID; AHelpBuildTasks: TGuidList);
  end;

var
  GManager: THelpBuildGuidManager;

function AllHelpBuildTypes: TGUIDList;
begin
  Result := GManager.FHelpBuildTypes;
end;

function AllHelpBuildTasks(const AHelpBuildType: TGUID): TGUIDList;
begin
  Result := GManager.Tasks[AHelpBuildType];
end;

function HelpBuildNiceStr(const AGUID: TGUID): string;
begin
  Result := GManager.HelpBuildNiceStr(AGUID);
end;

procedure UpdateTasks(const AHelpBuildType: TGUID; AHelpBuildTasks: TGuidList);
begin
  GManager.UpdateTasks(AHelpBuildType, AHelpBuildTasks);
end;

{ THelpBuildGuidManager }

procedure THelpBuildGuidManager.AddAlways(const AHelpBuildType,
  AHelpBuildTask: TGUID);
begin
  Tasks[AHelpBuildType].AddObject(AHelpBuildTask, TObject(False));
end;

procedure THelpBuildGuidManager.AddGuidNiceStr(const AGUID: TGUID;
  const ANiceStr: string);
var
  Index: Integer;
begin
  Index := FNiceStrs.Add(ANiceStr);
  FNiceStrGuids.AddObject(AGUID, TObject(Index));
end;

procedure THelpBuildGuidManager.AddHelpBuildTask(const AGUID: TGUID;
  const ANiceStr: string);
begin
  AddGuidNiceStr(AGUID, ANiceStr);
end;

procedure THelpBuildGuidManager.AddHelpBuildType(const AGUID: TGUID;
  const ANiceStr: string);
var
  ATasks: TGUIDList;
begin
  AddGuidNiceStr(AGUID, ANiceStr);

  ATasks := TGUIDList.Create(False);
  ATasks.Sorted := False;
  //  ATasks.Duplicates := dupIgnore;

  FHelpBuildTypes.AddObject(AGUID, ATasks);
end;

procedure THelpBuildGuidManager.AddOption(const AHelpBuildType,
  AHelpBuildTask: TGUID);
begin
  Tasks[AHelpBuildType].AddObject(AHelpBuildTask, TObject(True));
end;

constructor THelpBuildGuidManager.Create;
begin
  inherited Create;
  FHelpBuildTypes := TGUIDList.Create(True);
  FNiceStrs := TStringList.Create; // not sorted !!
  FNiceStrGuids := TGUIDList.Create(False);
  FNiceStrGuids.Sorted := True;

  RegisterHelpBuildTypes;
  RegisterHelpBuildTasks;
  RegisterLinks;
end;

destructor THelpBuildGuidManager.Destroy;
begin
  FHelpBuildTypes.Free;
  FNiceStrs.Free;
  FNiceStrGuids.Free;
  inherited Destroy;
end;

function THelpBuildGuidManager.GetTasks(
  const AHelpBuildType: TGUID): TGUIDList;
var
  Index: Integer;
begin
  Index := FHelpBuildTypes.IndexOf(AHelpBuildType);
  Result := TGUIDList(FHelpBuildTypes.Objects[Index]);
end;

function THelpBuildGuidManager.HelpBuildNiceStr(
  const AGUID: TGUID): string;
var
  GuidIndex, StrIndex: Integer;
begin
  GuidIndex := FNiceStrGuids.IndexOf(AGUID);
  if GuidIndex >= 0 then
  begin
    StrIndex := Integer(FNiceStrGuids.Objects[GuidIndex]);
    Result := FNiceStrs[StrIndex];
  end
  else
    Result := '';
end;

function THelpBuildGuidManager.IsOptionalTask(const AHelpBuildType,
  AHelpBuildTask: TGUID): Boolean;
var
  AIndex: Integer;
begin
  with Tasks[AHelpBuildType] do
  begin
    AIndex := IndexOf(AHelpBuildTask);
    if AIndex >= 0 then
      Result := Boolean(Objects[AIndex])
    else
      Result := False;
  end;
end;

procedure THelpBuildGuidManager.RegisterHelpBuildTasks;
begin
  AddHelpBuildTask(HelpBuildTask_BuildHelpDoxFile_HTML, 'Build HTML Help');
  AddHelpBuildTask(HelpBuildTask_BuildHelpDoxFile_WinHelp, 'Build Windows Help');
  AddHelpBuildTask(HelpBuildTask_BuildPackagesDoxFile_PDF, 'Build packages pdf file');
  AddHelpBuildTask(HelpBuildTask_BuildReferenceDoxFile_PDF, 'Build reference pdf file');
  AddHelpBuildTask(HelpBuildTask_CompileHtmlHelp, 'Compile HTML Help');
  AddHelpBuildTask(HelpBuildTask_CompileWinHelp, 'Compile Windows Help');
  AddHelpBuildTask(HelpBuildTask_DetermineMinDateForZippedFiles, 'Determine new min. date for zip files');
  AddHelpBuildTask(HelpBuildTask_GenerateDonatorsDtxFile, 'Generate donators file');
  AddHelpBuildTask(HelpBuildTask_GenerateDtxDuplicateIdentifiersFile, 'Generate file with duplicate identifiers words in the dtx files');
  AddHelpBuildTask(HelpBuildTask_GenerateDtxIdentifiersFile, 'Generate file with all identifiers in the dtx files');
  AddHelpBuildTask(HelpBuildTask_GenerateDtxInvalidWordsFile, 'Generate file with all invalid words in the dtx files');
  AddHelpBuildTask(HelpBuildTask_GenerateDtxWordsFile, 'Generate file with all words in the dtx files');
  AddHelpBuildTask(HelpBuildTask_GenerateFormattedDtxFiles, 'Generate formatted dtx files');
  AddHelpBuildTask(HelpBuildTask_GenerateFuncRefDtxFile, 'Generate func. ref file');
  AddHelpBuildTask(HelpBuildTask_GenerateGroupsFile, 'Generate groups file');
  AddHelpBuildTask(HelpBuildTask_GenerateHelpDoxFile, 'Generate dox file');
  AddHelpBuildTask(HelpBuildTask_GenerateImages, 'Generate images');
  AddHelpBuildTask(HelpBuildTask_GenerateOnlineHelpZipFiles, 'Generate zip files for updating the online help');
  AddHelpBuildTask(HelpBuildTask_GeneratePackageList, 'Generate package list');
  AddHelpBuildTask(HelpBuildTask_GeneratePackagesDtxFile, 'Generate packages file');
  AddHelpBuildTask(HelpBuildTask_GenerateRegisteredClassesFile, 'Generate reg. classes file');
  AddHelpBuildTask(HelpBuildTask_GenerateStrippedPasFiles, 'Generate stripped pas files');
  AddHelpBuildTask(HelpBuildTask_PostProcessHtmlHelpOutput, 'Post process HTML help output');
  AddHelpBuildTask(HelpBuildTask_PostProcessWInHelpOutput, 'Post process Windows help output');
  AddHelpBuildTask(HelpBuildTask_UnzipOnlineHelpZipFile, 'Unzip online help zip file');
  AddHelpBuildTask(HelpBuildTask_ZipHtmlHelp, 'Zip Html help files');
  AddHelpBuildTask(HelpBuildTask_ZipWinHelp, 'Zip WinHelp files');
  AddHelpBuildTask(HelpBuildTask_ZipPackagesPDF, 'Zip packages PDF file');
  AddHelpBuildTask(HelpBuildTask_ZipReferencePDF, 'Zip reference PDF file');
end;

procedure THelpBuildGuidManager.RegisterHelpBuildTypes;
begin
  AddHelpBuildType(HelpBuildType_SyncOnlineHelp, 'Sync online help');
  AddHelpBuildType(HelpBuildType_GenerateZipForUpdatingOnlineHelp, 'Generate zips for updating online help');
  AddHelpBuildType(HelpBuildType_CheckDtx, 'Check dtx files (JVCL)');
  AddHelpBuildType(HelpBuildType_CheckDtxJCL, 'Check dtx files (JCL)');
  AddHelpBuildType(HelpBuildType_BuildHelp, 'Build help');
  AddHelpBuildType(HelpBuildType_Diagnose, 'Diagnose');
  AddHelpBuildType(HelpBuildType_PostProcessHelpOutput, 'Post process help output');
  AddHelpBuildType(HelpBuildType_BuildReferenceHelp, 'Build reference help (pdf)');
  AddHelpBuildType(HelpBuildType_BuildPackageHelp, 'Build package help (pdf)');
end;

procedure THelpBuildGuidManager.RegisterLinks;
begin
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_GenerateDonatorsDtxFile);
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_GenerateFormattedDtxFiles);
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_GenerateFuncRefDtxFile);
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_GenerateImages);
  AddAlways(HelpBuildType_BuildReferenceHelp, HelpBuildTask_GenerateReferenceDoxFile);
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_BuildReferenceDoxFile_PDF);
  AddOption(HelpBuildType_BuildReferenceHelp, HelpBuildTask_ZipReferencePDF);

  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_GenerateDonatorsDtxFile);
  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_GenerateFormattedDtxFiles);
  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_GeneratePackagesDtxFile);
  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_GenerateImages);
  AddAlways(HelpBuildType_BuildPackageHelp, HelpBuildTask_GeneratePackagesDoxFile);
  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_BuildPackagesDoxFile_PDF);
  AddOption(HelpBuildType_BuildPackageHelp, HelpBuildTask_ZipPackagesPDF);

  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GenerateDonatorsDtxFile);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GenerateFormattedDtxFiles);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GenerateFuncRefDtxFile);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GenerateImages);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GeneratePackagesDtxFile);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_GenerateStrippedPasFiles);
  AddAlways(HelpBuildType_BuildHelp, HelpBuildTask_GenerateHelpDoxFile);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_BuildHelpDoxFile_HTML);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_BuildHelpDoxFile_WinHelp);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_PostProcessWinHelpOutput);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_PostProcessHTMLHelpOutput);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_CompileWinHelp);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_CompileHtmlHelp);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_ZipWinHelp);
  AddOption(HelpBuildType_BuildHelp, HelpBuildTask_ZipHtmlHelp);

  AddAlways(HelpBuildType_CheckDtx, HelpBuildTask_GenerateFormattedDtxFiles);
  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GenerateGroupsFile);
  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GeneratePackageList);
  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GenerateRegisteredClassesFile);
  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GenerateStrippedPasFiles);

  AddAlways(HelpBuildType_CheckDtxJCL, HelpBuildTask_GenerateFormattedDtxFiles);
//  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GenerateGroupsFile);
//  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GeneratePackageList);
//  AddOption(HelpBuildType_CheckDtx, HelpBuildTask_GenerateRegisteredClassesFile);
  AddOption(HelpBuildType_CheckDtxJCL, HelpBuildTask_GenerateStrippedPasFiles);

  AddOption(HelpBuildType_Diagnose, HelpBuildTask_GenerateDtxDuplicateIdentifiersFile);
  AddOption(HelpBuildType_Diagnose, HelpBuildTask_GenerateDtxIdentifiersFile);
  AddOption(HelpBuildType_Diagnose, HelpBuildTask_GenerateDtxInvalidWordsFile);
  AddOption(HelpBuildType_Diagnose, HelpBuildTask_GenerateDtxWordsFile);

  AddAlways(HelpBuildType_GenerateZipForUpdatingOnlineHelp, HelpBuildTask_GenerateOnlineHelpZipFiles);

  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_PostProcessWinHelpOutput);
  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_PostProcessHTMLHelpOutput);
  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_CompileWinHelp);
  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_CompileHtmlHelp);
  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_ZipWinHelp);
  AddOption(HelpBuildType_PostProcessHelpOutput, HelpBuildTask_ZipHtmlHelp);

  AddOption(HelpBuildType_SyncOnlineHelp, HelpBuildTask_UnzipOnlineHelpZipFile);
  AddOption(HelpBuildType_SyncOnlineHelp, HelpBuildTask_DetermineMinDateForZippedFiles);
  AddAlways(HelpBuildType_SyncOnlineHelp, HelpBuildTask_PreProcessOnlineDtxFiles);
  AddAlways(HelpBuildType_SyncOnlineHelp, HelpBuildTask_GenerateFormattedDtxFiles);
  AddOption(HelpBuildType_SyncOnlineHelp, HelpBuildTask_GenerateRegisteredClassesFile);
  AddOption(HelpBuildType_SyncOnlineHelp, HelpBuildTask_GenerateStrippedPasFiles);
end;

procedure THelpBuildGuidManager.UpdateTasks(const AHelpBuildType: TGUID;
  AHelpBuildTasks: TGuidList);
var
  ATasks: TGuidList;
  I: Integer;
begin
  ATasks := Tasks[AHelpBuildType];
  for I := 0 to ATasks.Count - 1 do
    // not optional -> add
    if not Boolean(ATasks.Objects[i]) then
      AHelpBuildTasks.Add(ATasks[i]);

  for I := AHelpBuildTasks.Count - 1 downto 0 do
    if ATasks.IndexOf(AHelpBuildTasks[i]) < 0 then
      AHelpBuildTasks.Delete(i);
end;

function THelpBuildData.GetIsOptionalTask(const AGUID: TGUID): Boolean;
begin
  Result := GManager.IsOptionalTask(HelpBuildType, AGUID);
end;

function THelpBuildData.GetReferenceDoxFileName: string;
begin
  Result := HelpDir + ChangeFileExt(DoxName + 'Reference', '.dox');
end;

function THelpBuildData.GetPackagesDoxFileName: string;
begin
  Result := HelpDir + ChangeFileExt(DoxName + 'Packages', '.dox');
end;

function THelpBuildData.GetPDFHelpOutputDir: string;
begin
  Result := HelpOutputDir + 'PDF\';
end;

initialization
  GManager := THelpBuildGuidManager.Create;
finalization
  GManager.Free;
end.

