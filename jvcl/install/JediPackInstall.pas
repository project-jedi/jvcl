{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediPackInstall.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ JEDI API/JCL/JVCL installation code                                                              }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: July 29, 2002                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JediPackInstall;

interface

{$I jcl.inc}

uses
  Windows, SysUtils, Classes, ComCtrls, JediInstallIntf;

type
  TJediPackInstall = class (TInterfacedObject, IJediInstall)
  private
    FApiPath: string;
    FApiSourcePath: string;
    FJclPath: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FVclDialogFileName: string;
    FClxDialogIconFileName: string;
    FVclDialogIconFileName: string;
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclReadmeFileName: string;
    FVclPath: string;
    FVclSourcePath: string;
    FVclCommonPath: string;
    FVclReadmeFileName: string;
    FVclChmHelpFileName: string;
    FVclHlpHelpFileName: string;
    FTool: IJediInstallTool;
  public
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
    function SelectedNodeCollapsing(Node: TTreeNode): Boolean;
    procedure SelectedNodeChanged(Node: TTreeNode);
    procedure SetTool(const Value: IJediInstallTool);
    property Tool: IJediInstallTool read FTool;
  end;

function CreateJediInstall: IJediInstall;

implementation

uses
  JclBase, JclFileUtils, JclStrings, DelphiInstall;

const
  // JCL
  DialogsPath       = 'examples\debugextension\dialog\';
  ClxDialogFileName = 'ClxExceptDlg.pas';
  VclDialogFileName = 'ExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  VclDialogName     = 'Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  JclChmHelpFile    = 'Help\JCLHelp.chm';
  JclHlpHelpFile    = 'Help\JCLHelp.hlp';
  JclHelpTitle      = 'JEDI Code Library Help';
  JclHelpIndexName  = 'JEDI Code Library Reference';
  HHFileName        = 'HH.EXE';

  JclRuntimeDpk     = 'packages\JCL%d0.dpk';
  JclIdeDebugDpk    = 'examples\debugextension\JclDebugIde%d0.dpk';
  JclIdeAnalyzerDpk = 'examples\projectanalyzer\ProjectAnalyzer%d0.dpk';
  JclIdeFavoriteDpk = 'examples\idefavopendialogs\IdeOpenDlgFavorite%d0.dpk';
  JclIdeThrNamesDpk = 'examples\debugextension\threadnames\ThreadNameExpert%d0.dpk';

  // JVCL
  VclRuntimeDpk     = 'packages\JVCL200_R%d0.dpk';
  VclRuntimeDpkPers = 'packages\JVCL200_R%d0Personal.dpk';
  VclDesignDpk      = 'packages\JVCL200_D%d0.dpk';
  VclDesignDpkPers  = 'packages\JVCL200_D%d0Personal.dpk';
  VclRuntimeDpkStd  = 'packages\JVCL200_R%d0Standard.dpk';
  VclDesignDpkStd   = 'packages\JVCL200_D%d0Standard.dpk';

  VclChmHelpFile    = 'Help\JVCL.chm';
  VclHlpHelpFile    = 'Help\JVCL.hlp';
  VclHelpTitle      = 'JEDI Visual Component Library Help';
  VclHelpIndexName  = 'JEDI Visual Component Library Reference';

  // Feature IDs
  FID_API                  = $01000000;
  FID_JCL                  = $02000000;
  FID_VCL                  = $03000000;

  FID_API_Env              = FID_API + $00010000;
  FID_API_EnvLibPath       = FID_API + $00010001;

  FID_JCL_Env              = FID_JCL + $00010000;
  FID_JCL_EnvLibPath       = FID_JCL + $00010001;
  FID_JCL_Help             = FID_JCL + $00020000;
  FID_JCL_HelpHlp          = FID_JCL + $00020100;
  FID_JCL_HelpChm          = FID_JCL + $00020200;
  FID_JCL_Experts          = FID_JCL + $00030000;
  FID_JCL_ExpertDebug      = FID_JCL + $00030100;
  FID_JCL_ExpertAnalyzer   = FID_JCL + $00030200;
  FID_JCL_ExpertFavorite   = FID_JCL + $00030300;
  FID_JCL_ExpertsThrNames  = FID_JCL + $00030400;
  FID_JCL_ExcDialog        = FID_JCL + $00040000;
  FID_JCL_ExcDialogVCL     = FID_JCL + $00040100;
  FID_JCL_ExcDialogCLX     = FID_JCL + $00040200;

  FID_VCL_Env              = FID_VCL + $00010000;
  FID_VCL_EnvLibPath       = FID_VCL + $00010001;
  FID_VCL_EnvDesignPkg     = FID_VCL + $00010002;
  FID_VCL_Help             = FID_VCL + $00020000;
  FID_VCL_HelpHlp          = FID_VCL + $00020100;
  FID_VCL_HelpChm          = FID_VCL + $00020200;

  // Products
  RsAPI             = 'JEDI API Library';
  RsJCL             = 'JEDI Code Library';
  RsJVCL            = 'JEDI Visual Component Library';

  // Common features
  RsEnvironment     = 'Environment';
  RsEnvLibPath      = 'Include source code location to IDE Library Path';
  RsHelpFiles       = 'Help files';
  RsIdeExperts      = 'IDE experts';
  RsIdeHelpHlp      = 'Add help file to Delphi IDE help system';
  RsIdeHelpChm      = 'Add HTML help to the Tools menu';

  // Product specific features
  RsJCLExceptDlg    = 'Sample Exception Dialogs in the Object Reporitory';
  RsJCLDialogVCL    = 'VCL Exception Dialog';
  RsJCLDialogCLX    = 'CLX Exception Dialog';
  RsJCLIdeDebug     = 'Debug Extension';
  RsJCLIdeAnalyzer  = 'Project Analyzer';
  RsJCLIdeFavorite  = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThrNames  = 'Displaying thread names in Thread Status window';
  RsVCLDesignPkg    = 'Install design-time component packages';

resourcestring
  RsSourceLibHint   = 'Adds "%s" to the Library Path';
  RsStatusMessage   = 'Compiling / Installing %s ...';
  RsInstallFailed   = 'Installation of %s failed. Try running the installer again. If it still doesn''t work, try doing a manual install from Delphi.';

function CreateJediInstall: IJediInstall;
begin
  Result := TJediPackInstall.Create as IJediInstall;
end;

{ TJediPackInstall }

function TJediPackInstall.InitInformation(const ApplicationFileName: string): Boolean;
begin
  // API
  FApiPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..\..\API'));
  FApiSourcePath := FApiPath + 'Pas';
  // JCL
  FJclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..'));
  FJclSourcePath := FJclPath + 'Source';
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ICO');
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ICO');
  FJclChmHelpFileName := FJclPath + JclChmHelpFile;
  FJclHlpHelpFileName := FJclPath + JclHlpHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  // Reset ReadOnly flag for dialog forms
  FileSetAttr(FClxDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FClxDialogFileName, '.xfm'), faArchive);
  FileSetAttr(FVclDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
  Result := (FileExists(FClxDialogFileName) and FileExists(FVclDialogFileName) and
    FileExists(FClxDialogIconFileName) and FileExists(FVclDialogIconFileName));
  FJclReadmeFileName := FJclPath + 'Readme.txt';
  // VCL
  FVclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(ApplicationFileName) + '..\..\JVCL'));
  FVclSourcePath := FVclPath + 'Source';
  FVclCommonPath := FVclPath + 'Common';
  FVclReadmeFileName := FVclPath + 'Readme.txt';
  FVclChmHelpFileName := FVclPath + VclChmHelpFile;
  FVclHlpHelpFileName := FVclPath + VclHlpHelpFile;
  if not FileExists(FVclHlpHelpFileName) then
    FVclHlpHelpFileName := '';
  if not FileExists(FVclChmHelpFileName) then
    FVclChmHelpFileName := '';
end;

function TJediPackInstall.Install: Boolean;
var
  Installation: TJclDelphiInstallation;

  procedure AddJclHelpToDelphiHelp;
  begin
    Installation.OpenHelp.AddHelpFile(FJclHlpHelpFileName, JclHelpIndexName);
  end;

  procedure AddJclHelpToIdeTools;
  var
    ToolsIndex: Integer;
  begin
    if Installation.IdeTools.IndexOfTitle(JclHelpTitle) = -1 then
    begin
      ToolsIndex := Installation.IdeTools.Count;
      Installation.IdeTools.Count := ToolsIndex + 1;
      Installation.IdeTools.Title[ToolsIndex] := JclHelpTitle;
      Installation.IdeTools.Path[ToolsIndex] := HHFileName;
      Installation.IdeTools.Parameters[ToolsIndex] := StrDoubleQuote(FJclChmHelpFileName);
      Installation.IdeTools.WorkingDir[ToolsIndex] := FJclPath;
    end;
  end;

  procedure AddVclHelpToDelphiHelp;
  begin
    Installation.OpenHelp.AddHelpFile(FVclHlpHelpFileName, VclHelpIndexName);
  end;

  procedure AddVclHelpToIdeTools;
  var
    ToolsIndex: Integer;
  begin
    if Installation.IdeTools.IndexOfTitle(VclHelpTitle) = -1 then
    begin
      ToolsIndex := Installation.IdeTools.Count;
      Installation.IdeTools.Count := ToolsIndex + 1;
      Installation.IdeTools.Title[ToolsIndex] := VclHelpTitle;
      Installation.IdeTools.Path[ToolsIndex] := HHFileName;
      Installation.IdeTools.Parameters[ToolsIndex] := StrDoubleQuote(FVclChmHelpFileName);
      Installation.IdeTools.WorkingDir[ToolsIndex] := FVclPath;
    end;
  end;

  function InstallPackage(const Path, Name: string): Boolean;
  var
    PackageFileName: string;
  begin
    PackageFileName := Path + Format(Name, [Installation.VersionNumber]);
    Tool.UpdateStatus(Format(RsStatusMessage, [ExtractFileName(PackageFileName)]));
    Result := Installation.Compiler.InstallPackage(PackageFileName,Tool.BPLPath(Installation.VersionNumber),
      Tool.DCPPath(Installation.VersionNumber));
    Tool.UpdateStatus('');
    if not Result then
      Tool.MessageBox(Format(RsInstallFailed, [PackageFileName]), MB_OK or MB_ICONERROR);
  end;

  procedure D4Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    // JCL
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddJclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddJclHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
  end;

  procedure D5Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    // JCL
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddJclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddJclHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_HelpHlp, Installation.VersionNumber) then
      AddVclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_VCL_HelpChm, Installation.VersionNumber) then
      AddVclHelpToIdeTools;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
      case Installation.Edition of
        deSTD:
          begin
            InstallPackage(FVclPath, VclRuntimeDpkStd);
            InstallPackage(FVclPath, VclDesignDpkStd);
          end;
        dePRO, deCSS:
          begin
            InstallPackage(FVclPath, VclRuntimeDpk);
            InstallPackage(FVclPath, VclDesignDpk);
          end;
      end;
  end;

  procedure D6Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    // JCL
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddJclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddJclHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertsThrNames, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeThrNamesDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_HelpHlp, Installation.VersionNumber) then
      AddVclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_VCL_HelpChm, Installation.VersionNumber) then
      AddVclHelpToIdeTools;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
      case Installation.Edition of
        deSTD:
          begin
            InstallPackage(FVclPath, VclRuntimeDpkPers);
            InstallPackage(FVclPath, VclDesignDpkPers);
          end;
        dePRO, deCSS:
          begin
            InstallPackage(FVclPath, VclRuntimeDpk);
            InstallPackage(FVclPath, VclDesignDpk);
          end;
      end;
  end;

  procedure D7Install;
  begin
    Tool.UpdateStatus(Format(RsStatusMessage, [Installation.Name]));
    // JCL
    if Tool.FeatureChecked(FID_JCL_EnvLibPath, Installation.VersionNumber) then
      Installation.AddToLibrarySearchPath(FJclSourcePath);
    if Tool.FeatureChecked(FID_JCL_HelpHlp, Installation.VersionNumber) then
      AddJclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_JCL_HelpChm, Installation.VersionNumber) then
      AddJclHelpToIdeTools;
    if Tool.FeatureChecked(FID_JCL_ExcDialogVCL, Installation.VersionNumber) then
      Installation.Repository.AddObject(FVclDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), VclDialogName, FVclDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerDfm);
    if Tool.FeatureChecked(FID_JCL_ExcDialogCLX, Installation.VersionNumber) then
      Installation.Repository.AddObject(FClxDialogFileName, DelphiRepositoryFormTemplate,
        Installation.Repository.FindPage(DialogPage, 1), ClxDialogName, FClxDialogIconFileName,
        DialogDescription, DialogAuthor, DelphiRepositoryDesignerXfm);
    if Tool.FeatureChecked(FID_JCL_Experts, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclRuntimeDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertDebug, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeDebugDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertAnalyzer, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeAnalyzerDpk);
    if Tool.FeatureChecked(FID_JCL_ExpertFavorite, Installation.VersionNumber) then
      InstallPackage(FJclPath, JclIdeFavoriteDpk);
    // JVCL
    if Tool.FeatureChecked(FID_VCL_EnvLibPath, Installation.VersionNumber) then
    begin
      Installation.AddToLibrarySearchPath(FVclSourcePath);
      Installation.AddToLibrarySearchPath(FVclCommonPath);
    end;
    if Tool.FeatureChecked(FID_VCL_HelpHlp, Installation.VersionNumber) then
      AddVclHelpToDelphiHelp;
    if Tool.FeatureChecked(FID_VCL_HelpChm, Installation.VersionNumber) then
      AddVclHelpToIdeTools;
    if Tool.FeatureChecked(FID_VCL_EnvDesignPkg, Installation.VersionNumber) then
      case Installation.Edition of
        deSTD:
          begin
            InstallPackage(FVclPath, VclRuntimeDpkPers);
            InstallPackage(FVclPath, VclDesignDpkPers);
          end;
        dePRO, deCSS:
          begin
            InstallPackage(FVclPath, VclRuntimeDpk);
            InstallPackage(FVclPath, VclDesignDpk);
          end;
      end;
  end;

begin
  Result := True;
  try
    Installation := Tool.DelphiInstallations.InstallationFromVersion[4];
    if Assigned(Installation) and Installation.Valid then
      D4Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[5];
    if Assigned(Installation) and Installation.Valid then
      D5Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[6];
    if Assigned(Installation) and Installation.Valid then
      D6Install;
    Installation := Tool.DelphiInstallations.InstallationFromVersion[7];
    if Assigned(Installation) and Installation.Valid then
      D7Install;
  finally
    Tool.UpdateStatus('');
  end;
end;

function TJediPackInstall.PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
var
  InstallationNode, ProductNode, TempNode: TTreeNode;
  Installation: TJclDelphiInstallation;

  function AddNode(Parent: TTreeNode; const Caption: string; FeatureID: Cardinal): TTreeNode;
  begin
    FeatureID := FeatureID or FID_Checked;
    Result := Nodes.AddChildObject(Parent, Caption, Pointer(FeatureID));
    Result.ImageIndex := IcoChecked;
    Result.SelectedIndex := IcoChecked;
  end;
  
begin
  Installation := Tool.DelphiInstallations.InstallationFromVersion[VersionNumber];
  Result := Assigned(Installation) and Installation.Valid;
  Nodes.BeginUpdate;
  try
    if Result then
    begin
      InstallationNode := AddNode(nil, Installation.Name, 0);
      InstallationNode.StateIndex := 0;
      if Assigned(Installation) and Installation.Valid then
      begin
        // API Library
        if DirectoryExists(FApiSourcePath) then
        begin
          ProductNode := AddNode(InstallationNode, RsAPI, FID_API);
          TempNode := AddNode(ProductNode, RsEnvironment, FID_API_Env);
          AddNode(TempNode, RsEnvLibPath, FID_API_EnvLibPath);
        end;
        // JCL
        ProductNode := AddNode(InstallationNode, RsJCL, FID_JCL);
        TempNode := AddNode(ProductNode, RsEnvironment, FID_JCL_Env);
        AddNode(TempNode, RsEnvLibPath, FID_JCL_EnvLibPath);
        if (FJclHlpHelpFileName <> '') or (FJclChmHelpFileName <> '') then
        begin
          TempNode := AddNode(ProductNode, RsHelpFiles, FID_JCL_Help);
          if FJclHlpHelpFileName <> '' then
            AddNode(TempNode, RsIdeHelpHlp, FID_JCL_HelpHlp);
          if FJclChmHelpFileName <> '' then
            AddNode(TempNode, RsIdeHelpChm, FID_JCL_HelpChm);
        end;
        TempNode := AddNode(ProductNode, RsJCLExceptDlg, FID_JCL_ExcDialog);
        AddNode(TempNode, RsJCLDialogVCL, FID_JCL_ExcDialogVCL);
        if (Installation.VersionNumber >= 6) and (Installation.Edition <> deSTD) then
          AddNode(TempNode, RsJCLDialogCLX, FID_JCL_ExcDialogCLX);
        TempNode := AddNode(ProductNode, RsIdeExperts, FID_JCL_Experts);
        AddNode(TempNode, RsJCLIdeDebug, FID_JCL_ExpertDebug);
        AddNode(TempNode, RsJCLIdeAnalyzer, FID_JCL_ExpertAnalyzer);
        AddNode(TempNode, RsJCLIdeFavorite, FID_JCL_ExpertFavorite);
        if Installation.VersionNumber <= 6 then
          AddNode(TempNode, RsJCLIdeThrNames, FID_JCL_ExpertsThrNames);
        InstallationNode.Expand(True);
        // JVCL
        if DirectoryExists(FVclSourcePath) and (Installation.VersionNumber >= 5) then
        begin
          ProductNode := AddNode(InstallationNode, RsJVCL, FID_VCL);
          TempNode := AddNode(ProductNode, RsEnvironment, FID_VCL_Env);
          AddNode(TempNode, RsEnvLibPath, FID_VCL_EnvLibPath);
          AddNode(TempNode, RsVCLDesignPkg, FID_VCL_EnvDesignPkg);
          if (FVclHlpHelpFileName <> '') or (FVclChmHelpFileName <> '') then
          begin
            TempNode := AddNode(ProductNode, RsHelpFiles, FID_VCL_Help);
            if FVclHlpHelpFileName <> '' then
              AddNode(TempNode, RsIdeHelpHlp, FID_VCL_HelpHlp);
            if FVclChmHelpFileName <> '' then
              AddNode(TempNode, RsIdeHelpChm, FID_VCL_HelpChm);
          end;
        end;
      end;
      InstallationNode.Expand(True);
    end;  
  finally
    Nodes.EndUpdate;
  end;
end;

function TJediPackInstall.SelectedNodeCollapsing(Node: TTreeNode): Boolean;
begin
  Result := False;
  //  Result := Cardinal(Node.Data) and FID_Category <> 0;
end;

procedure TJediPackInstall.SelectedNodeChanged(Node: TTreeNode);
var
  ReadmeText: string;
begin
  case Cardinal(Node.Data) and FID_Product of
    FID_JCL: ReadmeText := FileToString(FJclReadmeFileName);
    FID_VCL: ReadmeText := FileToString(FVclReadmeFileName);
  else
    ReadmeText := '';
  end;
  Tool.UpdateInfo(Tool.ActiveVersionNumberPage, ReadmeText);
end;

procedure TJediPackInstall.SetTool(const Value: IJediInstallTool);
begin
  FTool := Value;
end;

end.
