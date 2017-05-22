{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Intf.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Intf;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs, DelphiData, JVCLConfiguration, RegConfig;

type
  TPackageGroupKind = (pkVcl);

const
  pkFirst = Low(TPackageGroupKind);
  pkLast = High(TPackageGroupKind);
  PackageGroupKindToStr: array[TPackageGroupKind] of string = (
    'VCL'
  );

type
  ITargetConfig = interface;

  TOutputDirs = record
    UnitOutDir: string;
    BplDir: string;
    DcpDir: string;
    HppDir: string;
  end;

  TDeinstallProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer) of object;
  TDeleteFilesEvent = procedure(TargetConfig: ITargetConfig) of object;

  /// <summary>
  /// This interface is used to overcome the "circular unit references"
  /// </summary>
  ITargetConfig = interface
    function GetInstance: TObject;
      // returns the TTargetConfig instance

    function LinkMapFile(const BinaryFileName, MapFileName: string;
      var MapFileSize, JclDebugDataSize: Integer): Boolean;
    function CompressMapFileToJdbg(const MapFileName: string): Boolean;

    function GetBpgFilename(ForcePersonal: Boolean; Kind: TPackageGroupKind): string;
    function VersionedJclDcp(const Name: string): string;
    function VersionedJclBpl(const Name: string): string;
    function VersionedJVCLXmlDcp(const Name: string): string;
    function VersionedJVCLXmlBpl(const Name: string): string;
    procedure DeinstallJVCL(Progress: TDeinstallProgressEvent;
      DeleteFiles: TDeleteFilesEvent; RealUninstall: Boolean);
    procedure AddPathsToIDE;
    procedure RegisterDesigntimePackages;
    procedure GetPackageBinariesForDeletion(List: TStrings);
    procedure CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
    procedure RegisterJVCLVersionInfo;

    function GetTargetSymbol: string;
    function GetMainTargetSymbol: string;
    function GetAutoDependencies: Boolean;
    function GetDebugUnits: Boolean;
    function GetBuild: Boolean;
    function GetCompileOnly: Boolean;
    function GetDeveloperInstall: Boolean;
    function GetGenerateMapFiles: Boolean;
    function GetLinkMapfiles: Boolean;
    function GetDeleteMapFiles: Boolean;
    function GetCreateJdbgFiles: Boolean;
    function GetCleanPalettes: Boolean;
    function GetAddBplDirToPath: Boolean;


    function GetJVCLConfig: TJVCLConfig;
    function GetJVCLRegistryConfig: TJVCLRegistryConfig;

    function GetTarget: TCompileTarget;
    function GetPathEnvVar: string;
    function GetJVCLPackagesXmlDir: string;
    function GetJVCLDir: string;
    function GetJVCLPackagesDir: string;

    function GetUnitOutDir: string;
    function GetDebugUnitOutDir: string;

    function GetJclDir: string;
    function GetJclDcpDir: string;
    function GetJclDcuDir: string;
    function GetJclBplDir: string;
    function GetHppDir: string;
    function GetBplDir: string;
    function GetDcpDir: string;
    function GetDebugHppDir: string;
    function GetDebugBplDir: string;
    function GetDebugDcpDir: string;
    function GetDxgettextDir: string;

    function GetOutputDirs(DebugUnits: Boolean): TOutputDirs;

    property TargetSymbol: string read GetTargetSymbol; // includes personal/standard flag
    property MainTargetSymbol: string read GetMainTargetSymbol; // without personal/standard flag
    property Target: TCompileTarget read GetTarget;
    property AutoDependencies: Boolean read GetAutoDependencies;
    property DebugUnits: Boolean read GetDebugUnits;
    property Build: Boolean read GetBuild;
    property CompileOnly: Boolean read GetCompileOnly;
    property DeveloperInstall: Boolean read GetDeveloperInstall;
    property GenerateMapFiles: Boolean read GetGenerateMapFiles;
    property LinkMapFiles: Boolean read GetLinkMapFiles;
    property CreateJdbgFiles: Boolean read GetCreateJdbgFiles;
    property DeleteMapFiles: Boolean read GetDeleteMapFiles;
    property CleanPalettes: Boolean read GetCleanPalettes;
    property AddBplDirToPath: Boolean read GetAddBplDirToPath;

    property JVCLConfig: TJVCLConfig read GetJVCLConfig;
    property JVCLRegistryConfig: TJVCLRegistryConfig read GetJVCLRegistryConfig;

    property UnitOutDir: string read GetUnitOutDir;
    property DebugUnitOutDir: string read GetDebugUnitOutDir;
    property JclDir: string read GetJclDir;
    property JclDcpDir: string read GetJclDcpDir;
    property JclDcuDir: string read GetJclDcuDir;
    property JclBplDir: string read GetJclBplDir;
    property HppDir: string read GetHppDir;
    property BplDir: string read GetBplDir;
    property DcpDir: string read GetDcpDir;
    property DebugHppDir: string read GetDebugHppDir;
    property DebugBplDir: string read GetDebugBplDir;
    property DebugDcpDir: string read GetDebugDcpDir;
    property DxgettextDir: string read GetDxgettextDir;

    property JVCLPackagesXmlDir: string read GetJVCLPackagesXmlDir;
    property JVCLDir: string read GetJVCLDir;
    property JVCLPackagesDir: string read GetJVCLPackagesDir;
  end;

implementation

end.
