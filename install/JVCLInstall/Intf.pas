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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit Intf;

interface

uses
  SysUtils, Classes, Contnrs, DelphiData;

type
  TPackageGroupKind = (pkVcl, pkClx);

const
  pkFirst = Low(TPackageGroupKind);
  pkLast = High(TPackageGroupKind);
  PackageGroupKindToStr: array[TPackageGroupKind] of string = (
    'VCL', 'VisualCLX'
  );

type
  /// <summary>
  /// This interface is used to overcome the "circular unit references"
  /// </summary>
  ITargetConfig = interface
    function GetInstance: TObject;
      // returns the TTargetConfig instance

    function GetBpgFilename(ForcePersonal: Boolean; Kind: TPackageGroupKind): string;
    function GetTargetSymbol: string;
    function GetAutoDependencies: Boolean;
    function GetDebugUnits: Boolean;
    function GetBuild: Boolean;
    function GetCompileOnly: Boolean;

    function GetTarget: TCompileTarget;
    function GetJVCLPackagesXmlDir: string;
    function GetJVCLDir: string;
    function GetJVCLPackagesDir: string;

    function GetUnitOutDir: string;
    function GetJCLDir: string;
    function GetHppDir: string;
    function GetBplDir: string;
    function GetDcpDir: string;

    property TargetSymbol: string read GetTargetSymbol;
    property Target: TCompileTarget read GetTarget;
    property AutoDependencies: Boolean read GetAutoDependencies;
    property DebugUnits: Boolean read GetDebugUnits;
    property Build: Boolean read GetBuild;
    property CompileOnly: Boolean read GetCompileOnly;

    property UnitOutDir: string read GetUnitOutDir;
    property JCLDir: string read GetJCLDir;
    property HppDir: string read GetHppDir;
    property BplDir: string read GetBplDir;
    property DcpDir: string read GetDcpDir;

    property JVCLPackagesXmlDir: string read GetJVCLPackagesXmlDir;
    property JVCLDir: string read GetJVCLDir;
    property JVCLPackagesDir: string read GetJVCLPackagesDir;
  end;

implementation

end.
