{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCommonDialogD.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCustomFileMessageDialog;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes,
  Controls,
  JvTypes, JvComponentBase;

type
  TJvCustomFileMessageDialog = class(TJvComponent)
  private
    FTitle: string;
    FOwnerWindow: THandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: TJvDiskRes; virtual; abstract;
    property OwnerWindow: THandle read FOwnerWindow write FOwnerWindow stored False;
  published
    property Title: string read FTitle write FTitle;
  end;

function JvDiskStylesToDWORD(const Style: TJvDiskStyles): DWORD;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvSetupApi,
  JvResources;

const
  IDF_NOBROWSE     = $00000001;
  IDF_NOSKIP       = $00000002;
  IDF_NODETAILS    = $00000004;
  IDF_NOCOMPRESSED = $00000008;
  IDF_CHECKFIRST   = $00000100;
  IDF_NOBEEP       = $00000200;
  IDF_NOFOREGROUND = $00000400;
  IDF_WARNIFSKIP   = $00000800;
  IDF_OEMDISK      = DWORD($80000000);

function JvDiskStylesToDWORD(const Style: TJvDiskStyles): DWORD;
begin
  Result := 0;
  if idfCheckFirst in Style then
    Result := Result or IDF_CHECKFIRST;
  if idfNoBeep in Style then
    Result := Result or IDF_NOBEEP;
  if idfNoBrowse in Style then
    Result := Result or IDF_NOBROWSE;
  if idfNoCompressed in Style then
    Result := Result or IDF_NOCOMPRESSED;
  if idfNoDetails in Style then
    Result := Result or IDF_NODETAILS;
  if idfNoForeground in Style then
    Result := Result or IDF_NOFOREGROUND;
  if idfNoSkip in Style then
    Result := Result or IDF_NOSKIP;
  if idfOemDisk in Style then
    Result := Result or IDF_OEMDISK;
  if idfWarnIfSkip in Style then
    Result := Result or IDF_WARNIFSKIP;
end;

//=== { TJvCustomFileMessageDialog } ===================================================

constructor TJvCustomFileMessageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := '';
  if Owner is TWinControl then
    FOwnerWindow := (AOwner as TWinControl).Handle
  else
    FOwnerWindow := HWND_DESKTOP;
  LoadSetupApi;
  if not IsSetupApiLoaded then
    raise EJVCLException.CreateRes(@RsEErrorSetupDll);
end;

destructor TJvCustomFileMessageDialog.Destroy;
begin
  UnloadSetupApi;
  inherited Destroy;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.