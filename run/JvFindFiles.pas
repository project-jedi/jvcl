{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindFiles.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2002 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  A function and a component to wrap access to the FindFiles Dialog
  (accessible from the Explorer by hitting F3)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFindFiles;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, ShlObj, ShellAPI, ActiveX,
  JvBaseDlg;

type
  TJvSpecialFolder =
    (sfRecycleBin, sfControlPanel, sfDesktop, sfDesktopDirectory,
     sfMyComputer, sfFonts, sfNetHood, sfNetwork, sfPersonal, sfPrinters,
     sfPrograms, sfRecent, sfSendTo, sfStartMenu, stStartUp, sfTemplates);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFindFilesDialog = class(TJvCommonDialog)
  private
    FUseSpecialFolder: Boolean;
    FDirectory: string;
    FSpecial: TJvSpecialFolder;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  published
    // the directory to start the search in
    property Directory: string read FDirectory write FDirectory;
    // ... or a special folder to start in
    property SpecialFolder: TJvSpecialFolder read FSpecial write FSpecial default sfMyComputer;
    // set to True to use SpecialFolder instead of Directory
    property UseSpecialFolder: Boolean read FUseSpecialFolder write FUseSpecialFolder;
  end;

function FindFilesDlg(ParentWnd: HWND; const StartIn: string; SpecialFolder: TJvSpecialFolder; UseFolder: Boolean): Boolean;

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


const
  FFolder: array [TJvSpecialFolder] of Integer =
    (CSIDL_BITBUCKET, CSIDL_CONTROLS, CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY,
     CSIDL_DRIVES, CSIDL_FONTS, CSIDL_NETHOOD, CSIDL_NETWORK, CSIDL_PERSONAL,
     CSIDL_PRINTERS, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU,
     CSIDL_STARTUP, CSIDL_TEMPLATES);

constructor TJvFindFilesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // (rom) Create added to get a decent default for FSpecial
  FSpecial := sfMyComputer;
end;

function TJvFindFilesDialog.Execute(ParentWnd: HWND): Boolean;
begin
  Result := FindFilesDlg(ParentWnd, FDirectory, FSpecial, FUseSpecialFolder);
end;

function FindFilesDlg(ParentWnd: HWND; const StartIn: string; SpecialFolder: TJvSpecialFolder; UseFolder: Boolean): Boolean;
var
  Pidl: PITEMIDLIST;
  PMalloc: IMalloc;
  Sei: TShellExecuteInfo;
begin
  try
    SHGetMalloc(PMalloc);
    FillChar(Sei, SizeOf(TShellExecuteInfo), 0);
    Sei.lpVerb := 'find';
    Sei.cbSize := SizeOf(Sei);
    if UseFolder then
    begin
      SHGetSpecialFolderLocation(0, FFolder[SpecialFolder], Pidl);
      with Sei do
      begin
        fMask := SEE_MASK_INVOKEIDLIST;
        lpIDList := Pidl;
      end;
    end
    else
      Sei.lpFile := PChar(StartIn);
    Sei.Wnd := ParentWnd;
    Result := ShellExecuteEx(@Sei);
  finally
    PMalloc._Release;
    PMalloc := nil;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
