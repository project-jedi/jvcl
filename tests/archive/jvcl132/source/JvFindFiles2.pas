{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFindFiles.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{A function and a component to wrap access to the FindFiles Dialog
(accessible from the Explorer by hitting F3) }

unit JvFindFiles2;

interface
uses
  SysUtils, Classes, Controls, ShlObj, ShellAPI, ActiveX, Dialogs;

type
  TJvSpecialFolder = (sfRecycleBin, sfControlPanel, sfDesktop, sfDesktopDirectory,
    sfMyComputer, sfFonts, sfNetHood, sfNetwork, sfPersonal, sfPrinters,
    sfPrograms, sfRecent, sfSendTo, sfStartMenu, stStartUp, sfTemplates);

  TJvFindFilesDialog2 = class(TCommonDialog)
  private
    FUse: boolean;
    FDir: string;
    FSpecial: TJvSpecialFolder;
  public
    function Execute: boolean; override;
  published
    property Directory: string read FDir write FDir;
    property SpecialFolder: TJvSpecialFolder read FSpecial write FSpecial;
    property UseSpecialFolder: boolean read FUse write FUse;
  end;

function FindFilesDlg(StartIn: string; SpecialFolder: TJvSpecialFolder; UseFolder: boolean): boolean;

implementation

const
  FFolder: array[TJvSpecialFolder] of integer = (
    CSIDL_BITBUCKET, CSIDL_CONTROLS, CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY,
    CSIDL_DRIVES, CSIDL_FONTS, CSIDL_NETHOOD, CSIDL_NETWORK, CSIDL_PERSONAL,
    CSIDL_PRINTERS, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU,
    CSIDL_STARTUP, CSIDL_TEMPLATES);

function TJvFindFilesDialog2.Execute: boolean;
begin
  Result := FindFilesDlg(FDir, FSpecial, FUse);
end;

function FindFilesDlg(StartIn: string; SpecialFolder: TJvSpecialFolder; UseFolder: boolean): boolean;
var
  pidl: PITEMIDLIST;
  PMalloc: IMalloc;
  sei: TShellExecuteInfo;
begin
  try
    SHGetMalloc(PMalloc);
    FillChar(sei, sizeof(TShellExecuteInfo), 0);
    sei.lpVerb := 'find';
    sei.cbSize := SizeOf(sei);
    if UseFolder then
    begin
      SHGetSpecialFolderLocation(0, FFolder[SpecialFolder], pidl);
      with sei do
      begin
        fMask := SEE_MASK_INVOKEIDLIST;
        lpIDList := pidl;
      end;
    end
    else
      sei.lpFile := PChar(StartIn);
    Result := ShellExecuteEx(@sei);
  finally
    pMalloc._Release;
    pMalloc := nil;
  end;
end;

end.

