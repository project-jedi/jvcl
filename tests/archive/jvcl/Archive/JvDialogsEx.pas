{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDialogsEx.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvDialogsEx;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CommDlg, StdCtrls;

const
  OFN_EX_NOPLACESBAR = 1;
{$EXTERNALSYM OFN_EX_NOPLACESBAR}

type
  TOpenFileNameEx = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PWideChar;
    lpstrCustomFilter: PWideChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PWideChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PWideChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PWideChar;
    lpstrTitle: PWideChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PWideChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PWideChar;
    pvReserved: Pointer;
    dwReserved: DWORD;
    FlagsEx: DWORD;
  end;
{$EXTERNALSYM TOpenFileNameEx}

  TOpenDialogExOptions = class(TPersistent)
  private
    FPlaces: Boolean;
  public
    constructor Create;
  published
    property PlacesBar: Boolean read FPlaces write FPlaces default True;
  end;

function OsCompliant: Boolean;
{$EXTERNALSYM OsCompliant}

function GetOpenFileNameEx(var OpenFile: TOpenFilenameEx): BOOL; stdcall;
{$EXTERNALSYM GetOpenFileNameEx}
function GetSaveFileNameEx(var OpenFile: TOpenFilenameEx): BOOL; stdcall;
{$EXTERNALSYM GetSaveFileNameEx}

implementation

function GetOpenFileNameEx; external 'comdlg32.dll' name 'GetOpenFileNameA'
function GetSaveFileNameEx; external 'comdlg32.dll' name 'GetSaveFileNameA'

{**************************************************}

function OsCompliant: Boolean;
var
  ver: TOSVersionInfo;
begin
  Result := False;
  ver.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if not GetVersionEx(ver) then
    Exit;
  if (ver.dwMajorVersion >= 5) or
    ((ver.dwMajorVersion >= 4) and (ver.dwMinorVersion >= 90)) then
    Result := True;
end;

{**************************************************}

constructor TOpenDialogExOptions.Create;
begin
  FPlaces := True;
end;

end.
