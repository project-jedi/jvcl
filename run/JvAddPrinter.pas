{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAddPrinter.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                Serhiy Perevoznyk [serge_perevoznyk att hotmail dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAddPrinter;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvBaseDlg;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvAddPrinterDialog = class(TJvCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

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
  ActiveX, ShlObj, ShellAPI, SysUtils;

// (rom) move to JCL

function ItemIDListCreate(const Allocator: IMalloc; const Size: Integer): PItemIDList;
begin
  Result := Allocator.Alloc(Size);
  if Result <> nil then
    FillChar(Result^, Size, 0);
end;

function ItemIDListGetNextItem(const ItemIDList: PItemIDList): PItemIDList;
begin
  if ItemIDList = nil then
    Result := nil
  else
    Result := PItemIDList(Cardinal(ItemIDList) + ItemIDList.mkid.cb);
end;

function ItemIDListGetSize(const ItemIDList: PItemIDList): Cardinal;
var
  TempItemIDList: PItemIDList;
begin
  Result := 0;
  TempItemIDList := ItemIDList;
  if TempItemIDList <> nil then
  begin
    while TempItemIDList.mkid.cb > 0 do
    begin
      Inc(Result, TempItemIDList.mkid.cb);
      TempItemIDList := ItemIDListGetNextItem(TempItemIDList);
    end;
    Inc(Result, 2 * SizeOf(Byte));
  end;
end;

function ItemIDListsConcatenate(const Allocator: IMalloc; const List1, List2: PItemIDList): PItemIDList;
var
  List1Length: Cardinal;
  List2Length: Cardinal;
  NewItemIDList: PItemIDList;
begin
  List1Length := 0;
  if List1 <> nil then
    List1Length := ItemIDListGetSize(List1) - 2 * SizeOf(Byte);
  List2Length := ItemIDListGetSize(List2);
  NewItemIDList := ItemIDListCreate(Allocator, List1Length + List2Length);
  if NewItemIDList <> nil then
  begin
    if List1 <> nil then
      CopyMemory(NewItemIDList, List1, List1Length);
    CopyMemory(Pointer(Cardinal(NewItemIDList) + List1Length), List2, List2Length);
  end;
  Result := NewItemIDList;
end;

function GetPrinterItemIDList(const DesktopFolder: IShellFolder): PItemIDList;
begin
  Result := nil;
  if DesktopFolder <> nil then
    if Failed(SHGetSpecialFolderLocation(0, CSIDL_PRINTERS, Result)) then
      Result := nil;
end;

function GetAddPrinterItem(const Allocator: IMalloc): PItemIDList;
var
  DesktopFolder: IShellFolder;
  EnumIDList: IEnumIDList;
  hOK: HRESULT;
  PrinterItemIDList: PItemIDList;
  PrintersFolder: IShellFolder;
  Retrieved: Integer;
  TempItemIDList: PItemIDList;
begin
  Result := nil;
  if Allocator <> nil then
    if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    begin
      PrinterItemIDList := GetPrinterItemIDList(DesktopFolder);
      if PrinterItemIDList <> nil then
      begin
        hOK := DesktopFolder.BindToObject(PrinterItemIDList, nil, IID_IShellFolder, Pointer(PrintersFolder));
        if Succeeded(hOK) then
          if Succeeded(PrintersFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS, EnumIDList)) then
          begin
            hOK := EnumIDList.Next(1, TempItemIDList, Cardinal(Retrieved));
            if (Retrieved > 0) and Succeeded(hOK) then
              Result := ItemIDListsConcatenate(Allocator, PrinterItemIDList, TempItemIDList);
          end;
      end;
    end;
end;

//=== { TJvAddPrinterDialog } ================================================

function TJvAddPrinterDialog.Execute(ParentWnd: HWND): Boolean;
var
  AddPrinterItemIDList: PItemIDList;
  Allocator: IMalloc;
  ShellExecuteInfo: TShellExecuteInfo;
begin
  Result := False;
  if CoGetMalloc(MEMCTX_TASK, Allocator) = S_OK then
  begin
    AddPrinterItemIDList := GetAddPrinterItem(Allocator);
    try
      if AddPrinterItemIDList <> nil then
      begin
        FillChar(ShellExecuteInfo, SizeOf(TShellExecuteInfo), 0);
        with ShellExecuteInfo do
        begin
          cbSize := SizeOf(TShellExecuteInfo);
          fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_FLAG_NO_UI;
          lpIDList := AddPrinterItemIDList;
          nShow := SW_SHOWDEFAULT;
          Wnd := ParentWnd;
        end;
        // (rom) now reports success
        Result := ShellExecuteEx(@ShellExecuteInfo);
      end;
    finally
      Allocator.Free(AddPrinterItemIDList);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
