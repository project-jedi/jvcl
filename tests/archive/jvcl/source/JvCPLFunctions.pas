{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFunctions.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Anthony Steele [asteele@iafrica.com]
Peter Thörnqvist [peter3@peter3.com]

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCPLFunctions;

interface
uses
  Graphics, Classes, Controls,
  SysUtils, JvTypes, ImgList;

{
  GetControlPanelApplets retrieves information about all control panel applets in a specified folder.
  APath is the path to the folder to search and AMask is the filename mask (containing wildcards if necessary) to use.

  The information is returned in the Strings and Images lists according to the following rules:
   The Display Name and Path to the CPL file is returned in Strings with the following format:
     '<displayname>=<path>'
   You can access the DisplayName by using the Strings.Names array and the Path by accessing the Strings.Values array
   Strings.Objects can contain either of two values depending on if Images is nil or not:
     * If Images is nil then Strings.Objects contains the image for the applet as a TBitmap. Note that the caller (you)
     is responsible for freeing the bitmaps in this case
     * If Images <> nil, then the Strings.Objects array contains the index of the image in the Images array for the selected item.
       To access and use the ImageIndex, typecast Strings.Objects to an int:
         tmp.Name := Strings.Name[i];
         tmp.ImageIndex := integer(Strings.Objects[i]);
  The function returns true if any Control Panel Applets were found (i.e Strings.Count is > 0 when returning)
}

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
{ GetControlPanelApplet works like GetControlPanelApplets, with the difference that it only loads and searches one cpl file (according to AFilename).
  Note though, that some CPL's contains multiple applets, so the Strings and Images lists can contain multiple return values.
  The function returns true if any Control Panel Applets were found in AFilename (i.e if items were added to Strings)
}
function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;

implementation
uses
  Windows, CommCtrl, Cpl, JvFunctions;

resourcestring
  RC_CplAddress = 'CPlApplet';


function GetControlPanelApplet(const AFilename: string; Strings: TStrings; Images: TImageList = nil): Boolean;
var
  hLib: HMODULE; // Library Handle to *.cpl file
  hIco: HICON;
  CplCall: TCPLApplet; // Pointer to CPlApplet() function
  i: LongInt;
  tmpCount, Count: LongInt;
  S: WideString;
  // the three types of information that can be returned
  CPLInfo: TCPLInfo;
  InfoW: TNewCPLInfoW;
  InfoA: TNewCPLInfoA;
begin
  Result := False;
  hLib := SafeLoadLibrary(AFilename);
  if hLib = 0 then
    Exit;
  tmpCount := Strings.Count;
  try
    @CplCall := GetProcAddress(hLib, PChar(RC_CplAddress));
    if @CplCall = nil then
      Exit;
    CplCall(GetFocus, CPL_INIT, 0, 0); // Init the *.cpl file
    try
      Count := CplCall(GetFocus, CPL_GETCOUNT, 0, 0);
      for i := 0 to Count - 1 do
      begin
        FillChar(InfoW, sizeof(InfoW), 0);
        FillChar(InfoA, sizeof(InfoA), 0);
        FillChar(CPLInfo, sizeof(CPLInfo), 0);
        S := '';
        CplCall(GetFocus, CPL_NEWINQUIRE, i, LongInt(@InfoW));
        if InfoW.dwSize = sizeof(InfoW) then
        begin
          hIco := InfoW.hIcon;
          S := WideString(InfoW.szName);
        end
        else
        begin
          if InfoW.dwSize = sizeof(InfoA) then
          begin
            Move(InfoW, InfoA, sizeof(InfoA));
            hIco := CopyIcon(InfoA.hIcon);
            S := string(InfoA.szName);
          end
          else
          begin
            CplCall(GetFocus, CPL_INQUIRE, i, LongInt(@CPLInfo));
            LoadStringA(hLib, CPLInfo.idName, InfoA.szName, sizeof(InfoA.szName));
            hIco := LoadImage(hLib, PChar(CPLInfo.idIcon), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
            S := string(InfoA.szName);
          end;
        end;
        if S <> '' then
        begin
          S := Format('%s=%s,@%d', [S, AFilename, i]);
          if Images <> nil then
          begin
            hIco := CopyIcon(hIco);
            ImageList_AddIcon(Images.Handle, hIco);
            Strings.AddObject(S, TObject(Images.Count - 1));
          end
          else
            Strings.AddObject(S, IconToBitmap2(hIco, 16, clMenu));
          // (p3) not sure this is really needed...
          // DestroyIcon(hIco);
        end;
      end;
      Result := tmpCount < Strings.Count;
    finally
      CplCall(GetFocus, CPL_EXIT, 0, 0);
    end;
  finally
    FreeLibrary(hLib);
  end;
end;

function GetControlPanelApplets(const APath, AMask: string; Strings: TStrings; Images: TImageList = nil): Boolean;
var H: THandle; F: TSearchRec;
begin
  H := FindFirst(IncludeTrailingPathDelimiter(APath) + AMask, faAnyFile, F);
  if Images <> nil then
  begin
    Images.Clear;
    Images.BkColor := clMenu;
  end;
  if Strings <> nil then
    Strings.Clear;
  while H = 0 do
  begin
    if F.Attr and faDirectory = 0 then
      //    if (F.Name <> '.') and (F.Name <> '..') then
      GetControlPanelApplet(APath + F.Name, Strings, Images);
    H := FindNext(F);
  end;
  SysUtils.FindClose(F);
  Result := Strings.Count > 0;
end;


end.
