{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRecentMenuBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvRecentMenuButton;

interface

uses
  Windows, ShellApi, SysUtils, Classes,
  Graphics, Controls, StdCtrls, Menus,
  JvButton, JvComputerInfoEx, JvTypes, JvJVCLUtils;

// (rom) best separate out a TJvRecentPopupMenu

type
  TJvRecentMenuButton = class(TJvCustomButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvSystemFolders;
    FOnLinkClick: TJvLinkClickEvent;
    FOnPopup: TNotifyEvent;
    procedure UrlClick(Sender: TObject);
    procedure InternalFileFind(const Path, FileMask: string; Strings: TStringList);
  protected
    procedure CreatePopup(Sender: TObject);
    procedure DynBuild(Item: TMenuItem; Directory: string);
    procedure DeleteItem(Item: TMenuItem; LookTag: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property OnLinkClick: TJvLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

implementation

uses
  ShlObj, ActiveX, Math,
  JvResources;

const
  cMaxItems = 15;

constructor TJvRecentMenuButton.Create(AOwner: TComponent);
var
  It: TMenuItem;
begin
  inherited Create(AOwner);
  FDirs := TJvSystemFolders.Create;

  //Create Popup
  FPopup := TPopupMenu.Create(Self);
  It := TMenuItem.Create(FPopup);
  with It do
  begin
    Enabled := False;
    Caption := RsEmptyItem;
    Tag := 1;
  end;
  FPopup.Items.Add(It);
  FPopup.OnPopup := CreatePopup;
end;

destructor TJvRecentMenuButton.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited Destroy;
end;

procedure TJvRecentMenuButton.Click;
var
  P: TPoint;
begin
  inherited Click;
  P.X := 0;
  P.Y := Height;
  P := ClientToScreen(P);
  FPopup.Popup(P.X, P.Y);
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TJvRecentMenuButton.UrlClick(Sender: TObject);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, (Sender as TMenuItem).Hint);
end;

procedure TJvRecentMenuButton.CreatePopup(Sender: TObject);
begin
  DynBuild(FPopup.Items, FDirs.Recent);
end;

function GetAssociatedIcon(const Filename: string; SmallIcon: Boolean): HICON;
const
  cSmall: array [Boolean] of Cardinal = (SHGFI_LARGEICON, SHGFI_SMALLICON);
var
  pfsi: TShFileInfo;
  hLarge: HICON;
  w: Word;
begin
  FillChar(pfsi, SizeOf(pfsi), 0);
  ShGetFileInfo(PChar(Filename), 0, pfsi, SizeOf(pfsi),
    SHGFI_ICONLOCATION or SHGFI_ATTRIBUTES or SHGFI_ICON or cSmall[SmallIcon] or SHGFI_USEFILEATTRIBUTES);
  Result := pfsi.hIcon;
  if Result = 0 then
    ExtractIconEx(pfsi.szDisplayName, pfsi.iIcon, hLarge, Result, 1);
  if not SmallIcon then
    Result := hLarge;
  if Result = 0 then
    ExtractAssociatedIcon(GetForegroundWindow, PChar(Filename), w);
end;

(* make Delphi 5 compiler happy // andreas
function SortByName(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(ExtractFileName(List[Index2]), ExtractFileName(List[Index2]));
end;
*)

function SortByObject(List: TStringList; Index1, Index2: Integer): Integer;
begin
  // note: higher values sorted at the top
  Result := Integer(List.Objects[Index2]) - Integer(List.Objects[Index1]);
end;

const
  IID_IShellLink: TGUID = { IID_IShellLinkA }
    (D1: $000214EE; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

type
  TUnicodePath = array [0..MAX_PATH - 1] of WideChar;

function ShellLinkResolve(const FileName: string): string;
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  LinkName: TUnicodePath;
  Buffer: string;
  Win32FindData: TWin32FindData;
  FullPath: string;
begin
  Result := '';
  if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IID_IShellLink, ShellLink)) then
  begin
    PersistFile := ShellLink as IPersistFile;
    // PersistFile.Load fails if the filename is not fully qualified
    FullPath := ExpandFileName(FileName);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(FullPath), -1, LinkName, MAX_PATH);
    if Succeeded(PersistFile.Load(LinkName, STGM_READ)) then
    begin
      //      Result := ShellLink.Resolve(0, SLR_ANY_MATCH or SLR_NO_UI);
      SetLength(Buffer, MAX_PATH);
      ShellLink.GetPath(PChar(Buffer), MAX_PATH, Win32FindData, SLGP_RAWPATH);
      Result := PChar(Buffer);
    end;
  end;
end;

procedure TJvRecentMenuButton.InternalFileFind(const Path, FileMask: string; Strings: TStringList);
var
  H: THandle;
  Sr: TSearchRec;
  Tmp: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    H := FindFirst(Path + FileMask, faAnyFile, Sr);
    try
      while H = 0 do
      begin
        if (Sr.FindData.cFilename[0] <> '.') then
        begin
          Tmp := ShellLinkResolve(Path + Sr.FindData.cFilename);
          if (Tmp <> '') and (ExtractFileExt(Tmp) <> '') then
            Strings.AddObject(Tmp, TObject(Sr.Time));
        end;
        H := FindNext(Sr);
      end;
    finally
      FindClose(Sr);
    end;
    Strings.CustomSort(SortByObject);
    while Strings.Count > cMaxItems do // delete any older files
      Strings.Delete(Strings.Count - 1);
    Strings.Sort; // CustomSort(SortByName); // sort by name instead
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvRecentMenuButton.DynBuild(Item: TMenuItem; Directory: string);
var
  It: TMenuItem;
  Bmp: TBitmap;
  S: TStringList;
  I: Integer;
begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  S := TStringList.Create;
  try
    InternalFileFind(Directory, '*.*', S);
    for I := 0 to Min(S.Count - 1, cMaxItems - 1) do
    begin
      It := TMenuItem.Create(Item);
      It.Caption := ExtractFilename(S[I]);
      It.OnClick := UrlClick;
      It.Hint := S[I];
      Bmp := IconToBitmap2(GetAssociatedIcon(S[I], True), 16, clMenu);
      It.Bitmap.Assign(Bmp);
      Bmp.Free;
      Item.Add(It);
    end;
  finally
    S.Free;
  end;
  Item.Items[0].Visible := (Item.Count = 1);
end;

procedure TJvRecentMenuButton.DeleteItem(Item: TMenuItem; LookTag: Boolean);
var
  I: Integer;
begin
  for I := Item.Count - 1 downto 0 do
    if (not LookTag) or (Item[I].Tag = 0) then
    begin
      DeleteItem(Item[I]);
      Item[I].Free;
    end;
end;

end.

