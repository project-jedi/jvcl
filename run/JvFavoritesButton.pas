{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFavoritesButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFavoritesButton;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Menus, ShellAPI, ImgList,
  JvTypes, JvButton, JvComputerInfoEx, JvJVCLUtils;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFavoritesButton = class(TJvCustomButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvSystemFolders;
    FImages: TImageList;
    FOnUrlClick: TJvLinkClickEvent;
    FOnPopup: TNotifyEvent;
    procedure UrlClick(Sender: TObject);
  protected
    procedure DeleteItem(Item: TMenuItem; LookTag: Boolean = False);
    procedure PopupCreate(Sender: TObject);
    procedure DirectoryClick(Sender: TObject);
    procedure DynBuild(Item: TMenuItem; Directory: string);
    procedure AddIconFrom(const Path: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property OnUrlClick: TJvLinkClickEvent read FOnUrlClick write FOnUrlClick;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
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
  JvResources;

constructor TJvFavoritesButton.Create(AOwner: TComponent);
var
  MenuItem: TMenuItem;
begin
  inherited Create(AOwner);
  FDirs := TJvSystemFolders.Create;

  //Create Popup
  FPopup := TPopupMenu.Create(Self);
  MenuItem := TMenuItem.Create(FPopup);
  MenuItem.Enabled := False;
  MenuItem.Caption := RsEmptyItem;
  MenuItem.Tag := 1;
  FPopup.Items.Add(MenuItem);
  FPopup.OnPopup := PopupCreate;

  //Create Images
  FImages := TImageList.Create(Self);
  FImages.Width := 16;
  FImages.Height := 16;
  FImages.DrawingStyle := dsTransparent;
  FImages.Masked := True;
  FPopup.Images := FImages;
  AddIconFrom(FDirs.Windows);
end;

destructor TJvFavoritesButton.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited Destroy;
end;

procedure TJvFavoritesButton.Click;
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

procedure TJvFavoritesButton.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrlClick) then
    FOnUrlClick(Self, (Sender as TMenuItem).Hint);
end;

procedure TJvFavoritesButton.DeleteItem(Item: TMenuItem; LookTag: Boolean);
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

procedure TJvFavoritesButton.PopupCreate(Sender: TObject);
begin
  DynBuild(FPopup.Items, FDirs.Favorites);
end;

procedure TJvFavoritesButton.DynBuild(Item: TMenuItem; Directory: string);
var
  Res: Integer;
  SearchRec: TSearchRec;
  It, It2: TMenuItem;
  First: Boolean;
  FolderIndex: Integer;
begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  Res := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  First := True;
  FolderIndex := 1;
  while Res = 0 do
  begin
    if SearchRec.FindData.cFilename[0] <> '.' then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if First then
          Item.Items[0].Visible := False;
        It := TMenuItem.Create(Item);
        It.Caption := SearchRec.Name;
        It.Hint := Directory + SearchRec.Name;
        It.OnClick := DirectoryClick;
        It.ImageIndex := 0;
        Item.Insert(FolderIndex, It);
        Inc(FolderIndex);
        It2 := TMenuItem.Create(It);
        with It2 do
        begin
          Caption := RsEmptyItem;
          Enabled := False;
          Tag := 1;
        end;
        It.Add(It2);
      end
      else
      if UpperCase(ExtractFileExt(SearchRec.Name)) = '.URL' then
      begin
        if First then
          Item.Items[0].Visible := False;
        if FImages.Count = 1 then
          AddIconFrom(Directory + SearchRec.Name);
        It := TMenuItem.Create(Item);
        It.Caption := ChangeFileExt(SearchRec.Name, '');
        It.OnClick := UrlClick;
        It.Hint := Directory + SearchRec.Name;
        It.ImageIndex := 1;
        Item.Add(It);
      end;
    end;
    Res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

procedure TJvFavoritesButton.DirectoryClick(Sender: TObject);
begin
  DynBuild((Sender as TMenuItem), (Sender as TMenuItem).Hint);
end;

procedure TJvFavoritesButton.AddIconFrom(const Path: string);
var
  FileInfo: SHFILEINFO;
  Bmp: TBitmap;
begin
  SHGetFileInfo(PChar(Path), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
  Bmp := IconToBitmap2(FileInfo.hIcon, 16, clMenu);
  FImages.AddMasked(Bmp, Bmp.TransparentColor);
  Bmp.Free;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
