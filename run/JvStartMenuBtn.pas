{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStartMenuBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvStartMenuBtn;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Menus, ImgList,
  JvTypes, JvButton, JvDirectories;

type
  TJvStartMenuOption = (smCurrentUser, smCommon, smAllUsers);
  TJvStartMenuOptions = set of TJvStartMenuOption;

  TJvStartMenuBtn = class(TJvCustomButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnLinkClick: TJvLinkClickEvent;
    FOnPopup: TNotifyEvent;
    FImages: TImageList;
    FOptions: TJvStartMenuOptions;
    procedure UrlClick(Sender: TObject);
  protected
    procedure AddIconFrom(Path: string);
    procedure DeleteItem(Item: TMenuItem; LookTag: Boolean = False);
    procedure PopupCreate(Sender: TObject);
    procedure DirectoryClick(Sender: TObject);
    procedure DynBuild(Item: TMenuItem; Directory: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Options: TJvStartMenuOptions read FOptions write FOptions default [smCurrentUser..smAllUsers];
    property OnLinkClick: TJvLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

implementation

uses
  ShellApi, JvFunctions;

resourcestring
  RC_EmptyItem = '<Empty>';

constructor TJvStartMenuBtn.Create(AOwner: TComponent);
var
  It: TMenuItem;
begin
  inherited Create(AOwner);
  FDirs := TJvDirectories.Create(Self);
  FOptions := [smCurrentUser..smAllUsers];
  //Create Popup
  FPopup := TPopupMenu.Create(Self);
  It := TMenuItem.Create(FPopup);
  with It do
  begin
    Enabled := False;
    Caption := RC_EmptyItem;
    Tag := 1;
  end;
  FPopup.Items.Add(It);
  FPopup.OnPopup := PopupCreate;

  //Create Images
  FImages := TImageList.Create(Self);
  FImages.Width := 16;
  FImages.Height := 16;
  FImages.DrawingStyle := dsTransparent;
  FPopup.Images := FImages;
  AddIconFrom(FDirs.WindowsDirectory);
end;

destructor TJvStartMenuBtn.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited Destroy;
end;

procedure TJvStartMenuBtn.Click;
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

procedure TJvStartMenuBtn.UrlClick(Sender: TObject);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, (Sender as TMenuItem).Hint);
end;

procedure TJvStartMenuBtn.DeleteItem(Item: TMenuItem; LookTag: Boolean);
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

procedure TJvStartMenuBtn.AddIconFrom(Path: string);
var
  FileInfo: SHFILEINFO;
  Bmp: TBitmap;
begin
  SHGetFileInfo(PChar(Path), 0, FileInfo, SizeOf(FileInfo), SHGFI_SMALLICON or SHGFI_ICON);
  Bmp := IconToBitmap2(FileInfo.hIcon, 16, clMenu);
  try
    FImages.AddMasked(Bmp, Bmp.TransparentColor);
  finally
    Bmp.Free;
  end;
end;

procedure TJvStartMenuBtn.DirectoryClick(Sender: TObject);
begin
  DynBuild((Sender as TMenuItem), (Sender as TMenuItem).Hint);
end;

procedure TJvStartMenuBtn.PopupCreate(Sender: TObject);
begin
  if smCurrentUser in Options then
    DynBuild(FPopup.Items, FDirs.StartMenu);
  if smCommon in Options then
    DynBuild(FPopup.Items, FDirs.CommonStartMenu);
  if smAllUsers in Options then
    DynBuild(FPopup.Items, FDirs.AllUsersStartMenu);
end;

procedure TJvStartMenuBtn.DynBuild(Item: TMenuItem; Directory: string);
var
  Res, FolderIndex: Integer;
  SearchRec: TSearchRec;
  It, It2: TMenuItem;
  First: Boolean;
  Bmp: TBitmap;

  function GetPathImage(const APath: string): TBitmap;
  var
    FileInfo: SHFILEINFO;
  begin
    SHGetFileInfo(PChar(APath), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
    Result := IconToBitmap2(FileInfo.hIcon, 16, clMenu);
//  Result := IconToBitmap2(ExtractAssociatedIcon(Application.Handle, PChar(It.Hint), w),16,clMenu);
  end;

begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  Res := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  First := True;
  FolderIndex := 1;
  while Res = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if First then
        Item.Items[0].Visible := False;
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
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
          Caption := RC_EmptyItem;
          Enabled := False;
          Tag := 1;
        end;
        It.Add(It2);
      end
      else
      begin
        It := TMenuItem.Create(Item);
        It.Caption := ChangeFileExt(SearchRec.Name, '');
        It.OnClick := UrlClick;
        It.Hint := Directory + SearchRec.Name;
        Bmp := GetPathImage(It.Hint);
        It.Bitmap.Assign(Bmp);
        Bmp.Free;
        Item.Add(It);
      end;
    end;
    Res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

end.

