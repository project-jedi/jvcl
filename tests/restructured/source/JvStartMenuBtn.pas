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

{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvStartMenuBtn;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Menus, ImgList, JvTypes, JvButton, JvDirectories;

type
  TJvStartMenuOption = (smCurrentUser,smCommon,smAllUsers);
  TJvStartMenuOptions = set of TJvStartMenuOption;
  TJvStartMenuBtn = class(TJvButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnUrl: TOnLinkClick;
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
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Options:TJvStartMenuOptions read FOptions write FOptions default [smCurrentUser..smAllUsers];
    property OnLinkClick: TOnLinkClick read FOnUrl write FOnUrl;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

implementation
uses
  ShellApi, JvFunctions;
  
resourcestring
  RC_EmptyItem = '<Empty>';

  {*******************************************************}

constructor TJvStartMenuBtn.Create(AOwner: TComponent);
var
  it: TMenuItem;
begin
  inherited;
  FDirs := TJvDirectories.Create(Self);
  FOptions := [smCurrentUser..smAllUsers];
  //Create Popup
  FPopup := TPopupMenu.Create(Self);
  it := TMenuItem.Create(FPopup);
  with it do
  begin
    Enabled := False;
    Caption := RC_EmptyItem;
    Tag := 1;
  end;
  FPopup.Items.Add(it);
  FPopup.OnPopup := PopupCreate;

  //Create Images
  FImages := TImageList.Create(Self);
  FImages.Width := 16;
  FImages.Height := 16;
  FImages.DrawingStyle := dsTransparent;
  FPopup.Images := FImages;
  AddIconFrom(FDirs.WindowsDirectory);
end;

{*******************************************************}

destructor TJvStartMenuBtn.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited;
end;

{*******************************************************}

procedure TJvStartMenuBtn.Click;
var
  p: TPoint;
begin
  inherited;
  p.x := 0;
  p.y := Height;
  p := ClientToScreen(p);
  FPopup.Popup(p.x, p.y);
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

{*******************************************************}

procedure TJvStartMenuBtn.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrl) then
    FOnUrl(Self, (Sender as TMenuItem).Hint);
end;

{*******************************************************}

procedure TJvStartMenuBtn.DeleteItem(Item: TMenuItem; LookTag: Boolean);
var
  i: Integer;
begin
  for i := Item.Count - 1 downto 0 do
    if (not LookTag) or (Item[i].Tag = 0) then
    begin
      DeleteItem(Item[i]);
      Item[i].Free;
    end;
end;

{*******************************************************}

procedure TJvStartMenuBtn.AddIconFrom(Path: string);
var
  FileInfo: SHFILEINFO;
  bmp: TBitmap;
begin
  SHGetFileInfo(PChar(Path), 0, FileInfo, SizeOf(FileInfo), SHGFI_SMALLICON or SHGFI_ICON);
  bmp := IconToBitmap2(FileInfo.hIcon, 16, clMenu);
  try
    FImages.AddMasked(bmp, bmp.TransparentColor);
  finally
    bmp.Free;
  end;
end;

{*******************************************************}

procedure TJvStartMenuBtn.DirectoryClick(Sender: TObject);
begin
  DynBuild((Sender as TMenuItem), (Sender as TMenuItem).Hint);
end;

{*******************************************************}

procedure TJvStartMenuBtn.PopupCreate(Sender: TObject);
begin
  if smCurrentUser in Options then
    DynBuild(FPopup.Items, FDirs.StartMenu);
  if smCommon in Options then
    DynBuild(FPopup.Items, FDirs.CommonStartMenu);
  if smAllUsers in Options then
    DynBuild(FPopup.Items, FDirs.AllUsersStartMenu);
end;

{*******************************************************}

procedure TJvStartMenuBtn.DynBuild(Item: TMenuItem; Directory: string);
var
  res, FolderIndex: Integer;
  SearchRec: TSearchRec;
  it, it2: TMenuItem;
  first: Boolean;
  bmp: TBitmap;

  function GetPathImage(const APath: string): TBitmap;
  var
    FileInfo: SHFILEINFO;
  begin
    SHGetFileInfo(PChar(APath), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
    Result := IconToBitmap2(FileInfo.hIcon, 16, clMenu);
//  Result := IconToBitmap2(ExtractAssociatedIcon(Application.Handle, PChar(it.Hint), w),16,clMenu);
  end;

begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  res := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  first := True;
  FolderIndex := 1;
  while res = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if first then
        Item.Items[0].Visible := False;
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        it := TMenuItem.Create(Item);
        it.Caption := SearchRec.Name;
        it.Hint := Directory + SearchRec.Name;
        it.OnClick := DirectoryClick;
        it.ImageIndex := 0;
        Item.Insert(FolderIndex, it);
        Inc(FolderIndex);
        it2 := TMenuItem.Create(it);
        with it2 do
        begin
          Caption := RC_EmptyItem;
          Enabled := False;
          Tag := 1;
        end;
        it.Add(it2);
      end
      else
      begin
        it := TMenuItem.Create(Item);
        it.Caption := ChangeFileExt(SearchRec.Name, '');
        it.OnClick := UrlClick;
        it.Hint := Directory + SearchRec.Name;
        bmp := GetPathImage(it.Hint);
        it.Bitmap.Assign(bmp);
        bmp.Free;
        Item.Add(it);
      end;
    end;
    res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;


end.

