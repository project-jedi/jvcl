{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFavoritesButton.PAS, released on 2001-02-28.

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

unit JvFavoritesButton;


{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, Inifiles, ShellApi, imglist,
  JvTypes, JvButton, JvDirectories, JvFunctions;

type
  TJvFavoritesButton = class(TJvButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnUrl: TOnUrlClick;
    FOnPopup: TNotifyEvent;
    FImages: TImageList;
    procedure UrlClick(Sender: TObject);
  protected
    procedure DeleteItem(Item: TMenuItem; LookTag: Boolean = False);
    procedure PopupCreate(Sender: TObject);
    procedure DirectoryClick(Sender: TObject);
    procedure DynBuild(Item: TMenuItem; Directory: string);
    procedure AddIconFrom(Path: string);
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnUrlClick: TOnUrlClick read FOnUrl write FOnUrl;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

implementation

resourcestring
  RC_EmptyItem = '<Empty>';

  {*******************************************************}

constructor TJvFavoritesButton.Create(AOwner: TComponent);
var
  it: TMenuItem;
begin
  inherited;
  FDirs := TJvDirectories.Create(Self);

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
  FImages.Masked := true;
  FPopup.Images := FImages;
  AddIconFrom(FDirs.WindowsDirectory);
end;

{*******************************************************}

destructor TJvFavoritesButton.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited;
end;

{*******************************************************}

procedure TJvFavoritesButton.Click;
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

procedure TJvFavoritesButton.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrl) then
    FOnUrl(Self, (Sender as TMenuItem).Hint);
end;

{*******************************************************}

procedure TJvFavoritesButton.DeleteItem(Item: TMenuItem; LookTag: Boolean);
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

procedure TJvFavoritesButton.PopupCreate(Sender: TObject);
begin
  DynBuild(FPopup.Items, FDirs.Favorites);
end;

{*******************************************************}

procedure TJvFavoritesButton.DynBuild(Item: TMenuItem; Directory: string);
var
  res: Integer;
  SearchRec: TSearchRec;
  it, it2: TMenuItem;
  first: Boolean;
  FolderIndex:integer;
begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  res := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
  first := True;
  FolderIndex := 1;
  while res = 0 do
  begin
    if (SearchRec.FindData.cFilename[0] <> '.') then
    begin
      if (SearchRec.Attr and faDirectory = faDirectory) then
      begin
        if first then
          Item.Items[0].Visible := False;
        it := TMenuItem.Create(Item);
        it.Caption := SearchRec.Name;
        it.Hint := Directory + SearchRec.Name;
        it.OnClick := DirectoryClick;
        it.ImageIndex := 0;
        Item.Insert(FolderIndex,it);
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
      else if UpperCase(ExtractFileExt(SearchRec.Name)) = '.URL' then
      begin
        if first then
          Item.Items[0].Visible := False;
        if FImages.Count = 1 then
          AddIconFrom(Directory + SearchRec.Name);
        it := TMenuItem.Create(Item);
        it.Caption := ChangeFileExt(SearchRec.Name, '');
        it.OnClick := UrlClick;
        it.Hint := Directory + SearchRec.Name;
        it.ImageIndex := 1;
        Item.Add(it);
      end;
    end;
    res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

{*******************************************************}

procedure TJvFavoritesButton.DirectoryClick(Sender: TObject);
begin
  DynBuild((Sender as TMenuItem), (Sender as TMenuItem).Hint);
end;

{*******************************************************}

procedure TJvFavoritesButton.AddIconFrom(Path: string);
var
  FileInfo: SHFILEINFO;
  bmp: TBitmap;
begin
  SHGetFileInfo(PChar(Path), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
  bmp := IconToBitmap2(FileInfo.hIcon,16,clMenu);
  FImages.AddMasked(bmp, bmp.TransparentColor);
  bmp.Free;
end;

end.
