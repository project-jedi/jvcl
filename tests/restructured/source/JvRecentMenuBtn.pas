{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRecentMenuBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvRecentMenuBtn;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, ShellApi,
  JvButton, JvDirectories, JvTypes, JvFunctions, JvWinDialogs;

type
  TJvRecentMenuBtn = class(TJvButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnUrl: TOnLinkClick;
    FOnPopup: TNotifyEvent;
    procedure UrlClick(Sender: TObject);
    procedure InternalFileFind(const Path, FileMask: string; Strings: TStringList);
  protected
    procedure CreatePopup(Sender: TObject);
    procedure DynBuild(Item: TMenuItem; Directory: string);
    procedure DeleteItem(Item: TMenuItem; LookTag: Boolean = False);
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnLinkClick: TOnLinkClick read FOnUrl write FOnUrl;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

implementation

resourcestring
  RC_EmptyItem = '<Empty>';
const
  cMaxItems = 15;

  {*******************************************************}

constructor TJvRecentMenuBtn.Create(AOwner: TComponent);
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
  FPopup.OnPopup := CreatePopup;
end;

{*******************************************************}

destructor TJvRecentMenuBtn.Destroy;
begin
  FDirs.Free;
  DeleteItem(FPopup.Items);
  FPopup.Free;
  inherited;
end;

{*******************************************************}

procedure TJvRecentMenuBtn.Click;
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

procedure TJvRecentMenuBtn.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrl) then
    FOnUrl(Self, (Sender as TMenuItem).Hint);
end;

{*******************************************************}

procedure TJvRecentMenuBtn.CreatePopup(Sender: TObject);
begin
  DynBuild(FPopup.Items, FDirs.Recent);
end;

{*******************************************************}

function GetAssociatedIcon(const Filename: string; SmallIcon: boolean): HICON;
const
  cSmall: array[boolean] of Cardinal = (SHGFI_LARGEICON, SHGFI_SMALLICON);
var pfsi: TShFileInfo; hLarge: HICON; w: word;
begin
  FillChar(pfsi, sizeof(pfsi), 0);
  ShGetFileInfo(PChar(Filename), 0, pfsi, sizeof(pfsi),
    SHGFI_ICONLOCATION or SHGFI_ATTRIBUTES or SHGFI_ICON or cSmall[SmallIcon] or SHGFI_USEFILEATTRIBUTES);
  Result := pfsi.hIcon;
  if Result = 0 then
    ExtractIconEx(pfsi.szDisplayName, pfsi.iIcon, hLarge, Result, 1);
  if not SmallIcon then
    Result := hLarge;
  if Result = 0 then
    ExtractAssociatedIcon(GetFocus, PChar(Filename), w);
end;

function SortByName(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(ExtractFileName(List[Index2]),ExtractFileName(List[Index2]));
end;  

function SortByObject(List: TStringList; Index1, Index2: Integer): Integer;
begin
  // note: higher values sorted at the top
  Result := integer(List.Objects[Index2]) - integer(List.Objects[Index1]);
end;

function RemoveLnk(const S: string): string;
var i: integer;
begin
  Result := trim(StringReplace(S, '.lnk', '', [rfReplaceAll, rfIgnoreCase]));
    // now strip any extras from the end (like " (2)"):
  for i := Length(Result) downto 1 do
    if Result[i] = ' ' then
    begin
      SetLength(Result, i - 1);
      Exit;
    end;
end;

procedure TJvRecentMenuBtn.InternalFileFind(const Path, FileMask: string; Strings: TStringList);
var H: THandle; sr: TSearchRec; tmp:string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    H := FindFirst(Path + FileMask, faAnyFile, sr);
    try
      while H = 0 do
      begin
        // (p3) kludge to get past the '.lnk' extension to discover folders...
        tmp := RemoveLnk(sr.Name);
        if (sr.FindData.cFilename[0] <> '.') and (ExtractFileExt(tmp) <> '') then
          Strings.AddObject(tmp, TObject(sr.Time));
        H := FindNext(sr);
      end;
    finally
      FindClose(sr);
    end;
    Strings.CustomSort(SortByObject);
    while Strings.Count > cMaxItems do // delete any older files  
      Strings.Delete(Strings.Count-1);
    Strings.Sort; // CustomSort(SortByName); // sort by name instead
  finally
    Strings.EndUpdate;
  end;
end;

function Min(Val1,Val2:integer):integer;
begin
  Result := Val1;
  if Val2 < Val1 then
    Result := Val2;
end;  

procedure TJvRecentMenuBtn.DynBuild(Item: TMenuItem; Directory: string);
var
  it: TMenuItem;
  bmp: TBitmap;
  S: TStringlist;
  i:integer;
begin
  DeleteItem(Item, True);
  if (Directory <> '') and (Directory[Length(Directory)] <> '\') then
    Directory := Directory + '\';
  S := TStringlist.Create;
  try
    InternalFileFind(Directory, '*.*', S);
    for i := 0 to Min(S.Count-1,cMaxItems - 1) do
    begin
      it := TMenuItem.Create(Item);
      it.Caption := S[i];
      it.OnClick := UrlClick;
      it.Hint := Directory + S[i];
      bmp := IconToBitmap2(GetAssociatedIcon(S[i], true), 16, clMenu);
      it.Bitmap.Assign(bmp);
      bmp.Free;
      Item.Add(it);
    end;
  finally
    S.Free;
  end;
  Item.Items[0].Visible := (Item.Count = 1);
end;

{*******************************************************}

procedure TJvRecentMenuBtn.DeleteItem(Item: TMenuItem; LookTag: Boolean);
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

end.

