{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirFrm.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDirectoryListForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Controls, Forms, StdCtrls, ComCtrls,
  JvComponent;

type
  TJvDirectoryListDialog = class(TJvForm)
    AddBtn: TButton;
    RemoveBtn: TButton;
    ModifyBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    DirectoryList: TListView;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure DirectoryListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DirectoryListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    procedure CheckButtons;
  end;

function EditFolderList(Folders: TStrings): Boolean;

implementation

uses
  JvJVCLUtils, JvJCLUtils, JvBrowseFolder, JvConsts;

{$R *.dfm}

function EditFolderList(Folders: TStrings): Boolean;
var
  I: Integer;
begin
  with TJvDirectoryListDialog.Create(Application) do
  try
    if Assigned(Folders) then
      for I := 0 to Folders.Count - 1 do
        DirectoryList.Items.Add.Caption := Folders[I];
    Result := ShowModal = mrOK;
    if Result and Assigned(Folders) then
    begin
      Folders.Clear;
      for I := 0 to DirectoryList.Items.Count - 1 do
        Folders.Add(DirectoryList.Items[I].Caption);
    end;
  finally
    Free;
  end;
end;

//=== { TJvDirectoryListDialog } =============================================

procedure TJvDirectoryListDialog.CheckButtons;
begin
  ModifyBtn.Enabled := (DirectoryList.Items.Count > 0) and
    (DirectoryList.Selected <> nil);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
  DirectoryList.AlphaSort;
end;

procedure TJvDirectoryListDialog.AddBtnClick(Sender: TObject);
var
  S: string;
begin
  S := '';
  if BrowseDirectory(S, '', 0) then
  begin
    if DirectoryList.FindCaption(0, S, False, True, True) = nil then
      with DirectoryList.Items.Add do
      begin
        Caption := S;
        Selected := True;
        Focused := True;
      end;
    CheckButtons;
  end;
end;

procedure TJvDirectoryListDialog.ModifyBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if DirectoryList.Selected = nil then
    Exit;
  I := DirectoryList.Selected.Index;
  S := DirectoryList.Items[I].Caption;
  if BrowseDirectory(S, '', 0) then
    DirectoryList.Items[I].Caption := S;
end;

procedure TJvDirectoryListDialog.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  if DirectoryList.Selected = nil then
    Exit;
  I := DirectoryList.Selected.Index;
  DirectoryList.Items.Delete(I);
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvDirectoryListDialog.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
//  BoxMoveFocusedItem(DirectoryList, DirectoryList.ItemAtPos(Point(X, Y), True));
//  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
//  BoxDragOver(DirectoryList, Source, X, Y, State, Accept, DirectoryList.Sorted);
//  CheckButtons;
end;

end.

