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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDirFrm;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls,
  JvxCtrls, JvPlacemnt, JvComponent, JvListBox, JvCtrls;

type
  TJvDirectoryListDialog = class(TJvForm)
    DirectoryList: TJvListBox;
    AddBtn: TButton;
    RemoveBtn: TButton;
    ModifyBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    Storage: TJvFormStorage;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure DirectoryListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DirectoryListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    procedure CheckButtons;
  end;

function EditFolderList(Folders: TStrings): Boolean;

implementation

uses
  JvFileUtil, JvBoxProcs, JvConst;

{$R *.DFM}

function EditFolderList(Folders: TStrings): Boolean;
begin
  with TJvDirectoryListDialog.Create(Application) do
  try
    if Assigned(Folders) then
      DirectoryList.Items.Assign(Folders);
    Result := ShowModal = mrOk;
    if Result and Assigned(Folders) then
      Folders.Assign(DirectoryList.Items);
  finally
    Free;
  end;
end;

procedure TJvDirectoryListDialog.CheckButtons;
begin
  ModifyBtn.Enabled := (DirectoryList.Items.Count > 0) and
    (DirectoryList.ItemIndex >= 0);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
end;

procedure TJvDirectoryListDialog.AddBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  if BrowseDirectory(S, '', 0) then
  begin
    I := DirectoryList.Items.Add(S);
    DirectoryList.ItemIndex := I;
    CheckButtons;
  end;
end;

procedure TJvDirectoryListDialog.ModifyBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := DirectoryList.ItemIndex;
  if I >= 0 then
  begin
    S := DirectoryList.Items[I];
    if BrowseDirectory(S, '', 0) then
      DirectoryList.Items[I] := S;
  end;
end;

procedure TJvDirectoryListDialog.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryList.ItemIndex;
  if I >= 0 then
  begin
    DirectoryList.Items.Delete(I);
    CheckButtons;
  end;
end;

procedure TJvDirectoryListDialog.DirectoryListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvDirectoryListDialog.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListDragDrop(Sender,Source: TObject;
  X, Y: Integer);
begin
  BoxMoveFocusedItem(DirectoryList, DirectoryList.ItemAtPos(Point(X, Y), True));
  CheckButtons;
end;

procedure TJvDirectoryListDialog.DirectoryListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DirectoryList, Source, X, Y, State, Accept, DirectoryList.Sorted);
  CheckButtons;
end;

procedure TJvDirectoryListDialog.FormCreate(Sender: TObject);
begin
  with Storage do
  begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
end;

end.
