{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvPlayListMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, Menus, JvPlaylist, JvListBox, JvCtrls, JvExStdCtrls;

type
  TJvPlayListMainForm = class(TForm)
    JvPlaylist1: TJvPlaylist;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ActionList1: TActionList;
    Open: TAction;
    Delete: TAction;
    Exit: TAction;
    Label2: TLabel;
    Options1: TMenuItem;
    ShowNumbers1: TMenuItem;
    ShowExtensions1: TMenuItem;
    Operations1: TMenuItem;
    DeleteDead: TAction;
    DeleteDeadFiles1: TMenuItem;
    Delete2: TMenuItem;
    SortSong: TAction;
    SortPah: TAction;
    SortPathI: TAction;
    SortSongNameInverted: TAction;
    RandomOrder: TAction;
    Reverse: TAction;
    SortByPath1: TMenuItem;
    SortByPathInverted1: TMenuItem;
    SortBySongName1: TMenuItem;
    SortBySongNameInverted1: TMenuItem;
    N2: TMenuItem;
    RandomOrder1: TMenuItem;
    ReverseOrder1: TMenuItem;
    Selection1: TMenuItem;
    SelectAll: TAction;
    UnselectAll: TAction;
    InvSelect: TAction;
    SelectAll1: TMenuItem;
    UnselectAll1: TMenuItem;
    InverseSelection1: TMenuItem;
    N3: TMenuItem;
    MoveUp: TAction;
    MoveDown: TAction;
    MoveSelectedUp1: TMenuItem;
    MoveSelectedDown1: TMenuItem;
    ShowDrives1: TMenuItem;
    procedure JvPlaylist1Click(Sender: TObject);
    procedure OpenExecute(Sender: TObject);
    procedure ExitExecute(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure ShowNumbers1Click(Sender: TObject);
    procedure ShowExtensions1Click(Sender: TObject);
    procedure SortSongExecute(Sender: TObject);
    procedure SortPahExecute(Sender: TObject);
    procedure SortPathIExecute(Sender: TObject);
    procedure SortSongNameInvertedExecute(Sender: TObject);
    procedure RandomOrderExecute(Sender: TObject);
    procedure ReverseExecute(Sender: TObject);
    procedure DeleteDeadExecute(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure UnselectAllExecute(Sender: TObject);
    procedure InvSelectExecute(Sender: TObject);
    procedure MoveUpExecute(Sender: TObject);
    procedure MoveDownExecute(Sender: TObject);
    procedure ShowDrives1Click(Sender: TObject);
  end;

var
  JvPlayListMainForm: TJvPlayListMainForm;

implementation

{$R *.DFM}


procedure TJvPlayListMainForm.JvPlaylist1Click(Sender: TObject);
begin
  if JvPlayList1.ItemIndex<>-1 then
    Label1.Caption := JvPlayList1.Items[JvPlayList1.ItemIndex];
end;

procedure TJvPlayListMainForm.OpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    JvPlayList1.AddItems(OpenDialog1.Files);
end;

procedure TJvPlayListMainForm.ExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TJvPlayListMainForm.DeleteExecute(Sender: TObject);
begin
  JvPlayList1.DeleteSelected;
end;

procedure TJvPlayListMainForm.Options1Click(Sender: TObject);
begin
  ShowNumbers1.Checked := JvPlayList1.ShowNumbers;
  ShowExtensions1.Checked := JvPlayList1.ShowExtension;
end;

procedure TJvPlayListMainForm.ShowNumbers1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    JvPlayList1.ShowNumbers := Checked;
  end;
end;

procedure TJvPlayListMainForm.ShowExtensions1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    JvPlayList1.ShowExtension := Checked;
  end;
end;

procedure TJvPlayListMainForm.SortSongExecute(Sender: TObject);
begin
  JvPlayList1.SortBySongName;
end;

procedure TJvPlayListMainForm.SortPahExecute(Sender: TObject);
begin
  JvPlayList1.SortByPath;
end;

procedure TJvPlayListMainForm.SortPathIExecute(Sender: TObject);
begin
  JvPlayList1.SortByPathInverted;
end;

procedure TJvPlayListMainForm.SortSongNameInvertedExecute(Sender: TObject);
begin
  JvPlayList1.SortBySongNameInverted;
end;

procedure TJvPlayListMainForm.RandomOrderExecute(Sender: TObject);
begin
  JvPlayList1.RandomOrder;
end;

procedure TJvPlayListMainForm.ReverseExecute(Sender: TObject);
begin
  JvPlayList1.ReverseOrder;
end;

procedure TJvPlayListMainForm.DeleteDeadExecute(Sender: TObject);
begin
  JvPlayList1.DeleteDeadFiles;
end;

procedure TJvPlayListMainForm.SelectAllExecute(Sender: TObject);
begin
  JvPlayList1.SelectAll;
end;

procedure TJvPlayListMainForm.UnselectAllExecute(Sender: TObject);
begin
  JvPlayList1.UnselectAll;
end;

procedure TJvPlayListMainForm.InvSelectExecute(Sender: TObject);
begin
  JvPlayList1.InvertSelection;
end;

procedure TJvPlayListMainForm.MoveUpExecute(Sender: TObject);
begin
  JvPlayList1.MoveSelectedUp;
end;

procedure TJvPlayListMainForm.MoveDownExecute(Sender: TObject);
begin
  JvPlayList1.MoveSelectedDown;
end;

procedure TJvPlayListMainForm.ShowDrives1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    JvPlayList1.ShowDrive := Checked;
  end;
end;

end.
