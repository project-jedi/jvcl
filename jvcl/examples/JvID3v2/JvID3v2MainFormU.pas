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

unit JvID3v2MainFormU;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  JvSearchFiles, ComCtrls,
  JvDriveCtrls, ExtCtrls, JvID3v2Base, JvId3v2, JvID3v2Types, JvComponent,
  StdCtrls, JvListBox, JvCombobox, JvExStdCtrls;

type
  TJvID3v2MainForm = class(TForm)
    ListView1: TListView;
    JvSearchFiles1: TJvSearchFiles;
    Splitter1: TSplitter;
    JvID3v21: TJvID3v2;
    Panel1: TPanel;
    JvDriveCombo1: TJvDriveCombo;
    JvDirectoryListBox1: TJvDirectoryListBox;
    procedure JvDirectoryListBox1Change(Sender: TObject);
    procedure JvSearchFiles1FindFile(Sender: TObject; const AName: string);
    procedure ListView1DblClick(Sender: TObject);
  private
    FDir: string;
  public
    procedure UpdateItem(Item: TListItem; const AFileName: string);
    procedure ChangeToDir(const ANewDir: string);
  end;

var
  JvID3v2MainForm: TJvID3v2MainForm;

implementation

uses
  JvJCLUtils, JvID3v2EditFormU;

{$R *.dfm}

procedure TJvID3v2MainForm.ChangeToDir(const ANewDir: string);
var
  Cursor: TCursor;
begin
  if ANewDir = FDir then
    Exit;

  FDir := ANewDir;

  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    ListView1.Items.BeginUpdate;
    try
      ListView1.Items.Clear;
      JvSearchFiles1.RootDirectory := FDir;
      JvSearchFiles1.Search;
    finally
      ListView1.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TJvID3v2MainForm.JvDirectoryListBox1Change(Sender: TObject);
begin
  ChangeToDir(JvDirectoryListBox1.Directory);
end;

procedure TJvID3v2MainForm.JvSearchFiles1FindFile(Sender: TObject;
  const AName: string);
var
  Item: TListItem;
  HasTag: Boolean;
  Version: TJvID3Version;
begin
  Item := ListView1.Items.Add;
  GetID3v2Version(AName, HasTag, Version);
  if HasTag then
    case Version of
      iveLowerThan2_2: Item.Caption := '<2.2';
      ive2_2: Item.Caption := '2.2';
      ive2_3: Item.Caption := '2.3';
      ive2_4: Item.Caption := '2.4';
      iveHigherThan2_4: Item.Caption := '>2.4'
    else
      Item.Caption := '?';
    end
  else
    Item.Caption := '-';

  Item.SubItems.Add(ExtractFileName(AName));
end;

procedure TJvID3v2MainForm.ListView1DblClick(Sender: TObject);
var
  LFileName: string;
begin
  if Assigned(ListView1.Selected) then
  begin
    LFileName := IncludeTrailingPathDelimiter(JvDirectoryListBox1.Directory) +
      ListView1.Selected.SubItems[0];
    if TJvID3v2EditForm.Execute(LFileName) then
      UpdateItem(ListView1.Selected, LFileName);
  end;
end;

procedure TJvID3v2MainForm.UpdateItem(Item: TListItem; const AFileName: string);
var
  HasTag: Boolean;
  Version: TJvID3Version;
begin
  GetID3v2Version(AFileName, HasTag, Version);
  if HasTag then
    case Version of
      iveLowerThan2_2: Item.Caption := '<2.2';
      ive2_2: Item.Caption := '2.2';
      ive2_3: Item.Caption := '2.3';
      ive2_4: Item.Caption := '2.4';
      iveHigherThan2_4: Item.Caption := '>2.4'
    else
      Item.Caption := '?';
    end
  else
    Item.Caption := '-';

  Item.SubItems[0] := ExtractFileName(AFileName);
end;

end.

