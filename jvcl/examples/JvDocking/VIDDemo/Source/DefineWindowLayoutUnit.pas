{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit DefineWindowLayoutUnit;
{$I jvcl.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IniFiles, JvDockControlForm;

type
  TDefineWindowLayoutForm = class(TForm)
    Label1: TLabel;
    ViewName_Edit: TEdit;
    Views_ListBox: TListBox;
    Label2: TLabel;
    Close_Button: TButton;
    Apply_Button: TButton;
    Update_Button: TButton;
    Delete_Button: TButton;
    Rename_Button: TButton;
    Help_Button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Views_ListBoxClick(Sender: TObject);
    procedure ViewName_EditChange(Sender: TObject);
    procedure Close_ButtonClick(Sender: TObject);
    procedure Apply_ButtonClick(Sender: TObject);
    procedure Update_ButtonClick(Sender: TObject);
    procedure Delete_ButtonClick(Sender: TObject);
    procedure Rename_ButtonClick(Sender: TObject);
  private
    { Private declarations }
    FIniFile: TIniFile;
    FSections: TStringList;
  public
    { Public declarations }
    SelectItemIndex: Integer;
  end;

var
  DefineWindowLayoutForm: TDefineWindowLayoutForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TDefineWindowLayoutForm.FormCreate(Sender: TObject);
var i: Integer;
  Str: string;
begin
  FSections := TStringList.Create;
  FIniFile := TIniFile.Create(ExtractFilePath(Application.EXEName) + DefineWindowLayoutFileName);
  FIniFile.ReadSection(SectionString, FSections);
  i := FSections.IndexOf(DefaultLayout);
  if i <> -1 then
    FSections.Delete(i);
  FSections.Sort;
  for i := 0 to FSections.Count - 1 do
  begin
    Str := FIniFile.ReadString(SectionString, FSections[i], 'ERROR');
    if Str <> 'ERROR' then
      Views_ListBox.Items.Add(FSections[i]);
  end;

  Views_ListBoxClick(Views_ListBox);
  ViewName_EditChange(ViewName_Edit);
end;

procedure TDefineWindowLayoutForm.FormDestroy(Sender: TObject);
var i: Integer;
begin
  if FIniFile <> nil then
  begin
    FIniFile.EraseSection(SectionString);
    for i := 0 to Views_ListBox.Items.Count - 1 do
      FIniFile.WriteString(SectionString, Views_ListBox.Items[i], Views_ListBox.Items[i] + '.ini');
    FIniFile.Free;
    FIniFile := nil;
  end;
  if FSections <> nil then
  begin
    FSections.Free;
    FSections := nil;
  end;
end;

procedure TDefineWindowLayoutForm.Views_ListBoxClick(Sender: TObject);
begin
  if Views_ListBox.ItemIndex <> -1 then
  begin
    Update_Button.Caption := '&Update';
    ViewName_Edit.Text := Views_ListBox.Items[Views_ListBox.ItemIndex];
    Apply_Button.Enabled := True;
    Update_Button.Enabled := True;
    Delete_Button.Enabled := True;
    Rename_Button.Enabled := True;
  end else
  begin
    Delete_Button.Enabled := False;
    Rename_Button.Enabled := False;
  end;
end;

procedure TDefineWindowLayoutForm.ViewName_EditChange(Sender: TObject);
var Str: string;
begin
  Str := Trim(ViewName_Edit.Text);
  if Str = '' then
  begin
    Update_Button.Enabled := False;
    Rename_Button.Enabled := False;
  end else
  begin
    Update_Button.Enabled := True;
    Rename_Button.Enabled := True;
  end;

  if Views_ListBox.Items.IndexOf(Str) = -1 then
  begin
    Update_Button.Caption := '&Add';
    Apply_Button.Enabled := False;
  end else
  begin
    Update_Button.Caption := '&Update';
    Rename_Button.Enabled := False;
  end;
end;

procedure TDefineWindowLayoutForm.Close_ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDefineWindowLayoutForm.Apply_ButtonClick(Sender: TObject);
begin
  if Views_ListBox.ItemIndex <> -1 then
  begin
    {$IFDEF USEJVCL}
    LoadDockTreeFromAppStorage(MainForm.JvAppStorage, Views_ListBox.Items[Views_ListBox.ItemIndex]);
    {$ELSE}
    LoadDocktreeFromFile(ExtractFilePath(Application.EXEName) + Views_ListBox.Items[Views_ListBox.ItemIndex] + '.ini');
    {$ENDIF}
    SelectItemIndex := Views_ListBox.ItemIndex;
    SetForegroundWindow(Handle);
  end;
end;

procedure TDefineWindowLayoutForm.Update_ButtonClick(Sender: TObject);
begin
  if Update_Button.Caption = '&Add' then
  begin
    Views_ListBox.Items.Add(Trim(ViewName_Edit.Text));
  end;
  {$IFDEF USEJVCL}
  SaveDockTreeToAppStorage(MainForm.JvAppStorage, Trim(ViewName_Edit.Text));
  {$ELSE}
  SaveDockTreeToFile(ExtractFilePath(Application.EXEName) + Trim(ViewName_Edit.Text) + '.ini');
  {$ENDIF}
  ViewName_EditChange(ViewName_Edit);
end;

procedure TDefineWindowLayoutForm.Delete_ButtonClick(Sender: TObject);
begin
  DeleteFile(ExtractFilePath(Application.EXEName) + Views_ListBox.Items[Views_ListBox.ItemIndex] + '.ini');
  Views_ListBox.Items.Delete(Views_ListBox.ItemIndex);
  SelectItemIndex := 0;
end;

procedure TDefineWindowLayoutForm.Rename_ButtonClick(Sender: TObject);
begin
  if (Views_ListBox.ItemIndex <> -1) and (Trim(ViewName_Edit.Text) <> Views_ListBox.Items[Views_ListBox.ItemIndex]) then
  begin
    RenameFile(ExtractFilePath(Application.EXEName) + Views_ListBox.Items[Views_ListBox.ItemIndex] + '.ini',
      ExtractFilePath(Application.EXEName) + Trim(ViewName_Edit.Text) + '.ini');
    Views_ListBox.Items[Views_ListBox.ItemIndex] := Trim(ViewName_Edit.Text);
  end;
end;

end.
