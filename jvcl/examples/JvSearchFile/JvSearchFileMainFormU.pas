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

unit JvSearchFileMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvListbox, JvComponent, JvCtrls, JvSearchFiles, Mask, JvToolEdit,
  ComCtrls, JvEdit, Menus, JvFormPlacement, JvExMask, JvAppStorage,
  JvAppIniStorage;

type
  TJvSearchFileMainForm = class(TForm)
    JvSearchFile1: TJvSearchFiles;
    GroupBox1: TGroupBox;
    JvDirectoryBox1: TJvDirectoryEdit;
    btnSearch: TButton;
    Label1: TLabel;
    chkRecursive: TCheckBox;
    Label2: TLabel;
    edFileMask: TEdit;
    GroupBox2: TGroupBox;
    btnCancel: TButton;
    reFoundFiles: TRichEdit;
    JvFormStorage1: TJvFormStorage;
    StatusBar1: TStatusBar;
    chkClearList: TCheckBox;
    chkNoDupes: TCheckBox;
    cbContainText: TComboBox;
    rbInclude: TRadioButton;
    rbExclude: TRadioButton;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    procedure btnSearchClick(Sender: TObject);
    procedure JvSearchFile1FindFile(Sender: TObject; const AName: string);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure JvSearchFile1BeginScanDir(Sender: TObject;
      const AName: String);
    procedure OptionsChange(Sender: TObject);
    procedure Sort1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure JvSearchFile1Progress(Sender: TObject);
  private
    { Private declarations }
    procedure AddSearchTextToComboBox;
  end;

var
  JvSearchFileMainForm: TJvSearchFileMainForm;

implementation
uses
  JvJVCLUtils, JvJCLUtils;

{$R *.DFM}

procedure TJvSearchFileMainForm.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled := false;
  btnCancel.Enabled := true;
  Screen.Cursor := crHourGlass;
  try
    if chkClearList.Checked then
      reFoundFiles.Lines.Clear;
    AddSearchTextToComboBox;
    JvSearchFile1.Files.Clear;
    JvSearchFile1.Directories.Clear;
    JvSearchFile1.FileParams.FileMasks.Text := edFileMask.Text;
    if chkRecursive.Checked then
      JvSearchFile1.DirOption := doIncludeSubDirs
    else
      JvSearchFile1.DirOption := doExcludeSubDirs;
    // don't store file and folder names - we do that in the memo
    JvSearchFile1.Options := JvSearchFile1.Options + [soOwnerData];
    JvSearchFile1.RootDirectory := JvDirectoryBox1.EditText;
    JvSearchFile1.Search;
  finally
    StatusBar1.Panels[0].Text := Format('(%d matching items found)',[reFoundFiles.Lines.Count]);
    btnSearch.Enabled := true;
    btnCancel.Enabled := false;
    Screen.Cursor := crDefault;
  end;
end;

function ContainsText(const Filename,AText:string):boolean;
var S:TMemoryStream;tmp:string;
begin
  Result := false;
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(Filename);
    if S.Memory <> nil then
    begin
      tmp := PChar(S.Memory);
      tmp := AnsiLowerCase(tmp);
      Result := Pos(AnsiLowerCase(AText),tmp) > 0;
    end;
  finally
    S.Free;
  end;
end;

procedure TJvSearchFileMainForm.JvSearchFile1FindFile(Sender: TObject;
  const AName: string);
begin
  StatusBar1.Panels[0].Text := Format('Searching in %s...',[AName]);
  StatusBar1.Update;
  if (cbContainText.Text <> '') then
    if rbInclude.Checked <> ContainsText(AName,cbContainText.Text) then
      Exit;
  if not chkNoDupes.Checked or (reFoundFiles.Lines.IndexOf(AName) < 0) then
    reFoundFiles.Lines.Add(AName);
end;

procedure TJvSearchFileMainForm.btnCancelClick(Sender: TObject);
begin
  JvSearchFile1.Abort;
  btnCancel.Enabled := false;
end;

procedure TJvSearchFileMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  btnCancel.Click;
end;

procedure TJvSearchFileMainForm.FormCreate(Sender: TObject);
begin
//  JvAppINIFileStore1.FileName := ChangeFileExt(Application.ExeName,'.ini');
end;

procedure TJvSearchFileMainForm.JvSearchFile1BeginScanDir(Sender: TObject;
  const AName: String);
begin
  StatusBar1.Panels[0].Text := Format('Searching in %s...',[ExcludeTrailingPathDelimiter(AName)]);
  StatusBar1.Update;
end;

procedure TJvSearchFileMainForm.OptionsChange(Sender: TObject);
begin
//  rbInclude.Enabled := cbContainText.Text <> '';
//  rbExclude.Enabled := rbInclude.Enabled;
  StatusBar1.Panels[0].Text := 'Ready';
  StatusBar1.Update;
end;

procedure TJvSearchFileMainForm.Sort1Click(Sender: TObject);
var S:TStringlist;
begin
  S := TStringlist.Create;
  try
   S.Assign(reFoundFiles.Lines);
   S.Sort;
   while (S.Count > 0) and (S[0] = '') do S.Delete(0);
   reFoundFiles.Lines := S;
  finally
    S.Free;
  end;
end;

procedure TJvSearchFileMainForm.Clear1Click(Sender: TObject);
begin
  reFoundFiles.Clear;
end;

procedure TJvSearchFileMainForm.AddSearchTextToComboBox;
begin
  with cbContainText do
    if (Text <> '') and (Items.IndexOf(Text) < 0) then
        Items.Add(Text);
end;

procedure TJvSearchFileMainForm.JvSearchFile1Progress(Sender: TObject);
begin
  Application.ProcessMessages;
end;

end.

