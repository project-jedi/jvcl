{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit ChangeNotificationDirDlgU;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvChangeNotify;

type
  TChangeNotificationDirDlg = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    cbAttributes: TCheckBox;
    cbDirNames: TCheckBox;
    cbFileNames: TCheckBox;
    cbSize: TCheckBox;
    cbWrite: TCheckBox;
    cbSubTrees: TCheckBox;
    btnOK: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
  public
    class function Execute(var Directory: string; var Options: TJvChangeActions; var IncludeSubDirs: boolean): boolean;
  end;


implementation

uses
  FileCtrl;
  
{$R *.dfm}

procedure TChangeNotificationDirDlg.Button1Click(Sender: TObject);
var S: string;
begin
  S := GetCurrentDir;
  if SelectDirectory(S, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    Edit1.Text := S;
end;

class function TChangeNotificationDirDlg.Execute(var Directory: string;
  var Options: TJvChangeActions; var IncludeSubDirs: boolean): boolean;
var f: TChangeNotificationDirDlg;
begin
  f := self.Create(Application);
  with f do
  try
    Edit1.Text := Directory;
    cbFileNames.Checked := caChangeFileName in Options;
    cbAttributes.Checked := caChangeAttributes in Options;
    cbDirNames.Checked := caChangeDirName in Options;
    cbSize.Checked := caChangeSize in Options;
    cbWrite.Checked := caChangeLastWrite in Options;
    cbSubTrees.Checked := IncludeSubDirs;
    Result := ShowModal = mrOK;
    if Result then
    begin
      Directory := Edit1.Text;
      Options := [];
      if cbFileNames.Checked then
        Include(Options, caChangeFileName);
      if cbAttributes.Checked then
        Include(Options, caChangeAttributes);
      if cbDirNames.Checked then
        Include(Options, caChangeDirName);
      if cbSize.Checked then
        Include(Options, caChangeSize);
      if cbWrite.Checked then
        Include(Options, caChangeLastWrite);
      IncludeSubDirs := cbSubTrees.Checked;
    end;
  finally
    f.Free;
  end;

end;

end.

