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

unit JvAniMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls,
  JvAni, JvCombobox, JvDriveCtrls, JvListBox, JvExStdCtrls;

type
  TJvAniMainForm = class(TForm)
    Image1: TImage;
    FileListBox1: TJvFileListBox;
    DirectoryListBox1: TJvDirectoryListBox;
    DriveComboBox1: TJvDriveCombo;
    Memo1: TMemo;
    procedure FileListBox1Click(Sender: TObject);
  end;

var
  JvAniMainForm: TJvAniMainForm;

implementation

{$R *.dfm}

procedure TJvAniMainForm.FileListBox1Click(Sender: TObject);
begin
  Image1.Picture.LoadFromFile(FileListBox1.FileName);

  with TJvAni(Image1.Picture.Graphic) do
  begin
    Animated := True;
    Memo1.Clear;
    Memo1.Lines.Add('Author: ' + Author);
    Memo1.Lines.Add('Title: ' + Title);
    Memo1.Lines.Add('Frames: ' + IntToStr(FramesCount));
  end;
end;

end.
