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

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls,
  JvAni, JvCombobox, JvDriveCtrls, JvListBox, JvExStdCtrls, JvExControls,
  JvComponent, JvAnimatedImage;

type
  TJvAniMainForm = class(TForm)
    FileListBox1: TJvFileListBox;
    DirectoryListBox1: TJvDirectoryListBox;
    DriveComboBox1: TJvDriveCombo;
    Memo1: TMemo;
    SaveDialog1: TSaveDialog;
    Save: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    ImageIcons: TImage;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ImageFrames: TImage;
    procedure FileListBox1Click(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Activated: Boolean;
  end;

var
  JvAniMainForm: TJvAniMainForm;

implementation

{$R *.dfm}

procedure TJvAniMainForm.FileListBox1Click(Sender: TObject);
begin
  Image1.Picture.LoadFromFile(FileListBox1.FileName);

  if Assigned(Image1.Picture) and Assigned(Image1.Picture.Graphic) and
    (Image1.Picture.Graphic is TJvAni) then
    with TJvAni(Image1.Picture.Graphic) do
    begin
      Animated := True;
      AssignIconsToBitmap(ImageIcons.Picture.Bitmap, clBtnFace, False, False);
      AssignToBitmap(ImageFrames.Picture.Bitmap, clBtnFace, False, False);
      Memo1.Clear;
      Memo1.Lines.Add('Author: ' + Author);
      Memo1.Lines.Add('Title: ' + Title);
      Memo1.Lines.Add('Icons: ' + IntToStr(IconCount));
      Memo1.Lines.Add('Frames: ' + IntToStr(FrameCount));
    end;
end;

procedure TJvAniMainForm.SaveClick(Sender: TObject);
begin
  SaveDialog1.InitialDir := DirectoryListBox1.Directory;
  if SaveDialog1.Execute then
    with TJvAni(Image1.Picture.Graphic) do
      SaveToFile(SaveDialog1.FileName);
end;

procedure TJvAniMainForm.FormActivate(Sender: TObject);
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  if not Activated then
  begin
    Activated := True;
    GetWindowsDirectory(Buffer, SizeOf(Buffer));
    DirectoryListBox1.Directory := Buffer + '\Cursors';
  end;
end;

procedure TJvAniMainForm.FormCreate(Sender: TObject);
begin
 // show the \data\ directtory where an example .ani is
 DirectoryListBox1.Directory := extractFileDir(Application.ExeName) + '\data\';
end;

end.
