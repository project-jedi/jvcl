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

unit JvThumbnailChildFormU;

interface

{$I jvcl.inc}
{$I windowsonly.inc}

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, FileCtrl, ComCtrls, JvThumbImage,
  JvThumbNails, JvBaseThumbnail, JvExExtCtrls;

type
  TJvThumbnailChildForm = class(TForm)
    Splitter2: TSplitter;
    Panel6: TPanel;
    Splitter4: TSplitter;
    DirectoryListBox2: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Panel8: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    REDBar: TTrackBar;
    GreenBar: TTrackBar;
    BlueBar: TTrackBar;
    contrastBar: TTrackBar;
    Button2: TButton;
    Panel9: TPanel;
    DriveComboBox2: TDriveComboBox;
    Panel10: TPanel;
    FilterComboBox1: TFilterComboBox;
    Panel7: TPanel;
    Panel5: TPanel;
    Label5: TLabel;
    Bevel1: TBevel;
    asbuttoncb: TCheckBox;
    autoloadcb: TCheckBox;
    minmemcb: TCheckBox;
    titleplacegr: TRadioGroup;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    LightnessBar: TTrackBar;
    Button1: TButton;
    AngleGr: TRadioGroup;
    ThumbNail1: TJVThumbNail;
    ThumbImage1: TJvThumbImage;
    procedure Button2Click(Sender: TObject);
    procedure Panel6Resize(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure asbuttoncbClick(Sender: TObject);
    procedure autoloadcbClick(Sender: TObject);
    procedure minmemcbClick(Sender: TObject);
    procedure titleplacegrClick(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure thumbnail1Click(Sender: TObject);
    procedure Panel10Resize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AngleGrClick(Sender: TObject);
  end;

var
  JvThumbnailChildForm: TJvThumbnailChildForm;

implementation

{$R *.DFM}

procedure TJvThumbnailChildForm.Button2Click(Sender: TObject);
begin
  thumbimage1.ChangeRGB(redbar.Position,greenbar.Position,bluebar.Position);
  thumbimage1.Contrast(contrastbar.Position);
  THumbImage1.Lightness(LightnessBar.Position);
  redbar.Position:=0;
  greenbar.Position :=0;
  bluebar.Position:=0;
  contrastbar.position:=0;
  lightnessbar.Position:=0;
end;

procedure TJvThumbnailChildForm.Panel6Resize(Sender: TObject);
begin
  DriveComboBox2.Height:= panel9.ClientHeight;
  DriveComboBox2.Width := panel9.ClientWidth;
end;

procedure TJvThumbnailChildForm.FileListBox1Change(Sender: TObject);
begin
  if filelistbox1.FileName<>'' then
    thumbnail1.FileName := filelistbox1.FileName;
end;

procedure TJvThumbnailChildForm.asbuttoncbClick(Sender: TObject);
begin
  THumbnail1.Asbutton := asbuttoncb.Checked;
end;

procedure TJvThumbnailChildForm.autoloadcbClick(Sender: TObject);
begin
  thumbnail1.autoload := autoloadcb.Checked;
end;

procedure TJvThumbnailChildForm.minmemcbClick(Sender: TObject);
begin
  thumbnail1.minimizememory:=minmemcb.Checked;
end;

procedure TJvThumbnailChildForm.titleplacegrClick(Sender: TObject);
begin
  thumbnail1.TitlePlacement := ttitlepos(titleplacegr.ItemIndex);
end;

procedure TJvThumbnailChildForm.Panel8Resize(Sender: TObject);
begin
  redbar.Width := panel8.ClientWidth;
end;

procedure TJvThumbnailChildForm.Button4Click(Sender: TObject);
begin
  thumbimage1.invert;
end;

procedure TJvThumbnailChildForm.Button5Click(Sender: TObject);
begin
  thumbimage1.grayscale;
end;

procedure TJvThumbnailChildForm.thumbnail1Click(Sender: TObject);
begin
  if thumbnail1.FileName<>'' then
    thumbimage1.Loadfromfile(thumbnail1.FileName);
end;

procedure TJvThumbnailChildForm.Panel10Resize(Sender: TObject);
begin
  filtercombobox1.Width := panel10.ClientWidth;
  filtercombobox1.Height:= panel10.ClientHeight;
end;

procedure TJvThumbnailChildForm.FormShow(Sender: TObject);
begin
  //thumbimage1.Picture.Free;
  TitlePlacegr.ItemIndex := integer(thumbnail1.titlePlacement);
  Anglegr.ItemIndex := integer(thumbimage1.angle);
end;

procedure TJvThumbnailChildForm.AngleGrClick(Sender: TObject);
begin
  thumbimage1.angle := TAngle(anglegr.ItemIndex)
end;

end.
