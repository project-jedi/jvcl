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

unit JvThumbnailMainFormU;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, FileCtrl, ComCtrls, Spin,
  JvThumbNails, JvSpecialProgress, JvThumbViews, JvBaseThumbnail,
  JvListBox, JvDriveCtrls, JvCombobox, JvExControls, JvComponent,
  JvExStdCtrls, JvExForms;

type
  TJvThumbnailMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    DriveComboBox1: TJvDriveCombo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Panel2: TPanel;
    DirectoryListBox1: TJvDirectoryListBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Panel3: TPanel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    THumbview1: TJVTHumbview;
    Panel5: TPanel;
    JvSpecialProgress1: TJvSpecialProgress;
    Bevel1: TBevel;
    procedure THumbview1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure THumbview1keyup(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure THumbview1ScanProgress(Sender: TObject; Position: Integer;
      var Break: Boolean);
    procedure THumbview1StartScanning(Sender: TObject; Max: Integer);
    procedure THumbview1StopScanning(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure THumbview1DblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure THumbview1Change(Sender: TObject);
  public
    NewDir: Boolean;
    Scanning: Boolean;
  end;

var
  JvThumbnailMainForm: TJvThumbnailMainForm;

implementation

uses JvThumbnailChildFormU;

{$R *.DFM}

procedure TJvThumbnailMainForm.THumbview1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //  panel4.Caption:=thumbview1.SelectedFile;
end;

procedure TJvThumbnailMainForm.THumbview1keyup(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //  panel4.Caption:=thumbview1.SelectedFile;
end;

procedure TJvThumbnailMainForm.DirectoryListBox1Change(Sender: TObject);
begin
  if not scanning then
    repeat
      thumbview1.Directory := directorylistbox1.Directory
    until thumbview1.Directory = directorylistbox1.Directory
  else
    NewDir := True;
end;

procedure TJvThumbnailMainForm.THumbview1ScanProgress(Sender: TObject; Position: Integer;
  var Break: Boolean);
begin
  JvSpecialProgress1.Position := Position;
  break := Newdir;
end;

procedure TJvThumbnailMainForm.THumbview1StartScanning(Sender: TObject; Max: Integer);
begin
  Scanning := True;
  //  directorylistbox1.Enabled := False;
  Button1.Enabled := True;
  JvSpecialProgress1.Maximum := Max;
  //  JvSpecialProgress1.Visible := true;
end;

procedure TJvThumbnailMainForm.THumbview1StopScanning(Sender: TObject);
begin
  Scanning := False;
  //  Directorylistbox1.Enabled := True;
  Spinedit2.MaxValue := thumbview1.Count - 1;
  newdir := False;
  Button1.Enabled := False;
  JvSpecialProgress1.Position := 0;
end;

procedure TJvThumbnailMainForm.Button1Click(Sender: TObject);
begin
  NewDir := True;
end;

procedure TJvThumbnailMainForm.CheckBox1Click(Sender: TObject);
begin
  THumbview1.AutoScrolling := checkbox1.Checked;
end;

procedure TJvThumbnailMainForm.CheckBox2Click(Sender: TObject);
begin
  thumbview1.AutoHandleKeyb := checkbox2.Checked;
end;

procedure TJvThumbnailMainForm.CheckBox4Click(Sender: TObject);
begin
  thumbview1.MinMemory := checkbox4.Checked;
end;

procedure TJvThumbnailMainForm.SpinEdit1Change(Sender: TObject);
begin
  if spinedit1.Text <> '' then thumbview1.ThumbGap := spinedit1.Value;
end;

procedure TJvThumbnailMainForm.SpinEdit2Change(Sender: TObject);
begin
  Thumbview1.Selected := spinedit2.Value;
end;

procedure TJvThumbnailMainForm.RadioGroup1Click(Sender: TObject);
begin
  thumbview1.AlignView := TViewType(radiogroup1.ItemIndex);
end;

procedure TJvThumbnailMainForm.RadioGroup2Click(Sender: TObject);
begin
  thumbview1.ScrollMode := TscrollMode(radiogroup2.ItemIndex);
end;

procedure TJvThumbnailMainForm.FormShow(Sender: TObject);
begin
  checkbox1.Checked := thumbview1.AutoScrolling;
  checkbox2.Checked := thumbview1.AutoHandleKeyb;
  checkbox3.Checked := thumbview1.Sorted;
  checkbox3.Checked := thumbview1.MinMemory;
  spinedit1.Value := thumbview1.ThumbGap;
  spinedit2.MaxValue := 0;
  spinedit1.MinValue := 0;
  radiogroup1.ItemIndex := integer(thumbview1.alignview);
  radiogroup2.ItemIndex := integer(thumbview1.scrollMode);
  Newdir := False;
  Scanning := False;
end;

procedure TJvThumbnailMainForm.TrackBar1Change(Sender: TObject);
begin
  thumbview1.Size := trackbar1.Position;
end;

procedure TJvThumbnailMainForm.THumbview1DblClick(Sender: TObject);
begin
  JvThumbnailChildForm := TJvThumbnailChildForm.Create(Self);
  JvThumbnailChildForm.DriveComboBox2.Drive := drivecombobox1.Drive;
  JvThumbnailChildForm.DirectoryListBox2.Directory := directorylistbox1.directory;
  if Sender is Tjvthumbview then
  begin
    JvThumbnailChildForm.FileListBox1.FileName := tjvThumbView(Sender).SelectedFile;
  end;
  if Sender is TjvTHumbnail then
  begin
    JvThumbnailChildForm.FileListBox1.FileName := tjvthumbnail(Sender).FileName;
  end;
  JvThumbnailChildForm.ShowModal;
  JvThumbnailChildForm.Free;
end;

procedure TJvThumbnailMainForm.Button2Click(Sender: TObject);
begin
  THumbview1DblClick(thumbview1);
end;

procedure TJvThumbnailMainForm.THumbview1Change(Sender: TObject);
begin
  panel4.Caption := thumbview1.SelectedFile;
end;

end.

