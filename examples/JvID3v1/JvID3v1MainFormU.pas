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

unit JvID3v1MainFormU;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, StdCtrls, Mask, JvToolEdit,
  JvDragDrop, JvId3v1, ComCtrls, ToolWin, ActnList, ImgList, 
  JvBaseDlg, JvTipOfDay, JvBalloonHint, JvMaskEdit, JvSpin, JvJVCLAboutForm,
  JvExMask;

type
  TJvID3v1MainForm = class(TForm)
    JvFilenameEdit1: TJvFilenameEdit;
    JvDragDrop1: TJvDragDrop;
    edtTitle: TEdit;
    JvId3v11: TJvId3v1;
    edtAlbum: TEdit;
    edtArtist: TEdit;
    edtYear: TEdit;
    edtComment: TEdit;
    cmbGenre: TComboBox;
    lblArtist: TLabel;
    lblAlbum: TLabel;
    lblYear: TLabel;
    lblComment: TLabel;
    lblGenre: TLabel;
    ActionList1: TActionList;
    actSave: TAction;
    actRefresh: TAction;
    actErase: TAction;
    actExit: TAction;
    actOnTop: TAction;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    lblHasTag: TLabel;
    JvTipOfDay1: TJvTipOfDay;
    JvJVCLAboutComponent1: TJvJVCLAboutComponent;
    actAbout: TAction;
    ToolButton6: TToolButton;
    JvBalloonHint1: TJvBalloonHint;
    sedTrack: TJvSpinEdit;
    lblTitle: TLabel;
    lblTrack: TLabel;
    procedure actSaveExecute(Sender: TObject);
    procedure actEraseExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actOnTopExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvFilenameEdit1KeyPress(Sender: TObject; var Key: Char);
    procedure JvFilenameEdit1AfterDialog(Sender: TObject; var Name: string;
      var Action: Boolean);
    procedure actAboutExecute(Sender: TObject);
    procedure JvDragDrop1Drop(Sender: TObject; Pos: TPoint;
      Value: TStrings);
  public
    procedure ChangeFileNameTo(S: string);
    procedure FillGenres(Strings: TStrings);
    procedure UpdateCtrls;
    procedure UpdateCaption;
  end;

var
  JvID3v1MainForm: TJvID3v1MainForm;

implementation

uses
  JvId3v2Types;

{$R *.dfm}

procedure TJvID3v1MainForm.ChangeFileNameTo(S: string);
begin
  JvFilenameEdit1.Text := S;
  JvFilenameEdit1.Hint := S;
  JvId3v11.FileName := S;
  JvId3v11.Open;
  UpdateCtrls;
  UpdateCaption;
  FocusControl(edtTitle);
end;

procedure TJvID3v1MainForm.FillGenres(Strings: TStrings);
begin
  ID3_Genres(Strings,true);
end;

procedure TJvID3v1MainForm.actSaveExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
    JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
  begin
    JvId3v11.SongName := edtTitle.Text;
    JvId3v11.Artist := edtArtist.Text;
    JvId3v11.Album := edtAlbum.Text;
    JvId3v11.Year := edtYear.Text;
    JvId3v11.GenreAsString := cmbGenre.Text;
    JvId3v11.Comment := edtComment.Text;
    JvId3v11.AlbumTrack := sedTrack.AsInteger;

    if JvId3v11.Commit then
      UpdateCaption
    else
      JvBalloonHint1.ActivateHint(ToolButton2, 'Could not save changes.'#13+
        'The file is probably opened by another application.', ikError, 'Error');
  end;
end;

procedure TJvID3v1MainForm.actEraseExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
    JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
  begin
    JvId3v11.Erase;
    UpdateCtrls;
    UpdateCaption;
  end;
end;

procedure TJvID3v1MainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TJvID3v1MainForm.actRefreshExecute(Sender: TObject);
begin
  if JvId3v11.FileName = '' then
    JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'First select a mp3 file', ikError, 'Error', 5000)
  else
    ChangeFileNameTo(JvId3v11.FileName);
end;

procedure TJvID3v1MainForm.actOnTopExecute(Sender: TObject);
const
  CStyle: array[Boolean] of TFormStyle = (fsNormal, fsStayOnTop);
begin
  JvDragDrop1.AcceptDrag := False;
  actOnTop.Checked := not actOnTop.Checked;
  FormStyle := CStyle[actOnTop.Checked];
  JvDragDrop1.AcceptDrag := True;
end;

procedure TJvID3v1MainForm.FormCreate(Sender: TObject);
begin
  { This is put in the OnCreate and not in the OnShow event, because we change
    Form1.FormStyle at run-time that will trigger the OnShow event }
  FillGenres(cmbGenre.Items);
  UpdateCaption;
end;

procedure TJvID3v1MainForm.JvFilenameEdit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if JvFilenameEdit1.Text = '' then
      JvBalloonHint1.ActivateHint(JvFilenameEdit1, 'Empty strings are no file names', ikError, 'Error', 5000)
    else
      ChangeFileNameTo(JvFilenameEdit1.FileName);
  end;
end;

procedure TJvID3v1MainForm.JvFilenameEdit1AfterDialog(Sender: TObject;
  var Name: string; var Action: Boolean);
begin
  if Action then
    ChangeFileNameTo(Name);
end;

procedure TJvID3v1MainForm.UpdateCaption;
const
  CHasTagStr: array[Boolean] of string = ('No tag', 'Has Tag');
  CHasTagColor: array[Boolean] of TColor = (clRed, clBlack);
var
  HasTag: Boolean;
begin
  if JvId3v11.FileName > '' then
  begin
    { Store TagPresent in variabele to prevent double checks whether the file
      has a tag }
    HasTag := JvId3v11.HasTag;
    lblHasTag.Font.Color := CHasTagColor[HasTag];
    lblHasTag.Caption := CHasTagStr[HasTag];
  end
  else
    lblHasTag.Caption := '';
end;

procedure TJvID3v1MainForm.UpdateCtrls;
begin
  edtTitle.Text := JvId3v11.SongName;
  edtAlbum.Text := JvId3v11.Album;
  edtArtist.Text := JvId3v11.Artist;
  edtYear.Text := JvId3v11.Year;
  edtComment.Text := JvId3v11.Comment;
  sedTrack.AsInteger := JvId3v11.AlbumTrack;
  cmbGenre.ItemIndex := cmbGenre.Items.IndexOfObject(TObject(JvId3v11.Genre));
end;

procedure TJvID3v1MainForm.actAboutExecute(Sender: TObject);
begin
  JvJVCLAboutComponent1.Execute;
end;

procedure TJvID3v1MainForm.JvDragDrop1Drop(Sender: TObject; Pos: TPoint;
  Value: TStrings);
begin
  if Value.Count > 0 then
    ChangeFileNameTo(Value[0]);
end;

end.

