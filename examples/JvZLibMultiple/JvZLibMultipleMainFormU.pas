{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit JvZLibMultipleMainFormU;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvZLibMultiple, StdCtrls, ComCtrls;

type
  TJvZLibMultipleMainForm = class(TForm)
    btnCompress: TButton;
    btnUnCompress: TButton;
    Label1: TLabel;
    edSrcFolder: TEdit;
    Label2: TLabel;
    edDestFolder: TEdit;
    Label3: TLabel;
    edFilename: TEdit;
    pbProgress: TProgressBar;
    lblFilename: TLabel;
    btnSrc: TButton;
    btnDestFile: TButton;
    btnDestFolder: TButton;
    procedure btnCompressClick(Sender: TObject);
    procedure btnUnCompressClick(Sender: TObject);
    procedure edDestFolderChange(Sender: TObject);
    procedure edSrcFolderChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSrcClick(Sender: TObject);
    procedure btnDestFolderClick(Sender: TObject);
    procedure btnDestFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoProgress(Sender: TObject; Position, Total: Integer);
    procedure DoCompressFile(Sender: TObject; const Filename: string);

    procedure DoDecompressFile(Sender:TObject;const Filename:string;var WriteFile:Boolean); // NEW!


  end;

var
  JvZLibMultipleMainForm: TJvZLibMultipleMainForm;

implementation

uses
  JvBrowseFolder;

{$R *.dfm}

procedure TJvZLibMultipleMainForm.DoCompressFile(Sender:TObject;const Filename:string);
begin
  lblFilename.Caption := Filename;
  Update;
end;

// Event handler for before the WRITING of each file. 
procedure TJvZLibMultipleMainForm.DoDecompressFile(Sender:TObject;const Filename:string;var WriteFile:Boolean);
begin

// NEW EVENT PARAMETER: var WriteFile:Boolean
//
// If you want to skip the writing of all or some files during decompression,
// such as when you might want to generate a listing of archive contents without
// writing to disk, just set WriteFile := false in this event!

  lblFilename.Caption := Filename;
  Update;
end;



procedure TJvZLibMultipleMainForm.btnCompressClick(Sender: TObject);
var
  z : TJvZlibMultiple;
begin
  ForceDirectories(ExtractFilePath(edFilename.Text));
  z := TJvZlibMultiple.Create(nil);
  Screen.Cursor := crHourGlass;
  try
    lblFilename.Caption := '';
    pbProgress.Position := 0;
    z.OnProgress := DoProgress;
    z.OnCompressingFile := DoCompressFile;
    z.CompressDirectory(edSrcFolder.Text,false,edFilename.Text);
  finally
    z.Free;
    Screen.Cursor := crDefault;
  end;
  pbProgress.Position := 0;
  lblFilename.Caption := 'Ready';
end;

procedure TJvZLibMultipleMainForm.btnUnCompressClick(Sender: TObject);
var z:TJvZlibMultiple;
begin
  z := TJvZlibMultiple.Create(nil);
  Screen.Cursor := crHourGlass;
  try
    lblFilename.Caption := '';
    pbProgress.Position := 0;
    z.OnProgress := DoProgress;
    z.OnDecompressingFile := DoDecompressFile;
    z.DecompressFile(edFilename.Text,edDestFolder.Text,true);
  finally
    z.Free;
    Screen.Cursor := crDefault;
  end;
  pbProgress.Position := 0;
  lblFilename.Caption := 'Ready';
end;

procedure TJvZLibMultipleMainForm.DoProgress(Sender: TObject; Position, Total: Integer);
begin
  pbProgress.Max := Total;
  pbProgress.Position := Position;
  Update;
end;

procedure TJvZLibMultipleMainForm.edDestFolderChange(Sender: TObject);
begin
  btnUnCompress.Enabled := edDestFolder.Text <> '';
end;

procedure TJvZLibMultipleMainForm.edSrcFolderChange(Sender: TObject);
begin
  btnCompress.Enabled := DirectoryExists(edSrcFolder.Text) and (edFilename.Text <> '');
end;

procedure TJvZLibMultipleMainForm.FormShow(Sender: TObject);
begin
  edSrcFolderChange(Sender);
  edDestFolderChange(Sender);
end;

procedure TJvZLibMultipleMainForm.btnSrcClick(Sender: TObject);
var S:string;
begin
  S := edSrcFolder.Text;
  if BrowseForFolder('Select source folder',false,S) then
    edSrcFolder.Text := S;
end;

procedure TJvZLibMultipleMainForm.btnDestFolderClick(Sender: TObject);
var S:string;
begin
  S := edDestFolder.Text;
  if BrowseForFolder('Select source folder',true,S) then
    edDestFolder.Text := S;
end;

procedure TJvZLibMultipleMainForm.btnDestFileClick(Sender: TObject);
begin
  with TSaveDialog.Create(self) do
  try

    InitialDir := '.';
    Title := 'Select destination file';
    Filename := edFilename.Text;
    if Execute then
      edFilename.Text := Filename;
  finally
    Free;
  end;

end;

end.