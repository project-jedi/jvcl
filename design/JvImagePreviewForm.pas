{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImagPrvw.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvImagePreviewForm;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  SysUtils, Classes,
  Windows, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  FileCtrl, Buttons, JvPicClip, JvFormPlacement, JvAppStorage,
  {$IFDEF MSWINDOWS}
  JvAppRegistryStorage,
  {$ENDIF MSWINDOWS}
  JvComponent;

type
  TImageForm = class(TJvForm)
    DirectoryList: TDirectoryListBox;
    DriveCombo: TDriveComboBox;
    PathLabel: TLabel;
    FileEdit: TEdit;
    ImagePanel: TPanel;
    Image: TImage;
    FileListBox: TFileListBox;
    ImageName: TLabel;
    FilterCombo: TFilterComboBox;
    StretchCheck: TCheckBox;
    FilePics: TJvPicClip;
    FormStorage: TJvFormStorage;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PreviewBtn: TSpeedButton;
    AppStorage: TJvAppRegistryStorage;
    procedure FileListBoxClick(Sender: TObject);
    procedure StretchCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileListBoxChange(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FFormCaption: string;
    FBmpImage: TBitmap;
    procedure ZoomImage;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure PreviewKeyPress(Sender: TObject; var Key: Char);
  public
    property FileName: string read GetFileName write SetFileName;
  end;

function SelectImage(var AFileName: string; const Extensions, Filter: string): Boolean;

implementation

uses
  Math,
  JclStrings, 
  JvConsts, JvJVCLUtils, JvDsgnConsts;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

function SelectImage(var AFileName: string; const Extensions, Filter: string): Boolean;
var
  ErrMode: Cardinal;
  Filters: TStrings;
begin
  with TImageForm.Create(Application) do
  try
    FileListBox.Mask := Extensions;
    FilterCombo.Filter := Filter;
    Filters := TStringList.Create;
    try
      ExtractStrings(['|'], [], PChar(Filter), Filters);
      if Filters.IndexOf(AllFilePattern) < 0 then
        FilterCombo.Filter := Filter + '|' + AllFilePattern
      else
        FilterCombo.Filter := Filter;
    finally
      Filters.Free;
    end;
    {$IFDEF MSWINDOWS}
    ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
    {$ENDIF MSWINDOWS}
    try
      if AFileName <> '' then
        FileName := AFileName;
      Result := ShowModal = mrOk;
    finally
      {$IFDEF MSWINDOWS}
      SetErrorMode(ErrMode);
      {$ENDIF MSWINDOWS}
    end;
    if Result then
      AFileName := FileName;
  finally
    Free;
  end;
end;

type
  TDirList = class(TDirectoryListBox);
  TFileList = class(TFileListBox);
  TDriveCombo = class(TDriveComboBox);

function ValidPicture(Pict: TPicture): Boolean;
begin
  Result := (Pict.Graphic <> nil) and (not Pict.Graphic.Empty) and
    (Pict.Width > 0) and (Pict.Height > 0);
end;

function TImageForm.GetFileName: string;
begin
  Result := FileListBox.FileName;
end;

procedure TImageForm.SetFileName(const Value: string);
begin
  FileListBox.FileName := Value;
end;

procedure TImageForm.ZoomImage;
begin
  if ValidPicture(Image.Picture) then
  begin
    with JvJVCLUtils.ZoomImage(Image.Picture.Width, Image.Picture.Height,
      ImagePanel.ClientWidth - 4, ImagePanel.ClientHeight - 4,
      StretchCheck.Checked) do
    begin
      Image.Width := X;
      Image.Height := Y;
    end;
    CenterControl(Image);
  end;
end;

procedure TImageForm.FileListBoxClick(Sender: TObject);
var
  FileExt: string;
begin
  FileExt := UpperCase(ExtractFileExt(FileListBox.FileName));
  try
    StartWait;
    try
      Image.Picture.LoadFromFile(FileListBox.FileName);
    finally
      StopWait;
    end;
    {$IFDEF MSWINDOWS}
    ImageName.Caption := Format('%s (%d x %d)',
      [AnsiLowerCase(ExtractFileName(FileListBox.FileName)),
       Image.Picture.Width, Image.Picture.Height]);
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    ImageName.Caption := Format('%s (%d x %d)',
      [ExtractFileName(FileListBox.FileName),
       Image.Picture.Width, Image.Picture.Height]);
    {$ENDIF LINUX}
  except
    Image.Picture.Assign(nil);
    ImageName.Caption := '';
  end;
  ZoomImage;
  {$IFDEF MSWINDOWS}
  FileExt := AnsiLowerCase(FileName);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FileExt := FileName;
  {$ENDIF LINUX}
  if FileExt <> '' then
    Caption := FFormCaption + ' - ' + MinimizeName(FileExt, PathLabel.Canvas,
      PathLabel.Width)
  else
    Caption := FFormCaption;
  PreviewBtn.Enabled := ValidPicture(Image.Picture);
end;

procedure TImageForm.StretchCheckClick(Sender: TObject);
begin
  ZoomImage;
  Image.Stretch := StretchCheck.Checked;
end;

procedure TImageForm.FormCreate(Sender: TObject);
begin
  FFormCaption := Caption;
  Image.Align := alNone;
  FBmpImage := TBitmap.Create;
  FBmpImage.Assign(FilePics.GraphicCell[5]);
  if not NewStyleControls then
    Font.Style := [fsBold];
  AppStorage.Root := SDelphiKey;
  with TDirList(DirectoryList) do
  begin
    ClosedBmp.Assign(FilePics.GraphicCell[0]);
    OpenedBmp.Assign(FilePics.GraphicCell[1]);
    CurrentBmp.Assign(FilePics.GraphicCell[2]);
  end;
  with TFileList(FileListBox) do
  begin
    DirBmp.Assign(FilePics.GraphicCell[0]);
    ExeBmp.Assign(FilePics.GraphicCell[3]);
    UnknownBmp.Assign(FilePics.GraphicCell[4]);
  end;
  with TDriveCombo(DriveCombo) do
  begin
    FloppyBMP.Assign(FilePics.GraphicCell[6]);
    FixedBMP.Assign(FilePics.GraphicCell[7]);
    CDROMBMP.Assign(FilePics.GraphicCell[8]);
    NetworkBMP.Assign(FilePics.GraphicCell[9]);
    RAMBMP.Assign(FilePics.GraphicCell[10]);
  end;
  FileListBoxChange(nil);
  TComboBox(FilterCombo).ItemHeight := Max(TComboBox(FilterCombo).ItemHeight,
    FilePics.Height);
  TComboBox(DriveCombo).ItemHeight := Max(TComboBox(FilterCombo).ItemHeight,
    FilePics.Height);
  TFileList(FileListBox).ItemHeight := Max(TFileList(FileListBox).ItemHeight,
    FilePics.Height + 1);
  TFileList(FileListBox).ItemHeight := Max(TFileList(FileListBox).ItemHeight,
    FilePics.Height + 1);
  DirectoryList.Height := FileListBox.Height;
end;

procedure TImageForm.FileListBoxChange(Sender: TObject);
var
  I: Integer;
  FileExt: string;
begin
  for I := 0 to TFileList(FileListBox).Items.Count - 1 do
  begin
    FileExt := ExtractFileExt(TFileList(FileListBox).Items[I]);
    if (TFileList(FileListBox).Items[I][1] <> '[') and
      (CompareText(FileExt, '.bmp') = 0) then
      TFileList(FileListBox).Items.Objects[I] := FBmpImage;
  end;
end;

procedure TImageForm.FileListBoxDblClick(Sender: TObject);
begin
  if ValidPicture(Image.Picture) then
    ModalResult := mrOk;
end;

procedure TImageForm.FormDestroy(Sender: TObject);
begin
  FBmpImage.Free;
  FBmpImage := nil;
end;

procedure TImageForm.PreviewKeyPress(Sender: TObject; var Key: Char);
begin
//  if Ord(Key) = VK_ESCAPE then
  if Ord(Key) = 27 then   //asn: With VisualCLX VK_ESCAPE <> 27
    TForm(Sender).Close;
end;

procedure TImageForm.PreviewBtnClick(Sender: TObject);
var
  PreviewForm: TForm;
begin
  if not ValidPicture(Image.Picture) then
    Exit;
  {$IFDEF BCB}
  PreviewForm := TForm.CreateNew(Self, 0);
  {$ELSE}
  PreviewForm := TForm.CreateNew(Self);
  {$ENDIF BCB}
  with PreviewForm do
  try
    Caption := RsPreview;
    {$IFDEF VCL}
    BorderStyle := bsSizeToolWin;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BorderStyle := fbsSizeToolWin;
    {$ENDIF VisualCLX}
    Icon := Self.Icon;
    KeyPreview := True;
    Position := poScreenCenter;
    OnKeyPress := PreviewKeyPress;
    with TImage.Create(PreviewForm) do
    begin
      Left := 0;
      Top := 0;
      Stretch := False;
      AutoSize := True;
      Picture.Assign(Image.Picture);
      Parent := PreviewForm;
    end;
    if Image.Picture.Width > 0 then
    begin
      ClientWidth := Image.Picture.Width;
      ClientHeight := Image.Picture.Height;
    end;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TImageForm.OkBtnClick(Sender: TObject);
begin
  if ActiveControl = FileEdit then
    FileListBox.ApplyFilePath(FileEdit.Text)
  else
  if ValidPicture(Image.Picture) then
    ModalResult := mrOk
  else
    SysUtils.Beep;
end;

end.

