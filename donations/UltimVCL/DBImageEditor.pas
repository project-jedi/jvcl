{
-----------------------------------------------------------------------------

                              DBImageEditor 1.0

                        VCL for Delphi and C++Builder
                       (C) Frédéric Leneuf-Magaud 2001

-----------------------------------------------------------------------------

 Please read ReadMe.txt for conditions of use

 This component is provided 'as-is', without any express or implied warranty.
 In no event shall the author be held liable for any damages arising from the
 use of this component.

-----------------------------------------------------------------------------
}

unit DBImageEditor;

interface

{$I OPTIONS.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  ExtDlgs, Buttons, ComCtrls, ToolWin, FileUtil, DB, DBCtrls;

{$I LANG_STR.INC}

type
  TImageEditButtons = (ibImport, ibEdit, ibEditWith, ibExport);
  TIEButtonsSet = set of TImageEditButtons;

  TDBImageEditor = class(TFrame)
    DBImage: TDBImage;
    ToolBar: TToolBar;
    SB_Import: TSpeedButton;
    SB_Edit: TSpeedButton;
    SB_EditWith: TSpeedButton;
    SB_Export: TSpeedButton;
    procedure DBImageClick(Sender: TObject);
    procedure ToolBarClick(Sender: TObject);
    procedure SB_ImportClick(Sender: TObject);
    procedure SB_EditClick(Sender: TObject);
    procedure SB_EditWithClick(Sender: TObject);
    procedure SB_ExportClick(Sender: TObject);
  private
    FImageLink: TFieldDataLink;
    FButtons: TIEButtonsSet;
    FPaintProgram: string;
    FWaitBeforeUpdate: Boolean;
    FCanRevert: Boolean;
    FPicSaved: Boolean;
    FOriginalPic: string;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDataField: string;
    procedure SetDataField(Value: string);
    procedure SetButtons(Value: TIEButtonsSet);
    procedure ActiveChange(Sender: TObject);
    procedure SetPaintProgram(Value: string);
    procedure SetWaitBeforeUpdate(Value: Boolean);
    procedure SetCanRevert(Value: Boolean);
    procedure DataChange(Sender: TObject);
    procedure SaveOriginalPic;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(const Command, Path: string): Boolean;
    function GetUniqueFileName(const Prefix, Extension: string): string;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
      { Buttons: define the buttons displayed on the toolsbar }
    property Buttons: TIEButtonsSet read FButtons write SetButtons;
      { PaintProgram: define the paint program used for editing pictures }
    property PaintProgram: string read FPaintProgram write SetPaintProgram;
      { WaitBeforeUpdate: wait for a confirmation by the user of the end of changes before updating.
        Normalement, le composant est capable de détecter la fermeture du programme d'édition mais
        certains programmes (plutôt rares) ont un chargement en cascade qui leurre le composant.
        Pour être sûr que la mise à jour se fasse bien après l'édition et non pendant, on peut
        demander une confirmation de la fin d'édition en mettant WaitBeforeUpdate à True.
        PaintBrush, par exemple, nécessite WaitBeforeUpdate à True. }
    property WaitBeforeUpdate: Boolean read FWaitBeforeUpdate write SetWaitBeforeUpdate default True;
      { CanRevert: allow to revert with Escape key to the last saved version of the picture }
    property CanRevert: Boolean read FCanRevert write SetCanRevert default True;
  end;

{ NB: this component and the paint program share a temporary file with an unique name.
  This temp file is deleted after editing. }

implementation

{$R *.dfm}

constructor TDBImageEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageLink := TFieldDataLink.Create;
  FImageLink.Control := Self;
  FImageLink.OnActiveChange := ActiveChange;
  FImageLink.OnDataChange := DataChange;
  FButtons := [ibImport, ibEdit, ibEditWith, ibExport];
  FPaintProgram := '';
  FWaitBeforeUpdate := True;
  FCanRevert := True;
  FPicSaved := False;
  FOriginalPic := '';
end;

destructor TDBImageEditor.Destroy;
begin
  FImageLink.Control := nil;
  FImageLink.Free;
  FImageLink := nil;
  inherited Destroy;
end;

function TDBImageEditor.GetDataSource: TDataSource;
begin
  if DBImage <> nil then
    Result := DBImage.DataSource
  else
    Result := nil;
end;

procedure TDBImageEditor.SetDataSource(Value: TDataSource);
begin
  if DBImage <> nil then
    DBImage.DataSource := Value;
  if FImageLink <> nil then
    FImageLink.DataSource := Value;
end;

function TDBImageEditor.GetDataField: string;
begin
  if DBImage <> nil then
    Result := DBImage.DataField
  else
    Result := '';
end;

procedure TDBImageEditor.SetDataField(Value: string);
begin
  if DBImage <> nil then
    DBImage.DataField := Value;
  if FImageLink <> nil then
  begin
    FImageLink.FieldName := Value;
    ActiveChange(Self);
  end;
end;

procedure TDBImageEditor.SetButtons(Value: TIEButtonsSet);
begin
  FButtons := Value;
   { We cannot have the EditWith button without the Edit one }
  if not SB_Edit.Visible and (ibEditWith in FButtons) then
    FButtons := FButtons + [ibEdit];
  if not (ibEdit in FButtons) then
    FButtons := FButtons - [ibEditWith];
   { The buttons visibility is inverted }
  SB_Import.Visible := (ibImport in FButtons);
  SB_Edit.Visible := (ibEdit in FButtons);
  SB_EditWith.Visible := (ibEditWith in FButtons);
  SB_Export.Visible := (ibExport in FButtons);
end;

procedure TDBImageEditor.ActiveChange(Sender: TObject);
begin
  if FImageLink.Active then
  begin
    SB_Import.Enabled := FImageLink.CanModify;
    SB_Edit.Enabled := FImageLink.CanModify;
    SB_EditWith.Enabled := SB_Edit.Enabled;
    SB_Export.Enabled := FImageLink.Field <> nil;
  end
  else
  begin
    SB_Import.Enabled := False;
    SB_Edit.Enabled := False;
    SB_EditWith.Enabled := False;
    SB_Export.Enabled := False;
  end;
end;

procedure TDBImageEditor.SetPaintProgram(Value: string);
begin
  if FPaintProgram <> Value then
    FPaintProgram := Value;
end;

procedure TDBImageEditor.SetWaitBeforeUpdate(Value: Boolean);
begin
  if FWaitBeforeUpdate <> Value then
    FWaitBeforeUpdate := Value;
end;

procedure TDBImageEditor.SetCanRevert(Value: Boolean);
begin
  if FCanRevert <> Value then
    FCanRevert := Value;
end;

procedure TDBImageEditor.DataChange(Sender: TObject);
begin
  if Assigned(DBImage.DataSource) and (DBImage.DataSource.State = dsBrowse) then
    FPicSaved := False;
end;

procedure TDBImageEditor.SaveOriginalPic;
begin
  if FCanRevert then
  begin
    { The original picture (before any change) is saved in FOriginalPic }
    if (DBImage.Field <> nil) and (DBImage.Field is TBlobField) then
      FOriginalPic := DBImage.Field.AsString
    else
      FOriginalPic := '';
    FPicSaved := True;
  end;
end;

procedure TDBImageEditor.WMChar(var Msg: TWMChar);
begin
  if (Char(Msg.CharCode) = #27) and FCanRevert and FPicSaved then
  begin
    { Cancel: we revert to the picture stored in FOriginalPic }
    if (DBImage.Field <> nil) and (DBImage.Field is TBlobField) then
    begin
      DBImage.Field.AsString := FOriginalPic;
      FPicSaved := False;
    end;
  end;
  inherited;
end;

procedure TDBImageEditor.DBImageClick(Sender: TObject);
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
end;

procedure TDBImageEditor.ToolBarClick(Sender: TObject);
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
end;

procedure TDBImageEditor.SB_ImportClick(Sender: TObject);
var
  FileOpen: TOpenPictureDialog;
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
  { Dataset opened ? }
  if (DBImage.DataSource.DataSet = nil) or not DBImage.DataSource.DataSet.Active then
    Exit;
  { Opening of the picture dialog }
  FileOpen := TOpenPictureDialog.Create(Self);
  try
    with FileOpen do
    begin
      Title := lsChooseImage;
      Filename := '';
      InitialDir := '';
      Filter := lsBMPFilter + '|' + lsDefaultFilter;
      Options := Options + [ofHideReadOnly, ofPathMustExist, ofFileMustExist];
    end;
    if FileOpen.Execute then
    begin
      { Store the picture to replace }
      SaveOriginalPic;
      { Load the new picture }
      DBImage.DataSource.Edit;
      DBImage.Picture.LoadFromFile(FileOpen.FileName);
      DBImage.Field.Assign(DBImage.Picture);
    end;
  finally
    FileOpen.Free;
  end;
end;

function TDBImageEditor.Execute(const Command, Path: string): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  { Initializations }
  Result := False;
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := SW_SHOWNORMAL;
  end;
  { Launch program }
  if not CreateProcess(nil, PChar(Command), nil, nil, False, NORMAL_PRIORITY_CLASS, nil,
    PChar(Path), StartupInfo, ProcessInfo) then
    Exit;
  { Wait until it ends }
  Application.Minimize;
  with ProcessInfo do
  begin
    WaitForInputIdle(hProcess, INFINITE);
    WaitForSingleObject(hProcess, INFINITE);
    CloseHandle(hThread);
    CloseHandle(hProcess);
  end;
  Application.Restore;
  Result := True;
end;

function TDBImageEditor.GetUniqueFileName(const Prefix, Extension: string): string;
var
  CalcSuffix: string;
begin
  { Format: prefix + number of ticks (in hexa) }
  repeat
    CalcSuffix := IntToHex(GetTickCount, 4);
    Result := Prefix + CalcSuffix + Extension;
  until not FileExists(Result);
end;

procedure TDBImageEditor.SB_EditClick(Sender: TObject);
var
  TempImg: string;
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
  { Dataset opened ? }
  if (DBImage.DataSource.DataSet = nil) or not DBImage.DataSource.DataSet.Active then
    Exit;
  { Paint program defined ? }
  FPaintProgram := Trim(FPaintProgram);
  if FPaintProgram = '' then
  begin
    Application.MessageBox(lsChooseBefore, lsError, MB_ICONWARNING);
    Exit;
  end;
  { Creation of an unique temp file }
  TempImg := GetUniqueFileName('~T', '.BMP'); // .BMP mandatory for Picture.LoadFromFile
  TempImg := GetTempDir + TempImg;
  { Fill and edit the temp file }
  Screen.Cursor := crHourGlass;
  try
    DBImage.Picture.SaveToFile(TempImg);
    if Execute('"' + FPaintProgram + '" "' + TempImg + '"', ExtractFilePath(FPaintProgram)) then
    begin
      { Waiting message ? }
      if FWaitBeforeUpdate then
        Application.MessageBox(lsWaiting, PChar(ExtractFileName(TempImg)), MB_OK);
      { Store the picture to replace }
      SaveOriginalPic;
      { Load the modified picture }
      DBImage.DataSource.Edit;
      DBImage.Picture.LoadFromFile(TempImg);
      DBImage.Field.Assign(DBImage.Picture);
    end;
  finally
    { Deletion of the temp file }
    DeleteFile(TempImg);
    Screen.Cursor := crDefault;
  end;
  { Go back to the component }
  Application.BringToFront;
  if Visible then
    SetFocus;
end;

procedure TDBImageEditor.SB_EditWithClick(Sender: TObject);
var
  FileOpen: TOpenDialog;
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
  { Opening of the program selection dialog }
  FileOpen := TOpenDialog.Create(Self);
  try
    with FileOpen do
    begin
      Title := lsChoosePaintProg;
      Filename := FPaintProgram;
      InitialDir := ExtractFilePath(FPaintProgram);
      Filter := lsEXEFilter + '|' + lsDefaultFilter;
      Options := Options + [ofHideReadOnly, ofPathMustExist, ofFileMustExist];
    end;
    if FileOpen.Execute then
    begin
      { Store in a var the selected program }
      SetPaintProgram(FileOpen.FileName);
      SetWaitBeforeUpdate(Application.MessageBox(lsWaitBeforeUpdate,
        PChar(ExtractFileName(FileOpen.FileName)), MB_ICONQUESTION + MB_YESNO) = ID_YES);
    end;
  finally
    FileOpen.Free;
  end;
end;

procedure TDBImageEditor.SB_ExportClick(Sender: TObject);
var
  FileSave: TSavePictureDialog;
begin
  { The focus is given to the TFrame parent }
  if Visible then
    SetFocus;
  { Dataset opened ? }
  if (DBImage.DataSource.DataSet = nil) or not DBImage.DataSource.DataSet.Active then
    Exit;
  { Opening of a save picture dialog }
  FileSave := TSavePictureDialog.Create(Self);
  try
    with FileSave do
    begin
      Title := '';
      Filename := '';
      InitialDir := '';
      DefaultExt := 'BMP';
      Filter := lsBMPFilter + '|' + lsDefaultFilter;
      Options := Options + [ofHideReadOnly, ofNoReadOnlyReturn, ofOverwritePrompt];
    end;
    if FileSave.Execute then
    begin
      { Save the picture into the file selected }
      DBImage.Picture.SaveToFile(FileSave.FileName);
    end;
  finally
    FileSave.Free;
  end;
end;

end.
