{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: fJvclConverterMain.PAS, released on 2002-07-15.

The Initial Developer of the Original Code is Michel Beck <mbeck1@compuserve.com>
Portions created by Michael Beck are Copyright (C) 2002 Michael Beck
All Rights Reserved.

This program contains code released and copyrighted by:
Igor Komar (the original Replace routines)
Dave Jewell (new GUI for Igor's code)
Martin Waldenburg (FastTime)

Contributor(s): Peter Thörnqvist <peter3 at sourceforge dot net>
                Arioch <the_Arioch@chat.ru>

Last Modified: 2002-07-18

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit fJvclConverterMain;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Grids, ValEdit, JvComCtrls, Menus, ActnList, StdActns, ImgList,
  ToolWin,
  JvComponent, JvSearchFiles, JvBaseDlg, JvBrowseFolder, JVCLConvertUtils,
  JvFormPlacement, JvAppStorage, JvAppIniStorage, JvDialogs;

type
  { TValueListEditor (imposer class that allows "=" in strings) }
  TValueListEditor = class(ValEdit.TValueListEditor)
  protected
    procedure SetEditText(ACol: integer; ARow: integer;
      const Value: string); override;
    function GetEditText(ACol: integer; ARow: integer): string; override;
    procedure DrawCell(ACol: integer; ARow: integer; ARect: TRect;
      AState: TGridDrawState); override;
  end;

  TfrmMain = class(TForm)
    OpenDialog: TJvOpenDialog;
    JvPageControl: TPageControl;
    tbsFiles: TTabSheet;
    tbsStrings: TTabSheet;
    vleUnits: TValueListEditor;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Conversion1: TMenuItem;
    Help1: TMenuItem;
    AboutMe1: TMenuItem;
    OpenReplacementFile1: TMenuItem;
    Convert1: TMenuItem;
    OpenFilestoConvert1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    N2: TMenuItem;
    ToolBar1: TToolBar;
    btnAddFiles: TToolButton;
    btnOpenRepository: TToolButton;
    btnConvert: TToolButton;
    btnAboutMe: TToolButton;
    btnExit: TToolButton;
    ActionList1: TActionList;
    FileExit: TFileExit;
    FileOpen: TAction;
    Convert: TAction;
    OpenData: TAction;
    AboutMe: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    btnDeleteFiles: TToolButton;
    Remove1: TMenuItem;
    RemoveFiles: TAction;
    SaveData: TAction;
    btnSaveRepository: TToolButton;
    SaveConversionData1: TMenuItem;
    DeleteLine: TAction;
    btnDeleteLine: TToolButton;
    DeleteLine1: TMenuItem;
    Options1: TMenuItem;
    NewLine: TAction;
    btnInsertNewLine: TToolButton;
    InsertLine1: TMenuItem;
    JvSearchFiles1: TJvSearchFiles;
    IterateSubdirectories: TAction;
    btnIterateDir: TToolButton;
    IterateSubdirectories1: TMenuItem;
    SortArrows: TImageList;
    JvBrowseFolder1: TJvBrowseForFolderDialog;
    mnuFileMask: TMenuItem;
    btnNewRepository: TToolButton;
    NewRepository: TAction;
    SelectAll: TAction;
    Edit: TMenuItem;
    SelectAll1: TMenuItem;
    StandardImages: TImageList;
    SearchList: TListView;
    sbStatus: TStatusBar;
    Options: TAction;
    Options2: TMenuItem;
    N3: TMenuItem;
    FileMask: TAction;
    JvAppIniFileStorage: TJvAppIniFileStorage;
    JvFormStorage: TJvFormStorage;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure SearchListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddLineClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure AboutMeExecute(Sender: TObject);
    procedure OpenDataExecute(Sender: TObject);
    procedure vleUnitsStringsChange(Sender: TObject);
    procedure ConvertUpdate(Sender: TObject);
    procedure NewLineUpdate(Sender: TObject);
    procedure DeleteLineUpdate(Sender: TObject);
    procedure RemoveFilesUpdate(Sender: TObject);
    procedure IterateSubdirectoriesExecute(Sender: TObject);
    procedure SearchListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: boolean);
    procedure JvSearchFiles1FindFile(Sender: TObject; const AName: string);
    procedure NewRepositoryExecute(Sender: TObject);
    procedure SearchListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure OptionsExecute(Sender: TObject);
    procedure FileMaskExecute(Sender: TObject);
  private
    { Private declarations }
    fCurrentDataFile: string;
    FAppOptions: TAppOptions;
    procedure SetStatus(const Msgs: array of string);
    procedure AddFiles(const FileName: string);
    function StringReplace(const FullFileName: string; WholeWord, Backup, Simulate: boolean; var ReplaceTime:
      TLargeInteger): integer;
    function FileNameReplace(var FileItem: TListItem): string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SortListColumn(LV: TListView; Column: TListColumn);
    procedure LoadDATFile(const FileName: string);
  public
    { Public declarations }
    procedure WMDropFiles(var Msg: TWMDropFiles); message wm_DropFiles;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  ShellAPI, FastTime, fAboutMe, CommCtrl, OptionsFrm,
  JvPropertyStore;

const
  Allowed = (['a'..'z', 'A'..'Z', '0'..'9', '_']);
  cReplaceEquals = #2;

var
  Lines: TStringlist; // create this once to speed things up
  ch: char;

type
  TValueListStringsEx = class(TValueListStrings);

{ TValueListEditor }

procedure TValueListEditor.DrawCell(ACol, ARow: integer; ARect: TRect;
  AState: TGridDrawState);
begin
  Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, StringReplace(Cells[ACol, ARow], cReplaceEquals, '=',
    [rfReplaceAll]));
end;

function TValueListEditor.GetEditText(ACol, ARow: integer): string;
begin
  Result := StringReplace(inherited GetEditText(ACol, ARow), cReplaceEquals, '=', [rfReplaceAll]);
end;

procedure TValueListEditor.SetEditText(ACol, ARow: integer;
  const Value: string);
var
  tmp: string;
begin
  tmp := StringReplace(Value, '=', cReplaceEquals, [rfReplaceAll]);
  inherited SetEditText(ACol, ARow, tmp);
end;

{ TfrmMain }

function TfrmMain.StringReplace(const FullFileName: string; WholeWord, Backup, Simulate: boolean; var ReplaceTime:
  TLargeInteger): integer;
var
  OldLine, NewLine, HiLine, BackupName: string;
  FromStr, ToStr: string;
  i: integer;
  InputStream: TFileStream;
  OutputStream: TMemoryStream;

  function SearchReplace: integer;
  var
    LineNum, P: integer;
  begin
    Result := 0;
    for LineNum := 0 to Lines.Count - 1 do
    begin
      OldLine := Lines[LineNum];
      NewLine := '';
      HiLine := UpperCase(OldLine);

      while true do
      begin
        P := Pos(FromStr, HiLine);
        if P <= 0 then
          break;
        if not WholeWord or (((P = 1) or (not (OldLine[P - 1] in Allowed))) and ((P + Length(FromStr) > Length(OldLine))
          or (not (OldLine[P + Length(FromStr)] in Allowed)))) then
        begin
          Inc(Result);
          NewLine := NewLine + Copy(OldLine, 1, P - 1) + ToStr;
        end
        else
        begin
          NewLine := NewLine + Copy(OldLine, 1, P + Length(FromStr) - 1);
        end;
        Delete(OldLine, 1, P + Length(FromStr) - 1);
        Delete(HiLine, 1, P + Length(FromStr) - 1);
      end;

      Lines.Strings[LineNum] := NewLine + OldLine;
    end;
  end;

begin
  Result := 0;
  if not FileExists(FullFilename) then Exit;
  //if this is DFM file, convert it to text
  if UpperCase(ExtractFileExt(FullFileName)) = '.DFM' then
  begin
    InputStream := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
    try
      begin
          //      if Size < 2 then memLog.Lines.Add('File size ' + IntToStr(Size) + ' for form ' + s);
        ch := #0;
        InputStream.Read(ch, 1);
        case ch of
          'o', 'O', 'i', 'I':
            Lines.LoadFromFile(FullFileName); //ok - text
          #$FF: //memLog.Lines.Add('Form been saved as BINary: ' + s);
            begin
              OutputStream := TMemoryStream.Create;
              try
                InputStream.Position := 0;
                ObjectResourceToText(InputStream, OutputStream);
                OutputStream.Position := 0;
                Lines.LoadFromStream(OutputStream);
              finally
                OutputStream.Free;
              end; // try/finally
            end;
        end;
      end;
    finally // wrap up
      InputStream.Free;
    end; // try/finally
  end
  else
    Lines.LoadFromFile(FullFileName);

  // For each line in the file...
  FastTimer.Start;
  for i := 1 to vleUnits.Strings.Count do // Iterate
  begin
    FromStr := UpperCase(SysUtils.StringReplace(vleUnits.Cells[0, i], cReplaceEquals, '=', [rfReplaceAll]));
    ToStr := SysUtils.StringReplace(vleUnits.Cells[1, i], cReplaceEquals, '=', [rfReplaceAll]);
    Result := Result + SearchReplace;
  end;

  FastTimer.Stop;
  ReplaceTime := FastTimer.ElapsedMicroseconds;

  if (Result > 0) and not Simulate then
  begin
    if Backup then
    begin
      BackupName := ChangeFileExt(FullFileName, '.BAK');
      if FileExists(BackupName) then
        DeleteFile(PChar(BackupName));
      RenameFile(FullFileName, BackupName);
    end;
    Lines.SaveToFile(FullFileName);
  end;
end;

function TfrmMain.FileNameReplace(var FileItem: TListItem): string;
var
  FilePath: string;
  FileName, NewFileName: string;
  FileExtension: string;
  FromStr, ToStr: string;
  i: integer;
begin
  FileName := ChangeFileExt(ExtractFileName(FileItem.Caption), '');
  FilePath := ExtractFilePath(FileItem.Caption);
  FileExtension := ExtractFileExt(FileItem.Caption);
  NewFileName := FileItem.Caption;

  for i := 1 to vleUnits.Strings.Count do // Iterate
  begin
    FromStr := UpperCase(vleUnits.Cells[0, i]);
    ToStr := vleUnits.Cells[1, i];
    if FAppOptions.WholeWords and (UpperCase(NewFileName) = FromStr) then
      NewFileName := FilePath + ToStr + FileExtension
    else if not FAppOptions.WholeWords then
      NewFileName := SysUtils.StringReplace(NewFileName, vleUnits.Cells[0, i], vleUnits.Cells[1, i], [rfReplaceAll,
        rfIgnoreCase]);
  end;

  if (NewFileName <> '') and not FAppOptions.Simulate then
  begin
    Lines.LoadFromFile(FileItem.Caption);
    if FAppOptions.Backup then
      Lines.SaveToFile(FilePath + FileName + '.~' + Copy(FileExtension, 2, Length(FileExtension) - 1));
    ForceDirectories(ExtractFilePath(NewFilename));
    if not RenameFile(FileItem.Caption, NewFileName) then
      raise Exception.CreateFmt('Unable to rename file "%s" to "%s".', [FileItem.Caption, NewFileName]);
    FileItem.Caption := NewFileName;
  end;
  Result := NewFileName;
end;

procedure TfrmMain.AddFiles(const FileName: string);
var
  //  Idx: Integer;
  Item: TListItem;
begin
  Item := SearchList.Items.Add;
  Item.Caption := FileName;
  Item.SubItems.Add('0');
  Item.SubItems.Add('Waiting');
  Item.SubItems.Add('-');
  Item.SubItems.Add('');
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  Idx: integer;
  //  Item: TListItem;
begin
  if OpenDialog.Execute then
  begin
    for Idx := 0 to OpenDialog.Files.Count - 1 do
      AddFiles(OpenDialog.Files[Idx]);
  end;
end;

procedure TfrmMain.btnRemoveClick(Sender: TObject);
var
  Idx: integer;
begin
  with sbStatus do
    SetStatus(['Removing files...', Panels[1].Text, Panels[2].Text]);
  // this is faster...
  for Idx := SearchList.Items.Count - 1 downto 0 do
    if SearchList.Items[Idx].Selected then
      SearchList.Items.Delete(Idx);
  with sbStatus do
    SetStatus(['Ready', Panels[1].Text, Panels[2].Text]);
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  Item: TListItem;
  FileNum, ReplaceCount: integer;
  ReplaceTime, TotalTime: TLargeInteger;
begin
  Screen.Cursor := crHourglass;
  with sbStatus do
    SetStatus(['Scanning...', Panels[1].Text, Panels[2].Text]);
  TotalTime := 0;
  try
    // Reset all items
    for FileNum := 0 to SearchList.Items.Count - 1 do
    begin
      Item := SearchList.Items[FileNum];
      Item.SubItems[0] := '0';
      Item.SubItems[1] := 'Waiting';
      Item.SubItems[2] := '-';
      Item.SubItems[3] := '';
      Item.Update;
    end;

    for FileNum := 0 to SearchList.Items.Count - 1 do
    begin
      Item := SearchList.Items[FileNum];
      Item.SubItems[1] := 'Busy';
      Item.Update;

      ReplaceCount := StringReplace(Item.Caption, FAppOptions.WholeWords, FAppOptions.Backup,
        FAppOptions.Simulate, ReplaceTime);
      Inc(TotalTime, ReplaceTime);
      Item.SubItems[0] := IntToStr(ReplaceCount);
      Item.SubItems[1] := 'Done';
      Item.SubItems[2] := IntToStr(ReplaceTime);
      if FAppOptions.ReplaceFileNames then
        Item.SubItems[3] := FileNameReplace(Item);
      Item.Update;
    end;
  finally // wrap up
    Screen.Cursor := crDefault;
    with sbStatus do
      SetStatus(['Ready', Panels[1].Text, Format('Total: %f secs', [TotalTime / 1000000])]);
  end; // try/finally
end;

procedure TfrmMain.SearchListDblClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := SearchList.Selected;
  if Item <> nil then
    WinExec(PChar('Notepad ' + Item.Caption), sw_Normal);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
  FAppOptions:= TAppOptions.Create(self);
  FAppOptions.AppStorage := JvAppIniFileStorage;
  FAppOptions.AppStoragePath := 'Settings';
  JvAppIniFileStorage.FileName := ChangeFileExt(Application.ExeName, '.ini');
  LoadSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, false);
  SaveSettings;
  FreeAndNil(FAppOptions);
end;

procedure TfrmMain.WMDropFiles(var Msg: TWMDropFiles);
{$IFDEF Windows}
const
  MAX_PATH = 255;
{$ENDIF}
var
  Pt: TPoint;
  Count, Loop: integer;
  Buf: array[0..MAX_PATH] of char;
begin
  try
    Msg.Result := 0;
    DragQueryPoint(Msg.Drop, Pt);
    Count := DragQueryFile(Msg.Drop, Cardinal(-1), Buf, sizeof(Buf));
    for Loop := 0 to Pred(Count) do
    begin
      DragQueryFile(Msg.Drop, Loop, Buf, sizeof(Buf));
      AddFiles(StrPas(Buf));
    end
  finally
    DragFinish(Msg.Drop)
  end

end;

procedure TfrmMain.btnAddLineClick(Sender: TObject);
begin
  vleUnits.InsertRow('', '', true);
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    FileName := ExtractFileName(fCurrentDataFile);
    Filter := 'Conversion files (*.dat)|*.dat';
    DefaultExt := 'dat';
    Options := Options + [ofOverWritePrompt];
    if Execute then { Display Open dialog box }
    begin
      fCurrentDataFile := FileName;
      vleUnits.Strings.SaveToFile(fCurrentDataFile);
      SaveData.Enabled := false;
    end;
  finally // wrap up
    Free;
  end; // try/finally
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
begin
  with vleUnits do
    if Strings.Count > 0 then
      DeleteRow(Row);
end;

procedure TfrmMain.AboutMeExecute(Sender: TObject);
begin
  with TfrmAboutMe.Create(nil) do
  try
    showModal;
  finally // wrap up
    Free;
  end; // try/finally
end;

procedure TfrmMain.LoadDATFile(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    vleUnits.Strings.LoadFromFile(FileName);
    fCurrentDataFile := FileName;
  end;
end;

procedure TfrmMain.OpenDataExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    FileName := fCurrentDataFile;
    Filter := 'Conversion files (*.dat)|*.dat';
    DefaultExt := 'dat';
    if Execute then { Display Open dialog box }
    begin
      LoadDATFile(FileName);
      Caption := 'JVCL Convert:  ' + ExtractFileName(FileName);
      JvPageControl.ActivePage := tbsStrings;
    end;
  finally // wrap up
    Free;
  end; // try/finally
end;

procedure TfrmMain.vleUnitsStringsChange(Sender: TObject);
begin
  SaveData.Enabled := true;
end;

procedure TfrmMain.ConvertUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := vleUnits.Strings.Count > 0;
end;

procedure TfrmMain.NewLineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvPageControl.ActivePage = tbsStrings;
end;

procedure TfrmMain.DeleteLineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (vleUnits.Strings.Count > 0) and (JvPageControl.ActivePage = tbsStrings);
end;

procedure TfrmMain.RemoveFilesUpdate(Sender: TObject);
begin
  RemoveFiles.Enabled := (SearchList.Selected <> nil) and (JvPageControl.ActivePage = tbsFiles);
end;

procedure TfrmMain.IterateSubdirectoriesExecute(Sender: TObject);
begin
  JvBrowseFolder1.Directory := ExcludeTrailingPathDelimiter(JvSearchFiles1.RootDirectory);
  if JvBrowseFolder1.Execute then
  begin
    JvSearchFiles1.RootDirectory := JvBrowseFolder1.Directory;
    if JvSearchFiles1.FileParams.FileMask = '' then
      JvSearchFiles1.FileParams.FileMask := '*.pas;*.dpr;*.dpk;*.dfm';
    JvSearchFiles1.Search;
  end;
end;

procedure TfrmMain.JvSearchFiles1FindFile(Sender: TObject; const AName: string);
begin
  AddFiles(AName);
end;

function SortFilename(lParam1, lParam2, lParamSort: integer): integer stdcall;
begin
  Result := AnsiCompareFilename(TListItem(lParam1).Caption, TListItem(lParam2).Caption);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortReplaceCount(lParam1, lParam2, lParamSort: integer): integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[0], 0) - StrToIntDef(TListItem(lParam2).SubItems[0], 0);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortStatus(lParam1, lParam2, lParamSort: integer): integer stdcall;
begin
  Result := AnsiCompareText(TListItem(lParam1).SubItems[1], TListItem(lParam2).SubItems[1]);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortMSecs(lParam1, lParam2, lParamSort: integer): integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[2], 0) - StrToIntDef(TListItem(lParam2).SubItems[2], 0);
  if lParamSort = 0 then
    Result := -Result;
end;

procedure TfrmMain.SortListColumn(LV: TListView; Column: TListColumn);
var
  i: integer;
  FDescending: boolean;
  SortFunc: TLVCompare;
begin
  FDescending := (Column.ImageIndex <= 1);
  for i := 0 to LV.Columns.Count - 1 do
    LV.Columns[i].ImageIndex := -1;
  case Column.Index of
    0: SortFunc := SortFilename;
    1: SortFunc := SortReplaceCount;
    2: SortFunc := SortStatus;
    3: SortFunc := SortMSecs;
  else
    SortFunc := nil;
  end;
  if Assigned(SortFunc) then
  begin
    LV.CustomSort(SortFunc, Ord(FDescending));
    Column.ImageIndex := Ord(FDescending) + 1;
  end;
end;

procedure TfrmMain.SearchListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortListColumn(SearchList, Column);
end;

procedure TfrmMain.LoadSettings;
begin
  with FAppOptions do
  begin
    LoadProperties;
    JvSearchFiles1.RootDirectory := RootDirectory;
    JvSearchFiles1.FileParams.FileMask := FileMask;
    fCurrentDataFile := DATFile;
  end;
  with sbStatus do
    SetStatus(['Ready', JvSearchFiles1.FileParams.FileMask, '']);
  LoadDATFile(fCurrentDataFile);
  Caption := 'JVCL Convert:  ' + ExtractFileName(fCurrentDataFile);
end;

procedure TfrmMain.SaveSettings;
begin
  with FAppOptions do
  begin
    RootDirectory := JvSearchFiles1.RootDirectory;
    FileMask := JvSearchFiles1.FileParams.FileMask;
    DATFile := fCurrentDataFile;
    StoreProperties;
  end;
end;

procedure TfrmMain.ActionList1Update(Action: TBasicAction;
  var Handled: boolean);
const
  cViewColor: array[boolean] of TColor = (clWindow, clBtnFace);
begin
  SearchList.Color := cViewColor[FAppOptions.Simulate];
end;

procedure TfrmMain.NewRepositoryExecute(Sender: TObject);
var
  i: integer;
begin
  btnSaveClick(Self);
  for i := vleUnits.Strings.Count downto 1 do
    vleUnits.DeleteRow(i);
  fCurrentDataFile := '';
end;

procedure TfrmMain.SearchListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Delete then
    if RemoveFiles.Enabled then btnRemoveClick(Self);
end;

procedure TfrmMain.SelectAllExecute(Sender: TObject);
begin
  SearchList.SelectAll;
end;

procedure TfrmMain.SelectAllUpdate(Sender: TObject);
begin
  SelectAll.Enabled := (SearchList.Items.Count > 0) and (JvPageControl.ActivePage = tbsFiles);
end;

procedure TfrmMain.SetStatus(const Msgs: array of string);
var
  i: integer;

  function Min(Val1, Val2: integer): integer;
  begin
    Result := Val1;
    if Val2 < Val1 then
      Result := Val2;
  end;
begin
  for i := 0 to sbStatus.Panels.Count - 1 do
  begin
    if i <= High(Msgs) then
      sbStatus.Panels[i].Text := PChar(Msgs[i])
    else
      sbStatus.Panels[i].Text := '';
  end;
  sbStatus.Update;
end;

procedure TfrmMain.OptionsExecute(Sender: TObject);
begin
  FAppOptions.FileMask := JvSearchFiles1.FileParams.FileMask;
  if TfrmOptions.Edit(FAppOptions) then
    JvSearchFiles1.FileParams.FileMask := FAppOptions.FileMask;
end;

procedure TfrmMain.FileMaskExecute(Sender: TObject);
var
  S: string;
begin
  S := JvSearchFiles1.FileParams.FileMask;
  if InputQuery('File Mask', 'Set new file mask:', S) and (S <> '') then
    JvSearchFiles1.FileParams.FileMask := S;
  with sbStatus do
    SetStatus([Panels[0].Text, JvSearchFiles1.FileParams.FileMask]);
end;

initialization
  Lines := TStringlist.Create;
finalization
  Lines.Free;
end.

