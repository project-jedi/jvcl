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

Contributor(s): Peter Thörnqvist <peter3@peter3.com>

Last Modified: 2002-07-18

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit fJvclConverterMain;

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Grids,
  ValEdit,
  JvComCtrls,
  Menus,
  ActnList,
  StdActns,
  ImgList,
  ToolWin,
  JvComponent,
  JvSearchFiles,
  JvBaseDlg,
  JvBrowseFolder;

type
  TfrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    SearchList: TListView;
    Panel1: TPanel;
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
    StandardImages: TImageList;
    FileExit1: TFileExit;
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
    mnuBackup: TMenuItem;
    mnuWholeWords: TMenuItem;
    NewLine: TAction;
    btnInsertNewLine: TToolButton;
    InsertLine1: TMenuItem;
    mnuReplaceFileNames: TMenuItem;
    JvSearchFiles1: TJvSearchFiles;
    IterateSubdirectories: TAction;
    btnIterateDir: TToolButton;
    IterateSubdirectories1: TMenuItem;
    mnuSimulate: TMenuItem;
    SortArrows: TImageList;
    JvBrowseFolder1: TJvBrowseFolder;
    N3: TMenuItem;
    mnuFileMask: TMenuItem;
    btnNewRepository: TToolButton;
    NewRepository: TAction;
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
    procedure mnuBackupClick(Sender: TObject);
    procedure vleUnitsStringsChange(Sender: TObject);
    procedure ConvertUpdate(Sender: TObject);
    procedure NewLineUpdate(Sender: TObject);
    procedure DeleteLineUpdate(Sender: TObject);
    procedure RemoveFilesUpdate(Sender: TObject);
    procedure mnuWholeWordsClick(Sender: TObject);
    procedure mnuReplaceFileNamesClick(Sender: TObject);
    procedure IterateSubdirectoriesExecute(Sender: TObject);
    procedure mnuSimulateClick(Sender: TObject);
    procedure SearchListColumnClick(Sender: TObject; Column: TListColumn);
    procedure mnuFileMaskClick(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure JvSearchFiles1FindFile(Sender: TObject; const AName: string);
    procedure NewRepositoryExecute(Sender: TObject);
  private
    { Private declarations }
    fCurrentDataFile: string;
    procedure AddFiles(const FileName: string);
    function StringReplace(const FullFileName: string; WholeWord, Backup, Simulate: Boolean; var ReplaceTime: TLargeInteger): Integer;
    procedure FileNameReplace(var FileItem: TListItem);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SortListColumn(LV: TListView; Column: TListColumn);
    procedure LoadDATFile(const FIlename: string);
  public
    { Public declarations }
    procedure WMDropFiles(var Msg: TWMDropFiles); message wm_DropFiles;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  ShellAPI,
  FastTime,
  fAboutMe,
  IniFiles,
  CommCtrl
  ;

const
  Allowed = (['a'..'z', 'A'..'Z', '0'..'9', '_']);

var
  Lines: TStringList; // create this once to speed things up

function TfrmMain.StringReplace(const FullFileName: string; WholeWord, Backup, Simulate: Boolean; var ReplaceTime: TLargeInteger): Integer;
var
  OldLine, NewLine, HiLine, BackupName: string;
  FromStr, ToStr: string;
  i: Integer;

  function SearchReplace: Integer;
  var
    LineNum, P: Integer;
  begin
    Result := 0;
    for LineNum := 0 to Lines.Count - 1 do
      begin
        OldLine := Lines[LineNum];
        NewLine := '';
        HiLine := UpperCase(OldLine);

        while True do
          begin
            P := Pos(FromStr, HiLine);
            if P <= 0 then
              break;
            if not WholeWord or (((P = 1) or (not (OldLine[P - 1] in Allowed))) and ((P + Length(FromStr) > Length(OldLine)) or (not (OldLine[P + Length(FromStr)] in Allowed)))) then
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

  // Let's just assume the filename exists - ok?
  Lines.LoadFromFile(FullFileName);
  // For each line in the file...
  FastTimer.Start;
  for I := 1 to vleUnits.Strings.Count do // Iterate
    begin
      FromStr := UpperCase(vleUnits.Cells[0, i]);
      ToStr := vleUnits.Cells[1, i];
      Result := Result + SearchReplace;
    end;

  FastTimer.Stop;
  ReplaceTime := FastTimer.ElapsedMicroseconds;

  if (Result > 0) and not Simulate then
    begin
      if Backup then
        begin
          BackUpName := ChangeFileExt(FullFileName, '.BAK');
          if FileExists(BackUpName) then
            DeleteFile(PChar(BackUpName));
          RenameFile(FullFileName, BackUpName);
        end;
      Lines.SaveToFile(FullFileName);
    end;
end;

procedure TfrmMain.FileNameReplace(var FileItem: TListItem);
var
  FilePath: string;
  FileName, NewFileName: string;
  FileExtension: string;
  FromStr, ToStr: string;
  i: Integer;
begin
  NewFileName := '';
  FileName := ChangeFileExt(ExtractFileName(FileItem.Caption), '');
  FilePath := ExtractFilePath(FileItem.Caption);
  FileExtension := ExtractFileExt(FileItem.Caption);

  for I := 1 to vleUnits.Strings.Count do // Iterate
    begin
      FromStr := UpperCase(vleUnits.Cells[0, i]);
      ToStr := vleUnits.Cells[1, i];
      if UpperCase(FileName) = FromStr then
        NewFileName := FilePath + ToStr + FileExtension;
    end;

  if NewFileName <> '' then
    begin
      Lines.LoadFromFile(FileItem.Caption);
      Lines.SaveToFile(FilePath + FileName + '.~' + copy(FileExtension, 2, length(FileExtension) - 1));

      if not RenameFile(FileItem.Caption, NewFileName) then
        raise Exception.Create('Unable to rename file.');

      FileItem.Caption := NewFileName;
    end;
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
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  Idx: Integer;
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
  Idx: Integer;
begin
  while SearchList.Selected <> nil do
    begin
      Idx := SearchList.Items.IndexOf(SearchList.Selected);
      SearchList.Items.Delete(Idx);
    end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  Item: TListItem;
  FileNum, ReplaceCount: Integer;
  ReplaceTime: TLargeInteger;
begin
  Screen.Cursor := crHourglass;

  try
    // Reset all items
    for FileNum := 0 to SearchList.Items.Count - 1 do
      begin
        Item := SearchList.Items[FileNum];
        Item.SubItems[0] := '0';
        Item.SubItems[1] := 'Waiting';
        Item.SubItems[2] := '-';
        if mnuReplaceFileNames.Checked then
          FileNameReplace(Item);
        Item.Update;
      end;

    for FileNum := 0 to SearchList.Items.Count - 1 do
      begin
        Item := SearchList.Items[FileNum];
        Item.SubItems[1] := 'Busy';
        Item.Update;

        ReplaceCount := StringReplace(Item.Caption, mnuWholeWords.Checked, mnuBackup.Checked, mnuSimulate.Checked, ReplaceTime);

        Item.SubItems[0] := IntToStr(ReplaceCount);
        Item.SubItems[1] := 'Done';
        Item.SubItems[2] := IntToStr(ReplaceTime);
        Item.Update;
      end;
  finally // wrap up
    Screen.Cursor := crDefault;
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
  DragAcceptFiles(Handle, True);
  LoadSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
  SaveSettings;
end;

procedure TfrmMain.WMDropFiles(var Msg: TWMDropFiles);
{$IFDEF Windows}
const
  MAX_PATH = 255;
  {$ENDIF}
var
  Pt: TPoint;
  Count, Loop: Integer;
  Buf: array[0..MAX_PATH] of Char;
begin
  try
    Msg.Result := 0;
    DragQueryPoint(Msg.Drop, Pt);
    Count := DragQueryFile(Msg.Drop, Cardinal(-1), Buf, SizeOf(Buf));
    for Loop := 0 to Pred(Count) do
      begin
        DragQueryFile(Msg.Drop, Loop, Buf, SizeOf(Buf));
        AddFiles(StrPas(Buf));
      end
  finally
    DragFinish(Msg.Drop)
  end

end;

procedure TfrmMain.btnAddLineClick(Sender: TObject);
begin
  vleUnits.InsertRow('', '', True);
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
    try
      Filename := ExtractFileName(fCurrentDataFile);
      Filter := 'Conversion files (*.dat)|*.dat';
      DefaultExt := 'dat';
      Options := Options + [ofOverWritePrompt];
      if Execute then { Display Open dialog box }
        begin
          fCurrentDataFile := FileName;
          vleUnits.Strings.SaveToFile(fCurrentDataFile);
          SaveData.Enabled := False;
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

procedure TfrmMain.LoadDATFile(const Filename: string);
begin
  if FileExists(Filename) then
    begin
      vleUnits.Strings.LoadFromFile(Filename);
      fCurrentDataFile := Filename;
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
          LoadDATFile(Filename);
          JvPageControl.ActivePage := tbsStrings;
        end;
    finally // wrap up
      Free;
    end; // try/finally
end;

procedure TfrmMain.mnuBackupClick(Sender: TObject);
begin
  mnuBackup.Checked := not mnuBackup.Checked;
end;

procedure TfrmMain.mnuWholeWordsClick(Sender: TObject);
begin
  mnuWholeWords.Checked := not mnuWholeWords.Checked;
end;

procedure TfrmMain.vleUnitsStringsChange(Sender: TObject);
begin
  SaveData.Enabled := True;
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

procedure TfrmMain.mnuReplaceFileNamesClick(Sender: TObject);
begin
  mnuReplaceFileNames.Checked := not mnuReplaceFileNames.Checked;
end;

procedure TfrmMain.IterateSubdirectoriesExecute(Sender: TObject);
begin
  JvBrowseFolder1.Directory := ExcludeTrailingPathDelimiter(JvSearchFiles1.RootDirectory);
  if JvBrowseFolder1.Execute then
    begin
      JvSearchFiles1.RootDirectory := JvBrowseFolder1.Directory;
      if JvSearchFiles1.SearchParams.FileMask = '' then
        JvSearchFiles1.SearchParams.FileMask := '*.pas;*.dpr;*.dpk';
      JvSearchFiles1.Search;
    end;
end;

procedure TfrmMain.JvSearchFiles1FindFile(Sender: TObject; const AName: string);
begin
  AddFiles(AName);
end;

procedure TfrmMain.mnuSimulateClick(Sender: TObject);
begin
  mnuSimulate.Checked := not mnuSimulate.Checked;
end;

function SortFilename(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := AnsiCompareFilename(TListItem(lParam1).Caption, TListItem(lParam2).Caption);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortReplaceCount(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[0], 0) - StrToIntDef(TListItem(lParam2).SubItems[0], 0);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortStatus(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := AnsiCompareText(TListItem(lParam1).SubItems[1], TListItem(lParam2).SubItems[1]);
  if lParamSort = 0 then
    Result := -Result;
end;

function SortMSecs(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[2], 0) - StrToIntDef(TListItem(lParam2).SubItems[2], 0);
  if lParamSort = 0 then
    Result := -Result;
end;

procedure TfrmMain.SortListColumn(LV: TListView; Column: TListColumn);
var
  i: integer; FDescending: boolean; SortFunc: TLVCompare;
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
  try
    with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
      try
        JvSearchFiles1.RootDirectory := ReadString('Settings', 'Path', '');
        JvSearchFiles1.SearchParams.FileMask := ReadString('Settings', 'Mask', '*.dpr;*.dpk;*.pas');
        fCurrentDataFile := ReadString('Settings', 'DATFile', '');
        mnuBackup.Checked := ReadBool('Settings', 'Backup', true);
        mnuWholeWords.Checked := ReadBool('Settings', 'WholeWords', true);
        mnuReplaceFileNames.Checked := ReadBool('Settings', 'ReaplceFileNames', true);
        mnuSimulate.Checked := ReadBool('Settings', 'Simulate', false);
      finally
        Free;
      end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  LoadDATFile(fCurrentDataFile);
end;

procedure TfrmMain.SaveSettings;
begin
  try
    with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
      try
        WriteString('Settings', 'Path', JvSearchFiles1.RootDirectory);
        WriteString('Settings', 'Mask', JvSearchFiles1.SearchParams.FileMask);
        WriteString('Settings', 'DATFile', fCurrentDataFile);
        WriteBool('Settings', 'Backup', mnuBackup.Checked);
        WriteBool('Settings', 'WholeWords', mnuWholeWords.Checked);
        WriteBool('Settings', 'ReaplceFileNames', mnuReplaceFileNames.Checked);
        WriteBool('Settings', 'Simulate', mnuSimulate.Checked);
      finally
        Free;
      end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmMain.mnuFileMaskClick(Sender: TObject);
var
  S: string;
begin
  S := JvSearchFiles1.SearchParams.FileMask;
  if InputQuery('File Mask', 'Set new file mask:', S) and (S <> '') then
    JvSearchFiles1.SearchParams.FileMask := S;
end;

procedure TfrmMain.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
const
  cViewColor: array[boolean] of TColor = (clWindow, clBtnFace);
begin
  SearchList.Color := cViewColor[mnuSimulate.Checked];
end;

procedure TfrmMain.NewRepositoryExecute(Sender: TObject);
var
  i: integer;
begin
  btnSaveClick(self);
  for i := vleUnits.Strings.Count downto 1 do
    vleUnits.DeleteRow(i);
  fCurrentDataFile := '';
end;

initialization
  Lines := TStringList.Create;
finalization
  Lines.Free;

end.

