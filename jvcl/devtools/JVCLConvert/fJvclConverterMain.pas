{ -----------------------------------------------------------------------------
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
  located at http://jvcl.delphi-jedi.org

  Known Issues:
  ----------------------------------------------------------------------------- }

unit fJvclConverterMain;

{$I jvcl.inc}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Grids, JvComCtrls,
  Menus, ActnList, StdActns, ImgList, ToolWin, JvComponent, JvSearchFiles, JvBaseDlg, JvBrowseFolder, JVCLConvertUtils,
  JvFormPlacement, JvAppStorage, JvAppIniStorage, JvDialogs, JvComponentBase, JvExGrids, JvStringGrid;

type

  TfrmMain = class(TForm)
    AboutMe: TAction;
    AboutMe1: TMenuItem;
    ActionList1: TActionList;
    btnAboutMe: TToolButton;
    btnAddFiles: TToolButton;
    btnConvert: TToolButton;
    btnDeleteFiles: TToolButton;
    btnDeleteLine: TToolButton;
    btnExit: TToolButton;
    btnInsertNewLine: TToolButton;
    btnIterateDir: TToolButton;
    btnNewRepository: TToolButton;
    btnOpenRepository: TToolButton;
    btnSaveRepository: TToolButton;
    Conversion1: TMenuItem;
    Convert: TAction;
    Convert1: TMenuItem;
    DeleteLine: TAction;
    DeleteLine1: TMenuItem;
    Edit: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    FileExit: TFileExit;
    FileMask: TAction;
    FileOpen: TAction;
    Help1: TMenuItem;
    InsertLine1: TMenuItem;
    IterateSubdirectories: TAction;
    IterateSubdirectories1: TMenuItem;
    JvAppIniFileStorage: TJvAppIniFileStorage;
    JvBrowseFolder1: TJvBrowseForFolderDialog;
    JvFormStorage: TJvFormStorage;
    JvPageControl: TPageControl;
    JvSearchFiles1: TJvSearchFiles;
    MainMenu1: TMainMenu;
    mnuFileMask: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    NewLine: TAction;
    NewRepository: TAction;
    OpenData: TAction;
    OpenDialog: TJvOpenDialog;
    OpenFilestoConvert1: TMenuItem;
    OpenReplacementFile1: TMenuItem;
    Options: TAction;
    Options1: TMenuItem;
    Options2: TMenuItem;
    Remove1: TMenuItem;
    RemoveFiles: TAction;
    ReplaceListGrid: TJvStringGrid;
    SaveConversionData1: TMenuItem;
    SaveData: TAction;
    sbStatus: TStatusBar;
    SearchList: TListView;
    SelectAll: TAction;
    SelectAll1: TMenuItem;
    SortArrows: TImageList;
    StandardImages: TImageList;
    tbsFiles: TTabSheet;
    tbsStrings: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure AboutMeExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddLineClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure ConvertUpdate(Sender: TObject);
    procedure DeleteLineUpdate(Sender: TObject);
    procedure FileMaskExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IterateSubdirectoriesExecute(Sender: TObject);
    procedure JvSearchFiles1FindFile(Sender: TObject; const AName: string);
    procedure NewLineUpdate(Sender: TObject);
    procedure NewRepositoryExecute(Sender: TObject);
    procedure OpenDataExecute(Sender: TObject);
    procedure OptionsExecute(Sender: TObject);
    procedure RemoveFilesUpdate(Sender: TObject);
    procedure ReplaceListGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure SaveDataUpdate(Sender: TObject);
    procedure SearchListColumnClick(Sender: TObject; Column: TListColumn);
    procedure SearchListDblClick(Sender: TObject);
    procedure SearchListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
  private
    FAppOptions: TAppOptions;
    FConvertDefinitions: TConvertDefinitionList;
    { Private declarations }
    fCurrentDefinitionFile: string;
    procedure AddFiles(const FileName: string);
    function FileNameReplace(var FileItem: TListItem): string;
    function FileStringReplace(const FullFileName: string; Backup, Simulate, WholeWord, IgnoreInsideString, IgnoreInsideComments:
      Boolean; var ReplaceTime: TLargeInteger): Integer;
    procedure LoadDATFile(const FileName: string);
    procedure LoadDefinitionFile(const FileName: string);
    procedure LoadSettings;
    procedure LoadXMLFile(const FileName: string);
    procedure MoveReplaceListGridRow(Row, Value: Integer);
    procedure SaveDATFile(const FileName: string);
    procedure SaveDefinitionFile(const FileName: string);
    procedure SaveSettings;
    procedure SaveXMLFile(const FileName: string);
    procedure SetStatus(const Msgs: array of string);
    procedure SortListColumn(LV: TListView; Column: TListColumn);
  public
    { Public declarations }
    procedure WMDropFiles(var Msg: TWMDropFiles); message wm_DropFiles;
    property ConvertDefinitions: TConvertDefinitionList read FConvertDefinitions;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  ShellAPI, FastTime, fAboutMe, CommCtrl, OptionsFrm,
  JvPropertyStore, JvAppXMLStorage;

const
  Allowed = (['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_']);
  cReplaceEquals = #2;

var
  Lines: TStringlist; // create this once to speed things up
  ch: char;

function SortFilename(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := AnsiCompareFilename(TListItem(lParam1).Caption, TListItem(lParam2).Caption);
  if lParamSort = 0 then
    Result := - Result;
end;

function SortReplaceCount(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[0], 0) - StrToIntDef(TListItem(lParam2).SubItems[0], 0);
  if lParamSort = 0 then
    Result := - Result;
end;

function SortStatus(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := AnsiCompareText(TListItem(lParam1).SubItems[1], TListItem(lParam2).SubItems[1]);
  if lParamSort = 0 then
    Result := - Result;
end;

function SortMSecs(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
begin
  Result := StrToIntDef(TListItem(lParam1).SubItems[2], 0) - StrToIntDef(TListItem(lParam2).SubItems[2], 0);
  if lParamSort = 0 then
    Result := - Result;
end;

procedure TfrmMain.AboutMeExecute(Sender: TObject);
var
  Frm: TfrmAboutMe;
begin
  Frm := TfrmAboutMe.Create(nil);
  try
    Frm.showModal;
  finally // wrap up
    Frm.Free;
  end; // try/finally
end;

procedure TfrmMain.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
const
  cViewColor: array [Boolean] of TColor = (clWindow, clBtnFace);
begin
  SearchList.Color := cViewColor[FAppOptions.Simulate];
end;

procedure TfrmMain.AddFiles(const FileName: string);
var
  // Idx: Integer;
  Item: TListItem;
begin
  Item := SearchList.Items.Add;
  Item.Caption := FileName;
  Item.SubItems.Add(Format('%6d', [0]));
  Item.SubItems.Add('Waiting');
  Item.SubItems.Add('-');
  Item.SubItems.Add('');
  Item.SubItems.Add('');
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var
  Idx: Integer;
begin
  if OpenDialog.Execute then
  begin
    for Idx := 0 to OpenDialog.Files.Count - 1 do
      AddFiles(OpenDialog.Files[Idx]);
  end;
end;

procedure TfrmMain.btnAddLineClick(Sender: TObject);
var
  i: Integer;
  r: Integer;
begin
  r := ReplaceListGrid.Row;
  for i := ReplaceListGrid.RowCount - 1 downto r do
    MoveReplaceListGridRow(i, 1);
  for i := 0 to ReplaceListGrid.ColCount - 1 do
    ReplaceListGrid.Cells[i, r] := '';
  ReplaceListGrid.Cells[0, r] := Format('%5d', [r]);
  ReplaceListGrid.RowCount := ReplaceListGrid.RowCount + 1;
  ReplaceListGrid.RowHeights[ReplaceListGrid.RowCount - 1] := 18;
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if (ReplaceListGrid.Row >= 1) and (ReplaceListGrid.Row < ReplaceListGrid.RowCount) then
    for i := ReplaceListGrid.Row + 1 to ReplaceListGrid.RowCount - 1 do
      MoveReplaceListGridRow(i, - 1);
  if ReplaceListGrid.RowCount <= 2 then
    ReplaceListGrid.RowCount := 2
  else
    ReplaceListGrid.RowCount := ReplaceListGrid.RowCount - 1;
end;

procedure TfrmMain.btnRemoveClick(Sender: TObject);
var
  Idx: Integer;
begin
  SetStatus(['Removing files...', sbStatus.Panels[1].Text, sbStatus.Panels[2].Text]);
  // this is faster...
  for Idx := SearchList.Items.Count - 1 downto 0 do
    if SearchList.Items[Idx].Selected then
      SearchList.Items.Delete(Idx);
  SetStatus(['Ready', sbStatus.Panels[1].Text, sbStatus.Panels[2].Text]);
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.FileName := ExtractFileName(fCurrentDefinitionFile);
    Dlg.Filter := 'Easy conversion files (*.dat;*.xml)|*.dat;*.xml';
    Dlg.DefaultExt := 'xml';
    Dlg.Options := Dlg.Options + [ofOverWritePrompt];
    if Dlg.Execute then { Display Open dialog box }
      SaveDefinitionFile(Dlg.FileName);
  finally // wrap up
    Dlg.Free;
  end; // try/finally
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var
  Item: TListItem;
  FileNum, ReplaceCount: Integer;
  ReplaceTime, TotalTime: TLargeInteger;
begin
  Screen.Cursor := crHourglass;
  SetStatus(['Scanning...', sbStatus.Panels[1].Text, sbStatus.Panels[2].Text]);
  TotalTime := 0;
  try
    // Reset all items
    for FileNum := 0 to SearchList.Items.Count - 1 do
    begin
      Item := SearchList.Items[FileNum];
      Item.SubItems[0] := Format('%6d', [0]);
      Item.SubItems[1] := 'Waiting';
      Item.SubItems[2] := '-';
      Item.SubItems[3] := '';
      Item.SubItems[4] := '';
      Item.Update;
    end;
    Application.ProcessMessages;

    ConvertDefinitions.FillFromStringGrid(ReplaceListGrid);
    for FileNum := 0 to SearchList.Items.Count - 1 do
    begin
      Item := SearchList.Items[FileNum];
      Item.SubItems[1] := 'Busy';
      Item.Update;
      SetStatus([sbStatus.Panels[0].Text, sbStatus.Panels[1].Text,
        Format('%s (%d/%d)', [Item.Caption, FileNum + 1, SearchList.Items.Count])]);
      Application.ProcessMessages;

      ReplaceCount := FileStringReplace(Item.Caption, FAppOptions.Backup, FAppOptions.Simulate, FAppOptions.WholeWords,
        FAppOptions.IgnoreInsideStrings, FAppOptions.IgnoreInsideComments, ReplaceTime);
      Inc(TotalTime, ReplaceTime);
      Item.SubItems[0] := Format('%6d', [ReplaceCount]);
      Item.SubItems[1] := 'Done';
      Item.SubItems[2] := Format('%8d', [ReplaceTime]);
      Item.SubItems[3] := Format('%6d', [Lines.Count]);
      if FAppOptions.ReplaceFileNames then
        Item.SubItems[4] := FileNameReplace(Item);
      Item.Update;
      Application.ProcessMessages;
    end;
  finally // wrap up
    Screen.Cursor := crDefault;
    SetStatus(['Ready', sbStatus.Panels[1].Text, Format('Total: %f secs', [TotalTime / 1000000])]);
  end; // try/finally
end;

procedure TfrmMain.ConvertUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (ReplaceListGrid.RowCount > 1) and (SearchList.Items.Count > 0);
end;

procedure TfrmMain.DeleteLineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (ReplaceListGrid.RowCount > 1) and (JvPageControl.ActivePage = tbsStrings);
end;

procedure TfrmMain.FileMaskExecute(Sender: TObject);
var
  S: string;
begin
  S := JvSearchFiles1.FileParams.FileMask;
  if InputQuery('File Mask', 'Set new file mask:', S) and (S <> '') then
    JvSearchFiles1.FileParams.FileMask := S;
  SetStatus([sbStatus.Panels[0].Text, JvSearchFiles1.FileParams.FileMask]);
end;

function TfrmMain.FileNameReplace(var FileItem: TListItem): string;
var
  FilePath: string;
  FileName, NewFileName: string;
  FileExtension: string;
  FromStr, ToStr: string;
  i: Integer;
begin
  FileName := ChangeFileExt(ExtractFileName(FileItem.Caption), '');
  FilePath := ExtractFilePath(FileItem.Caption);
  FileExtension := ExtractFileExt(FileItem.Caption);
  NewFileName := FileItem.Caption;

  for i := 0 to ConvertDefinitions.Count - 1 do // Iterate
  begin
    FromStr := ConvertDefinitions.Definition[i].ReplaceFrom;
    ToStr := ConvertDefinitions.Definition[i].ReplaceFrom;
    if FAppOptions.WholeWords and (UpperCase(NewFileName) = FromStr) then
      NewFileName := FilePath + ToStr + FileExtension
    else
      if not FAppOptions.WholeWords then
        NewFileName := StringReplace(NewFileName, FromStr, ToStr, [rfReplaceAll, rfIgnoreCase]);
  end;

  if (NewFileName <> '') and not FAppOptions.Simulate then
  begin
    Lines.LoadFromFile(FileItem.Caption);
    if FAppOptions.Backup then
      Lines.SaveToFile(FilePath + FileName + '.~' + Copy(FileExtension, 2, Length(FileExtension) - 1));
    ForceDirectories(ExtractFilePath(NewFileName));
    if not RenameFile(FileItem.Caption, NewFileName) then
      raise Exception.CreateFmt('Unable to rename file "%s" to "%s".', [FileItem.Caption, NewFileName]);
    FileItem.Caption := NewFileName;
  end;
  Result := NewFileName;
end;

{ TfrmMain }

function TfrmMain.FileStringReplace(const FullFileName: string; Backup, Simulate, WholeWord, IgnoreInsideString,
  IgnoreInsideComments: Boolean; var ReplaceTime: TLargeInteger): Integer;
var
  OldLine, NewLine, HiLine, BackupName: string;
  FromStr, ToStr: string;
  FromStrLength: Integer;
  OldLineLength: Integer;
  UpperCaseReplace: Boolean;
  i: Integer;
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  PSFound, PLFound: Boolean; // Optimization Variables to remember if the PS or PL is found once inside the line
  intWholeWord, intIgnoreInsideString, intIgnoreInsideComments: Boolean;
  Def: TConvertDefinition;
  LineComment: string;
  Comment1Start,
    Comment1End,
    Comment2Start,
    Comment2End,
    StringBeginEnd: string;
  Comment1Length: Integer;
  Comment2Length: Integer;
  FileExt: string;

  function SearchReplace: Integer;
  var
    LineNum, P, PS, PL, PC1, PC2: Integer;
    Comment1Active: Boolean;
    Comment2Active: Boolean;

    procedure GetPositions;
    begin
      if intIgnoreInsideComments then
      begin
        if Comment1Active then
        begin
          PC1 := Pos(Comment1End, HiLine);
          Exit;
        end
        else
          if Comment2Active then
          begin
            PC2 := Pos(Comment2End, HiLine);
            Exit;
          end
          else
          begin
            PC1 := Pos(Comment1Start, HiLine);
            PC2 := Pos(Comment2Start, HiLine);
            if (PC1 > 0) and (PC2 > 0) then
              if (PC1 < PC2) then
                PC2 := 0
              else
                PC1 := 0;
          end;
      end;
      if not Comment1Active and not Comment2Active then
        P := Pos(FromStr, HiLine)
      else
        P := 0;
      if intIgnoreInsideComments then
      begin
        if not Comment1Active and not Comment2Active then
          if (P > 0) and PLFound then
            PL := Pos(LineComment, HiLine)
          else
          begin
            PL := 0;
            PLFound := False;
          end;
      end;
      if intIgnoreInsideString then
      begin
        if (P > 0) and PSFound then
          PS := Pos(StringBeginEnd, HiLine)
        else
        begin
          PS := 0;
          PSFound := False;
        end;
      end;
    end;

    procedure DeleteLines(ToPosition: Integer);
    begin
      Delete(OldLine, 1, ToPosition);
      OldLineLength := OldLineLength - ToPosition;
      Delete(HiLine, 1, ToPosition);
    end;

  begin
    Result := 0;
    if Copy(FromStr, 1, 1) = ';' then
      Exit;
    for LineNum := 0 to Lines.Count - 1 do
    begin
      OldLine := Lines[LineNum];
      OldLineLength := Length(OldLine);
      NewLine := '';
      HiLine := UpperCase(OldLine);
      PSFound := True;
      PLFound := True;

      while True do
      begin
        GetPositions;
        if Comment1Active then
          if (PC1 > 0) then
          begin
            NewLine := NewLine + Copy(OldLine, 1, PC1 + Comment1Length - 1);
            DeleteLines(PC1 + Comment1Length - 1);
            Comment1Active := False;
            Continue;
          end
          else
            break
        else
          if Comment2Active then
            if (PC2 > 0) then
            begin
              NewLine := NewLine + Copy(OldLine, 1, PC2 + Comment2Length - 1);
              DeleteLines(PC2 + Comment2Length - 1);
              Comment2Active := False;
              Continue;
            end
            else
              break
          else
          begin
            if (PC1 > 0) and ((PC1 < PS) or (PS = 0)) and ((PC1 < PL) or (PL = 0)) and ((PC1 < P) or (P = 0)) then
            begin
              NewLine := NewLine + Copy(OldLine, 1, PC1 + Comment1Length - 1);
              DeleteLines(PC1 + Comment1Length - 1);
              Comment1Active := True;
              Continue;
            end
            else
              if (PC2 > 0) and ((PC2 < PS) or (PS = 0)) and ((PC2 < PL) or (PL = 0)) and ((PC2 < P) or (P = 0)) then
              begin
                NewLine := NewLine + Copy(OldLine, 1, PC2 + Comment2Length - 1);
                DeleteLines(PC2 + Comment2Length - 1);
                Comment2Active := True;
                Continue;
              end;
          end;
        while intIgnoreInsideString and (P > PS) and (PS > 0) and ((PS < PL) or (PL = 0)) do
        begin
          NewLine := NewLine + Copy(OldLine, 1, PS);
          DeleteLines(PS);
          PS := Pos('''', HiLine);
          if PS > 0 then
          begin
            NewLine := NewLine + Copy(OldLine, 1, PS);
            DeleteLines(PS);
          end;
          GetPositions;
        end;
        if (P <= 0) or ((P > PL) and (PL > 0)) then
          break;
        if not intWholeWord or (((P = 1) or ( not (OldLine[P - 1] in Allowed))) and ((P + FromStrLength > OldLineLength)
          or ( not (OldLine[P + FromStrLength] in Allowed)))) then
        begin
          if not UpperCaseReplace or (Copy(OldLine, P, FromStrLength) <> ToStr) then
            Inc(Result);
          NewLine := NewLine + Copy(OldLine, 1, P - 1) + ToStr;
        end
        else
          NewLine := NewLine + Copy(OldLine, 1, P + FromStrLength - 1);
        DeleteLines(P + FromStrLength - 1);
      end;

      Lines.Strings[LineNum] := NewLine + OldLine;
    end;
  end;

begin
  Result := 0;
  if not FileExists(FullFileName) then
    Exit;
  // if this is DFM file, convert it to text
  FileExt := UpperCase(ExtractFileExt(FullFileName));
  if FileExt = '.DFM' then
  begin
    InputStream := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
    try
      begin
        // if Size < 2 then memLog.Lines.Add('File size ' + IntToStr(Size) + ' for form ' + s);
        ch := #0;
        InputStream.Read(ch, 1);
        case ch of
          'o', 'O', 'i', 'I':
            Lines.LoadFromFile(FullFileName); // ok - text
          #$FF: // memLog.Lines.Add('Form been saved as BINary: ' + s);
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

  if FileExt = '.PAS' then
  begin
    LineComment := '//';
    Comment1Start := '{';
    Comment1End := '}';
    Comment2Start := '(*';
    Comment2End := '*)';
    Comment1Length := 1;
    Comment2Length := 2;
    StringBeginEnd := '''';
  end
  else
    if FileExt = '.DFM' then
    begin
      LineComment := '';
      Comment1Start := '{';
      Comment1End := '}';
      Comment2Start := '';
      Comment2End := '';
      Comment1Length := 1;
      Comment2Length := 0;
      StringBeginEnd := '''';
    end
    else
      if (FileExt = '.C') or (FileExt = '.CS') or (FileExt = '.CPP') or (FileExt = '.H') or (FileExt = '.HPP') then
      begin
        LineComment := '//';
        Comment1Start := '/*';
        Comment1End := '*/';
        Comment2Start := '';
        Comment2End := '';
        Comment1Length := 2;
        Comment2Length := 0;
        StringBeginEnd := '"';
      end
      else
        if FileExt = '.SQL' then
        begin
          LineComment := '--';
          Comment1Start := '/*';
          Comment1End := '*/';
          Comment2Start := '';
          Comment2End := '';
          Comment1Length := 2;
          Comment2Length := 0;
          StringBeginEnd := '''';
        end
        else
        begin
          LineComment := '';
          Comment1Start := '';
          Comment1End := '';
          Comment2Start := '';
          Comment2End := '';
          Comment1Length := 0;
          Comment2Length := 0;
          StringBeginEnd := '';
        end;

  // For each line in the file...
  FastTimer.Start;
  for i := 0 to ConvertDefinitions.Count - 1 do
  begin
    Def := ConvertDefinitions.Definition[i];
    FromStr := UpperCase(Def.ReplaceFrom);
    FromStrLength := Length(FromStr);
    ToStr := Def.ReplaceTo;
    UpperCaseReplace := (FromStr = UpperCase(ToStr));
    intWholeWord := Def.ToBoolean(Def.WholeWord, WholeWord);
    intIgnoreInsideString := Def.ToBoolean(Def.IgnoreInsideStrings, IgnoreInsideString) and (StringBeginEnd <> '');
    intIgnoreInsideComments := Def.ToBoolean(Def.IgnoreInsideComments, IgnoreInsideComments) and (Comment1Length > 0);
    Result := Result + SearchReplace;
  end;

  FastTimer.Stop;
  ReplaceTime := FastTimer.ElapsedMicroseconds;

  if (Result > 0) then
    if not Simulate then
    begin
      if Backup then
      begin
        BackupName := ChangeFileExt(FullFileName, '.BAK');
        if FileExists(BackupName) then
          DeleteFile(PChar(BackupName));
        RenameFile(FullFileName, BackupName);
      end;
      Lines.SaveToFile(FullFileName);
    end
    else
      Lines.SaveToFile(ChangeFileExt(FullFileName, '.SIM'));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  FAppOptions := TAppOptions.Create(self);
  FAppOptions.AppStorage := JvAppIniFileStorage;
  FAppOptions.AppStoragePath := 'Settings';
  JvAppIniFileStorage.FileName := ChangeFileExt(Application.ExeName, '.ini');
  FConvertDefinitions := TConvertDefinitionList.Create(self);
  LoadSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FConvertDefinitions);
  DragAcceptFiles(Handle, False);
  SaveSettings;
  FreeAndNil(FAppOptions);
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

procedure TfrmMain.LoadDATFile(const FileName: string);
var
  sl: TStringlist;
begin
  if FileExists(FileName) then
  begin
    sl := TStringlist.Create;
    try
      sl.LoadFromFile(FileName);
      ConvertDefinitions.FillFromStringList(sl);
    finally
      sl.Free;
    end;
  end
  else
    ConvertDefinitions.clear;
end;

procedure TfrmMain.LoadDefinitionFile(const FileName: string);
begin
  fCurrentDefinitionFile := FileName;
  Caption := 'JVCL Convert:  ' + ExtractFileName(FileName);
  if not FileExists(FileName) then
  begin
    ConvertDefinitions.clear;
  end
  else
  begin
    if UpperCase(ExtractFileExt(FileName)) = '.XML' then
      LoadXMLFile(FileName)
    else
      LoadDATFile(FileName);
  end;
  ConvertDefinitions.FillToStringGrid(ReplaceListGrid);
end;

procedure TfrmMain.LoadSettings;
begin
  FAppOptions.LoadProperties;
  JvSearchFiles1.RootDirectory := FAppOptions.RootDirectory;
  JvSearchFiles1.FileParams.FileMask := FAppOptions.FileMask;
  LoadDefinitionFile(FAppOptions.DefinitionFile);
  SetStatus(['Ready', JvSearchFiles1.FileParams.FileMask, '']);
  Caption := 'JVCL Convert:  ' + ExtractFileName(fCurrentDefinitionFile);
end;

procedure TfrmMain.LoadXMLFile(const FileName: string);
begin
  LoadPropertyStoreFromXmlFile(ConvertDefinitions, FileName);
end;

procedure TfrmMain.MoveReplaceListGridRow(Row, Value: Integer);
var
  c: Integer;
begin
  for c := 1 to ReplaceListGrid.ColCount - 1 do
    ReplaceListGrid.Cells[c, Row + Value] := ReplaceListGrid.Cells[c, Row];
  ReplaceListGrid.Cells[0, Row + Value] := Format('%5d', [Row + Value]);
end;

procedure TfrmMain.NewLineUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := JvPageControl.ActivePage = tbsStrings;
end;

procedure TfrmMain.NewRepositoryExecute(Sender: TObject);
var
  i: Integer;
begin
  btnSaveClick(self);
  ConvertDefinitions.clear;
  ConvertDefinitions.FillToStringGrid(ReplaceListGrid);
  fCurrentDefinitionFile := '';
end;

procedure TfrmMain.OpenDataExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.FileName := fCurrentDefinitionFile;
    Dlg.Filter := 'Easy conversion files (*.dat;*.xml)|*.dat;*.xml';
    Dlg.DefaultExt := 'xml';
    if Dlg.Execute then { Display Open dialog box }
    begin
      LoadDefinitionFile(Dlg.FileName);
    end;
  finally // wrap up
    Dlg.Free;
  end; // try/finally
end;

procedure TfrmMain.OptionsExecute(Sender: TObject);
begin
  FAppOptions.FileMask := JvSearchFiles1.FileParams.FileMask;
  if TfrmOptions.Edit(FAppOptions) then
    JvSearchFiles1.FileParams.FileMask := FAppOptions.FileMask;
end;

procedure TfrmMain.RemoveFilesUpdate(Sender: TObject);
begin
  RemoveFiles.Enabled := (SearchList.Selected <> nil) and (JvPageControl.ActivePage = tbsFiles);
end;

procedure TfrmMain.ReplaceListGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  Def: TConvertDefinition;
begin
  Def := ConvertDefinitions.Definition[ARow];
  if not Assigned(Def) then
    Exit;
  if ACol = 3 then
  begin
    Def.WholeWordString := Value;
    ReplaceListGrid.Cells[ACol, ARow] := Def.WholeWordString;
  end
  else
    if ACol = 4 then
    begin
      Def.IgnoreInsideStringsString := Value;
      ReplaceListGrid.Cells[ACol, ARow] := Def.IgnoreInsideStringsString;
    end
    else
      if ACol = 5 then
      begin
        Def.IgnoreInsideCommentsString := Value;
        ReplaceListGrid.Cells[ACol, ARow] := Def.IgnoreInsideCommentsString;
      end;
end;

procedure TfrmMain.SaveDataUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ConvertDefinitions.Count > 0;
end;

procedure TfrmMain.SaveDATFile(const FileName: string);
var
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  try
    ConvertDefinitions.FillToStringList(sl);
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

procedure TfrmMain.SaveDefinitionFile(const FileName: string);
begin
  fCurrentDefinitionFile := FileName;
  ConvertDefinitions.FillFromStringGrid(ReplaceListGrid);
  if UpperCase(ExtractFileExt(FileName)) = '.XML' then
    SaveXMLFile(FileName)
  else
    SaveDATFile(FileName);
end;

procedure TfrmMain.SaveSettings;
begin
  FAppOptions.RootDirectory := JvSearchFiles1.RootDirectory;
  FAppOptions.FileMask := JvSearchFiles1.FileParams.FileMask;
  FAppOptions.DefinitionFile := fCurrentDefinitionFile;
  FAppOptions.StoreProperties;
end;

procedure TfrmMain.SaveXMLFile(const FileName: string);
begin
  StorePropertyStoreToXmlFile(ConvertDefinitions, FileName);
end;

procedure TfrmMain.SearchListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortListColumn(SearchList, Column);
end;

procedure TfrmMain.SearchListDblClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := SearchList.Selected;
  if Item <> nil then
    WinExec(PAnsiChar('Notepad ' + AnsiString(Item.Caption)), sw_Normal);
end;

procedure TfrmMain.SearchListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Delete then
    if RemoveFiles.Enabled then
      btnRemoveClick(self);
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
  i: Integer;

  function Min(Val1, Val2: Integer): Integer;
  begin
    Result := Val1;
    if Val2 < Val1 then
      Result := Val2;
  end;

begin
  for i := 0 to sbStatus.Panels.Count - 1 do
  begin
    if i <= high(Msgs) then
      sbStatus.Panels[i].Text := PChar(Msgs[i])
    else
      sbStatus.Panels[i].Text := '';
  end;
  sbStatus.Update;
end;

procedure TfrmMain.SortListColumn(LV: TListView; Column: TListColumn);
var
  i: Integer;
  FDescending: Boolean;
  SortFunc: TLVCompare;
begin
  FDescending := (Column.ImageIndex <= 1);
  for i := 0 to LV.Columns.Count - 1 do
    LV.Columns[i].ImageIndex := - 1;
  case Column.Index of
    0:
      SortFunc := SortFilename;
    1:
      SortFunc := SortReplaceCount;
    2:
      SortFunc := SortStatus;
    3:
      SortFunc := SortMSecs;
  else
    SortFunc := nil;
  end;
  if Assigned(SortFunc) then
  begin
    LV.CustomSort(SortFunc, Ord(FDescending));
    Column.ImageIndex := Ord(FDescending) + 1;
  end;
end;

procedure TfrmMain.WMDropFiles(var Msg: TWMDropFiles);
{$IFDEF Windows}
const
  MAX_PATH = 255;
{$ENDIF}
var
  Pt: TPoint;
  Count, Loop: Integer;
  Buf: array [0 .. MAX_PATH] of char;
begin
  try
    Msg.Result := 0;
    DragQueryPoint(Msg.Drop, Pt);
    Count := DragQueryFile(Msg.Drop, Cardinal( - 1), Buf, sizeof(Buf));
    for Loop := 0 to Pred(Count) do
    begin
      DragQueryFile(Msg.Drop, Loop, Buf, sizeof(Buf));
      AddFiles(StrPas(Buf));
    end
  finally
    DragFinish(Msg.Drop)
  end

end;

initialization

Lines := TStringlist.Create;

finalization

Lines.Free;

end.
