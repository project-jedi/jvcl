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

{$I jvcl.inc}

//!!! (p3) NB! All storage commented out using //!!!

unit fMain;

interface

uses
  Windows, Messages, {$IFDEF COMPILER6_UP} Variants, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvEditor, JvEditorCommon, JvHLEditor, Menus,
  ShellApi, JvInterpreter, ImgList, JvComponent, JvHLEditorPropertyForm, JvFormPlacement,
  JvExControls;

const
  WM_CHECKFILEMODIFIED = WM_USER + $101;

type
  TMain = class(TForm)
    RegAuto1: TJvFormStorage;
    RAHLEditor1: TJvHLEditor;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miFileOpen: TMenuItem;
    N1: TMenuItem;
    miExit: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    raCommon: TJvFormStorage;
    N2: TMenuItem;
    miHelpAbout: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    miSearch: TMenuItem;
    Search1: TMenuItem;
    miSearchAgain: TMenuItem;
    miSearchReplace: TMenuItem;
    N3: TMenuItem;
    miOptions: TMenuItem;
    PopupMenu1: TPopupMenu;
    miEditorProperties: TMenuItem;
    JvInterpreterProgram1: TJvInterpreterProgram;
    GutterImages: TImageList;
    miEdit: TMenuItem;
    RAHLEdPropDlg1: TJvHLEdPropDlg;
    procedure RAHLEditor1ChangeStatus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miHelpAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure raCommonAfterLoad(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure miSearchAgainClick(Sender: TObject);
    procedure miSearchReplaceClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure raCommonAfterSave(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvInterpreterProgram1GetValue(Sender: TObject; Identifer: String;
      var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
    procedure RAHLEditor1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RAHLEditor1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RAHLEditor1KeyPress(Sender: TObject; var Key: Char);
    procedure JvInterpreterProgram1GetUnitSource(UnitName: String;
      var Source: String; var Done: Boolean);
    procedure FindDialog1Find(Sender: TObject);
    procedure RAHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure FormShow(Sender: TObject);
    procedure JvInterpreterProgram1SetValue(Sender: TObject; Identifer: String;
      const Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
  private
    FFileName: TFileName;
    FileTime: Integer;
    Exts: array[TJvHighlighter] of string;
    Capt: string;
    BaseLine: Integer;
    procedure OpenFile(AFileName: TFileName);
    procedure SetHighlighter;
    procedure LoadColors;
    procedure UpdateCaption;
    procedure CheckSave;
    procedure UpdateEditorSettings;
    procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
    procedure FindNext;
    procedure CheckFileModified;
    procedure ApplicationActivate(Sender: TObject);
    procedure WMCheckFileModified(var Message: TMessage); message WM_CHECKFILEMODIFIED;
  private { JvInterpreter support }
    JvInterpreterFileName: TFileName;
    Args: TJvInterpreterArgs;
    procedure ErrorLogFmt(const Message: string; const Args: array of const);
    function JvInterpreterScript: boolean;
    procedure JvInterpreterInitialize;
    procedure JvInterpreterUnInitialize;
    function JvInterpreterSafeCall(const FunName: string; Args: TJvInterpreterArgs;
      Params: array of Variant): Variant;
    procedure JvInterpreterFileOpened;
    procedure JvInterpreterFileClosed;
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses JvJCLUtils, JvConsts,
  JvJVCLUtils, JvInterpreter_JvUtils,
  JvInterpreter_System, JvInterpreter_Windows, JvInterpreter_SysUtils,
  JvInterpreter_Graphics, JvInterpreter_Classes, JvInterpreter_Controls,
  JvInterpreter_StdCtrls, JvInterpreter_ComCtrls, JvInterpreter_ExtCtrls, JvInterpreter_Forms,
  JvInterpreter_Menus, JvInterpreter_Dialogs,
  JvInterpreterFm,
  JvInterpreter_JvEditor;

{$R *.DFM}

const
  PadExt = '.pad'; { extension for macro-files }

{
procedure FailProc;
var
  Func: procedure; far;
begin
end;
}

procedure TMain.RAHLEditor1ChangeStatus(Sender: TObject);
const
  Modi: array[boolean] of string[10] = ('', 'Modified');
  Modes: array[boolean] of string[10] = ('Overwrite', 'Insert');
begin
  with StatusBar, RAHLEditor1 do
  begin
    Panels[0].Text := IntToStr(CaretY + 1 - BaseLine) + ':' + IntToStr(CaretX + 1);
    Panels[1].Text := Modi[Modified];
    if ReadOnly then
      Panels[2].Text := 'ReadOnly'
    else if Recording then
      Panels[2].Text := 'Recording'
    else
      Panels[2].Text := Modes[InsertMode];
    miFileSave.Enabled := Modified;
  end;
end;

procedure TMain.OpenFile(AFileName: TFileName);
begin
 {$IFDEF COMPILER3_UP}
  AFileName := TargetFileName(AFileName);
 {$ENDIF COMPILER3_UP}
  RAHLEditor1.BeginUpdate;
  try
    RAHLEditor1.Lines.LoadFromFile(AFileName);
    FileTime := FileAge(AFileName);
    RAHLEditor1.SetLeftTop(0, 0);
    RAHLEditor1.Modified := False;
    RAHLEditor1ChangeStatus(nil);
    FFileName := AFileName;
    SetHighlighter;
    UpdateCaption;
    Application.BringToFront;
    JvInterpreterFileOpened;
  finally
    RAHLEditor1.EndUpdate;
  end;
end;

procedure TMain.CheckFileModified;
begin
  if FFileName = '' then Exit;
  if FileExists(FFileName) then
  begin
    if FileTime <> FileAge(FFileName) then
    begin
      if RAHLEditor1.Modified then
        if MessageDlg('File time/date changed.'#13 +
           'Reload ?'#13#13 +
           'WARNING: Document has been modified.',
           mtWarning, [mbYes, mbNo], 0) = idYes then
          OpenFile(FFileName) else
      else
        if MessageDlg('File time/date changed.'#13 +
           'Reload ?', mtInformation, [mbYes, mbNo], 0) = idYes then
          OpenFile(FFileName);
    end;
  end
  else
    { To inform, that the user should save the file somewhere [translated] }
    MessageDlg('File removed from disk.'#13 +
      'Choose File|SaveAs menu item to save file.',
       mtWarning, [mbOK], 0);
end;

procedure TMain.ApplicationActivate(Sender: TObject);
begin
  PostMessage(Handle, WM_CHECKFILEMODIFIED, 0, 0);
end;

procedure TMain.WMCheckFileModified(var Message: TMessage);
begin
  CheckFileModified;
end;

procedure TMain.miFileSaveClick(Sender: TObject);
begin
  RAHLEditor1.Lines.SaveToFile(FFileName);
  FileTime := FileAge(FFileName);
  RAHLEditor1.Modified := False;
  RAHLEditor1ChangeStatus(nil);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Application.OnActivate := ApplicationActivate;
  Capt := Caption;
//!!!  raCommon.IniFile := ExtractFilePath(ParamStr(0)) + 'ranotepad.ini';
  Exts[hlPascal] := '*.pas;*.dpk;*.dpr;*.inc;*.pad';
  Exts[hlCBuilder] := '*.cpp;*.c;*.hpp;*.h';
  Exts[hlSql] := '*.sql';
  Exts[hlPython] := '*.py';
  Exts[hlJava] := '*.java';
  Exts[hlVB] := '*.bas';
  Exts[hlHtml] := '*.htm;*.html;*.asp';
  Exts[hlPerl] := '*.pl';
  Exts[hlIni] := '*.ini';
  DragAcceptFiles(Handle, True);
//!!!  raCommon.Load;
  JvInterpreterInitialize;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  JvInterpreterUnInitialize;
//!!!  raCommon.Save;
end;

procedure TMain.FormShow(Sender: TObject);
begin
  if ParamCount > 0 then
    OpenFile(GetLongFileName(ParamStr(1)));
end;

procedure TMain.UpdateEditorSettings;
begin
  SetHighlighter;
end;    { UpdateEditorSettings }

procedure TMain.SetHighlighter;
var
  Ext: TFileName;
  i, H: TJvHighlighter;
begin
  Ext := ExtractFileExt(FFileName);
  H := hlNone;
  if RAHLEditor1.SyntaxHighlighting then
    for i := Low(TJvHighlighter) to High(TJvHighlighter) do
      if FileEquMasks(FFileName, Exts[i]) then
      begin
        H := i;
        break;
      end;
  RAHLEditor1.HighLighter := H;
  LoadColors;
end;

procedure TMain.raCommonAfterSave(Sender: TObject);
begin
  RAHLEdPropDlg1.Save;
end;

procedure TMain.raCommonAfterLoad(Sender: TObject);
// var
//   i: TJvHighlighter;
begin
//!!!  for i := Low(TJvHighlighter) to High(TJvHighlighter) do
//!!!    Exts[i] := Trim(raCommon.ReadString('Highlighters', HighLighters[i], Exts[i]));
  RAHLEdPropDlg1.Restore;
  UpdateEditorSettings;
end;

procedure TMain.LoadColors;
begin
  RAHLEdPropDlg1.LoadCurrentHighLighterColors;
end;

procedure TMain.miHelpAboutClick(Sender: TObject);
begin
  Application.MessageBox('JVCL Notepad 2.0 Freeware'#13#13 +
    'Based on Delphi components TJvHLEditor and TJvInterpreterProgram.'#13 +
    'Available (free) at JVCL Library home page:'#13 +
    '   http://jvcl.sourceforge.net'#13#13 +
    'programming - Andrei Prygounkov:'#13 +
    '   a dott prygounkov att gmx dott de'#13,
    'About', MB_ICONINFORMATION);
end;

procedure TMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CheckSave;
  CanClose := True;
end;

procedure TMain.CheckSave;
begin
  if RAHLEditor1.Modified then
    case MessageDlg('Save changes ?', mtConfirmation,
       [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        miFileSave.Click;
      mrCancel:
        Abort;
    end;
  JvInterpreterFileClosed;
end;

procedure TMain.miFileOpenClick(Sender: TObject);
begin
  CheckSave;
  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TMain.miFileSaveAsClick(Sender: TObject);
begin
  if FFileName <> '' then
    SaveDialog1.FileName := FFileName
  else
    SaveDialog1.FileName := ExtractFilePath(OpenDialog1.FileName) + 'NONAME.TXT';
  if SaveDialog1.Execute then
  begin
    RAHLEditor1.Lines.SaveToFile(SaveDialog1.FileName);
    FFileName := SaveDialog1.FileName;
    FileTime := FileAge(FFileName);
  end
  else
    Abort;
  OpenDialog1.FileName := SaveDialog1.FileName;
  UpdateCaption;
end;

procedure TMain.UpdateCaption;
begin
  if FFileName <> '' then
  begin
    Caption := Capt + ' - ' + FFileName;
    Application.Title := ExtractFileName(FFileName) + ' - ' + Capt;
  end
  else
  begin
    Caption := Capt + ' - new file' ;
    Application.Title := 'new file - ' + Capt;
  end;
end;

procedure TMain.WMDropFiles(var Message: TMessage);
var
  FN: string;
begin
  SetLength(FN, 260);
  SetLength(FN, DragQueryFile(Message.WParam, 0, PChar(FN), 260));
  CheckSave;
  OpenFile(FN);
end;

procedure TMain.Search1Click(Sender: TObject);
begin
  FindDialog1.FindText := RAHLEditor1.GetWordOnCaret;
  FindDialog1.Execute;
end;

procedure TMain.miSearchAgainClick(Sender: TObject);
begin
  FindNext;
end;

procedure TMain.FindDialog1Find(Sender: TObject);
begin
  FindNext;
end;

procedure TMain.FindNext;
var
  S, S1: string;
  F: PChar;
begin
  S := RAHLEditor1.Lines.Text;
  S1 := FindDialog1.FindText;
  if not (frMatchCase in FindDialog1.Options) then
  begin
    S := ANSIUpperCase(S);
    S1 := ANSIUpperCase(S1);
  end;
  F := StrPos(PChar(S) + RAHLEditor1.SelStart, PChar(S1));
  if F <> nil then
  begin
    RAHLEditor1.SelStart := F - PChar(S);
    RAHLEditor1.SelLength := Length(S1);
  end;
end;

procedure TMain.miSearchReplaceClick(Sender: TObject);
begin
//  SAR1.ReplaceDialog;
end;

procedure TMain.miOptionsClick(Sender: TObject);
begin
  if RAHLEdPropDlg1.Execute then
    RAHLEditor1.Invalidate;
end;




{*********************** JvInterpreter support ***********************}
type
  { small hack }
  TMyProgram = class(TJvInterpreterProgram);

procedure TMain.ErrorLogFmt(const Message: string; const Args: array of const);
var
  S: string;
begin
  S := Format(Message, Args);
  { save S to log file }
  //.. not implemented
  ShowMessage(S);
end;    { ErrorLogFmt }

procedure TMain.JvInterpreterInitialize;
begin
  Args := TJvInterpreterArgs.Create;
 { from JvInterpreter_all.pas }
  JvInterpreter_System.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Windows.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_SysUtils.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Classes.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Graphics.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Controls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreter_StdCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_ComCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_ExtCtrls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Forms.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Dialogs.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Menus .RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreterFm.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

  JvInterpreter_JvEditor.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_JvUtils.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);

//!!!  JvInterpreterFileName := raCommon.ReadString('Params', 'JvInterpreterFile', '');
  if JvInterpreterFileName = '' then Exit;
  JvInterpreterFileName := AddPath(JvInterpreterFileName, ExePath);
  if not FileExists(JvInterpreterFileName) then
  begin
    ErrorLogFmt('File %s not found', [JvInterpreterFileName]);
    Exit;
  end;           
  try
    JvInterpreterProgram1.Pas.LoadFromFile(JvInterpreterFileName);
  except
    ErrorLogFmt('Failed to load file %s', [JvInterpreterFileName]);
    Exit;
  end;
  JvInterpreterProgram1.Compile;
  JvInterpreterSafeCall('main', nil, [Null]);
end;    { RegisterJvInterpreterAdapters }

procedure TMain.JvInterpreterUnInitialize;
begin
  JvInterpreterSafeCall('done', nil, [Null]);
  Args.Free;
end;

function TMain.JvInterpreterScript: boolean;
begin
  Result := JvInterpreterProgram1.Source <> '';
end;    {  }

function TMain.JvInterpreterSafeCall(const FunName: string; Args: TJvInterpreterArgs;
  Params: array of Variant): Variant;
begin
  Result := Null;
  if JvInterpreterScript and JvInterpreterProgram1.FunctionExists('', FunName) then
    try
      Result := JvInterpreterProgram1.CallFunction(FunName, Args, Params);
    except
      on E: EJvInterpreterError do
        ErrorLogFmt('Call to function %s failed: %s', [FunName, E.Message]);
    end
end;    { JvInterpreterSafeCall }

procedure TMain.JvInterpreterFileOpened;
begin
  JvInterpreterSafeCall('FileOpened', nil, [FFileName]);
end;    { JvInterpreterOpen }

procedure TMain.JvInterpreterFileClosed;
begin
  JvInterpreterSafeCall('FileClosed', nil, [FFileName]);
end;    { JvInterpreterFileClosed }

procedure TMain.JvInterpreterProgram1GetValue(Sender: TObject; Identifer: String;
  var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
var
  S: string;
begin
  if Cmp(Identifer, 'MainWindow') then
  begin
    Value := O2V(Self);
    Done := True;
  end
  else if Cmp(Identifer, 'Editor') then
  begin
    Value := O2V(RAHLEditor1);
    Done := True;
  end
  else if Cmp(Identifer, 'ODS') then
  begin
    //ODS(Args.Values[0]);
    Value := Null;
    Done := True;
  end
  else if Cmp(Identifer, 'Call') then
  begin
    S := Args.Values[0];
    Args.Delete(0);
    Value := JvInterpreterProgram1.CallFunction(S, Args, [Null]);
    Done := True;
  end
  else if Cmp(Identifer, 'UseUnit') then
  begin
    TMyProgram(JvInterpreterProgram1).ReadUnit(Args.Values[0]);
    Value := Null;
    Done := True;
  end
  else if Args.Obj = RAHLEditor1 then
  begin
    if Cmp(Identifer, 'FileName') then
    begin
      Value := FFileName;
      Done := True;
    end
    else if Cmp(Identifer, 'FileOpen') then
    begin
      OpenFile(Args.Values[0]);
      Value := Null;
      Done := True;
    end
    else if Cmp(Identifer, 'FileSave') then
    begin
      miFileSave.Click;
      Value := Null;
      Done := True;
    end
    else if Cmp(Identifer, 'CheckSave') then
    begin
      CheckSave;
      Value := Null;
      Done := True;
    end
    else if Cmp(Identifer, 'HighlighterName') then
    begin
      Value := string(Highlighters[RAHLEditor1.Highlighter]);
      Done := True;
    end
    else if Cmp(Identifer, 'LoadColors') then
    begin
      LoadColors;
      Done := True;
    end
    else if Cmp(Identifer, 'BaseLine') then
    begin
      Value := BaseLine;
      Done := True;
    end
  end;
end;

procedure TMain.JvInterpreterProgram1SetValue(Sender: TObject; Identifer: String;
  const Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
begin
  if Args.Obj = RAHLEditor1 then
  begin
    if Cmp(Identifer, 'BaseLine') then
    begin
      BaseLine := Value;
      Done := True;
    end
  end;
end;

procedure TMain.JvInterpreterProgram1GetUnitSource(UnitName: String;
  var Source: String; var Done: Boolean);
begin
  try
    Source := LoadTextFile(AddPath(UnitName + PadExt, ExtractFilePath(JvInterpreterFileName)));
    Done := True;
  except
  end;
end;

procedure TMain.RAHLEditor1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Args.Clear;
  Args.Values[0] := Key;
  Args.Values[1] := S2V(Byte(Shift));
  Args.Types[0] := varInteger or varByRef;
  Args.Count := 2;
  JvInterpreterSafeCall('KeyDown', Args, [Null]);
  Key := Args.Values[0];
end;

procedure TMain.RAHLEditor1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Args.Clear;
  Args.Values[0] := Key;
  Args.Values[1] := S2V(Byte(Shift));
  Args.Types[0] := varInteger or varByRef;
  Args.Count := 2;
  JvInterpreterSafeCall('KeyUp', Args, [Null]);
  Key := Args.Values[0];
end;

procedure TMain.RAHLEditor1KeyPress(Sender: TObject; var Key: Char);
begin
  Args.Clear;
  Args.Values[0] := Key;
  Args.Types[0] := varInteger or varByRef;
  Args.Count := 1;
  JvInterpreterSafeCall('KeyPress', Args, [Null]);
  if string(Args.Values[0]) <> '' then
    Key := string(Args.Values[0])[1]
  else
    Key := #0;
end;


procedure TMain.RAHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
  procedure Draw(Y, ImageIndex : integer);
  var
    Ro : integer;
    R : TRect;
  begin
    if Y <> -1 then
      with Sender as TJvEditor do begin
        Ro := Y - TopRow;
        R := CalcCellRect(0, Ro);
        GutterImages.Draw(Canvas,
          GutterWidth -GutterRightMargin -GutterImages.Width{R.Left},
          R.Top + (CellRect.Height - GutterImages.Height) div 2 +1,
          ImageIndex);
      end;
  end;
var
  i  : integer;
begin
  for i := 0 to 9 do
    if (Sender as TJvEditor).Bookmarks[i].Valid then
      Draw((Sender as TJvEditor).Bookmarks[i].Y, i);
end;



end.
