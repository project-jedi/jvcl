{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

unit fJvInterpreterTest;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  JvInterpreter, JvInterpreterFm, JvEditor, JvHLParser, JvHLEditor,
  Db, DBTables, Grids, DBGrids,
  Variants,
  JvExControls, JvComponent, JvFormPlacement, JvComponentBase,
  JvEditorCommon, JvAppStorage, JvAppIniStorage;


type
  TTest = class(TForm)
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    Table1: TTable;
    DataSource1: TDataSource;
    JvInterpreterProgram1: TJvInterpreterFm;
    memSource: TJvHLEditor;
    Panel2: TPanel;
    Notebook1: TNotebook;
    bRunReport: TButton;
    bRunForm: TButton;
    Label1: TLabel;
    Button1: TButton;
    Button5: TButton;
    memDescription: TMemo;
    pnlTime: TPanel;
    Label3: TLabel;
    cmbExamples: TComboBox;
    Panel3: TPanel;
    pnlResult: TPanel;
    Button2: TButton;
    Button3: TButton;
    AppStorage: TJvAppIniFileStorage;
    JvFormStorage1: TJvFormStorage;
    FixedExamplesStorage: TJvAppIniFileStorage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure bRunFormClick(Sender: TObject);
    procedure bRunReportClick(Sender: TObject);
    procedure JvInterpreterProgram1GetUnitSource(UnitName: string; var Source: string;
      var Done: Boolean);
    procedure cmbExamplesChange(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure JvInterpreterProgram1Statement(Sender: TObject);
    procedure memSourceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbExamplesDropDown(Sender: TObject);
    procedure JvInterpreterProgram1GetDfmFileName(Sender: TObject; UnitName: String;
      var FileName: String; var Done: Boolean);
    procedure JvInterpreterProgram1GetValue(Sender: TObject;
      Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
      var Done: Boolean);
    procedure JvFormStorage1StoredValues0Restore(Sender: TJvStoredValue;
      var AValue: Variant);
    procedure JvFormStorage1StoredValues0Save(Sender: TJvStoredValue;
      var AValue: Variant);
  private
    { Private declarations }
    Parser : TJvIParser;
    FFixedExampleCount: Integer;
    CurFileName: TFileName;
  public
    { Public declarations }
    V: Variant;

    procedure GotoExample(const AName: string);
    procedure GotoFixedExample(const AName: string);
    procedure GotoCustomExample(const AName: string);
    procedure FillExamples(Examples: TStrings);
    procedure ClearScreen;
    procedure SaveCustomExample;
  end;

var
  Test: TTest;

implementation

uses JclFileUtils, JclStrings, JvJCLUtils, JvJVCLUtils, JvInterpreter_all, JvInterpreter_SysUtils{, JvInterpreter_iMTracer};

{$R *.DFM}

{$IFNDEF COMPILER3_UP}
type
  TQuickRep = TQuickReport;
{$ENDIF}

//======================================================
function ReadFolder(const Folder, Mask : TFileName; FileList : TStrings) : integer;
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  FileList.Clear;
  Result := FindFirst(PathAddSeparator(Folder)+Mask, faAnyFile, SearchRec);
  DosError := Result;
  while DosError = 0 do begin
    if not ((SearchRec.Attr and faDirectory) = faDirectory)  then
      FileList.Add(SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function Pixels(Control : TControl; APixels : integer) : integer;
var
  Form : TForm;
begin
  Result := APixels;
  if Control is TForm then
    Form := TForm(Control) else
    Form := TForm(GetParentForm(Control));
  if Form.Scaled then
    Result := Result * Form.PixelsPerInch div 96;
end;
//======================================================

function FindInPath(const FileName, PathList: string): TFileName;
var
  i: Integer;
  paths : TStringList;
begin
  i := 0;
  Result := '';
  paths := TStringList.Create;
  try
    StrToStrings(PathList, ';', paths, False);
    while i < paths.Count do
    begin
      Result := PathAddSeparator(paths[i]) + FileName;
      if FileExists(Result) then
        Exit;
      inc(i);
    end;
    Result := '';
  finally
    paths.Free;
  end;
end;

{ constructor Create(Msg: string) }
procedure EZeroDivide_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(EZeroDivide.Create(Args.Values[0]));
end;

function SourceDir: string;
var
  AExeDir: string;
begin
  AExeDir := ExtractFilePath(Application.ExeName);
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(AExeDir)) + 'examples\JvInterpreterDemos\JvInterpreterTest\';
end;

function SamplesDir: string;
begin
  Result := SourceDir + 'samples\';
end;

procedure TTest.FormCreate(Sender: TObject);
begin
  FillExamples(cmbExamples.Items);

  JvInterpreterProgram1.Adapter.AddGet(EZeroDivide, 'Create', EZeroDivide_Create, 1, [varEmpty], varEmpty);
  DecimalSeparator := '.';
  Parser := TJvIParser.Create;
end;

procedure TTest.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TTest.Button1Click(Sender: TObject);
const
  Bool : array [boolean] of string = ('False', 'True');
var
  T1: longword;
  obj:TObject;
  vtype:TVarType;
begin
  SaveCustomExample;
  
  if (Sender = Button1) or (Sender = Button2) or (Sender = Button5) then
  begin
    JvInterpreterProgram1.Source := memSource.Lines.Text;
    CurFileName := '';
  end
  else if Sender = Button3 then
  begin
    if not OpenDialog1.Execute then Exit;
    CurFileName := OpenDialog1.FileName;
    memSource.Lines.Text := LoadTextFile(CurFileName);
    JvInterpreterProgram1.Source := LoadTextFile(CurFileName);
  end;                            

  pnlResult.Caption := 'Working';
  pnlResult.Color := clRed;
  pnlResult.Update;
  T1 := GetTickCount;

  try try
    if (Sender = Button1) or (Sender = Button2) or (Sender = Button3) then
      JvInterpreterProgram1.Run
    else if Sender = Button5 then
      JvInterpreterProgram1.Compile;

  pnlTime.Caption := 'ms: ' + IntToStr(GetTickCount - T1);

  vtype := VarType(JvInterpreterProgram1.VResult);
  if vtype = varBoolean then
	pnlResult.Caption := Bool[boolean(JvInterpreterProgram1.VResult)]
  else if (vtype = varString) or (vtype = varInteger) or (vtype = varDouble) then
	pnlResult.Caption := JvInterpreterProgram1.VResult
  else if vtype = varEmpty then
        pnlResult.Caption := 'Empty'
  else if vtype = varNull then
	pnlResult.Caption := 'Null'
  else if vtype = varObject then begin
           obj := V2O(JvInterpreterProgram1.VResult);
           if Assigned(obj) then
                            pnlResult.Caption := 'Object: nil'
           else
                            pnlResult.Caption := 'Object: ' + obj.ClassName;
      end
  else if vtype = varSet then
        pnlResult.Caption := 'Set: ' + IntToStr(V2S(JvInterpreterProgram1.VResult))
  else
	pnlResult.Caption := '!Unknown!';
  
  except
    on E : EJvInterpreterError do
    begin
      pnlResult.Caption := IntToStr(E.ErrCode) + ': ' + ReplaceString(E.Message, #10, ' ');
      if E.ErrPos > -1 then
      begin
        memSource.SelStart := E.ErrPos;
        memSource.SelLength := 0;
      end;
      memSource.SetFocus;
    end;
    on E : Exception do
    begin
      pnlResult.Caption := IntToStr(JvInterpreterProgram1.LastError.ErrCode) + ': ' +
        ReplaceString(JvInterpreterProgram1.LastError.Message, #10, ' ');
      if JvInterpreterProgram1.LastError.ErrPos > -1 then
      begin
        memSource.SelStart := JvInterpreterProgram1.LastError.ErrPos;
        memSource.SelLength := 0;
      end;
      memSource.SetFocus;
      raise;
    end
    else
    begin
      pnlResult.Caption := 'error';
      raise;
    end;
  end;
  finally
    pnlResult.Color := clBtnFace;
  end;
end;

procedure TTest.SaveCustomExample;
begin
  if cmbExamples.ItemIndex > FFixedExampleCount then
  begin
    memSource.Lines.SaveToFile(SourceDir + 'samples\' + cmbExamples.Text);
  end;
end;

var
  temp: TSearchRec;

procedure TTest.JvInterpreterProgram1GetUnitSource(UnitName: string; var Source: string;
  var Done: Boolean);
var
  FN: TFileName;
begin
  FN := FindInPath(UnitName + '.pas', ConcatSep(ExtractFilePath(CurFileName),
    SourceDir + ';' + SamplesDir, ';'));
  if FileExists(FN) then
  begin
    Source := LoadTextFile(FN);
    Done := True;
  end;
end;

procedure TTest.JvInterpreterProgram1GetDfmFileName(Sender: TObject; UnitName: String;
  var FileName: String; var Done: Boolean);
begin
  FileName := FindInPath(UnitName + '.dfm', ConcatSep(ExtractFilePath(CurFileName),
    SourceDir + ';' + SamplesDir, ';'));
  Done := FileExists(FileName);
end;

procedure TTest.bRunFormClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    JvInterpreterProgram1.RunFormModal(OpenDialog1.FileName);
end;

procedure TTest.bRunReportClick(Sender: TObject);
// var
//   QuickRep1: TQuickRep;
begin
//   if OpenDialog1.Execute then
//   begin
//     with JvInterpreterProgram1.MakeForm(OpenDialog1.FileName) do
//       try
//        {$IFDEF COMPILER3_UP}
//         QuickRep1 := (FindComponent('QuickRep1') as TQuickRep);
//        {$ELSE}
//         QuickRep1 := (FindComponent('QuickReport1') as TQuickRep);
//        {$ENDIF COMPILER3_UP}
//         if QuickRep1 = nil then raise Exception.Create('QuickRep1 not found on this form');
//         QuickRep1.Preview;
//       finally { wrap up }
//         Free;
//       end;    { try/finally }
//   end;
end;

procedure TTest.cmbExamplesChange(Sender: TObject);
begin
  GotoExample(cmbExamples.Text);
end;

procedure TTest.Panel1Resize(Sender: TObject);
begin
  cmbExamples.Width := Panel1.Width - cmbExamples.Left - Pixels(Self, 8);
end;

procedure TTest.JvInterpreterProgram1Statement(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TTest.memSourceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = ord('S')) and ([ssCtrl] = Shift) then
    SaveCustomExample;
end;

procedure TTest.cmbExamplesDropDown(Sender: TObject);
begin
  SaveCustomExample;
end;
                                                    
procedure TTest.JvInterpreterProgram1GetValue(Sender: TObject;
  Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
  var Done: Boolean);
begin
  if Cmp(Identifier, 'Test') then
  begin
    Done := True;
    Value := O2V(Self);
  end
  else if Cmp(Identifier, 'Rec') then
  begin
    Done := True;
    //Value := SearchRec2Var(temp);
    JvInterpreterVarCopy(Value, SearchRec2Var(temp));
  end
  else
  if Cmp(Identifier, 'ShowMessage') and (Args.Obj = Self) then
  begin
    Done := True;
    ShowMessage(Args.Values[0]);
    Value := Null;
  end
  else
  if Cmp(Identifier, 'MyFunction') then
  begin
    Done := True;
    Value := Args.Values[0] + 1;
  end
  else
  if Cmp(Identifier, 'SamplesDir') then
  begin
    Done := True;
    Value := SamplesDir;
  end
end;

procedure TTest.GotoFixedExample(const AName: string);
begin
  memSource.Lines.BeginUpdate;
  try
    memSource.Clear;
    FixedExamplesStorage.ReadStringList(AName + '\Source', memSource.Lines);
  finally
    memSource.Lines.EndUpdate;
  end;

  memDescription.Lines.BeginUpdate;
  try
    memDescription.Clear;
    FixedExamplesStorage.ReadStringList(AName + '\Description', memDescription.Lines);
  finally
    memDescription.Lines.EndUpdate;
  end;

  Notebook1.ActivePage := FixedExamplesStorage.ReadString(AName + '\Page', 'Default');
end;

procedure TTest.GotoCustomExample(const AName: string);
begin
  memSource.Lines.LoadFromFile(SamplesDir + AName);
  memDescription.Clear;
  Notebook1.ActivePage := 'Default';
end;

procedure TTest.FillExamples(Examples: TStrings);
var
  CustomExamples: TStringList;
  I: Integer;
begin
  FixedExamplesStorage.FileName := SourceDir + 'JvInterpreterTest.ini';

  Examples.BeginUpdate;
  try
    Examples.Clear;

    // add fixed examples from the ini file
    FixedExamplesStorage.ReadStringList('Demos',Examples);
    FFixedExampleCount :=Examples.Count;

    // add custom examples from the samples directory
    CustomExamples := TStringList.Create;
    try
      ReadFolder(SamplesDir, 'sample - *.pas', CustomExamples);
      if CustomExamples.Count > 0 then
      begin
        Examples.Add('------ custom files (samples folder) ------');
        CustomExamples.Sort;
        for i := 0 to CustomExamples.Count - 1 do
          Examples.Add(CustomExamples[i]);
      end;
     finally
       CustomExamples.Free;
     end;
  finally
    Examples.EndUpdate;
  end;
end;

procedure TTest.JvFormStorage1StoredValues0Restore(Sender: TJvStoredValue;
  var AValue: Variant);
begin
  GotoExample(AValue);
end;

procedure TTest.JvFormStorage1StoredValues0Save(Sender: TJvStoredValue;
  var AValue: Variant);
begin
  AValue := cmbExamples.Text;
end;

procedure TTest.GotoExample(const AName: string);
var
  Index: Integer;
begin
  Index := cmbExamples.Items.IndexOf(AName);
  cmbExamples.ItemIndex := Index;

  if Index < 0 then
    ClearScreen
  else if Index < FFixedExampleCount then
    GotoFixedExample(AName)
  else if Index = FFixedExampleCount then
    ClearScreen
  else if Index > FFixedExampleCount then
    GotoCustomExample(AName);
end;

procedure TTest.ClearScreen;
begin
  memSource.Clear;
  memDescription.Clear;
  Notebook1.ActivePage := 'Empty';
end;

initialization
  //JvInterpreter_QRExpr.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  //JvInterpreter_iMTracer.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
end.