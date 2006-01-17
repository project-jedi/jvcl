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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}


{$I jvcl.inc}

unit fJvInterpreterTest;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  JvInterpreter, JvInterpreterFm, JvEditor, JvHLParser, JvHLEditor,
  Db, DBTables, Grids, DBGrids,
 {$IFDEF COMPILER6_UP}
   Variants,
   {$ENDIF}
 JvExControls, JvComponent, JvFormPlacement, JvComponentBase,
  JvEditorCommon;


type
  TTest = class(TForm)
    RegAuto1: TJvFormStorage;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    Table1: TTable;
    DataSource1: TDataSource;
    JvInterpreterProgram1: TJvInterpreterFm;
    Memo1: TJvHLEditor;
    Panel2: TPanel;
    Notebook1: TNotebook;
    bRunReport: TButton;
    bRunForm: TButton;
    Label1: TLabel;
    Button1: TButton;
    Button5: TButton;
    Memo2: TMemo;
    pnlTime: TPanel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    RegAuto2: TJvFormStorage;
    Panel3: TPanel;
    pnlResult: TPanel;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure RegAuto1AfterLoad(Sender: TObject);
    procedure bRunFormClick(Sender: TObject);
    procedure bRunReportClick(Sender: TObject);
    procedure JvInterpreterProgram1GetUnitSource(UnitName: string; var Source: string;
      var Done: Boolean);
    procedure ComboBox1Change(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure JvInterpreterProgram1Statement(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure JvInterpreterProgram1GetDfmFileName(Sender: TObject; UnitName: String;
      var FileName: String; var Done: Boolean);
    procedure JvInterpreterProgram1GetValue(Sender: TObject;
      Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
      var Done: Boolean);
  private
    { Private declarations }
    Parser : TJvIParser;
    InternalExamplesCount: Integer;
    CurFileName: TFileName;
  public
    { Public declarations }
    V: Variant;
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

procedure TTest.FormCreate(Sender: TObject);
var
  SS: TStringList;
  i: Integer;
begin
{
//!!!
  try
    RegAuto2.IniStrings.LoadFromFile(ExePath + 'JvInterpreterTest.ini');
  except
    MessageDlg('Can''t load file "JvInterpreterTest.ini".'#13+
      'Please put it in same folder as JvInterpreterTest.exe.',
      mtError, [mbCancel], -1);
  end;

  RegAuto2.ReadSection('Demos', ComboBox1.Items);
}
  InternalExamplesCount := ComboBox1.Items.Count;
  SS := TStringList.Create;
  try
    ReadFolder(ExePath + 'samples', 'sample - *.pas', SS);
    if SS.Count > 0 then
      ComboBox1.Items.Add('------ custom files (samples folder) ------');
    SS.Sort;
    for i := 0 to SS.Count - 1 do
      ComboBox1.Items.Add(SS[i]);
  finally
    SS.Free;
  end;
  JvInterpreterProgram1.Adapter.AddGet(EZeroDivide, 'Create', EZeroDivide_Create, 1, [varEmpty], varEmpty);
  DecimalSeparator := '.';
  Parser := TJvIParser.Create;
end;

procedure TTest.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

{$IFNDEF COMPILER6_UP}
type TVarType = Word;
{$ENDIF}
procedure TTest.Button1Click(Sender: TObject);
const
  Bool : array [boolean] of string = ('False', 'True');
var
  T1: longword;
  obj:TObject;
  vtype:TVarType;
begin
  RegAuto1AfterSave(nil);
  if (Sender = Button1) or (Sender = Button2) or (Sender = Button5) then
  begin
    JvInterpreterProgram1.Source := Memo1.Lines.Text;
    CurFileName := '';
  end
  else if Sender = Button3 then
  begin
    if not OpenDialog1.Execute then Exit;
    CurFileName := OpenDialog1.FileName;
    Memo1.Lines.Text := LoadTextFile(CurFileName);
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
        Memo1.SelStart := E.ErrPos;
        Memo1.SelLength := 0;
      end;
      Memo1.SetFocus;
    end;
    on E : Exception do
    begin
      pnlResult.Caption := IntToStr(JvInterpreterProgram1.LastError.ErrCode) + ': ' +
        ReplaceString(JvInterpreterProgram1.LastError.Message, #10, ' ');
      if JvInterpreterProgram1.LastError.ErrPos > -1 then
      begin
        Memo1.SelStart := JvInterpreterProgram1.LastError.ErrPos;
        Memo1.SelLength := 0;
      end;
      Memo1.SetFocus;
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
              
procedure TTest.RegAuto1AfterSave(Sender: TObject);
begin
{
//!!!
  RegAuto1.WriteInteger(Name, 'PrId', ComboBox1.ItemIndex);
  if ComboBox1.ItemIndex >= InternalExamplesCount then
    Memo1.Lines.SaveToFile(ExePath + 'samples\' + ComboBox1.Text);
}    
end;

procedure TTest.RegAuto1AfterLoad(Sender: TObject);
begin
//!!!  ComboBox1.ItemIndex := RegAuto1.ReadInteger(Name, 'PrId', 0);
//!!!  ComboBox1Change(nil);
end;

var
  temp: TSearchRec;

procedure TTest.JvInterpreterProgram1GetUnitSource(UnitName: string; var Source: string;
  var Done: Boolean);
var
  FN: TFileName;
begin
  FN := FindInPath(UnitName + '.pas', ConcatSep(ExtractFilePath(CurFileName),
    ExePath + ';' + ExePath + 'samples', ';'));
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
      ExePath + ';' + ExePath + 'samples', ';'));
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

procedure TTest.ComboBox1Change(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  if ComboBox1.ItemIndex < InternalExamplesCount then
  begin
//!!!    RegAuto2.ReadWholeSection(ComboBox1.Text + '\Source', Memo1.Lines);
//!!!    RegAuto2.ReadWholeSection(ComboBox1.Text + '\Description', Memo2.Lines);
//!!!    Notebook1.ActivePage := RegAuto2.ReadString(ComboBox1.Text, 'Page', 'Default');
  end else
  begin
    Memo1.Lines.LoadFromFile(ExePath + 'samples\' + ComboBox1.Text);
    Notebook1.ActivePage := 'Default';
  end;
//  Memo1.Refresh;
end;

procedure TTest.Panel1Resize(Sender: TObject);
begin
  ComboBox1.Width := Panel1.Width - ComboBox1.Left - Pixels(Self, 8);
end;

procedure TTest.JvInterpreterProgram1Statement(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TTest.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = ord('S')) and ([ssCtrl] = Shift) then
    RegAuto1AfterSave(nil);
end;

procedure TTest.ComboBox1DropDown(Sender: TObject);
begin
  RegAuto1AfterSave(nil);
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
end;

initialization
  //JvInterpreter_QRExpr.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  //JvInterpreter_iMTracer.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
end.
