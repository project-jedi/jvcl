unit main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, JvRegAuto, IniFiles, JvInterpreter;

type
  TMainForm   = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel3: TPanel;
    Run: TButton;
    JvInterpreterProgram1: TJvInterpreterProgram;
    pnlResult: TPanel;
    Panel4: TPanel;
    SampleSelector: TComboBox;
    pnlTime: TPanel;
    Label1: TLabel;
    RegAuto1: TJvRegAuto;
    RegAuto2: TJvRegAuto;
    procedure RunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SampleSelectorSelect(Sender: TObject);
    procedure JvInterpreterProgram1GetValue(Sender: TObject; Identifer: String;
      var Value: Variant; Args: TArgs; var Done: Boolean);
    procedure RegAuto1AfterSave(Sender: TObject);
    procedure RegAuto1AfterLoad(Sender: TObject);
  private
    { Private declarations }
    InternalExamplesCount: Integer;
    //CurFileName: TFileName;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm  ;

implementation

uses
    JvInterpreter_JvInterpreter
  , JvInterpreter_System
  , JvInterpreter_Types
  , JvInterpreter_SysUtils
  , JvInterpreter_Classes
  , JvInterpreter_Contnrs
  , JvInterpreter_Graphics
  , JvInterpreter_Controls
  , JvInterpreter_Dialogs
  , JvInterpreter_Forms
  ;

{$R *.xfm}

procedure TMainForm  .RunClick(Sender: TObject);
const
  Bool : array [boolean] of string = ('False', 'True');
var
  T1: TDateTime;
begin
  JvInterpreterProgram1.Source := Memo1.Lines.Text;

  pnlResult.Caption := 'Working';
  pnlResult.Color := clRed;
  pnlResult.Update;
  T1 := Now;      

  try try
    if Sender = Run then
      JvInterpreterProgram1.Run;
    {else if Sender = Compile  then
      JvInterpreterProgram1.Compile;}

  pnlTime.Caption := FormatDateTime('HH:mm:ss.zzz', Now - T1);

	case VarType(JvInterpreterProgram1.VResult) of
		varBoolean:
			pnlResult.Caption := Bool[boolean(JvInterpreterProgram1.VResult)];
		varString, varInteger, varDouble :
			pnlResult.Caption := JvInterpreterProgram1.VResult;
		varEmpty:
			pnlResult.Caption := 'Empty';
		varNull:
			pnlResult.Caption := 'Null';
    varObject:
      if V2O(JvInterpreterProgram1.VResult) = nil then
  			pnlResult.Caption := 'Object: nil'
      else
  			pnlResult.Caption := 'Object: ' + V2O(JvInterpreterProgram1.VResult).ClassName;
		varSet:
			pnlResult.Caption := 'Set: ' + IntToStr(V2S(JvInterpreterProgram1.VResult));
		else
			pnlResult.Caption := '!Unknown!';
	end;
  except
    on E : EJvInterpreterError do
    begin
      pnlResult.Caption := IntToStr(E.ErrCode) + ': ' + StringReplace(E.Message, #10, ' ', []);
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
        StringReplace(JvInterpreterProgram1.LastError.Message, #10, ' ', []);
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

procedure TMainForm  .FormCreate(Sender: TObject);
var
  SearchRec : TSearchRec;
  DosError  : integer;
begin
  try
    RegAuto2.IniStrings.LoadFromFile(ChangeFileExt(ParamStr(0), '.ini'));
  except
    MessageDlg('Can''t load file "JvInterpretertest.ini".'#13+
      'Please put it in same folder as JvInterpretertest executable.',
      mtError, [mbCancel], -1);
  end;
  RegAuto2.ReadSection('Demos', SampleSelector.Items);

  InternalExamplesCount := SampleSelector.Items.Count;

  DosError := FindFirst(ExtractFilePath(ParamStr(0)) + 'samples/*.pas', faAnyFile, SearchRec);
  while DosError = 0 do begin
    if not ((SearchRec.Attr and faDirectory) = faDirectory)  then
    begin
      if (SampleSelector.Items.IndexOf('------ custom files (samples folder) ------') = -1) then
        SampleSelector.Items.Add('------ custom files (samples folder) ------');
      SampleSelector.Items.Add(ChangeFileExt(SearchRec.Name, ''));
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

procedure TMainForm  .RegAuto1AfterSave(Sender: TObject);
begin
  RegAuto1.WriteString(Name, 'sample', SampleSelector.Text);
end;

procedure TMainForm  .RegAuto1AfterLoad(Sender: TObject);
var
  sample: string;
begin
  sample := RegAuto1.ReadString(Name, 'sample', '');
  if (sample <> '') then
    SampleSelector.ItemIndex := SampleSelector.Items.IndexOf(sample)
  else
    SampleSelector.ItemIndex := 0;
  SampleSelectorSelect(nil);
end;

procedure TMainForm  .SampleSelectorSelect(Sender: TObject);
var
  Sample: string;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  if SampleSelector.ItemIndex < InternalExamplesCount then
  begin
    Sample := SampleSelector.Text;
    if Pos('<Not yet implemented> - ', Sample) > 0 then
      Sample := Copy(Sample, Length('<Not yet implemented> - ') + 1, 1000);

    RegAuto2.ReadWholeSection(Sample + '\Source', Memo1.Lines);
    RegAuto2.ReadWholeSection(Sample + '\Description', Memo2.Lines);
    //Notebook1.ActivePage := RegAuto2.ReadString(SampleSelector.Text, 'Page', 'Default');
  end else
  if SampleSelector.ItemIndex = InternalExamplesCount then
  begin
    Memo1.Text := 'Please, select an example';
    Memo2.Text := 'Please, select an example';
  end
  else
  begin
    Memo1.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'samples/' + SampleSelector.Text + '.pas');
    //Notebook1.ActivePage := 'Default';
  end;
  Memo1.SelStart := 0;
  Memo2.SelStart := 0;
//  Memo1.Refresh;
end;
 procedure TMainForm  .JvInterpreterProgram1GetValue(Sender: TObject;
  Identifer: String; var Value: Variant; Args: TArgs; var Done: Boolean);
begin
  if Cmp(Identifer, 'Test') then
  begin
    Done := True;
    Value := O2V(Self);
  end
  else
  if Cmp(Identifer, 'ShowMessage') and (Args.Obj = Self) then
  begin
    Done := True;
    ShowMessage(VarToStr(Args.Values[0]));
    Value := Null;
  end
  else
  if Cmp(Identifer, 'MyFunction') then
  begin
    Done := True;
    Value := Args.Values[0] + 1;
  end
end;

initialization
  JvInterpreter_System.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Types.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_SysUtils.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Classes.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Contnrs.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Graphics.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Controls.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Dialogs.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
  JvInterpreter_Forms.RegisterJvInterpreterAdapter(GlobalJvInterpreterAdapter);
end.
