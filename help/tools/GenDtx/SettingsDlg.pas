unit SettingsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Settings, JvComponent, JvBaseDlg, JvBrowseFolder, ActnList, StdCtrls,
  ComCtrls;

type
  TfrmSettings = class(TForm)
    pgcSettings: TPageControl;
    btnCancel: TButton;
    btnOK: TButton;
    btnApply: TButton;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    actApply: TAction;
    tshOutput: TTabSheet;
    tbcOutputTypes: TTabControl;
    memOutput: TMemo;
    lsbFor: TListBox;
    lblOutput: TLabel;
    lblFor: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    actAdd: TAction;
    actDelete: TAction;
    tshNiceNames: TTabSheet;
    lsbNiceNames: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    actAddNiceName: TAction;
    actDeleteNiceName: TAction;
    actEditNiceName: TAction;
    lblDefaultNiceName: TLabel;
    edtDefaultNiceName: TEdit;
    chbEnabled: TCheckBox;
    tshDirectives: TTabSheet;
    lsbDirectives: TListBox;
    Button4: TButton;
    Button5: TButton;
    actAddDirective: TAction;
    actDeleteDirective: TAction;
    actEditDirective: TAction;
    edtDirective: TEdit;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    tshIgnoredUnits: TTabSheet;
    tshRegisteredClasses: TTabSheet;
    lsbIgnoredUnits: TListBox;
    lsbRegisteredClasses: TListBox;
    edtIgnoredUnits: TEdit;
    Button6: TButton;
    Button7: TButton;
    edtRegisteredClasses: TEdit;
    Button8: TButton;
    Button9: TButton;
    actRegisteredClasses_Add: TAction;
    actRegisteredClasses_Delete: TAction;
    actIgnoredUnits_Add: TAction;
    actIgnoredUnits_Delete: TAction;
    Button10: TButton;
    Button11: TButton;
    actRegisteredClasses_Load: TAction;
    actIgnoredUnits_Load: TAction;
    actDocumentedUnits_Add: TAction;
    actDocumentedUnits_Delete: TAction;
    actDocumentedUnits_Load: TAction;
    procedure actAddDirectiveExecute(Sender: TObject);
    procedure actAddDirectiveUpdate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actAddNiceNameExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actDeleteDirectiveExecute(Sender: TObject);
    procedure actDeleteDirectiveUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteNiceNameExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actEditNiceNameExecute(Sender: TObject);
    procedure actIgnoredUnits_AddExecute(Sender: TObject);
    procedure actIgnoredUnits_DeleteExecute(Sender: TObject);
    procedure actIgnoredUnits_LoadExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actRegisteredClasses_AddExecute(Sender: TObject);
    procedure actRegisteredClasses_DeleteExecute(Sender: TObject);
    procedure actRegisteredClasses_LoadExecute(Sender: TObject);
    procedure lsbForClick(Sender: TObject);
    procedure tbcOutputTypesChange(Sender: TObject);
  private
    FSettings: TSettings;
    FCurrentTab: Integer;
    FCurrentFor: string;
  protected
    procedure Init;
    procedure Final;
    procedure Uitvoeren;

    procedure InitTabs;

    procedure SaveEnabled;
    procedure SaveOutput;
    procedure SaveDefaultNiceName;
    procedure SaveDirectives;
    procedure SaveIgnoredUnits;
    procedure SaveRegisteredClasses;

    procedure UpdateEnabled;
    procedure UpdateFor;
    procedure UpdateOutput;
    procedure UpdateNiceNames;
    procedure UpdateDirectives;
  public
    class procedure Execute;
  end;

implementation

uses
  InputDlg, EditNiceNameDlg, Math;

{$R *.dfm}
const
  CDefault = '(Default)';

//=== TfrmSettings ===========================================================

procedure TfrmSettings.actAddDirectiveExecute(Sender: TObject);
begin
  with lsbDirectives do
    if Items.IndexOf(edtDirective.Text) < 0 then
      Items.Add(edtDirective.Text);
end;

procedure TfrmSettings.actAddDirectiveUpdate(Sender: TObject);
begin
  actAddDirective.Enabled := edtDirective.Text > '';
end;

procedure TfrmSettings.actAddExecute(Sender: TObject);
var
  NewName: string;
begin
  if FCurrentTab < 0 then
    Exit;
  if not TfrmInput.Execute(NewName) then
    Exit;

  if lsbFor.Items.IndexOf(NewName) >= 0 then
  begin
    ShowMessage('Name already exists');
    Exit;
  end;

  lsbFor.Items.Add(NewName);
  FSettings.OutputTypeDesc[TOutputType(FCurrentTab)].Add(NewName);
  FSettings.OutputTypeStrings[TOutputType(FCurrentTab)].Add('');
end;

procedure TfrmSettings.actAddNiceNameExecute(Sender: TObject);
var
  ClassStr, DescStr: string;
begin
  if not TfrmEditNiceName.ExecuteAdd(ClassStr, DescStr) then
    Exit;
  ClassStr := UpperCase(ClassStr);
  if FSettings.NiceNameClass.IndexOf(ClassStr) >= 0 then
  begin
    ShowMessage('Bestaat al');
    Exit;
  end;
  FSettings.NiceNameClass.Add(ClassStr);
  FSettings.NiceNameDesc.Add(DescStr);
  lsbNiceNames.Items.Add(Format('%s - %s', [ClassStr, DescStr]));
end;

procedure TfrmSettings.actAddUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := FCurrentTab >= 0;
end;

procedure TfrmSettings.actApplyExecute(Sender: TObject);
begin
  Uitvoeren;
end;

procedure TfrmSettings.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettings.actDeleteDirectiveExecute(Sender: TObject);
begin
  with lsbDirectives do
    if ItemIndex >= 0 then
      Items.Delete(ItemIndex);
end;

procedure TfrmSettings.actDeleteDirectiveUpdate(Sender: TObject);
begin
  actDeleteDirective.Enabled := lsbDirectives.ItemIndex >= 0;
end;

procedure TfrmSettings.actDeleteExecute(Sender: TObject);
var
  Index: Integer;
begin
  if (FCurrentTab < 0) or (lsbFor.ItemIndex < 0) then
    Exit;
  with lsbFor do
    Index :=
      FSettings.OutputTypeDesc[TOutputType(FCurrentTab)].IndexOf(Items[ItemIndex]);

  if Index < 0 then
  begin
    ShowMessage('Kan waarde niet vinden');
    Exit;
  end;

  with FSettings do
  begin
    OutputTypeDesc[TOutputType(FCurrentTab)].Delete(Index);
    OutputTypeStrings[TOutputType(FCurrentTab)].Delete(Index);
  end;

  UpdateFor;
end;

procedure TfrmSettings.actDeleteNiceNameExecute(Sender: TObject);
var
  S: string;
  Index: Integer;
begin
  with lsbNiceNames do
  begin
    if ItemIndex < 0 then
      Exit;
    S := Items[ItemIndex];
  end;

  S := Copy(S, 1, Pos(' - ', S) - 1);
  Index := FSettings.NiceNameClass.IndexOf(UpperCase(S));
  if Index < 0 then
  begin
    ShowMessage(Format('Kan %s niet vinden', [QuotedStr(S)]));
    Exit;
  end;

  FSettings.NiceNameClass.Delete(Index);
  FSettings.NiceNameDesc.Delete(Index);
  UpdateNiceNames;
end;

procedure TfrmSettings.actDeleteUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := (lsbFor.ItemIndex > 0) and (FCurrentTab >= 0);
end;

procedure TfrmSettings.actEditNiceNameExecute(Sender: TObject);
var
  S: string;
  Index: Integer;
  Desc: string;
begin
  with lsbNiceNames do
  begin
    if ItemIndex < 0 then
      Exit;
    S := Items[ItemIndex];
  end;

  S := Copy(S, 1, Pos(' - ', S) - 1);
  Index := FSettings.NiceNameClass.IndexOf(UpperCase(S));
  if Index < 0 then
  begin
    ShowMessage(Format('Kan %s niet vinden', [QuotedStr(S)]));
    Exit;
  end;

  Desc := FSettings.NiceNameDesc[Index];
  if not TfrmEditNiceName.ExecuteEdit(S, Desc) then
    Exit;
  FSettings.NiceNameDesc[Index] := Desc;
end;

procedure TfrmSettings.actIgnoredUnits_AddExecute(Sender: TObject);
var
  S: string;
begin
  S := edtIgnoredUnits.Text;

  if (S > '') and (lsbIgnoredUnits.Items.IndexOf(S) < 0) then
  begin
    lsbIgnoredUnits.Items.Add(S);
    lsbIgnoredUnits.ItemIndex := lsbIgnoredUnits.Items.IndexOf(S);
  end;
end;

procedure TfrmSettings.actIgnoredUnits_DeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lsbIgnoredUnits.ItemIndex;
  if I >= 0 then
  begin
    lsbIgnoredUnits.Items.Delete(I);
    if lsbIgnoredUnits.Count > 0 then
      lsbIgnoredUnits.ItemIndex := Min(lsbIgnoredUnits.Count - 1, I - 1);
  end;
end;

procedure TfrmSettings.actIgnoredUnits_LoadExecute(Sender: TObject);
var
  Strings: TStringList;
begin
  with TOpenDialog.Create(Application) do
  try
    if not Execute then
      Exit;

    Strings := TStringList.Create;
    try
      Strings.LoadFromFile(FileName);
      lsbIgnoredUnits.Items.AddStrings(Strings);
    finally
      Strings.Free;
    end;
  finally
    Free;
  end;
end;

procedure TfrmSettings.actOKExecute(Sender: TObject);
begin
  Uitvoeren;
  Close;
end;

procedure TfrmSettings.actRegisteredClasses_AddExecute(Sender: TObject);
var
  S: string;
begin
  S := edtRegisteredClasses.Text;

  if (S > '') and (lsbRegisteredClasses.Items.IndexOf(S) < 0) then
  begin
    lsbRegisteredClasses.Items.Add(S);
    lsbRegisteredClasses.ItemIndex := lsbRegisteredClasses.Items.IndexOf(S);
  end;
end;

procedure TfrmSettings.actRegisteredClasses_DeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lsbRegisteredClasses.ItemIndex;
  if I >= 0 then
  begin
    lsbRegisteredClasses.Items.Delete(I);
    if lsbRegisteredClasses.Count > 0 then
      lsbRegisteredClasses.ItemIndex := Min(lsbRegisteredClasses.Count - 1, I - 1);
  end;
end;

procedure TfrmSettings.actRegisteredClasses_LoadExecute(Sender: TObject);
var
  Strings: TStringList;
begin
  with TOpenDialog.Create(Application) do
  try
    if not Execute then
      Exit;

    Strings := TStringList.Create;
    try
      Strings.LoadFromFile(FileName);
      lsbRegisteredClasses.Items.AddStrings(Strings);
    finally
      Strings.Free;
    end;
  finally
    Free;
  end;
end;

class procedure TfrmSettings.Execute;
begin
  with TfrmSettings.Create(Application) do
  try
    Init;
    try
      ShowModal;
    finally
      Final;
    end;
  finally
    Free;
  end;
end;

procedure TfrmSettings.Final;
begin
  FSettings.Free;
end;

procedure TfrmSettings.Init;
begin
  FCurrentTab := -1;
  FSettings := TSettings.Create;
  FSettings.Assign(TSettings.Instance);

  InitTabs;
  
  (*with FSettings do
  begin
    edtRunTimePasDir.Text := RunTimePasDir;
    edtDesignTimePasDir.Text := DesignTimePasDir;
    edtGeneratedDtxDir.Text := GeneratedDtxDir;
    edtRealDtxDir.Text := RealDtxDir;
    edtPackageDir.Text := PackageDir;
    chbOverwriteExisting.Checked := OverwriteExisting;
  end;*)
  pgcSettings.ActivePage := tshOutput;
  tbcOutputTypes.TabIndex := 0;
  FCurrentTab := 0;
  FCurrentFor := CDefault;

  edtDefaultNiceName.Text := FSettings.DefaultNiceName;
  lsbIgnoredUnits.Items.Assign(FSettings.UnitsStatus[usIgnored]);
  {lsbDocumentedUnits.Items.Assign(FSettings.DocumentedUnits);}
  lsbRegisteredClasses.Items.Assign(FSettings.RegisteredClasses);

  UpdateNiceNames;
  UpdateEnabled;
  UpdateFor;
  UpdateOutput;
  UpdateDirectives;
end;

procedure TfrmSettings.InitTabs;
var
  OutputType: TOutputType;
begin
  tbcOutputTypes.Tabs.BeginUpdate;
  try
    tbcOutputTypes.Tabs.Clear;
    for OutputType := Low(TOutputType) to High(TOutputType) do
      tbcOutputTypes.Tabs.Add(COutputTypeSection[OutputType]);
  finally
    tbcOutputTypes.Tabs.EndUpdate;
  end;
end;

procedure TfrmSettings.lsbForClick(Sender: TObject);
begin
  SaveOutput;

  with lsbFor do
    if ItemIndex < 0 then
      FCurrentFor := ''
    else
      FCurrentFor := Items[ItemIndex];

  UpdateOutput;
end;

procedure TfrmSettings.SaveDefaultNiceName;
begin
  FSettings.DefaultNiceName := edtDefaultNiceName.Text;
end;

procedure TfrmSettings.SaveDirectives;
begin
  FSettings.AcceptCompilerDirectives := lsbDirectives.Items;
end;

procedure TfrmSettings.SaveEnabled;
begin
  FSettings.OutputTypeEnabled[TOutputType(FCurrentTab)] :=
    chbEnabled.Checked;
end;

procedure TfrmSettings.SaveIgnoredUnits;
begin
  FSettings.UnitsStatus[usIgnored] := lsbIgnoredUnits.Items;
end;

procedure TfrmSettings.SaveOutput;
var
  S: string;
  Index: Integer;
begin
  if (FCurrentTab < 0) or (FCurrentFor = '') then
    Exit;

  S := FCurrentFor;

  if SameText(S, CDefault) then
  begin
    FSettings.OutputTypeDefaults[TOutputType(FCurrentTab)] := memOutput.Text;
    Exit;
  end;

  Index := FSettings.OutputTypeDesc[TOutputType(FCurrentTab)].IndexOf(S);
  if Index < 0 then
  begin
    ShowMessage(Format('Kan %s niet vinden', [QuotedStr(S)]));
    Exit;
  end;
  FSettings.OutputTypeStrings[TOutputType(FCurrentTab)][Index] :=
    memOutput.Text;
end;

procedure TfrmSettings.SaveRegisteredClasses;
begin
  FSettings.RegisteredClasses.Assign(lsbRegisteredClasses.Items);
end;

procedure TfrmSettings.tbcOutputTypesChange(Sender: TObject);
begin
  SaveOutput;
  SaveEnabled;
  FCurrentTab := tbcOutputTypes.TabIndex;
  FCurrentFor := CDefault;
  UpdateEnabled;
  UpdateFor;
  UpdateOutput;
end;

procedure TfrmSettings.Uitvoeren;
begin
  SaveOutput;
  SaveEnabled;
  SaveDefaultNiceName;
  SaveDirectives;
  SaveIgnoredUnits;
  SaveRegisteredClasses;

  with TSettings.Instance do
  begin
    Assign(FSettings);
    SaveSettings;
    SaveUnitStatus(usIgnored);
    SaveRegisteredClasses;
  end;
end;

procedure TfrmSettings.UpdateDirectives;
begin
  lsbDirectives.Items := FSettings.AcceptCompilerDirectives;
end;

procedure TfrmSettings.UpdateEnabled;
begin
  chbEnabled.Checked := FSettings.OutputTypeEnabled[TOutputType(FCurrentTab)];
end;

procedure TfrmSettings.UpdateFor;
begin
  with lsbFor.Items do
  begin
    BeginUpdate;
    try
      Clear;
      if FCurrentTab < 0 then
        Exit;
      Assign(FSettings.OutputTypeDesc[TOutputType(FCurrentTab)]);
      Add(CDefault);
    finally
      EndUpdate;
    end;
  end;
  with lsbFor do
    ItemIndex := Items.IndexOf(CDefault);
  FCurrentFor := CDefault;
end;

procedure TfrmSettings.UpdateNiceNames;
var
  I: Integer;
begin
  with lsbNiceNames.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to FSettings.NiceNameClass.Count - 1 do
        Add(Format('%s - %s', [FSettings.NiceNameClass[I],
          FSettings.NiceNameDesc[I]]));
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmSettings.UpdateOutput;
var
  S: string;
  Index: Integer;
begin
  if (FCurrentTab < 0) or (FCurrentFor = '') then
    Exit;

  S := FCurrentFor;

  if SameText(S, CDefault) then
  begin
    memOutput.Text :=
      FSettings.OutputTypeDefaults[TOutputType(FCurrentTab)];
    Exit;
  end;

  Index := FSettings.OutputTypeDesc[TOutputType(FCurrentTab)].IndexOf(S);
  if Index < 0 then
  begin
    ShowMessage(Format('Kan %s niet vinden', [QuotedStr(S)]));
    Exit;
  end;
  memOutput.Text :=
    FSettings.OutputTypeStrings[TOutputType(FCurrentTab)][Index];
end;

end.
