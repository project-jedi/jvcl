unit OptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, JVCLConvertUtils;

type
  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkBackup: TCheckBox;
    chkWholeWords: TCheckBox;
    chkReplaceFilenames: TCheckBox;
    chkSimulate: TCheckBox;
    Label1: TLabel;
    cbFileMasks: TComboBox;
    btnDelete: TButton;
    ActionList1: TActionList;
    acDeleteMask: TAction;
    acAddMask: TAction;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    edMaskName: TEdit;
    Label3: TLabel;
    edMask: TEdit;
    btnAddMask: TButton;
    chkIgnoreInsideStrings: TCheckBox;
    chkIgnoreInsideComments: TCheckBox;
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acDeleteMaskExecute(Sender: TObject);
    procedure acAddMaskExecute(Sender: TObject);
  private
    { Private declarations }
    function FindPartial(const S: string): integer;
    function ExtractFilemask(const S, default: string): string;
  public
    { Public declarations }
    class function Edit(Options: TAppOptions): Boolean;
  end;

implementation

{$R *.dfm}


procedure TfrmOptions.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acDeleteMask.Enabled := cbFileMasks.ItemIndex >= 0;
  acAddMask.Enabled := (edMaskName.Text <> '') and (edMask.Text <> '');
end;

procedure TfrmOptions.acDeleteMaskExecute(Sender: TObject);
var
  i: integer;
begin
  i := cbFileMasks.ItemIndex;
  cbFileMasks.Items.Delete(cbFileMasks.ItemIndex);
  cbFileMasks.ItemIndex := i;
  if cbFileMasks.ItemIndex = - 1 then
    cbFileMasks.ItemIndex := i - 1;
end;

class function TfrmOptions.Edit(Options: TAppOptions): Boolean;
var
  frmOptions: TfrmOptions;
  S: string;
  i: integer;
begin
  frmOptions := Self.Create(Application);
  with frmOptions do
    try
      chkBackup.Checked := Options.Backup;
      chkWholeWords.Checked := Options.WholeWords;
      chkReplaceFilenames.Checked := Options.ReplaceFilenames;
      chkSimulate.Checked := Options.Simulate;
      chkIgnoreInsideStrings.Checked := Options.IgnoreInsideStrings;
      chkIgnoreInsideComments.Checked := Options.IgnoreInsideComments;
      cbFileMasks.Items.Text := Options.Filemasks;
      S := Format('Current (%s)', [Options.FileMask]);
      cbFileMasks.ItemIndex := cbFileMasks.Items.Add(S);
      Result := ShowModal = mrOK;
      if Result then
      begin
        i := cbFileMasks.Items.IndexOf(S);
        if i > - 1 then
          cbFileMasks.Items.Delete(i);
        Options.Backup := chkBackup.Checked;
        Options.WholeWords := chkWholeWords.Checked;
        Options.ReplaceFilenames := chkReplaceFilenames.Checked;
        Options.Simulate := chkSimulate.Checked;
        Options.Filemasks := cbFileMasks.Items.Text;
        Options.FileMask := ExtractFilemask(cbFileMasks.Text, Options.FileMask);
        Options.IgnoreInsideStrings := chkIgnoreInsideStrings.Checked;
        Options.IgnoreInsideComments := chkIgnoreInsideComments.Checked;
      end;
    finally
      Free;
    end;
end;

function TfrmOptions.FindPartial(const S: string): integer;
begin
  for Result := 0 to cbFileMasks.Items.Count - 1 do
    if Pos(AnsiLowerCase(S), AnsiLowerCase(cbFileMasks.Items[Result])) > 0 then
      Exit;
  Result := 0;
end;

function TfrmOptions.ExtractFilemask(const S, default: string): string;
begin
  Result := Copy(S, Pos('(', S) + 1, MaxInt);
  Result := Copy(Result, 1, Pos(')', Result) - 1);
  if Result = '' then
    Result := default;
end;

procedure TfrmOptions.acAddMaskExecute(Sender: TObject);
begin
  cbFileMasks.ItemIndex := cbFileMasks.Items.Add(
    Format('%s (%s)', [edMaskName.Text, StringReplace(edMask.Text, ',', ';', [rfReplaceAll])]));
end;

end.
