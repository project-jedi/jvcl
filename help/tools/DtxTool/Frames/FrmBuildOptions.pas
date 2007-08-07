unit FrmBuildOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Mask, JvExMask, JvToolEdit, JvSpin,

  HelpBuild, HelpBuildData;

type
  TFrameBuildOptions = class(TFrame)
    spnMaxDtx: TJvSpinEdit;
    Label2: TLabel;
    spnMinPerc: TJvSpinEdit;
    Label1: TLabel;
    rbtMaxDtx: TRadioButton;
    rbtPercDtx: TRadioButton;
    rbtSpecificDtx: TRadioButton;
    lsbSpecificFiles: TListBox;
    btnAddDtx: TButton;
    btnDeleteDtx: TButton;
    edtJVCLMajorVersion: TEdit;
    edtJVCLMinorVersion: TEdit;
    Label3: TLabel;
    procedure chbMaxDtxClick(Sender: TObject);
    procedure spnMaxDtxChange(Sender: TObject);
    procedure spnMaxDtxClick(Sender: TObject);
    procedure spnMinPercChange(Sender: TObject);
    procedure spnMinPercClick(Sender: TObject);
    procedure HandleChanged(Sender: TObject);
    procedure btnAddDtxClick(Sender: TObject);
    procedure btnDeleteDtxClick(Sender: TObject);
    procedure changeEvent(Sender: TObject);
    procedure exitEvent(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;

    procedure AddFiles(AFiles: TStrings);
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameBuildOptions;
  end;

implementation

uses
  Core, JVCLHelpUtils;

{$R *.dfm}
//=== { TFrameBuildOptions } =================================================

class function TFrameBuildOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameBuildOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameBuildOptions.chbMaxDtxClick(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.JVCLMajorVersion := edtJVCLMajorVersion.Text;
  HelpBuilder.Data.JVCLMinorVersion := edtJVCLMinorVersion.Text;
  HelpBuilder.Data.MaxFilesInDox := spnMaxDtx.AsInteger;
  if rbtMaxDtx.Checked then
    HelpBuilder.Data.DtxSelectionKind := dskMax;
  if rbtPercDtx.Checked then
    HelpBuilder.Data.DtxSelectionKind := dskPercentage;
  if rbtSpecificDtx.Checked then
    HelpBuilder.Data.DtxSelectionKind := dskSpecific;
  HelpBuilder.Data.MinPercComplete := spnMinPerc.AsInteger;
  HelpBuilder.Data.SpecificDtxFiles := lsbSpecificFiles.Items;
end;

procedure TFrameBuildOptions.Init;
begin
  SetOptions;
end;

procedure TFrameBuildOptions.SetOptions;
begin
  FSettingOptions := True;
  try
    edtJVCLMajorVersion.Text := HelpBuilder.Data.JVCLMajorVersion;
    edtJVCLMinorVersion.Text := HelpBuilder.Data.JVCLMinorVersion;
    spnMaxDtx.AsInteger := HelpBuilder.Data.MaxFilesInDox;
    spnMinPerc.AsInteger := HelpBuilder.Data.MinPercComplete;
    rbtMaxDtx.Checked := HelpBuilder.Data.DtxSelectionKind = dskMax;
    rbtPercDtx.Checked := HelpBuilder.Data.DtxSelectionKind = dskPercentage;
    rbtSpecificDtx.Checked := HelpBuilder.Data.DtxSelectionKind = dskSpecific;
    lsbSpecificFiles.Items := HelpBuilder.Data.SpecificDtxFiles;
  finally
    FSettingOptions := False;
  end;
end;

procedure TFrameBuildOptions.spnMaxDtxChange(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.spnMaxDtxClick(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.spnMinPercChange(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.spnMinPercClick(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.HandleChanged(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.btnAddDtxClick(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
  try
    Title := 'Select dtx files';
    Options := [ofNoChangeDir, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist,
      ofEnableSizing];
    DefaultExt := 'dtx';
    Filter := 'Dtx files (*.dtx)|*.DTX';
    InitialDir := HelpBuilder.Data.HelpDir;
    if Execute then
      AddFiles(Files);
  finally
    Free;
  end;
end;

procedure TFrameBuildOptions.btnDeleteDtxClick(Sender: TObject);
var
  AItemIndex: Integer;
begin
  with lsbSpecificFiles do
    if ItemIndex >= 0 then
    begin
      AItemIndex := ItemIndex;
      Items.Delete(ItemIndex);
      if AItemIndex >= Items.Count then
        AItemIndex := Items.Count - 1;
      ItemIndex := AItemIndex;
    end;
end;

procedure TFrameBuildOptions.AddFiles(AFiles: TStrings);
var
  I: Integer;
  AFileName: string;
begin
  lsbSpecificFiles.Items.BeginUpdate;
  try
    for I := 0 to AFiles.Count - 1 do
    begin
      AFileName := ExtractFileName(ChangeFileExt(AFiles[i], ''));
      if lsbSpecificFiles.Items.IndexOf(AFileName) < 0 then
        lsbSpecificFiles.Items.Add(AFileName);
    end;

    GetOptions;
  finally
    lsbSpecificFiles.Items.EndUpdate;
  end;
end;

procedure TFrameBuildOptions.changeEvent(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions.exitEvent(Sender: TObject);
begin
  GetOptions;
end;

end.

