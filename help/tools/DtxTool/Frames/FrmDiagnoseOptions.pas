unit FrmDiagnoseOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Mask, JvExMask, JvToolEdit, JvSpin,

  HelpBuild, HelpBuildData, ComCtrls, JvMaskEdit, JvCheckedMaskEdit,
  JvDatePickerEdit;

type
  TFrameDiagnoseOptions = class(TFrame)
    Label1: TLabel;
    edtDictionary: TJvFilenameEdit;
    Label2: TLabel;
    edtAllWords: TJvFilenameEdit;
    Label3: TLabel;
    edtInvalidWords: TJvFilenameEdit;
    Label4: TLabel;
    edtAllIdentifiers: TJvFilenameEdit;
    Label5: TLabel;
    edtDupIdentifiers: TJvFilenameEdit;
    procedure ChangeEvent(Sender: TObject);
    procedure EditExit(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameDiagnoseOptions;
  end;

implementation

uses
  Core;

{$R *.dfm}

//=== { TFrameZipOptions } =================================================

class function TFrameDiagnoseOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameDiagnoseOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameDiagnoseOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.DtxWordsFileName := edtAllWords.Text;
  HelpBuilder.Data.DtxIdentifiersFileName := edtAllIdentifiers.Text;
  HelpBuilder.Data.DictionaryFileName := edtDictionary.Text;
  HelpBuilder.Data.DtxInvalidWordsFileName := edtInvalidWords.Text;
  HelpBuilder.Data.DtxDuplicateIdentifiersFileName := edtDupIdentifiers.Text;
end;

procedure TFrameDiagnoseOptions.Init;
begin
  SetOptions;
end;

procedure TFrameDiagnoseOptions.SetOptions;
begin
  FSettingOptions := True;
  try
    edtAllWords.Text := HelpBuilder.Data.DtxWordsFileName;
    edtAllIdentifiers.Text := HelpBuilder.Data.DtxIdentifiersFileName;

    edtDictionary.Text := HelpBuilder.Data.DictionaryFileName;
    edtInvalidWords.Text := HelpBuilder.Data.DtxInvalidWordsFileName;
    edtDupIdentifiers.Text := HelpBuilder.Data.DtxDuplicateIdentifiersFileName;
  finally
    FSettingOptions := False;
  end;
end;

procedure TFrameDiagnoseOptions.ChangeEvent(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameDiagnoseOptions.EditExit(Sender: TObject);
begin
  GetOptions;
end;

end.

