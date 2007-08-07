unit FrmDoxCompileOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit,

  HelpBuild, HelpBuildData;

type
  TFrameDoxCompileOptions = class(TFrame)
    Label3: TLabel;
    edtDMCCFileName: TJvFilenameEdit;
    procedure edtDMCCFileNameExit(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameDoxCompileOptions;
  end;

implementation

{$R *.dfm}

class function TFrameDoxCompileOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameDoxCompileOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameDoxCompileOptions.edtDMCCFileNameExit(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameDoxCompileOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.DMCCFileName := edtDMCCFileName.Text;
end;

procedure TFrameDoxCompileOptions.Init;
begin
  SetOptions;
end;

procedure TFrameDoxCompileOptions.SetOptions;
begin
  FSettingOptions := True;
  try
    edtDMCCFileName.Text := HelpBuilder.Data.DMCCFileName;
  finally
    FSettingOptions := False;
  end;
end;

end.

