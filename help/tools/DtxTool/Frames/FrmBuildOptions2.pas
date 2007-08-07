unit FrmBuildOptions2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit,

  HelpBuild, HelpBuildData;

type
  TFrameBuildOptions2 = class(TFrame)
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
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameBuildOptions2;
  end;

implementation

{$R *.dfm}

class function TFrameBuildOptions2.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameBuildOptions2;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameBuildOptions2.edtDMCCFileNameExit(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameBuildOptions2.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.DMCCFileName := edtDMCCFileName.Text;
end;

procedure TFrameBuildOptions2.Init;
begin
  SetOptions;
end;

procedure TFrameBuildOptions2.SetOptions;
begin
  FSettingOptions := True;
  try
    edtDMCCFileName.Text := HelpBuilder.Data.DMCCFileName;
  finally
    FSettingOptions := False;
  end;
end;

end.

