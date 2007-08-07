unit FrmPostBuildOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Mask, JvExMask, JvToolEdit, JvSpin,

  HelpBuild, HelpBuildData;

type
  TFramePostBuildOptions = class(TFrame)
    edtHHCFileName: TJvFilenameEdit;
    Label3: TLabel;
    Label1: TLabel;
    edtHCRTFFileName: TJvFilenameEdit;
    procedure spnMinPercClick(Sender: TObject);
    procedure edtHHCFileNameExit(Sender: TObject);
    procedure edtHCRTFFileNameExit(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFramePostBuildOptions;
  end;

implementation

uses
  Core;

{$R *.dfm}
//=== { TFramePostBuildOptions } =============================================

{ TFrame1 }

class function TFramePostBuildOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFramePostBuildOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFramePostBuildOptions.edtHCRTFFileNameExit(Sender: TObject);
begin
  GetOptions;
end;

procedure TFramePostBuildOptions.edtHHCFileNameExit(Sender: TObject);
begin
  GetOptions;
end;

procedure TFramePostBuildOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.HHCFileName := edtHHCFileName.Text;
  HelpBuilder.Data.HCRTFFileName := edtHCRTFFileName.Text;
end;

procedure TFramePostBuildOptions.Init;
begin
  SetOptions;
end;

procedure TFramePostBuildOptions.SetOptions;
begin
  FSettingOptions := True;
  try
    edtHHCFileName.Text := HelpBuilder.Data.HHCFileName;
    edtHCRTFFileName.Text := HelpBuilder.Data.HCRTFFileName;
  finally
    FSettingOptions := False;
  end;
end;

procedure TFramePostBuildOptions.spnMinPercClick(Sender: TObject);
begin
  GetOptions;
end;

end.
