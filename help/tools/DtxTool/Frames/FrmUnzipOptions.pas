unit FrmUnzipOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Mask, JvExMask, JvToolEdit, JvSpin,

  HelpBuild, HelpBuildData;

type
  TFrameUnzipOptions = class(TFrame)
    Label2: TLabel;
    Label1: TLabel;
    edtZipFileName: TJvFilenameEdit;
    edtUnzipExec: TJvFilenameEdit;
    procedure chbMaxDtxClick(Sender: TObject);
    procedure edtZipFileNameExit(Sender: TObject);
    procedure edtUnzipExecExit(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
//    FLimitDtx: Boolean;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameUnzipOptions;
  end;

implementation

uses
  Core;

{$R *.dfm}

//=== { TFrameZipOptions } =================================================

class function TFrameUnzipOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameUnzipOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameUnzipOptions.chbMaxDtxClick(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameUnzipOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.OnlineHelpZipFileName := edtZipFileName.Text;
  HelpBuilder.Data.WZUnzipFileName := edtUnzipExec.Text;
end;

procedure TFrameUnzipOptions.Init;
begin
  SetOptions;
end;

procedure TFrameUnzipOptions.SetOptions;
begin
  FSettingOptions := True;
  try
    edtZipFileName.Text := HelpBuilder.Data.OnlineHelpZipFileName;
    edtUnzipExec.Text := HelpBuilder.Data.WZUnzipFileName;
  finally
    FSettingOptions := False;
  end;
end;

procedure TFrameUnzipOptions.edtZipFileNameExit(Sender: TObject);
begin
  GetOptions;
end;

procedure TFrameUnzipOptions.edtUnzipExecExit(Sender: TObject);
begin
  GetOptions;
end;

end.

