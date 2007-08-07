unit FrmZipOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Mask, JvExMask, JvToolEdit, JvSpin,

  HelpBuild, HelpBuildData, ComCtrls, JvMaskEdit, JvCheckedMaskEdit,
  JvDatePickerEdit;

type
  TFrameZipOptions = class(TFrame)
    Label1: TLabel;
    edtZipExec: TJvFilenameEdit;
    Label2: TLabel;
    spnMaxInZip: TJvSpinEdit;
    Label3: TLabel;
    edtDate: TJvDatePickerEdit;
    edtTime: TDateTimePicker;
    rbtDtxFilterTimestamp: TRadioButton;
    rbtDtxFilterAll: TRadioButton;
    procedure ChangeEvent(Sender: TObject);
  private
    FHelpBuilder: THelpBuilder;
    FSettingOptions: Boolean;
    procedure Init;
    procedure GetOptions;
    procedure SetOptions;
  protected
    property HelpBuilder: THelpBuilder read FHelpBuilder;
  public
    class function Build(AHelpBuilder: THelpBuilder; Client: TWinControl): TFrameZipOptions;
  end;

implementation

uses
  Core, FrmUnzipOptions;

{$R *.dfm}

//=== { TFrameZipOptions } =================================================

class function TFrameZipOptions.Build(AHelpBuilder: THelpBuilder;
  Client: TWinControl): TFrameZipOptions;
begin
  Result := Self.Create(Client);
  //  AHelpBuilder.PackageInstaller.Translate(Result);
  Result.FHelpBuilder := AHelpBuilder;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameZipOptions.GetOptions;
begin
  if FSettingOptions then
    Exit;

  HelpBuilder.Data.MaxFilesInZip := spnMaxInZip.AsInteger;
  HelpBuilder.Data.WZZipFileName := edtZipExec.Text;
  if rbtDtxFilterAll.Checked then
    HelpBuilder.Data.MinDateForZippedFiles := 0
  else
    HelpBuilder.Data.MinDateForZippedFiles := Trunc(edtDate.Date) + Frac(edtTime.Time);
end;

procedure TFrameZipOptions.Init;
begin
  SetOptions;
end;

procedure TFrameZipOptions.SetOptions;
var
  HasTimestamp: Boolean;
begin
  FSettingOptions := True;
  try
    spnMaxInZip.AsInteger := HelpBuilder.Data.MaxFilesInZip;
    edtZipExec.Text := HelpBuilder.Data.WZZipFileName;
    HasTimestamp := HelpBuilder.Data.MinDateForZippedFiles > 0;
    rbtDtxFilterAll.Checked := not HasTimestamp;
    rbtDtxFilterTimestamp.Checked := HasTimestamp;

    if HasTimestamp then
    begin
      edtDate.Date := Trunc(HelpBuilder.Data.MinDateForZippedFiles);
      edtTime.Time := Frac(HelpBuilder.Data.MinDateForZippedFiles);
    end
    else
    begin
      edtDate.Date := Trunc(Now);
      edtTime.Time := Frac(Now);
    end;
  finally
    FSettingOptions := False;
  end;
end;

procedure TFrameZipOptions.ChangeEvent(Sender: TObject);
begin
  GetOptions;
end;

end.

