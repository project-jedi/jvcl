unit CheckVersionInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, glSpeedButton, glWizardHeader, glShadow,
  ComCtrls, glSmallFontsDefence;

type
  TfCheckVersionInfo = class(TForm)
    glWizardHeader: TglWizardHeader;
    sbNext: TglSpeedButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eCurentVersion: TEdit;
    eCurentVersionDate: TEdit;
    eLastVersion: TEdit;
    eLastVersionDate: TEdit;
    Label5: TLabel;
    Shape1: TShape;
    lURL: TLabel;
    glSmallFontsDefence1: TglSmallFontsDefence;
    Label7: TLabel;
    reComments: TRichEdit;
    glShadow1: TglShadow;
    procedure sbNextClick(Sender: TObject);
    procedure lURLClick(Sender: TObject);
  private
  public
    procedure Execute(WinControl: TWinControl);
  end;

var
  fCheckVersionInfo: TfCheckVersionInfo;

implementation
uses ShellApi, glHTTPVersionInfo, globCon;
{$R *.DFM}

{ TfCheckVersionInfo }

procedure TfCheckVersionInfo.Execute(WinControl: TWinControl);
var
  VersionInfo: TglHTTPVersionInfo;
begin
  eCurentVersion.Text := globCon.APP_VERSION;
  eCurentVersionDate.Text := globCon.APP_DATE;

  VersionInfo := TglHTTPVersionInfo.Create(self);
  try
    VersionInfo.VersionDataURL := 'http://shop.biblio-globus.ru/cpr/VersionInfo/SiteBuilder.htm';
    if VersionInfo.GetVersionInfo(WinControl) and (VersionInfo.Version > eCurentVersion.Text) then
    begin
      eLastVersion.Text := VersionInfo.Version;
      eLastVersionDate.Text := VersionInfo.Date;
      if VersionInfo.ProgramURL <> '' then
        lURL.Caption := VersionInfo.ProgramURL;
      reComments.Text := VersionInfo.Comments;

      ShowModal;
    end else
      Application.MessageBox('Данных о новой версии программы не найдено.', 'SiteBuilder',  MB_OK + MB_ICONINFORMATION);

  finally
    VersionInfo.Free;
  end;

end;

procedure TfCheckVersionInfo.sbNextClick(Sender: TObject);
begin
  ModalResult := mrOK
end;

procedure TfCheckVersionInfo.lURLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(lURL.Caption), nil, '', SW_SHOW);
end;

end.
