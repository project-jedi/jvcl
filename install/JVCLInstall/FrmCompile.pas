unit FrmCompile;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls;

type
  TFormCompile = class(TForm)
    PanelClient: TPanel;
    BtnOk: TButton;
    BevelProject: TBevel;
    BevelStatus: TBevel;
    BevelCurrentLine: TBevel;
    BevelHints: TBevel;
    LblProject: TLabel;
    LblStatusCaption: TLabel;
    BevelTotalLines: TBevel;
    LblCurrentLineCaption: TLabel;
    LblCurrentLine: TLabel;
    LblTotalLinesCaption: TLabel;
    LblTotalLines: TLabel;
    BevelWarnings: TBevel;
    BevelErrors: TBevel;
    LblHintsCaption: TLabel;
    LblHints: TLabel;
    LblWarningsCaption: TLabel;
    LblWarnings: TLabel;
    LblErrorsCaption: TLabel;
    LblErrors: TLabel;
    LblProjectCaption: TLabel;
    LblStatus: TLabel;
    LblErrorReason: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FHints: Cardinal;
    FWarnings: Cardinal;
    FErrors: Cardinal;
    FCurrentLine: Cardinal;
    FTotalLines: Cardinal;
    FCurFilename: string;
    procedure SetCurrentLine(Line: Cardinal);
  public
    procedure Init(const ProjectName: string; Clear: Boolean = True);
    procedure Compiling(const Filename: string);
    procedure Linking(const Filename: string);
    procedure Done(const ErrorReason: string = '');

    procedure IncHint;
    procedure IncWarning;
    procedure IncError;

    property Hints: Cardinal read FHints;
    property Warnings: Cardinal read FWarnings;
    property Errors: Cardinal read FErrors;
    property CurrentLine: Cardinal read FCurrentLine write SetCurrentLine;
  end;

var
  FormCompile: TFormCompile;

implementation

uses
  FileCtrl;

{$R *.dfm}

resourcestring
  RsPreparing = 'Preparing...';
  RsCompiling = 'Compiling';
  RsLinking = 'Linking';
  RsDone = 'Done';
  RsThereAreErrors = 'There are errors.';
  RsThereAreWarnings = 'There are warnings.';
  RsThereAreHints = 'There are hints.';
  RsCompiled = 'compiled.';

{ TFormCompile }

procedure TFormCompile.BtnOkClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;

procedure TFormCompile.Init(const ProjectName: string; Clear: Boolean);
begin
  Tag := 0;
  LblProject.Caption := MinimizeName(ProjectName, LblProject.Canvas, LblProject.ClientWidth);

  LblStatusCaption.Font.Style := [];
  LblStatus.Font.Style := [];

  if Clear then
  begin
    FHints := 0;
    FErrors := 0;
    FWarnings := 0;
    FTotalLines := 0;
  end;
  FCurrentLine := 0;
  FCurFilename := '';

  LblHints.Caption := IntToStr(FHints);
  LblWarnings.Caption := IntToStr(FWarnings);
  LblErrors.Caption := IntToStr(FErrors);
  LblCurrentLine.Caption := IntToStr(FCurrentLine);
  LblTotalLines.Caption := IntToStr(FTotalLines);
  LblStatusCaption.Caption := RsPreparing;
  LblStatus.Caption := '';

  BtnOk.Enabled := False;

  Show;
end;

procedure TFormCompile.Compiling(const Filename: string);
begin
  if Filename <> FCurFilename then
  begin
    FCurFilename := Filename;
    FTotalLines := FTotalLines + FCurrentLine;
    CurrentLine := 0; // updates total lines and current lines
    LblStatusCaption.Caption := RsCompiling + ':';
    LblStatus.Caption := ExtractFileName(Filename);
    Application.ProcessMessages;
  end;
end;

procedure TFormCompile.Linking(const Filename: string);
begin
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblStatusCaption.Caption := RsLinking + ':';
  LblStatus.Caption := ExtractFileName(Filename);
  Application.ProcessMessages;
end;

procedure TFormCompile.Done(const ErrorReason: string);
begin
  FCurFilename := '';
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblErrorReason.Caption := ErrorReason;
  LblErrorReason.Visible := ErrorReason <> '';
  LblStatusCaption.Font.Style := [fsBold];
  LblStatus.Font.Style := [fsBold];
  LblStatusCaption.Caption := RsDone + ':';

  if FErrors > 0 then
    LblStatus.Caption := RsThereAreErrors
  else if FWarnings > 0 then
    LblStatus.Caption := RsThereAreWarnings
  else if FHints > 0 then
    LblStatus.Caption := RsThereAreHints
  else
    LblStatus.Caption := RsCompiled;
  BtnOk.Enabled := ErrorReason <> '';
  if ErrorReason <> '' then
  begin
    Hide;
    ShowModal;
  end;
end;

procedure TFormCompile.IncError;
begin
  Inc(FErrors);
  LblErrors.Caption := IntToStr(FErrors);
  Application.ProcessMessages;
end;

procedure TFormCompile.IncHint;
begin
  Inc(FHints);
  LblHints.Caption := IntToStr(FHints);
  Application.ProcessMessages;
end;

procedure TFormCompile.IncWarning;
begin
  Inc(FWarnings);
  LblWarnings.Caption := IntToStr(FWarnings);
  Application.ProcessMessages;
end;

procedure TFormCompile.SetCurrentLine(Line: Cardinal);
begin
  FCurrentLine := Line;
  LblCurrentLine.Caption := IntToStr(Line);
  LblTotalLines.Caption := IntToStr(FTotalLines + FCurrentLine);
  Application.ProcessMessages;
end;

procedure TFormCompile.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Tag = 1;
end;

end.
