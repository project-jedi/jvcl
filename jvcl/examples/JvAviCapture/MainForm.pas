unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvAVICapture;

type
  TfrmMain = class(TForm)
    btnConnect: TButton;
    btnStartPreview: TButton;
    AviCap: TJvAviCapture;
    btnStopPreview: TButton;
    btnCapture: TButton;
    btnSource: TButton;
    btnFormat: TButton;
    btnCompression: TButton;
    btnDisplay: TButton;
    memExplanations: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnStartPreviewClick(Sender: TObject);
    procedure btnStopPreviewClick(Sender: TObject);
    procedure btnCaptureClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnFormatClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnCompressionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  try
    AviCap.DriverIndex := 0;
  except
    on EInvalidDriverIndexError do
      ShowMessage('No device found. Verify your connection and configuration.');
  end;
end;

procedure TfrmMain.btnStartPreviewClick(Sender: TObject);
begin
  AviCap.Previewing := true;
end;

procedure TfrmMain.btnStopPreviewClick(Sender: TObject);
begin
  AviCap.StopPreview;
end;

procedure TfrmMain.btnCaptureClick(Sender: TObject);
begin
  AviCap.CaptureSettings.ConfirmCapture := true;
  AviCap.CaptureSettings.LimitEnabled := true;
  AviCap.CaptureSettings.TimeLimit := 3;
  AviCap.StartCapture;
end;

procedure TfrmMain.btnSourceClick(Sender: TObject);
begin
  if not AviCap.ShowDialog(vdSource) then
    Application.MessageBox('Your driver doesn''t provide this dialog '+
                           'or you are not connected to a driver',
                           'Unable to show dialog',
                           MB_ICONINFORMATION);
end;

procedure TfrmMain.btnFormatClick(Sender: TObject);
begin
  if not AviCap.ShowDialog(vdFormat) then
    Application.MessageBox('Your driver doesn''t provide this dialog '+
                           'or you are not connected to a driver',
                           'Unable to show dialog',
                           MB_ICONINFORMATION);
end;

procedure TfrmMain.btnDisplayClick(Sender: TObject);
begin
  if not AviCap.ShowDialog(vdDisplay) then
    Application.MessageBox('Your driver doesn''t provide this dialog '+
                           'or you are not connected to a driver',
                           'Unable to show dialog',
                           MB_ICONINFORMATION);
end;

procedure TfrmMain.btnCompressionClick(Sender: TObject);
begin
  if not AviCap.ShowDialog(vdCompression) then
    Application.MessageBox('Your driver doesn''t provide this dialog '+
                           'or you are not connected to a driver',
                           'Unable to show dialog',
                           MB_ICONINFORMATION);
end;

end.
