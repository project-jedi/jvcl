{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
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
    lblExplanations: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure btnStartPreviewClick(Sender: TObject);
    procedure btnStopPreviewClick(Sender: TObject);
    procedure btnCaptureClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnFormatClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnCompressionClick(Sender: TObject);
  public
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
