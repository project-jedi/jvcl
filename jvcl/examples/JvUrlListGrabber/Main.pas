{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvUrlListGrabber, StdCtrls, JvComponent, ComCtrls;

type
  TfrmMain = class(TForm)
    julGrabber: TJvUrlListGrabber;
    memExplanation: TMemo;
    grbDesign: TGroupBox;
    memUrls: TMemo;
    lblExpl: TLabel;
    btnGoDesign: TButton;
    btnClear: TButton;
    btnStop: TButton;
    StatusBar1: TStatusBar;
    procedure btnClearClick(Sender: TObject);
    procedure btnGoDesignClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure julGrabberConnectedToServer(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber);
    procedure julGrabberStatusChange(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber);
    procedure julGrabberError(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber; ErrorMsg: String);
    procedure julGrabberProgress(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber; Position, TotalSize: Int64;
      Url: String; var Continue: Boolean);
    procedure julGrabberConnectionClosed(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber);
    procedure julGrabberRequestSent(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber);
    procedure julGrabberSendingRequest(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber);
    procedure julGrabberDoneFile(Sender: TJvUrlListGrabber;
      Grabber: TJvCustomUrlGrabber; FileName: String; FileSize: Integer;
      Url: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
uses JvTypes;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  memUrls.Lines.Clear;
end;

procedure TfrmMain.btnGoDesignClick(Sender: TObject);
var
  i : Integer;
begin
  if memUrls.Lines.Count = 0 then
  begin
    Application.MessageBox('Url List cannot be empty',
                           'Error in Url List',
                           MB_ICONERROR);
  end
  else
  begin
    memExplanation.Lines.Clear;
    julGrabber.URLs.Clear;
    julGrabber.Cleanup;
    for i := 0 to memUrls.Lines.Count -1 do
      if memUrls.Lines[i] <> '' then
        julGrabber.URLs.Add(memUrls.Lines[i]);
    for i := 0 to julGrabber.URLs.Count -1 do
    begin
      with julGrabber.Grabbers[i] do
      begin
        Id := i;
        OutputMode := omFile;
        FileName := ExtractFilePath(Application.ExeName) + '\result' + IntToStr(i) + '.txt';
      end;
    end;
    julGrabber.StartAll;
  end;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  julGrabber.StopAll;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  memExplanation.WordWrap := true;
end;

procedure TfrmMain.julGrabberConnectedToServer(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Connected to server',[Grabber.Id]));
end;

procedure TfrmMain.julGrabberStatusChange(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Status change',[Grabber.Id]));
end;

procedure TfrmMain.julGrabberError(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber; ErrorMsg: String);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Error: %s',[Grabber.Id, ErrorMsg]));
end;

procedure TfrmMain.julGrabberProgress(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber; Position, TotalSize: Int64; Url: String;
  var Continue: Boolean);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Url: %s, Position: %d of %d',[Grabber.Id, Url, Position, TotalSize]));
end;

procedure TfrmMain.julGrabberConnectionClosed(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Connection closed',[Grabber.Id]));
end;

procedure TfrmMain.julGrabberRequestSent(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Request sent',[Grabber.Id]));
end;

procedure TfrmMain.julGrabberSendingRequest(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Sending request',[Grabber.Id]));
end;

procedure TfrmMain.julGrabberDoneFile(Sender: TJvUrlListGrabber;
  Grabber: TJvCustomUrlGrabber; FileName: String; FileSize: Integer;
  Url: String);
begin
  memExplanation.Lines.Add(Format('Grabber %d: Done file %s of size %d from %s',[Grabber.Id, FileName, FileSize, Url]));
end;

end.
