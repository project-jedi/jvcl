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
  Dialogs, JvUrlListGrabber, JvUrlGrabbers, StdCtrls, JvComponent;

type
  TfrmMain = class(TForm)
    btnGoDynamic: TButton;
    julGrabber: TJvUrlListGrabber;
    memExplanation: TMemo;
    grbDynamic: TGroupBox;
    grbDesign: TGroupBox;
    memUrls: TMemo;
    lblExpl: TLabel;
    btnGoDesign: TButton;
    btnClear: TButton;
    btnStop: TButton;
    procedure btnGoDynamicClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnGoDesignClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    { Private declarations }
    grabber : TJvUrlListGrabber;
  public
    { Public declarations }
    procedure grabberConnectionClosed(Sender : TJvUrlListGrabber; Grabber : TJvUrlGrabber);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
uses JvTypes;

procedure TfrmMain.btnGoDynamicClick(Sender: TObject);
begin
  grabber := TJvUrlListGrabber.Create(Self);
  grabber.URLs.Add(InputBox('Url to grab', 'Please give a url to grab', 'http://jvcl.sf.net/'));
  with grabber.Grabbers[0] do
  begin
    OutputMode := omFile;
    FileName := ExtractFilePath(Application.Exename)+'\test.txt';
    Start;
  end;
  grabber.OnConnectionClosed := grabberConnectionClosed;
end;

procedure TfrmMain.grabberConnectionClosed(Sender: TJvUrlListGrabber; Grabber : TJvUrlGrabber);
begin
  Application.messagebox('Finished', '', 0);
  grabber.Free;
end;

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
    julGrabber.URLs.Clear;
    julGrabber.Cleanup;
    julGrabber.URLs.Assign(memUrls.Lines);
    for i := 0 to julGrabber.URLs.Count -1 do
    begin
      with julGrabber.Grabbers[i] do
      begin
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

end.
