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

unit DSAExamplesProgressDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvProgressBar;

type
  TMsgProgressCallback = function(const Position, Max: Integer): Integer of object;

  TfrmDSAExamplesProgressDlg = class(TForm)
    lblItemCount: TLabel;
    edItems: TEdit;
    lblNote: TLabel;
    btnStart: TButton;
    btnCancel: TButton;
    pbProgress: TJvProgressBar;
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    DoMessage: TMsgProgressCallback;
  end;

procedure DoProgress(const MessageCallback: TMsgProgressCallback);

implementation

uses
  JvDSADialogs;

{$R *.DFM}

procedure DoProgress(const MessageCallback: TMsgProgressCallback);
begin
  with TfrmDSAExamplesProgressDlg.Create(Screen.ActiveForm) do
  try
    DoMessage := MessageCallback;
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmDSAExamplesProgressDlg }

procedure TfrmDSAExamplesProgressDlg.FormResize(Sender: TObject);
var
  TotCtrlW: Integer;
  Distance: Integer;
begin
  TotCtrlW := btnCancel.Left + btnCancel.Width - btnStart.Left;
  Distance := btnCancel.Left - btnStart.Left;
  btnStart.Left := (ClientWidth - TotCtrlW) div 2;
  btnCancel.Left := btnStart.Left + Distance;
end;

procedure TfrmDSAExamplesProgressDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := btnStart.Enabled;
end;

procedure TfrmDSAExamplesProgressDlg.btnStartClick(Sender: TObject);
var
  I: Integer;
begin
  btnStart.Enabled := False;
  pbProgress.Position := 0;
  pbProgress.Max := StrToInt(edItems.Text);
  Application.ProcessMessages;
  try
    I := 1;
    while I <= pbProgress.Max do
    begin
      pbProgress.Position := I;
      Application.ProcessMessages;
      if DoMessage(I, pbProgress.Max) in [mrNo, mrCancel] then
      begin
        ShowMessage('Canceled.', dckActiveForm);
        Break;
      end;
      Inc(I);
    end;
  finally
    pbProgress.Position := 0;
    btnStart.Enabled := True;
    DSAQueueStore.Clear;
  end;
end;

end.
