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

unit CommentsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ComCtrls, JvRichEdit;

type
  TfrmComments = class(TForm)
    reComments: TJvRichEdit;
    Button1: TButton;
    alCommentsFrm: TActionList;
    acClose: TAction;
    procedure acCloseExecute(Sender: TObject);
  private
  public
    class procedure Comments(const Title,Comments:string);
  end;


implementation

uses
  JvSurveyIntf, JvSurveyUtils;

{$R *.dfm}

procedure TfrmComments.acCloseExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
  if not (fsModal in FormState) then Close;
end;

class procedure TfrmComments.Comments(const Title, Comments: string);
var frm:TfrmComments;
begin
  frm := self.Create(Application);
  try
    frm.Caption := Format(frm.Caption,[Title]);
    frm.reComments.Lines.Text := DecodeResponse(Comments,stFreeForm);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

end.
