unit CommentsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ComCtrls, JvRichEd;

type
  TfrmComments = class(TForm)
    reComments: TJvRichEdit;
    Button1: TButton;
    alCommentsFrm: TActionList;
    acClose: TAction;
    procedure acCloseExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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
