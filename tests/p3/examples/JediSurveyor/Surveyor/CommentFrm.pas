unit CommentFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, JvDialogs, Menus, JvMemo;

type
  TfrmComment = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    reComments: TJvMemo;
    alCommentFrm: TActionList;
    acOK: TAction;
    acCancel: TAction;
    acLoad: TAction;
    acSave: TAction;
    OpenFileDlg: TJvOpenDialog;
    SaveFileDlg: TJvSaveDialog;
    popEdit: TPopupMenu;
    Load1: TMenuItem;
    Save1: TMenuItem;
    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure acLoadExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
  private
    { Private declarations }
    FFilename:string;
  public
    { Public declarations }
    class function Comment(const Title:string; var Comment:string):boolean;
  end;


implementation
uses
  JvSurveyIntf, JvSurveyUtils;

{$R *.dfm}

{ TfrmComment }

class function TfrmComment.Comment(const Title: string;
  var Comment: string): boolean;
var frm:TfrmComment;
begin
  frm := self.Create(Application);
  try
    frm.Caption := Format(frm.Caption,[Title]);
    frm.reComments.Lines.Text := DecodeResponse(Comment,stFreeForm);
    Result := frm.ShowModal = mrOK;
    if Result then
      Comment := EncodeResponse(frm.reComments.Lines.Text,stFreeForm);
  finally
    frm.Free;
  end;
end;

procedure TfrmComment.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
  if not (fsModal in FormState) then Close;
end;

procedure TfrmComment.acOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
  if not (fsModal in FormState) then Close;
end;

procedure TfrmComment.acLoadExecute(Sender: TObject);
begin
  OpenFileDlg.Filename := FFilename;
  if OpenFileDlg.Execute then
  begin
    FFilename := OpenFileDlg.Filename;
    reComments.Lines.LoadFromFile(FFilename);
  end;
end;

procedure TfrmComment.acSaveExecute(Sender: TObject);
begin
  SaveFileDlg.Filename := FFilename;
  if SaveFileDlg.Execute then
  begin
    FFilename := SaveFileDlg.Filename;
    reComments.Lines.SaveToFile(FFilename);
  end;
end;

end.
