unit InputDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls;

type
  TfrmInput = class(TForm)
    lblNaam: TLabel;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    edtName: TEdit;
    actOK: TAction;
    actCancel: TAction;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: Char);
  public
    class function Execute(var AName: string): Boolean;
  end;

implementation

{$R *.dfm}
//=== TfrmInput ==============================================================

procedure TfrmInput.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmInput.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmInput.actOKUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := edtName.Text > '';
end;

procedure TfrmInput.edtNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['A'..'Z', '0'..'9', 'a'..'z', #8, #13]) then
    Key := #0;
end;

class function TfrmInput.Execute(var AName: string): Boolean;
begin
  with TfrmInput.Create(Application) do
  try
    Result := ShowModal = mrOk;
    if Result then
      AName := UpperCase(edtName.Text);
  finally
    Free;
  end;
end;

end.
