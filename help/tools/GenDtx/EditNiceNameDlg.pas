unit EditNiceNameDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList;

type
  TfrmEditNiceName = class(TForm)
    lblClass: TLabel;
    edtClass: TEdit;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    Button1: TButton;
    Button2: TButton;
    lblNiceName: TLabel;
    edtNiceName: TEdit;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure edtClassKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    class function ExecuteAdd(var AClass, ADesc: string): Boolean;
    class function ExecuteEdit(const AClass: string; var ADesc: string): Boolean;
  end;

implementation

{$R *.dfm}
//=== TfrmEditNiceName =======================================================

procedure TfrmEditNiceName.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditNiceName.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmEditNiceName.actOKUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := edtClass.Text > '';
end;

procedure TfrmEditNiceName.edtClassKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in ['A'..'Z', 'a'..'z', '0'..'9', #8, #13]) then
    Key := #0;
end;

class function TfrmEditNiceName.ExecuteAdd(var AClass,
  ADesc: string): Boolean;
begin
  with TfrmEditNiceName.Create(Application) do
  try
    edtClass.Text := '';
    edtNiceName.Text := '';
    Result := ShowModal = mrOk;
    if Result then
    begin
      AClass := UpperCase(edtClass.Text);
      ADesc := edtNiceName.Text;
    end;
  finally
    Free;
  end;
end;

class function TfrmEditNiceName.ExecuteEdit(const AClass: string;
  var ADesc: string): Boolean;
begin
  with TfrmEditNiceName.Create(Application) do
  try
    edtClass.Text := AClass;
    edtNiceName.Text := ADesc;
    edtClass.ReadOnly := True;
    edtClass.TabStop := False;
    Result := ShowModal = mrOk;
    if Result then
      ADesc := edtNiceName.Text;
  finally
    Free;
  end;
end;

end.
