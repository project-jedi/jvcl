unit VisibilityDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls,

  ParserTypes;

type
  TfrmVisibility = class(TForm)
    chbPrivate: TCheckBox;
    chbProtected: TCheckBox;
    chbPublic: TCheckBox;
    chbPublished: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    lblVisibility: TLabel;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    actDefault: TAction;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actDefaultExecute(Sender: TObject);
  public
    class function Execute(var Visibility: TClassVisibilities): Boolean;
  end;

implementation

{$R *.dfm}
//=== TfrmVisibility =========================================================

procedure TfrmVisibility.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmVisibility.actDefaultExecute(Sender: TObject);
begin
  chbPrivate.Checked := False;
  chbProtected.Checked := False;
  chbPublic.Checked := True;
  chbPublished.Checked := True;
  ModalResult := mrOk;
end;

procedure TfrmVisibility.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TfrmVisibility.Execute(
  var Visibility: TClassVisibilities): Boolean;
begin
  with TfrmVisibility.Create(Application) do
  try
    chbPrivate.Checked := inPrivate in Visibility;
    chbProtected.Checked := inProtected in Visibility;
    chbPublic.Checked := inPublic in Visibility;
    chbPublished.Checked := inPublished in Visibility;

    Result := ShowModal = mrOk;
    if Result then
    begin
      Visibility := [];
      if chbPrivate.Checked then
        Include(Visibility, inPrivate);
      if chbProtected.Checked then
        Include(Visibility, inProtected);
      if chbPublic.Checked then
        Include(Visibility, inPublic);
      if chbPublished.Checked then
        Include(Visibility, inPublished);
    end;
  finally
    Free;
  end;
end;

end.
