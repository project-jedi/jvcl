unit DSAExamplesCustom1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvComponent, JvDSADialogs;

type
  TfrmDSAExamplesCustomDlg1 = class(TForm)
    JvDSADialog: TJvDSADialog;
    lblMessage: TLabel;
    cxSuppress: TCheckBox;
    btnOK: TButton;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure DoCustomDSA1;

implementation

{$R *.DFM}

procedure DoCustomDSA1;
begin
  with TfrmDSAExamplesCustomDlg1.Create(Screen.ActiveForm) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmDSAExamplesCustomDlg1 }

procedure TfrmDSAExamplesCustomDlg1.FormResize(Sender: TObject);
begin
  btnOK.Left := (ClientWidth - btnOK.Width) div 2;
end;

end.
