unit ClassStructureDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ExtCtrls;

type
  TSourceType  = (stDelphi, stJVCL);

  TfrmClassStructure = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    rgrSource: TRadioGroup;
    chbAddToOldList: TCheckBox;
    actOK: TAction;
    actCancel: TAction;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  public
    class function Execute(var Source: TSourceType; var AddToOldList: Boolean): Boolean;
  end;

implementation

{$R *.dfm}
//=== TfrmClassStructure =====================================================

procedure TfrmClassStructure.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmClassStructure.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TfrmClassStructure.Execute(var Source: TSourceType;
  var AddToOldList: Boolean): Boolean;
begin
  with TfrmClassStructure.Create(Application) do
  try
    chbAddToOldList.Checked := AddToOldList;
    rgrSource.ItemIndex := Integer(Source);
    Result := ShowModal = mrOk;
    if Result then
    begin
      AddToOldList := chbAddToOldList.Checked;
      Source := TSourceType(rgrSource.ItemIndex);
    end;
  finally
    Free;
  end;
end;

end.
