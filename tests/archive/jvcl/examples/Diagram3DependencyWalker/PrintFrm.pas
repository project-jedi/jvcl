unit PrintFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DepWalkConsts;

type
  TfrmPrint = class(TForm)
    Label1: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    class function Execute(var aFormat:TPrintFormat):boolean;
  end;


implementation

{$R *.dfm}

{ TfrmPrint }

class function TfrmPrint.Execute(var aFormat: TPrintFormat): boolean;
begin
  with self.Create(Application) do
  try
    cbFormat.ItemIndex := Ord(AFormat);
    Result := ShowModal = mrOK;
    if Result then
      AFormat := TPrintFormat(cbFormat.ItemIndex);
  finally
    Free;
  end;
end;

end.
