unit StatsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmUnitStats = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    reUsed: TRichEdit;
    Label3: TLabel;
    reUses: TRichEdit;
    btnOK: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute(const UnitName:string;UsedByStrings,UsesStrings:TStrings);
  end;


implementation

{$R *.dfm}

{ TfrmUnitStats }

class procedure TfrmUnitStats.Execute(const UnitName: string; UsedByStrings,
  UsesStrings: TStrings);
begin
  with self.Create(Application) do
  try
    Caption := Format(Caption,[UnitName]);
    edName.Text := UnitName;
    reUsed.Lines := UsedByStrings;
    reUses.Lines := UsesStrings;
    ShowModal;
  finally
    Free;
  end;
end;

end.
