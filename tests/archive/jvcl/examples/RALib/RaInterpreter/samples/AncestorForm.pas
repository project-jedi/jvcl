unit AncestorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TAncestor = class(TForm)
    Panel1: TPanel;
    AncestorButton1: TButton;
    procedure AncestorButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Ancestor: TAncestor;

implementation

{$R *.DFM}

procedure TAncestor.AncestorButton1Click(Sender: TObject);
begin
  ShowMessage('AncestorButton clicked');
end;

end.
