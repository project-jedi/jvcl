unit DescendantForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AncestorForm, StdCtrls, ExtCtrls;

type
  TDescendant = class(TAncestor)
    DescendantButton1: TButton;
    procedure DescendantButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Descendant: TDescendant;

implementation

{$R *.DFM}

procedure TDescendant.DescendantButton1Click(Sender: TObject);
begin
  //inherited;
  ShowMessage('DescendantButton clicked');
end;

end.
