unit fModalForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMyModalForm = class(TForm)
    Button2: TButton;
    Button1: TButton;
    Hello: TButton;
    procedure HelloClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MyModalForm: TMyModalForm;

implementation

{$R *.DFM}

procedure TMyModalForm.HelloClick(Sender: TObject);
begin
  ShowMessage('Click');
end;

end.
