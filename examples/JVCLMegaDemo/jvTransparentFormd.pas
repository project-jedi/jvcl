unit jvTransparentFormd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvComponent, JvTransparentForm;

type
  TfrTransparentForm = class(TForm)
    JvTransparentForm1: TJvTransparentForm;
    Label1: TLabel;
    Button1: TButton;
    procedure FormDeactivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
   end;

implementation

{$R *.DFM}

procedure TfrTransparentForm.FormDeactivate(Sender: TObject);
begin
  JvTransparentForm1.Active := false;
end;

procedure TfrTransparentForm.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
