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
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


implementation

{$R *.DFM}

procedure TfrTransparentForm.FormActivate(Sender: TObject);
begin
  JvTransparentForm1.Enable := true;
end;

procedure TfrTransparentForm.FormDeactivate(Sender: TObject);
begin
  JvTransparentForm1.Enable := FALSE;
end;

procedure TfrTransparentForm.Button1Click(Sender: TObject);
begin
  close;
end;


end.
