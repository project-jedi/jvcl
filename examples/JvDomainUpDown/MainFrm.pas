unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvPanel, ExtCtrls, ComCtrls, StdCtrls, Mask,
  JvToolEdit, JvComCtrls, JvPageListTreeView, JvUpDown;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    JvDomainUpDown1: TJvDomainUpDown;
    Label1: TLabel;
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Label1.Caption := Format('%d',[JvDomainUpDown1.Position]);
end;

end.
