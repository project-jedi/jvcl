unit frmVSNetStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle, Menus;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    lbDockClient1: TJvDockClient;
    procedure lbDockClient1FormShow(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses Main;

{$R *.DFM}

procedure TForm4.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm4.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
