unit frmVIDStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDockControlForm, StdCtrls, ComCtrls, Menus, ExtCtrls;

type
  TForm3 = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    Memo1: TMemo;
    procedure lbDockClient1FormShow(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Main;

{$R *.DFM}

procedure TForm3.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm3.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
