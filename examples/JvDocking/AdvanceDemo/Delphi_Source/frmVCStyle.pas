unit frmVCStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDockControlForm, Menus, StdCtrls, ComCtrls, ExtCtrls, JvDockSupportClass;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Main;

{$R *.DFM}

procedure TForm2.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

procedure TForm2.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

end.
