unit fEmbedded;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvDataEmbedded, JvComponent;

type
  TForm1 = class(TForm)
    JvDataEmbedded1: TJvDataEmbedded;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromStream(JvDataEmbedded1.Data);
end;

end.
