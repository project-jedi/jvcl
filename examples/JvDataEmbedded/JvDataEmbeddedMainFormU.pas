unit JvDataEmbeddedMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvDataEmbedded, JvComponent;

type
  TJvDataEmbeddedMainForm = class(TForm)
    JvDataEmbedded1: TJvDataEmbedded;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  end;

var
  JvDataEmbeddedMainForm: TJvDataEmbeddedMainForm;

implementation

{$R *.DFM}

procedure TJvDataEmbeddedMainForm.Button1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromStream(JvDataEmbedded1.Data);
end;

end.
