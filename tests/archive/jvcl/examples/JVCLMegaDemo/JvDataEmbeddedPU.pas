unit JvDataEmbeddedPU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvDataEmbedded, StdCtrls, ExtCtrls, JvCaptionPanel;

type
  TJvDataEmbeddedFrm = class(TFrame)
    JvDataEmbedded1: TJvDataEmbedded;
    JvCaptionPanel1: TJvCaptionPanel;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvDataEmbeddedFrm.Button1Click(Sender: TObject);
begin
   Memo1.Lines.LoadFromStream(JvDataEmbedded1.data);
end;

end.
