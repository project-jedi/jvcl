unit ContentScrollerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvGIF, ExtCtrls, JvCntScr, JvComponent, JvCaptionPanel;

type
  TContentScrollerMainForm = class(TForm)
    ContentScroller1: TJvContentScroller;
    Label1: TLabel;
    Image11: TImage;
    chkGoDown: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  ContentScrollerMainForm: TContentScrollerMainForm;

implementation

{$R *.dfm}

procedure TContentScrollerMainForm.Button1Click(Sender: TObject);
begin
  if chkGoDown.Checked then
    ContentScroller1.ScrollDirection := sdDown
  else
    ContentScroller1.ScrollDirection := sdUp;
  ContentScroller1.Active := not ContentScroller1.Active;
end;

end.
