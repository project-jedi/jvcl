unit JvContentScroller;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvGIF, ExtCtrls, JvCntScr, JvComponent, JvCaptionPanel;

type
  TJvContenScrollerForm = class(TForm)
    JvCaptionPanel1: TJvCaptionPanel;
    Button1: TButton;
    ContentScroller1: TJvContentScroller;
    Label1: TLabel;
    Image11: TImage;
    chkGoDown: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvContenScrollerForm: TJvContenScrollerForm;

implementation

{$R *.dfm}

procedure TJvContenScrollerForm.Button1Click(Sender: TObject);
begin
  if chkGoDown.Checked then
    ContentScroller1.ScrollDirection := sdDown
  else
    ContentScroller1.ScrollDirection := sdUp;
  ContentScroller1.Active := not ContentScroller1.Active;
end;

end.
