unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ImgList, JvImageWindow;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    ImageWindow1: TJvImageWindow;
    ImageList1: TImageList;
    Label1: TLabel;
    procedure ImageWindow1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.DFM}

procedure TForm3.ImageWindow1Click(Sender: TObject);
begin
  Caption := Format('Clicked image %d (ESC to quit)',[ImageWindow1.ImageIndex]);
end;

procedure TForm3.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

end.
