unit ImageWindowChild2U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ImgList, JvImageWindow, JvComponent;

type
  TImageWindowChild2 = class(TForm)
    Panel1: TPanel;
    ImageWindow1: TJvImageWindow;
    ImageList1: TImageList;
    Label1: TLabel;
    procedure ImageWindow1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  ImageWindowChild2: TImageWindowChild2;

implementation

{$R *.DFM}

procedure TImageWindowChild2.ImageWindow1Click(Sender: TObject);
begin
  Caption := Format('Clicked image %d (ESC to quit)',[ImageWindow1.ImageIndex]);
end;

procedure TImageWindowChild2.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

end.
