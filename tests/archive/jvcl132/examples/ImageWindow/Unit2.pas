unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, JvImageWindow, JvComponent;

type
  TForm2 = class(TForm)
    ImageList1: TImageList;
    ImageBox1: TJvImageBox2;
    ImageBox2: TJvImageBox2;
    ImageBox3: TJvImageBox2;
    ImageBox4: TJvImageBox2;
    ImageBox5: TJvImageBox2;
    ImageBox6: TJvImageBox2;
    ImageBox7: TJvImageBox2;
    ImageBox8: TJvImageBox2;
    Panel1: TPanel;
    procedure ImageBox1Click(Sender: TObject);
    procedure ImageBox1Enter(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.ImageBox1Click(Sender: TObject);
begin
{ this is the OnClick handler for all TImageBoxes on this Form }
   with Sender as TJvImageBox2 do
     Panel1.Caption := Format('Clicked image %d ',[ImageIndex]);
end;

procedure TForm2.ImageBox1Enter(Sender: TObject);
begin
{ this is the OnEnter handler for all TImageBoxes on this Form }
   with Sender as TJvImageBox2 do
     Panel1.Caption := Text;
end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Close;
end;

procedure TForm2.FormActivate(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
   if Components[i] is TJvImageBox2 then
   with Components[i] as TJvImageBox2 do
   begin
     Hint := Text;
     ShowHint := True;
   end;
end;

procedure TForm2.Panel1DblClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
   if Components[i] is TJvImageBox2 then
     TJvImageBox2(Components[i]).ShowClick := not TJvImageBox2(Components[i]).ShowClick;
end;

end.
