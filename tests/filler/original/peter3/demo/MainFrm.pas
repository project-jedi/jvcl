unit MainFrm;
// peter3
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvFillIntf5, StdCtrls, JvFillImpl, ImgList, JvListComb,
  JvFillerControls;

type
  TForm1 = class(TForm)
    JvFillListBox1: TJvFillListBox;
    JvTextFiller1: TJvTextFiller;
    JvImageFiller1: TJvImageFiller;
    ImageList1: TImageList;
    FillLabel1: TFillLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    lf:TJvFillListBox;
    tf:TJvTextFiller;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  lf := TJvFillListBox.Create(self);
  lf.Parent := self;
  lf.Align := alLeft;

  tf := TJvTextFiller.Create(self);
  lf.Items := tf;
  // this should trigger the FillerChanging/FillerChange events in lf
  tf.Items.Add.Caption := '1';
  tf.Items.Add.Caption := '2';
  tf.Items.Add.Caption := '3';
  tf.Items.Add.Caption := '4';
  tf.Items.Add.Caption := '5';
  tf.Items.Add.Caption := '6';
  tf.Items.Add.Caption := '7';
  tf.Items.Add.Caption := '8';
end;

end.
