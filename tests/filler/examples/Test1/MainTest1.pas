unit MainTest1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFillFontList, StdCtrls, JvListBox, JvFillerControls, JvLabel,
  JvComponent, JvFillBasicImpl, JvFillStringList, JvxCtrls;

type
  TForm1 = class(TForm)
    JvFillListBox1: TJvFillListBox;
    JvFontFiller1: TJvFontFiller;
    JvFillLabel1: TJvFillLabel;
    JvStringsFiller1: TJvStringsFiller;
    JvFillListBox2: TJvFillListBox;
    JvTreeFiller1: TJvTreeFiller;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
