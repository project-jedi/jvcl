unit JvFillFontLstMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvFillFontList, StdCtrls, JvFillListBox;

type
  TForm1 = class(TForm)
    JvFontFiller1: TJvFontFiller;
    JvFillListBox1: TJvFillListBox;
    JvFillListBox2: TJvFillListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
