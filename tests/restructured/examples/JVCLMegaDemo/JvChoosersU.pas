unit JvChoosersU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvGammaPanel, JvColorCombo, StdCtrls, JvCombobox, JvComponent,
  JvColorBox, JvColorBtn, JvArrow, JvLabel, JvxCtrls;

type
  TJvChoosersFrm = class(TFrame)
    JvLabel1: TJvLabel;
    JvLabel2: TJvLabel;
    JvColorSquare1: TJvColorSquare;
    JvLabel3: TJvLabel;
    Label5: TLabel;
    JvLabel4: TJvLabel;
    JvArrow1: TJvArrow;
    JvColorButton1: TJvColorButton;
    JvFontCombobox1: TJvFontComboBox;
    JvColorComboBox1: TJvColorComboBox;
    JvGammaPanel1: TJvGammaPanel;
    JvxLabel1: TJvxLabel;
    procedure JvGammaPanel1ChangeColor(Sender: TObject; Foreground,
      Background: TColor);
    procedure JvFontCombobox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvChoosersFrm.JvGammaPanel1ChangeColor(Sender: TObject; Foreground,
  Background: TColor);
begin
 JvColorsquare1.color:=Foreground;
end;

procedure TJvChoosersFrm.JvFontCombobox1Change(Sender: TObject);
begin
 JvxLabel1.Font.Name := JvFontCombobox1.Items[JvFontCombobox1.itemindex];
end;

end.
