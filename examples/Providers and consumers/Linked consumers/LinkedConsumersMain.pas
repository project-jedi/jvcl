unit LinkedConsumersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLabel, StdCtrls, JvCombobox, JvColorProvider,
  JvDataProviderImpl;

type
  TForm2 = class(TForm)
    dpColor: TJvColorProvider;
    dpColorMapping: TJvColorMappingProvider;
    lblMappingCaption: TLabel;
    cbMapping: TJvComboBox;
    lblColorCaptions: TLabel;
    lblColor1: TJvLabel;
    lblColor2: TJvLabel;
    lblColor3: TJvLabel;
    lblColor4: TJvLabel;
    lblColor5: TJvLabel;
    lblColor6: TJvLabel;
    lblColor7: TJvLabel;
    lblColor8: TJvLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

end.
