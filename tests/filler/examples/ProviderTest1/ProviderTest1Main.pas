unit ProviderTest1Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvStringListDataProvider, JvComponent, JvDataProviderImpl,
  JvFontDataProvider, JvxCtrls, JvLabel, JvDataProviderControls, StdCtrls,
  JvListBox, ImgList;

type
  TfrmTestProviders = class(TForm)
    JvProvidedListBox1: TJvProvidedListBox;
    JvProvidedLabel1: TJvProvidedLabel;
    JvProvidedListBox2: TJvProvidedListBox;
    JvFontDataProvider1: TJvFontDataProvider;
    JvTreeDataProvider1: TJvTreeDataProvider;
    ilTest: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTestProviders: TfrmTestProviders;

implementation

{$R *.DFM}

end.
