unit geRTFPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfRTFPreview = class(TForm)
    Rich: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fRTFPreview: TfRTFPreview;

implementation

{$R *.DFM}

end.
