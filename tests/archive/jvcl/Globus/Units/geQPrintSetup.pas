unit geQPrintSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfPrintSetup = class(TForm)
    rgOrientation: TRadioGroup;
    RadioGroup2: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fPrintSetup: TfPrintSetup;

implementation

{$R *.DFM}

end.
