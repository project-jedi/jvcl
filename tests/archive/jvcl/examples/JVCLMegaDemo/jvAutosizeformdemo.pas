unit jvAutosizeformdemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvAutoSizeCompo, StdCtrls;

type
  TfrAutosize = class(TForm)
    JvAutoSizeCompo1: TJvAutoSizeCompo;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

end.
