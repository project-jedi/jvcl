unit jvFormplacedemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvFormPlace, JvxCtrls;

type
  TfrFormplace = class(TForm)
    JvFormPlace1: TJvFormPlace;
    JvxLabel1: TJvxLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


implementation

{$R *.DFM}

end.
