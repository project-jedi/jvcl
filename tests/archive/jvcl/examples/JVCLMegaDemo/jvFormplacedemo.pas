unit jvFormplacedemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvFormPlace, JvxCtrls, JvLabel;

type
  TfrFormplace = class(TForm)
    JvFormPlace1: TJvFormPlace;
    JvxLabel1: TJvLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


implementation

{$R *.DFM}

end.
