{$I jvcl.inc}

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvDockControlForm;

type
  TForm2 = class(TForm)
    lbDockClient1: TJvDockClient;
    Label1: TLabel;
  private
  public
  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

end.
