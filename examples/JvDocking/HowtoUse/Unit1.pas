{$I jvcl.inc}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvDockControlForm, JvDockVIDStyle;

type
  TForm1 = class(TForm)
    lbDockServer1: TJvDockServer;
    JvDockVIDStyle1: TJvDockVIDStyle;
    Label1: TLabel;
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

end.
