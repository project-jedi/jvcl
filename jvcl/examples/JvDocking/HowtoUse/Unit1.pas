unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDockControlForm, JvDockVIDStyle, StdCtrls;

type
  TForm1 = class(TForm)
    lbDockServer1: TJvDockServer;
    JvDockVIDStyle1: TJvDockVIDStyle;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

end.
