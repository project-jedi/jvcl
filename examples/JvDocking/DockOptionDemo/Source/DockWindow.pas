unit DockWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvDockControlForm, ImgList, StdCtrls;

type
  TDockWindow_Form = class(TForm)
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DockWindow_Form: TDockWindow_Form;

implementation

uses MainForm;

{$R *.DFM}

end.
