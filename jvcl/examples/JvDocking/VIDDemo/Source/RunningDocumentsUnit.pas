unit RunningDocumentsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, StdCtrls, JvDockControlForm;

type
  TRunningDocumentsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    RichEdit1: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RunningDocumentsForm: TRunningDocumentsForm;

implementation

uses MainFormUnit;

{$R *.DFM}

end.
