unit IndexResultFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TIndexResultForm = class(TForm)
    lbDockClient1: TJvDockClient;
    ListView1: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IndexResultForm: TIndexResultForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

end.
