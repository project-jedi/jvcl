unit SearchResultFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TSearchResultForm = class(TForm)
    lbDockClient1: TJvDockClient;
    ListView1: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SearchResultForm: TSearchResultForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

end.
