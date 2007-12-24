unit JvControlActionMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxClasses, StdCtrls, cxCustomPivotGrid, cxDBPivotGrid,
  cxGridLevel, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, DBGrids, Grids, ComCtrls,
  JvControlActions, ActnList, JvActionsEngine,
  JvControlActionsEngineDBGrid,
  JvControlActionsEngineStringGrid,
  JvControlActionsEngineCxGrid,
  JvControlActionsEngineCxVerticalGrid,
  JvControlActionsEngineCxPivotGrid,
  JvControlActionsEngineCxTreeList,
  JvControlActionsEngineTreeView, ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls,
  cxGridBandedTableView, cxVGrid, cxDBVGrid, cxInplaceContainer;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    StringGrid1: TStringGrid;
    DBGrid1: TDBGrid;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1: TcxGrid;
    cxDBPivotGrid1: TcxDBPivotGrid;
    ActionManager1: TActionManager;
    ActionToolBar1: TActionToolBar;
    JvControlCollapseAction2: TJvControlCollapseAction;
    JvControlExpandAction1: TJvControlExpandAction;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1BandedTableView1: TcxGridBandedTableView;
    cxGrid1BandedTableView1Column1: TcxGridBandedColumn;
    cxGrid1BandedTableView1Column2: TcxGridBandedColumn;
    JvControlExportAction1: TJvControlExportAction;
    JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction;
    JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction;
    JvControlPrintAction1: TJvControlPrintAction;
    JvControlCustomizeAction1: TJvControlCustomizeAction;
    cxVerticalGrid1: TcxVerticalGrid;
    cxDBVerticalGrid1: TcxDBVerticalGrid;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
var
  I: Integer;
  j: Integer;
begin
  for I := 1 to 10 - 1 do
  for j := 1 to 5 - 1 do
    StringGrid1.Cells [i,j] := Format('Cell %d:%d',[i,j]);
end;

end.
