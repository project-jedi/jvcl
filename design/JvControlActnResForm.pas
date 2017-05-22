unit JvControlActnResForm;

interface

uses
  SysUtils, Classes, StdActns, ActnList, ImgList, Controls, Forms,
  JvControlActions, JvActionsEngine;

type
  TJvControlActionsDM = class(TDataModule)
    JvControlActionsList: TActionList;
    JvControlCollapseAction1: TJvControlCollapseAction;
    JvControlExpandAction1: TJvControlExpandAction;
    JvControlExportAction1: TJvControlExportAction;
    JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction;
    JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction;
    JvControlPrintAction1: TJvControlPrintAction;
    JvControlCustomizeAction1: TJvControlCustomizeAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvControlActionsDM: TJvControlActionsDM;

implementation

{$R *.dfm}

end.
