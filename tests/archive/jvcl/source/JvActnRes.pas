unit JvActnRes;

interface

uses
  SysUtils, Classes, ActnList, ImgList, Controls;

type
  TJvStandardActions = class(TDataModule)
    ImageList1: TImageList;
    ActionList1: TActionList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JvStandardActions: TJvStandardActions;

implementation

{$R *.dfm}

end.
