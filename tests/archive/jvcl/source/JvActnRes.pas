unit JvActnRes;
{$I JVCL.INC}
interface

uses
  SysUtils, Classes, ActnList, ImgList, {$IFNDEF COMPILER6_UP}Forms, {$ENDIF}Controls;

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
