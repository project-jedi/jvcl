unit DataModuleMain;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TDMMain = class(TDataModule)
    ImageListComponents: TImageList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  DMMain: TDMMain;

implementation

uses
  Configuration;

{$R *.dfm}

end.
