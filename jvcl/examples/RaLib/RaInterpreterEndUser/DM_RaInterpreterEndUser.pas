unit DM_RaInterpreterEndUser;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls,
  Forms, Dialogs, DB, DBTables;

type
  TDMRaIntrEndUsr = class(TDataModule)
    DataSource1: TDataSource;
    Table1: TTable;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  DMRaIntrEndUsr: TDMRaIntrEndUsr;

implementation

{$R *.DFM}

procedure TDMRaIntrEndUsr.DataModuleCreate(Sender: TObject);
begin
  Table1.Open;
end;

end.