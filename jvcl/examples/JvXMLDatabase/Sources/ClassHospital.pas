unit ClassHospital;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClassUtils, ClassRequest, JvXmlDatabase;

type
  THospital = class
  private
    FRequests: TRequestHandler;
    FDatabase: TJvXmlDatabase;
  public
    constructor Create;
    destructor Destroy;override;

    function GetDataPath: string;

    property Requests: TRequestHandler read FRequests write FRequests;
    property Database: TJvXmlDatabase read FDatabase write FDatabase;
  end;

var
  GHospital: THospital;

implementation


{ THospital }

{**********************************************************************}
constructor THospital.Create;
begin
  FRequests := TRequestHandler.Create;
  FDatabase := TJvXmlDatabase.Create(nil);
  FDatabase.TablesPath := ExtractFilePath(Application.ExeName) + 'Data\';
end;
{**********************************************************************}
destructor THospital.Destroy;
begin
  FDatabase.Free;
  FRequests.Free;
  inherited;
end;
{**********************************************************************}
function THospital.GetDataPath: string;
begin
  result := ExtractFilePath(Application.ExeName) + 'Data\';
end;
{**********************************************************************}

initialization
  GHospital := THospital.Create;
finalization
  FreeAndNil(GHospital);
end.

