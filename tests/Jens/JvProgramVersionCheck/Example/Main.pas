unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls;

type
  TForm1 = class(TForm)
    JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure VersionCheck;
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.VersionCheck;
Var ProgramVersionCheck : tJvProgramVersionCheck;
begin
  ProgramVersionCheck := tJvProgramVersionCheck.Create(self);
  try
    with ProgramVersionCheck do
    begin
      ProgramVersionCheckOptions.NetworkLocation.LocationPathVersion := GetCurrentDir+'\Version Check\Remote';
      ProgramVersionCheckOptions.LocalVersionInfoFileName := 'ProjektVersions.ini';
      ProgramVersionCheckOptions.LocalDirectory := GetCurrentDir+'\Version Check';
      AppStorage := JvAppIniFileStorageVersionCheck;
      Execute ;
    end;
  finally
    ProgramVersionCheck.Free;
  end;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  VersionCheck;
end;

end.
