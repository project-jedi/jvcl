unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls, JvUrlListGrabber, JvUrlGrabbers, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, ImgList, JvPropertyStore;

type
  TForm1 = class(TForm)
    JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage;
    Label1: TLabel;
    Label2: TLabel;
    JvHttpUrlGrabber1: TJvHttpUrlGrabber;
    IdHTTP1: TIdHTTP;
    ProgramVersionCheck: TJvProgramVersionCheck;
    ImageList1: TImageList;
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
begin
  with ProgramVersionCheck do
  begin
//    ProgramVersionCheckOptions.SupportedLocationTypes := [pvltNetwork, pvltHTTP, pvltFTP, pvltDatabase];
//    ProgramVersionCheckOptions.NetworkLocation.LocationPathVersion := GetCurrentDir+'\Version Check\Remote';
//    ProgramVersionCheckOptions.HTTPLocation.LocationPathVersion := 'www.oratool.de/test/ProjektVersions_http.ini';
//    ProgramVersionCheckOptions.LocalVersionInfoFileName := 'ProjektVersions.ini';
//    ProgramVersionCheckOptions.LocalDirectory := GetCurrentDir+'\Version Check';
    Execute ;
  end;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  VersionCheck;
end;

end.
