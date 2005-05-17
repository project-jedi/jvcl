unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls, JvUrlListGrabber, JvUrlGrabbers, ImgList, JvPropertyStore,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdFTP, IdHTTP;

type
  TForm1 = class(TForm)
    JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage;
    JvProgramVersionDatabaseLocation1: TJvProgramVersionDatabaseLocation;
    JvProgramVersionFTPLocation1: TJvProgramVersionFTPLocation;
    JvProgramVersionHTTPLocation1: TJvProgramVersionHTTPLocation;
    JvProgramVersionHTTPLocationIndy1: TJvProgramVersionHTTPLocationIndy;
    JvProgramVersionNetworkLocation1: TJvProgramVersionNetworkLocation;
    Label1: TLabel;
    Label2: TLabel;
    ProgramVersionCheck: TJvProgramVersionCheck;
    procedure FormShow(Sender: TObject);
  public
    procedure VersionCheck;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses JclFileUtils, JvTypes;

procedure TForm1.FormShow(Sender: TObject);
begin
  VersionCheck;
end;

procedure TForm1.VersionCheck;
begin
  with ProgramVersionCheck do
  begin
    Execute ;
  end;
end;

end.
