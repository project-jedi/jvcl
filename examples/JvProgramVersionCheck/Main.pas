unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls, JvUrlListGrabber, JvUrlGrabbers, ImgList, JvPropertyStore,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdFTP, IdHTTP,
  FtpCli, HttpProt;

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
    JvProgramVersionHTTPLocationICS1: TJvProgramVersionHTTPLocationICS;
    procedure FormShow(Sender: TObject);
    function JvProgramVersionFTPLocation1LoadFileFromRemote(
      iProgramVersionLocation: TJvProgramVersionFTPLocation; const iRemotePath,
      iRemoteFileName, iLocalPath, iLocalFileName: string): string;
    function JvProgramVersionHTTPLocation1LoadFileFromRemote(
      iProgramVersionLocation: TJvProgramVersionHTTPLocation; const iRemotePath,
      iRemoteFileName, iLocalPath, iLocalFileName: string): string;
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

function TForm1.JvProgramVersionFTPLocation1LoadFileFromRemote(
  iProgramVersionLocation: TJvProgramVersionFTPLocation; const iRemotePath,
  iRemoteFileName, iLocalPath, iLocalFileName: string): string;
begin
  With JvFtpUrlGrabber do
  begin
    FileName := PathAppend(iLocalPath, iLocalFileName);
    url := iRemotePath + iRemoteFilename;
    OutputMode := omFile;
    Mode := hmBinary;

    Start;
    while Status <> gsStopped do
      Application.HandleMessage;
    Result := FileName;
  end;
end;

function TForm1.JvProgramVersionHTTPLocation1LoadFileFromRemote(
  iProgramVersionLocation: TJvProgramVersionHTTPLocation; const iRemotePath,
  iRemoteFileName, iLocalPath, iLocalFileName: string): string;
begin
  With JvHttpUrlGrabber do
  begin
    FileName := PathAppend(iLocalPath, iLocalFileName);
    url := iRemotePath + iRemoteFilename;
    OutputMode := omFile;
    Start;
    sleep (1000);
    while Status <> gsStopped do
      Application.HandleMessage;
    Result := FileName;
  end;
end;

procedure TForm1.VersionCheck;
begin
  with ProgramVersionCheck do
  begin
    Execute ;
  end;
end;

end.
