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
    JvHttpUrlGrabber: TJvHttpUrlGrabber;
    IdHTTP: TIdHTTP;
    ProgramVersionCheck: TJvProgramVersionCheck;
    JvProgramVersionNetworkLocation1: TJvProgramVersionNetworkLocation;
    JvProgramVersionHTTPLocation1: TJvProgramVersionHTTPLocation;
    JvProgramVersionFTPLocation1: TJvProgramVersionFTPLocation;
    JvProgramVersionDatabaseLocation1: TJvProgramVersionDatabaseLocation;
    JvFtpUrlGrabber: TJvFtpUrlGrabber;
    function JvProgramVersionFTPLocation1LoadFileFromRemote(
      iProgramVersionLocation: TJvProgramVersionFTPLocation; const iRemotePath,
      iRemoteFileName, iLocalPath, iLocalFileName: string): string;
    function JvProgramVersionHTTPLocation1LoadFileFromRemote(
      iProgramVersionLocation: TJvProgramVersionHTTPLocation; const iRemotePath,
      iRemoteFileName, iLocalPath, iLocalFileName: string): string;
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

Uses JclFileUtils, JvTypes;

procedure TForm1.VersionCheck;
begin
  with ProgramVersionCheck do
  begin
    Execute ;
  end;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  VersionCheck;
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

end.
