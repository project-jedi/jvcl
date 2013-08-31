unit Main;

interface

{$I jvcl.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvProgramVersionCheck, JvComponent, JvAppStorage, JvAppIniStorage,
  StdCtrls, JvUrlListGrabber, JvUrlGrabbers, ImgList, JvPropertyStore,JvAppxmlStorage,
  JvComponentBase;

type
  TForm1 = class(TForm)
    JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage;
    JvProgramVersionDatabaseLocation: TJvProgramVersionDatabaseLocation;
    JvProgramVersionFTPLocation: TJvProgramVersionFTPLocation;
    JvProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation;
    JvProgramVersionNetworkLocation: TJvProgramVersionNetworkLocation;
    Label1: TLabel;
    Label2: TLabel;
    ProgramVersionCheck: TJvProgramVersionCheck;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure VersionCheck;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses JclFileUtils, JvTypes, JvPropertyStoreEditor,
  JvDynControlEngineJVCLInspector, JvDynControlEngineJVCL
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  ,  JvDynControlEngineDevExpCx
  {$ENDIF}
  , JvXMLBrowser;

procedure TForm1.Button1Click(Sender: TObject);
begin
  VersionCheck;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if EditPropertyStore(ProgramVersionCheck.RemoteProgramVersionHistory) then
  begin
    ProgramVersionCheck.StoreRemoteVersionHistoryToFile (hffini, 'LocalRemoteVersionHistory');
    ProgramVersionCheck.StoreRemoteVersionHistoryToFile (hffxml, 'LocalRemoteVersionHistory');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowXMLBrowserFromFile(
    PathAppend(JvProgramVersionNetworkLocation.ValidLocationPath, JvProgramVersionNetworkLocation.VersionInfoFileName),
    'Version History', 'Version');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  VersionCheck;
end;

procedure TForm1.VersionCheck;
begin
  ProgramVersionCheck.Execute ;
end;

begin
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  RegisterJvDynControlRTTIInspectorControl(DynControlEngineDevExpCx);
  {$ELSE}
  RegisterJvDynControlRTTIInspectorControl(DynControlEngineJVCL);
  {$ENDIF}
end.
