unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ClassUtils, JvXmlDatabase, IdBaseComponent, IdComponent, IdURI,
  IdTCPServer, IdCustomHTTPServer, IdHTTPServer, ClassHospital, ShellApi,
  JvComponent, JvTrayIcon, Menus, ActnList;

type
  TfoMain = class(TForm)
    IdHTTPServer1: TIdHTTPServer;
    JvTrayIcon1: TJvTrayIcon;
    MainMenu: TPopupMenu;
    Homepage1: TMenuItem;
    ActionList1: TActionList;
    actHome: TAction;
    actExit: TAction;
    Fermer1: TMenuItem;
    N1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AThread: TIdPeerThread;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure actHomeExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure JvTrayIcon1DblClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

{**********************************************************************}
procedure TfoMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;
{**********************************************************************}
procedure TfoMain.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' ' + TUtils.GetBuild;
end;
{**********************************************************************}
procedure TfoMain.IdHTTPServer1CommandGet(AThread: TIdPeerThread;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
 lHeaders: TStringList;
 lStream: TMemoryStream;
 i: Integer;
 lQuery: TJvXmlQuery;
begin
  //ARequestInfo.Authentication.Steps
  if (ARequestInfo.AuthExists) then
  begin
    lQuery := GHospital.Database.Query('SELECT * FROM users.xml WHERE (UserLogin = "' + ARequestInfo.AuthUsername +
      '") AND (UserPassword = "' + ARequestInfo.AuthPassword + '")');
    if lQuery.Results.Items.Count = 0 then
    begin
      AResponseInfo.AuthRealm := 'Kapere';
      AResponseInfo.ContentText := DateTimeToStr(Now);
      Exit;
    end;
  end
  else
  begin
    AResponseInfo.AuthRealm := 'Kapere';
    AResponseInfo.ContentText := DateTimeToStr(Now);
    Exit;
  end;
  
  try
    lHeaders := TStringList.Create;
    try
      lStream := TMemoryStream.Create;
      if not GHospital.Requests.Request(ARequestInfo.Document, TIdURI.URLDecode(ARequestInfo.UnparsedParams),
          lHeaders, lStream, lQuery.Results.Items[0].Properties.IntValue('UserId')) then
        AResponseInfo.ResponseNo := 404
      else
      begin
        for i:=lHeaders.Count-1 downto 0 do
          if Pos('Location', lHeaders[i]) = 1 then
          begin
            AResponseInfo.Redirect(Copy(lHeaders[i], 10, MAXINT));
            Exit;
          end;
        AResponseInfo.ContentStream := lStream;
        AResponseInfo.CustomHeaders.AddStdValues(lHeaders);
        AResponseInfo.FreeContentStream := true;
      end;
    finally
      lHeaders.Free;
    end;
  except
    AResponseInfo.ResponseNo := 500;
  end;
end;
{**********************************************************************}
procedure TfoMain.actHomeExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://127.0.0.1:8000', nil, nil, SW_SHOW);
end;
{**********************************************************************}
procedure TfoMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;
{**********************************************************************}
procedure TfoMain.JvTrayIcon1DblClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  actHome.Execute;
end;
{**********************************************************************}
end.
