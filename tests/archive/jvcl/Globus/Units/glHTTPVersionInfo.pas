unit glHTTPVersionInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, shdocvw;

type
  TglHTTPVersionInfo = class(TComponent)
  private
    WebBrowser: TWebBrowser;
    FVersionDataURL: string;
  protected
    function GetVersion: string;
    function GetDate: string;
    function GetProgramURL: string;
    function GetComments: string;

    procedure OnLoadVersionInfo(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
  public
    VersionInfo: TStringList;
    function GetVersionInfo(WinControl: TWinControl): boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: string read GetVersion;
    property Date: string read GetDate;
    property ProgramURL: string read GetProgramURL;
    property Comments: string read GetComments;
    property VersionDataURL: string read FVersionDataURL write FVersionDataURL;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gl Components', [TglHTTPVersionInfo]);
end;

{ TglHTTPVersionInfo }

constructor TglHTTPVersionInfo.Create(AOwner: TComponent);
begin
  inherited;
  VersionInfo := TStringList.Create;
end;

destructor TglHTTPVersionInfo.Destroy;
begin
  VersionInfo.Free;
  inherited;
end;

function TglHTTPVersionInfo.GetComments: string;
begin
  Result := VersionInfo.Values['comments'];
end;

function TglHTTPVersionInfo.GetDate: string;
begin
  Result := VersionInfo.Values['date'];
end;

function TglHTTPVersionInfo.GetProgramURL: string;
begin
  Result := VersionInfo.Values['url'];
end;

function TglHTTPVersionInfo.GetVersion: string;
begin
  Result := VersionInfo.Values['version'];
end;

function TglHTTPVersionInfo.GetVersionInfo(WinControl: TWinControl): boolean;
begin
  if trim(VersionDataURL) = '' then
    raise Exception.Create('Uncknown URL: property VersionDataURL is empty');

  WebBrowser := TWebBrowser.Create(nil);
  WebBrowser.Visible := false;
  WebBrowser.Left := -10;
  WebBrowser.Width := 1;
  WebBrowser.Height := 1;
  TWinControl(WebBrowser).Parent := WinControl;

  try
    WebBrowser.OnDocumentComplete := OnLoadVersionInfo;
    WebBrowser.Navigate(VersionDataURL);
    repeat
      Application.ProcessMessages;
    until not WebBrowser.Busy;
  finally
    WebBrowser.Free;
  end;
  Result := (Version <> '')or(Date <> '')or(ProgramURL <> '');
end;

procedure TglHTTPVersionInfo.OnLoadVersionInfo(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
var
  Doc: variant;
  i: integer;
begin
  Doc := WebBrowser.Document;
  VersionInfo.Text := Doc.Body.InnerText;
  //for i := 0 to VersionInfo.Count-1 do
  //  VersionInfo.Names[i] := LowerCase(VersionInfo.Names[i]);
end;

end.
