{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHTTPVersionInfo.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgHTTPVersionInfo;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  JvComponent,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  shdocvw;

type
  TJvgHTTPVersionInfo = class(TJvComponent)
  private
    WebBrowser: TWebBrowser;
    FVersionDataURL: string;
  protected
    function GetVersion: string;
    function GetDate: string;
    function GetProgramURL: string;
    function GetComments: string;

    procedure OnLoadVersionInfo(Sender: TObject; const pDisp: IDispatch; var
      URL: OleVariant);
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
    property VersionDataURL: string read FVersionDataURL write
      FVersionDataURL;

  end;

procedure Register;

implementation

procedure Register;
begin
  //   RegisterComponents('Gl Components', [TJvgHTTPVersionInfo]);
end;

{ TJvgHTTPVersionInfo }

constructor TJvgHTTPVersionInfo.Create(AOwner: TComponent);
begin
  inherited;
  VersionInfo := TStringList.Create;
end;

destructor TJvgHTTPVersionInfo.Destroy;
begin
  VersionInfo.Free;
  inherited;
end;

function TJvgHTTPVersionInfo.GetComments: string;
begin
  Result := VersionInfo.Values['comments'];
end;

function TJvgHTTPVersionInfo.GetDate: string;
begin
  Result := VersionInfo.Values['date'];
end;

function TJvgHTTPVersionInfo.GetProgramURL: string;
begin
  Result := VersionInfo.Values['url'];
end;

function TJvgHTTPVersionInfo.GetVersion: string;
begin
  Result := VersionInfo.Values['version'];
end;

function TJvgHTTPVersionInfo.GetVersionInfo(WinControl: TWinControl): boolean;
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
  Result := (Version <> '') or (Date <> '') or (ProgramURL <> '');
end;

procedure TJvgHTTPVersionInfo.OnLoadVersionInfo(Sender: TObject; const pDisp:
  IDispatch; var URL: OleVariant);
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
