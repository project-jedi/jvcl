{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHTTPVersionInfo.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgHTTPVersionInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, SHDocVw,
  JvComponent;

type
  TJvgHTTPVersionInfo = class(TJvComponent)
  private
    FVersionInfo: TStringList;
    FWebBrowser: TWebBrowser;
    FVersionDataURL: string;
    function GetVersionInfoProperty: TStrings;
  protected
    function GetVersion: string;
    function GetDate: string;
    function GetProgramURL: string;
    function GetComments: string;
    procedure OnLoadVersionInfo(Sender: TObject; const PDisp: IDispatch;
      var URL: OleVariant);
  public
    property VersionInfo: TStrings read GetVersionInfoProperty;
    function GetVersionInfo(WinControl: TWinControl): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: string read GetVersion;
    property Date: string read GetDate;
    property ProgramURL: string read GetProgramURL;
    property Comments: string read GetComments;
    property VersionDataURL: string read FVersionDataURL write FVersionDataURL;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts;

{$IFNDEF USEJVCL}
resourcestring
  RsEUnknownURLPropertyVersionDataURLIs = 'Unknown URL: property VersionDataURL is empty';
{$ENDIF USEJVCL}

constructor TJvgHTTPVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionInfo := TStringList.Create;
end;

destructor TJvgHTTPVersionInfo.Destroy;
begin
  FVersionInfo.Free;
  inherited Destroy;
end;

function TJvgHTTPVersionInfo.GetVersionInfoProperty: TStrings;
begin
  Result := FVersionInfo;
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

function TJvgHTTPVersionInfo.GetVersionInfo(WinControl: TWinControl): Boolean;
begin
  if Trim(VersionDataURL) = '' then
    raise Exception.CreateRes(@RsEUnknownURLPropertyVersionDataURLIs);

  FWebBrowser := TWebBrowser.Create(nil);
  FWebBrowser.Visible := False;
  FWebBrowser.Left := -10;
  FWebBrowser.Width := 1;
  FWebBrowser.Height := 1;
  TWinControl(FWebBrowser).Parent := WinControl;

  try
    FWebBrowser.OnDocumentComplete := OnLoadVersionInfo;
    FWebBrowser.Navigate(VersionDataURL);
    repeat
      Application.ProcessMessages;
    until not FWebBrowser.Busy;
  finally
    FWebBrowser.Free;
  end;
  Result := (Version <> '') or (Date <> '') or (ProgramURL <> '');
end;

procedure TJvgHTTPVersionInfo.OnLoadVersionInfo(Sender: TObject;
  const PDisp: IDispatch; var URL: OleVariant);
var
  Doc: Variant;
begin
  Doc := FWebBrowser.Document;
  VersionInfo.Text := Doc.Body.InnerText;
  //for i := 0 to VersionInfo.Count-1 do
  //  VersionInfo.Names[i] := LowerCase(VersionInfo.Names[i]);
end;

end.

