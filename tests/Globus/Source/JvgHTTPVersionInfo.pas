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

UNIT JvgHTTPVersionInfo;

INTERFACE

USES
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

TYPE
   TJvgHTTPVersionInfo = CLASS(TJvComponent)
   PRIVATE
      WebBrowser: TWebBrowser;
      FVersionDataURL: STRING;
   PROTECTED
      FUNCTION GetVersion: STRING;
      FUNCTION GetDate: STRING;
      FUNCTION GetProgramURL: STRING;
      FUNCTION GetComments: STRING;

      PROCEDURE OnLoadVersionInfo(Sender: TObject; CONST pDisp: IDispatch; VAR
         URL: OleVariant);
   PUBLIC
      VersionInfo: TStringList;
      FUNCTION GetVersionInfo(WinControl: TWinControl): boolean;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY Version: STRING READ GetVersion;
      PROPERTY Date: STRING READ GetDate;
      PROPERTY ProgramURL: STRING READ GetProgramURL;
      PROPERTY Comments: STRING READ GetComments;
      PROPERTY VersionDataURL: STRING READ FVersionDataURL WRITE
         FVersionDataURL;

   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
//   RegisterComponents('Gl Components', [TJvgHTTPVersionInfo]);
END;

{ TJvgHTTPVersionInfo }

CONSTRUCTOR TJvgHTTPVersionInfo.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   VersionInfo := TStringList.Create;
END;

DESTRUCTOR TJvgHTTPVersionInfo.Destroy;
BEGIN
   VersionInfo.Free;
   INHERITED;
END;

FUNCTION TJvgHTTPVersionInfo.GetComments: STRING;
BEGIN
   Result := VersionInfo.Values['comments'];
END;

FUNCTION TJvgHTTPVersionInfo.GetDate: STRING;
BEGIN
   Result := VersionInfo.Values['date'];
END;

FUNCTION TJvgHTTPVersionInfo.GetProgramURL: STRING;
BEGIN
   Result := VersionInfo.Values['url'];
END;

FUNCTION TJvgHTTPVersionInfo.GetVersion: STRING;
BEGIN
   Result := VersionInfo.Values['version'];
END;

FUNCTION TJvgHTTPVersionInfo.GetVersionInfo(WinControl: TWinControl): boolean;
BEGIN
   IF trim(VersionDataURL) = '' THEN
      RAISE Exception.Create('Uncknown URL: property VersionDataURL is empty');

   WebBrowser := TWebBrowser.Create(NIL);
   WebBrowser.Visible := false;
   WebBrowser.Left := -10;
   WebBrowser.Width := 1;
   WebBrowser.Height := 1;
   TWinControl(WebBrowser).Parent := WinControl;

   TRY
      WebBrowser.OnDocumentComplete := OnLoadVersionInfo;
      WebBrowser.Navigate(VersionDataURL);
      REPEAT
         Application.ProcessMessages;
      UNTIL NOT WebBrowser.Busy;
   FINALLY
      WebBrowser.Free;
   END;
   Result := (Version <> '') OR (Date <> '') OR (ProgramURL <> '');
END;

PROCEDURE TJvgHTTPVersionInfo.OnLoadVersionInfo(Sender: TObject; CONST pDisp:
   IDispatch; VAR URL: OleVariant);
VAR
   Doc                        : variant;
   i                          : integer;
BEGIN
   Doc := WebBrowser.Document;
   VersionInfo.Text := Doc.Body.InnerText;
   //for i := 0 to VersionInfo.Count-1 do
   //  VersionInfo.Names[i] := LowerCase(VersionInfo.Names[i]);
END;

END.

