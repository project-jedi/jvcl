{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCheckVersionInfo.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgCheckVersionInfoForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TJvgfCheckVersionInfo = class(TForm)
    sbNext: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eCurentVersion: TEdit;
    eCurentVersionDate: TEdit;
    eLastVersion: TEdit;
    eLastVersionDate: TEdit;
    Label5: TLabel;
    Shape1: TShape;
    lURL: TLabel;
    Label7: TLabel;
    reComments: TRichEdit;
    Panel1: TPanel;
    Label6: TLabel;
    Label8: TLabel;
    Image1: TImage;
    Bevel2: TBevel;
    procedure sbNextClick(Sender: TObject);
    procedure lURLClick(Sender: TObject);
  public
    procedure Execute(WinControl: TWinControl);
  end;

implementation

uses
  ShellAPI,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvgHTTPVersionInfo;

{$R *.dfm}

{$IFNDEF USEJVCL}
resourcestring
  RsNoNewerVersionOfProgramAvailable = 'No newer version of program available';
{$ENDIF USEJVCL}

procedure TJvgfCheckVersionInfo.Execute(WinControl: TWinControl);
var
  VersionInfo: TJvgHTTPVersionInfo;
  S: string;
begin
  //  eCurentVersion.Text := globCon.APP_VERSION;
  //  eCurentVersionDate.Text := globCon.APP_DATE;
  VersionInfo := TJvgHTTPVersionInfo.Create(Self);
  try
    VersionInfo.VersionDataURL := 'http://shop.biblio-globus.ru/cpr/VersionInfo/SiteBuilder.htm';
    if VersionInfo.GetVersionInfo(WinControl) and (VersionInfo.Version > eCurentVersion.Text) then
    begin
      eLastVersion.Text := VersionInfo.Version;
      eLastVersionDate.Text := VersionInfo.Date;
      if VersionInfo.ProgramURL <> '' then
        lURL.Caption := VersionInfo.ProgramURL;
      reComments.Text := VersionInfo.Comments;

      ShowModal;
    end
    else
    begin
      S := RsNoNewerVersionOfProgramAvailable;
      Application.MessageBox(PChar(S), 'SiteBuilder', MB_OK + MB_ICONINFORMATION);
    end;
  finally
    VersionInfo.Free;
  end;
end;

procedure TJvgfCheckVersionInfo.sbNextClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TJvgfCheckVersionInfo.lURLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(lURL.Caption), nil, '', SW_SHOW);
end;

end.
