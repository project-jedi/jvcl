{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit glHelpPanel_demo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvgHelpPanel, JvComponent, JvExExtCtrls;

type
  TfTglHelpPanel = class(TForm)
    glHelpPanel1: TJvgHelpPanel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  fTglHelpPanel: TfTglHelpPanel;

implementation

{$R *.dfm}

procedure TfTglHelpPanel.FormCreate(Sender: TObject);
begin
  if FileExists(ExtractFilePath(Application.ExeName) + 'GlobusDemo.rtf') then
    glHelpPanel1.Strings.LoadFromFile(ExtractFilePath(Application.ExeName) + 'GlobusDemo.rtf')
  else
  begin
    glHelpPanel1.Strings.Add('GLOBUS VCL Extensions Library (JvGlobus)');
    glHelpPanel1.Strings.Add('A Set of Native Delphi Components for');
    glHelpPanel1.Strings.Add('Borland Delphi versions 5 - 7 and Borland C++ Builder 5 and 6.');
  end;
end;

end.

