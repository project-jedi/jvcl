{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit ToolboxUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, JvComponent;

type
  TToolboxForm = class(TForm)
    lbDockClient1: TJvDockClient;
    HTML_Panel: TPanel;
    Server_Objects_Panel: TPanel;
    Design_Time_Controls_Panel: TPanel;
    ActiveX_Controls_Panel: TPanel;
    General_Panel1: TPanel;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ToolboxForm: TToolboxForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TToolboxForm.FormResize(Sender: TObject);
begin
  HTML_Panel.Width := ClientWidth;
  Server_Objects_Panel.Width := ClientWidth;
  Design_Time_Controls_Panel.Width := ClientWidth;
  ActiveX_Controls_Panel.Width := ClientWidth;
  General_Panel1.Width := ClientWidth;
end;

end.
