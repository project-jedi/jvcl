{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
unit AutosUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, JvComponent;

type
  TAutosForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    procedure PanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AutosForm: TAutosForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TAutosForm.PanelResize(Sender: TObject);
begin
  if Header.Width > 70 then
  begin
    Header.SectionWidth[2] := 66;
    Header.SectionWidth[1] := (Header.Width - 64) * 2 div 3;
    Header.SectionWidth[0] := (Header.Width - 64) div 3;
  end else
  begin
    Header.SectionWidth[0] := 2;
    Header.SectionWidth[1] := 2;
    Header.SectionWidth[2] := Header.Width - 4;
  end;
end;

end.