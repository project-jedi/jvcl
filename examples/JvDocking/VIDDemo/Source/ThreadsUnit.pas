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
unit ThreadsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, JvComponent;

type
  TThreadsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    ComboBoxPanel: TPanel;
    ComboBox: TComboBox;
    procedure PanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ThreadsForm: TThreadsForm;

implementation

uses MainFormUnit, Math;

{$R *.DFM}

procedure TThreadsForm.PanelResize(Sender: TObject);
begin
  ComboBox.Width := ComboBoxPanel.Width;
  Header.SectionWidth[0] := 18;
  Header.SectionWidth[1] := Max((Header.Width - Header.SectionWidth[0]) div 10, 3);
  Header.SectionWidth[2] := Max((Header.Width - Header.SectionWidth[0]) * 4 div 10, 3);
  Header.SectionWidth[3] := Max((Header.Width - Header.SectionWidth[0]) * 4 div 10, 3);
  Header.SectionWidth[4] := Max((Header.Width - Header.SectionWidth[0]) div 10, 3);
end;

end.
