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
unit PropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls, Grids,
  JvComponent;

type
  TPropertiesForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ComboBox: TComboBox;
    Panel1: TPanel;
    StringGrid: TStringGrid;
    procedure PanelResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TPropertiesForm.PanelResize(Sender: TObject);
begin
  ComboBox.Left := 0;
  ComboBox.Width := Panel.Width;
end;

procedure TPropertiesForm.FormResize(Sender: TObject);
begin
  StringGrid.ColWidths[0] := (StringGrid.Width - 23) div 2;
  StringGrid.ColWidths[1] := (StringGrid.Width - 23) div 2;
end;

end.
