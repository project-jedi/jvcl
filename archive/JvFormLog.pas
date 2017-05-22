{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormLog.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormLog;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs,
  ComCtrls, ActnList, ImgList,
  JvListView, JvPrint, JvComponent, ToolWin;

type
  TFoLog = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Save: TAction;
    Print: TAction;
    ListView1: TJvListView;
    SaveDialog1: TSaveDialog;
    Print1: TJvPrint;
    procedure SaveExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TFoLog.SaveExecute(Sender: TObject);
begin
  if not SaveDialog1.Execute then
    Exit;
  if SaveDialog1.FilterIndex = 1 then
    ListView1.SaveToCSV(SaveDialog1.FileName)
  else
    ListView1.SaveToFile(SaveDialog1.FileName)
end;

procedure TFoLog.PrintExecute(Sender: TObject);
var
 I: Integer;
 Ts: TStringList;
begin
  Ts := TStringList.Create;
  with Ts do
    try
      for I := 0 to ListView1.Items.Count-1 do
        with ListView1.Items[I] do
         Add('[' + Caption + ']' + SubItems[0] + ' > ' + SubItems[1]);
      Print1.Print(Ts);
    finally
      Free;
    end;
end;

end.
