{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit fJvLineNumbersMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvEditor, JvHLEditor, StdCtrls, JvExControls, JvComponent;

type
  TJvLineNumbersMain = class(TForm)
    Panel1: TPanel;
    GutterFont: TLabel;
    JvHLEditor1: TJvEditor;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure JvHLEditor1PaintGutter(Sender: TObject; Canvas: TCanvas);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LineNumbersMain: TJvLineNumbersMain;

implementation

{$R *.DFM}

procedure TJvLineNumbersMain.JvHLEditor1PaintGutter(Sender: TObject;
  Canvas: TCanvas);
var
  i: integer;
  Rect: TRect;
  oldFont: TFont;
begin
  oldFont := TFont.Create;
  try
    oldFont.Assign(Canvas.Font);
    Canvas.Font := GutterFont.Font;
    with JvHLEditor1 do
      for i := TopRow to TopRow + VisibleRowCount do
      begin
        Rect := Bounds(2, (i - TopRow) * CellRect.Height, GutterWidth - 2 - 5, CellRect.Height);
        DrawText(Canvas.Handle, PChar(IntToStr(i + 1)), -1, Rect, DT_RIGHT or DT_VCENTER or DT_SINGLELINE);
      end;
  finally
    Canvas.Font := oldFont;
    oldFont.Free;
  end;
end;

procedure TJvLineNumbersMain.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    JVHLEditor1.Lines.LoadFromFile(OpenDialog1.Filename);
end;

end.

