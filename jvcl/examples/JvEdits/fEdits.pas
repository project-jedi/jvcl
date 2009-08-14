{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit fEdits;

{$I 'jvcl.inc'}

interface

{$IFDEF VCL}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask,
  JvEdit, JvToolEdit, JvBaseEdits, JvExMask;
{$ENDIF VCL}
{$IFDEF VisualCLX}
uses
  QWindows, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QMask,
  JvQEdit, JvQToolEdit, JvQBaseEdits, JvQExMask, QComboEdits,
  JvQExComboEdits, JvQExControls, JvQComponent, JvQGradient;
{$ENDIF VisualCLX}

type
  TForm1 = class(TForm)
    JvFileNameBox1: TJvFilenameEdit;
    JvDirectoryBox1: TJvDirectoryEdit;
    JvDateEdit1: TJvDateEdit;
    JvButtonBox1: TJvComboEdit;
    JvCalcEdit1: TJvCalcEdit;
    procedure JvButtonBox1ButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

procedure TForm1.JvButtonBox1ButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked');
end;

procedure TForm1.FormShow(Sender: TObject);
var
  I: Integer;
begin
 for I := 0 to ControlCount - 1 do
   with Controls[I] do
   begin
     Hint := ClassName;
     ShowHint := True;
   end;

end;

end.