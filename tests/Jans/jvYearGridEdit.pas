{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvYearGridEdit.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvYearGridEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TYearGridEditF = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    btnload: TButton;
    btnsave: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure btnloadClick(Sender: TObject);
    procedure btnsaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  YearGridEditF: TYearGridEditF;

implementation

{$R *.DFM}

procedure TYearGridEditF.btnloadClick(Sender: TObject);
begin
  if opendialog1.execute then
    memo1.Lines.LoadFromFile(opendialog1.filename);
  memo1.setfocus;
end;

procedure TYearGridEditF.btnsaveClick(Sender: TObject);
begin
  if savedialog1.execute then
    memo1.lines.SaveToFile(savedialog1.filename);
  memo1.setfocus;
end;

procedure TYearGridEditF.FormShow(Sender: TObject);
begin
  memo1.setfocus;
end;

end.
