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
unit frmVSNetStyle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle, Menus,
  JvComponent;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    lbDockClient1: TJvDockClient;
    procedure lbDockClient1FormShow(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses Main;

{$R *.DFM}

procedure TForm4.lbDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm4.lbDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
