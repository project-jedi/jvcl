{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}


{$I jvcl.inc}

unit RaHtHintsMainFormU;

interface
     
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvHint, JvComponent, JvFormPlacement;

type
  TRaHtHintsMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    RegAuto1: TJvFormStorage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

var
  RaHtHintsMainForm: TRaHtHintsMainForm;

implementation

{$R *.DFM}

procedure TRaHtHintsMainForm.FormCreate(Sender: TObject);
begin
  if (Memo1.Lines.Count > 0) and
     (Memo1.Lines[Memo1.Lines.Count - 1] = '') then
    Memo1.Lines.Delete(Memo1.Lines.Count - 1);
  Button1.Click;
  Application.HintHidePause := MaxInt;
end;

procedure TRaHtHintsMainForm.Button1Click(Sender: TObject);
begin
  Button1.Hint := Memo1.Text;
end;

initialization
  RegisterHtHints;
end.
