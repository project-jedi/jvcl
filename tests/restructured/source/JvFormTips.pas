{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormTips.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormTips;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  JvButton, JvSpeedButton;

type
  TFormTip = class(TForm)
    Panel1: TPanel;
    pnlInner: TPanel;
    pnlTip: TPanel;
    Image1: TImage;
    did: TLabel;
    tip: TLabel;
    pnlBottom: TPanel;
    showtips: TCheckBox;
    BUSpeedButton1: TJvSpeedButton;
    BUSpeedButton2: TJvSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BUButton1Click(Sender: TObject);
  private
  public
    Hints: TStringList;
    It: Integer;
  end;

implementation

{$R *.DFM}

{**************************************************}

procedure TFormTip.FormCreate(Sender: TObject);
begin
  Hints := TStringList.Create;
end;

{**************************************************}

procedure TFormTip.FormDestroy(Sender: TObject);
begin
  Hints.Free;
end;

{**************************************************}

procedure TFormTip.BUButton1Click(Sender: TObject);
begin
  if It = Hints.Count - 1 then
    It := 0
  else
    Inc(It);
  tip.Caption := Hints[It];
end;

end.
