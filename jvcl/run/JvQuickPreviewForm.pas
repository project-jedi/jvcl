{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQuickPreviewU.PAS, released on 2002-06-15.

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

unit JvQuickPreviewForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, JvDrawImage;

type
  TQuickPreviewF = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    btnUse: TSpeedButton;
    procedure btnUseClick(Sender: TObject);
  private
    { Private declarations }
    PainterF: TJvDrawImage;
  public
    { Public declarations }
    procedure setDrawImage(ADrawImage: TJvDrawImage);
  end;

var
  QuickPreviewF: TQuickPreviewF;
  anifile: string;
  aniframe: integer;

implementation

{$R *.DFM}

procedure TQuickPreviewF.btnUseClick(Sender: TObject);

begin
  PainterF.canvas.draw(0, 0, image1.picture.bitmap);
  //PainterF.mypaint.Update ;
  close;
end;

procedure TQuickPreviewF.setDrawImage(ADrawImage: TJvDrawImage);
begin
  PainterF := aDrawImage;
end;

end.
