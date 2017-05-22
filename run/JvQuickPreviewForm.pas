{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQuickPreviewU.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQuickPreviewForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Controls, Forms, Dialogs, Buttons, ExtCtrls, StdCtrls,
  JvDrawImage, JvComponent;

type
  TQuickPreviewForm = class(TJvForm)
    ScrollBox1: TScrollBox;
    PreviewImage: TImage;
    Panel1: TPanel;
    BtnUse: TSpeedButton;
    procedure BtnUseClick(Sender: TObject);
  private
    FDrawImage: TJvDrawImage;
  public
    procedure SetDrawImage(ADrawImage: TJvDrawImage);
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


{$R *.dfm}

procedure TQuickPreviewForm.BtnUseClick(Sender: TObject);

begin
  if Assigned(FDrawImage) then
    FDrawImage.Canvas.Draw(0, 0, PreviewImage.Picture.Bitmap);
  Close;
end;

procedure TQuickPreviewForm.SetDrawImage(ADrawImage: TJvDrawImage);
begin
  FDrawImage := ADrawImage;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.