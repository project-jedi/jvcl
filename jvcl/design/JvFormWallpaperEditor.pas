{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormWallpaperEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormWallpaperEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, Types,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvFormWallpaper;

type
  TJvFormWallpaperEditor = class(TClassProperty)
  private
    procedure ApplyImage(Sender: TObject);
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

uses
  JvWallpaperEditForm, JvDsgnConsts;

procedure TJvFormWallpaperEditor.ApplyImage(Sender: TObject);
begin
  TJvFormWallpaper(GetComponent(0)).Image.Bitmap := TBitmap(Sender);
end;

procedure TJvFormWallpaperEditor.Edit;
var
  Image: TPicture;
  OldImage: TPicture;
begin
  Image := TJvFormWallpaper(GetComponent(0)).Image;
  OldImage := TPicture.Create;
  OldImage.Assign(Image);

  with TFoWallpaperChooser.Create(nil) do
  begin
    OnGlyph := ApplyImage;
    if ShowModal = mrOk then
      TJvFormWallpaper(GetComponent(0)).Image.Assign(Image)
    else
      TJvFormWallpaper(GetComponent(0)).Image.Assign(OldImage);
    Free;
  end;

  OldImage.Free;
end;

function TJvFormWallpaperEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

function TJvFormWallpaperEditor.GetValue: string;
begin
  Result := RsJvEditorString;
end;

procedure TJvFormWallpaperEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue(RsJvEditorString);
end;

procedure TJvFormWallpaperEditor.SetValue(const Value: string);
begin
  SetStrValue(RsJvEditorString);
end;

end.
