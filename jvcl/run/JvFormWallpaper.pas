{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormWallpaper.PAS, released on 2001-02-28.

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

unit JvFormWallpaper;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  JVCLVer;

type
  TJvFormWallpaper = class(TGraphicControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FImage: TPicture;
    procedure SetImage(Value: TPicture);
    procedure FormPaint(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align default alClient;
    property Image: TPicture read FImage write SetImage;
  end;

implementation

constructor TJvFormWallpaper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TPicture.Create;
  FImage.OnChange := FormPaint;
  Align := alClient;
end;

destructor TJvFormWallpaper.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TJvFormWallpaper.SetImage(Value: TPicture);
begin
  FImage.Assign(Value);
  Invalidate;
end;

procedure TJvFormWallpaper.Paint;
var
  C, L: Integer;
  Bmp: TBitmap;
begin
  inherited;
  if (FImage <> nil) and (FImage.Width > 0) and (FImage.Height > 0) then
  begin
    Bmp := TBitmap.Create;
    Bmp.Width := Width;
    Bmp.Height := Height;
    for C := 0 to (Width div FImage.Width) do
      for L := 0 to (Height div FImage.Height) do
        Bmp.Canvas.Draw(C * FImage.Width, L * FImage.Height, FImage.Graphic);
    Canvas.Draw(0, 0, Bmp);
    Bmp.Free;
  end;
end;

procedure TJvFormWallpaper.FormPaint(Sender: TObject);
begin
  Invalidate;
end;

end.

