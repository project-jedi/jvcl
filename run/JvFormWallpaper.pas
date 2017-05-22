{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormWallpaper.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFormWallpaper;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Graphics, Controls,
  JvComponent;

type
  TJvOffsetMode = (omRows, omColumns);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvFormWallpaper = class(TJvGraphicControl)
  private
    FImage: TPicture;
    FOffset: Integer;
    FOffsetMode: TJvOffsetMode;
    procedure SetImage(Value: TPicture);
    procedure FormPaint(Sender: TObject);
    procedure SetOffset(const Value: Integer);
    procedure SetOffsetMode(const Value: TJvOffsetMode);
    procedure ValidateOffset;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align default alClient;
    property Image: TPicture read FImage write SetImage;
    property Offset: Integer read FOffset write SetOffset default 0;
    property OffsetMode: TJvOffsetMode read FOffsetMode write SetOffsetMode default omRows;
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

uses
  Forms;

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
  ValidateOffset;
  Invalidate;
end;

procedure TJvFormWallpaper.ValidateOffset;
begin
  case OffsetMode of
    omRows:
      if FOffset > FImage.Width then
        FOffset := FImage.Width;
    omColumns:
      if FOffset > FImage.Height then
        FOffset := FImage.Height;
  end;
end;

procedure TJvFormWallpaper.SetOffset(const Value: Integer);
begin
  FOffset := Value;
  ValidateOffset;
  Invalidate;
end;

procedure TJvFormWallpaper.SetOffsetMode(const Value: TJvOffsetMode);
begin
  FOffsetMode := Value;
  ValidateOffset;
  Invalidate;
end;

procedure TJvFormWallpaper.Paint;
var
  X, Y, OX, OY: Integer;
  Bmp: TBitmap;
begin
  if (FImage <> nil) and (FImage.Width > 0) and (FImage.Height > 0) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Width := Width;
      Bmp.Height := Height;
      OX := 0;
      OY := 0;
      for X := 0 to (Width + Abs(FOffset - FImage.Width)) div FImage.Width do
      begin
        if OffsetMode = omColumns then
          if X mod 2 = 0 then
            OY := 0
          else
            OY := FOffset - FImage.Height;
        for Y := 0 to (Height + Abs(OY)) div FImage.Height do
        begin
          if OffsetMode = omRows then
            if Y mod 2 = 0 then
              OX := 0
            else
              OX := FOffset - FImage.Width;
          Bmp.Canvas.Draw(X * FImage.Width + OX, Y * FImage.Height + OY, FImage.Graphic);
        end;
      end;
      Canvas.Draw(0, 0, Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TJvFormWallpaper.FormPaint(Sender: TObject);
begin
  if (Parent is TForm) and (TForm(Parent).FormStyle = fsMDIForm) then
    RequestAlign // Invalidate DOES NOT work here (Mantis 5689)
  else
    Invalidate;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
