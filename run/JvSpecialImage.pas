{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpecialImage.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSpecialImage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls, Forms,
  JvTypes, JvExExtCtrls;

type
  TJvBright = 0..200;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSpecialImage = class(TJvExImage)
  private
    FInverted: Boolean;
    FFlipped: Boolean;
    FBrightness: TJvBright;
    FOriginal: TPicture;
    FMirrored: Boolean;
    FWorking: Boolean;
    FChangingLocalProperty: Boolean;
    procedure SetBright(Value: TJvBright);
    procedure SetFlipped(const Value: Boolean);
    procedure SetInverted(const Value: Boolean);
    procedure SetMirrored(const Value: Boolean);
    procedure PictureChanged(Sender: TObject);
    procedure ApplyChanges;
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brightness: TJvBright read FBrightness write SetBright default 100;
    property Inverted: Boolean read FInverted write SetInverted default False;
    property Flipped: Boolean read FFlipped write SetFlipped default False;
    property Mirrored: Boolean read FMirrored write SetMirrored default False;
    property Picture: TPicture read GetPicture write SetPicture;
    procedure FadeIn;
    procedure FadeOut;
    procedure Reset;
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


constructor TJvSpecialImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOriginal := TPicture.Create;
  FBrightness := 100;
  FInverted := False;
  FFlipped := False;
  FMirrored := False;
  FWorking := False;
  FChangingLocalProperty := False;
  Picture.OnChange := PictureChanged;
end;

destructor TJvSpecialImage.Destroy;
begin
  Picture.Assign(FOriginal);
  FOriginal.Free;
  inherited Destroy;
end;

procedure TJvSpecialImage.Loaded;
begin
  inherited Loaded;
  FOriginal.Assign(Picture);
end;

procedure TJvSpecialImage.ApplyChanges;
var
  I, J: Integer;
  Line, Line2: PJvRGBArray;
  Dest: TBitmap;
  Val: Integer;
  Tmp: TJvRGBTriple;
begin
  if FWorking or (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FWorking := True;
  Dest := TBitmap.Create;
  try
    //Copy original bitmap
    Dest.Width := FOriginal.Width;
    Dest.Height := FOriginal.Height;
    Dest.Canvas.Draw(0, 0, FOriginal.Graphic);
    Dest.PixelFormat := pf24Bit;

    if not Dest.Empty then
    begin
      // Set brightness
      Val := (FBrightness - 100) * 255 div 100;
      if Val > 0 then
      begin
        for I := 0 to Dest.Height - 1 do
        begin
          Line := Dest.ScanLine[I];
          for J := 0 to Dest.Width - 1 do
            with Line[J] do
            begin
              if rgbBlue + Val > 255 then
                rgbBlue := 255
              else
                rgbBlue := rgbBlue + Val;
              if rgbGreen + Val > 255 then
                rgbGreen := 255
              else
                rgbGreen := rgbGreen + Val;
              if rgbRed + Val > 255 then
                rgbRed := 255
              else
                rgbRed := rgbRed + Val;
            end;
        end;
      end
      else
      if Val < 0 then
      begin
        for I := 0 to Dest.Height - 1 do
        begin
          Line := Dest.ScanLine[I];
          for J := 0 to Dest.Width - 1 do
            with Line[J] do
            begin
              if rgbBlue + Val < 0 then
                rgbBlue := 0
              else
                rgbBlue := rgbBlue + Val;
              if rgbGreen + Val < 0 then
                rgbGreen := 0
              else
                rgbGreen := rgbGreen + Val;
              if rgbRed + Val < 0 then
                rgbRed := 0
              else
                rgbRed := rgbRed + Val;
            end;
        end;
      end;

      //Set Flipped
      if FFlipped then
      begin
        for I := 0 to (Dest.Height - 1) div 2 do
        begin
          Line := Dest.ScanLine[I];
          Line2 := Dest.ScanLine[Dest.Height - I - 1];
          for J := 0 to Dest.Width - 1 do
          begin
            Tmp := Line[J];
            Line[J] := Line2[J];
            Line2[J] := Tmp;
          end;
        end;
      end;

      //Set inverted
      if FInverted then
      begin
        for I := 0 to Dest.Height - 1 do
        begin
          Line := Dest.ScanLine[I];
          for J := 0 to Dest.Width - 1 do
            with Line[J] do
            begin
              rgbBlue := not rgbBlue;
              rgbGreen := not rgbGreen;
              rgbRed := not rgbRed;
            end;
        end;
      end;

      //Set mirrored
      if FMirrored then
      begin
        for I := 0 to Dest.Height - 1 do
        begin
          Line := Dest.ScanLine[I];
          for J := 0 to (Dest.Width - 1) div 2 do
          begin
            Tmp := Line[J];
            Line[J] := Line[Dest.Width - J - 1];
            Line[Dest.Width - J - 1] := Tmp;
          end;
        end;
      end;
    end;
    // We only need to assign the new picture if it occured after having
    // changed one of the local properties: Mirrored, Brightness, Inverted,
    // Flipped. This way we prevent freeing the graphic used by the inherited
    // procedures with the following assignment.
    // The most common example is when setting Transparent:
    //
    // In ExtCtrls.pas, you have this code:
    //
    // G.Transparent := FTransparent;
    //
    // This changes the picture, leading to this call stack:
    // TJvSpecialImage.PictureChanged -> calls ApplyChanges then
    // TJvSpecialImage.ApplyChanges -> calls inherited Picture.Assign(Dest) then
    // TPicture.Assign -> calls TPicture.SetGraphic then
    // FGraphic is freed, which is Picture.Graphic which is G.
    //
    // Hence, to prevent the freeing of G, we don't call the inherited
    // Picture.Assign, and it does not event prevent the image from being
    // updated with the correct values for the local properties as thoses were
    // already applied at a previous time when their calls were made.
    // This was Mantis 2693.
    if FChangingLocalProperty then
      inherited Picture.Assign(Dest);
  finally
    Dest.Free;
    FWorking := False;
  end;
end;

procedure TJvSpecialImage.FadeIn;
var
  I: Integer;
begin
  // (rom) needs better implementation. Timing is by CPU/graphics speed.
  for I := 0 to 50 do
  begin
    Brightness := I * 2;
    Application.ProcessMessages;
  end;
end;

procedure TJvSpecialImage.FadeOut;
var
  I: Integer;
begin
  // (rom) needs better implementation. Timing is by CPU/graphics speed.
  for I := 50 downto 0 do
  begin
    Brightness := I * 2;
    Application.ProcessMessages;
  end;
end;

function TJvSpecialImage.GetPicture: TPicture;
begin
  Result := inherited Picture;
end;

procedure TJvSpecialImage.PictureChanged(Sender: TObject);
begin
  if FWorking = False then
  begin
    FOriginal.Assign(inherited Picture);
    ApplyChanges; // SetBright(FBrightness);
  end;
  Invalidate;
end;

procedure TJvSpecialImage.Reset;
begin
  FWorking := True;
  Brightness := 100;
  Inverted := False;
  Flipped := False;
  Mirrored := False;
  FWorking := False;
  Picture.Assign(FOriginal);
end;

procedure TJvSpecialImage.SetBright(Value: TJvBright);
begin
  FChangingLocalProperty := True;
  try
    FBrightness := Value;
    ApplyChanges;
  finally
    FChangingLocalProperty := False;
  end;
end;

procedure TJvSpecialImage.SetFlipped(const Value: Boolean);
begin
  if Value <> FFlipped then
  begin
    FChangingLocalProperty := True;
    try
      FFlipped := Value;
      ApplyChanges;
    finally
      FChangingLocalProperty := False;
    end;
  end;
end;

procedure TJvSpecialImage.SetInverted(const Value: Boolean);
begin
  if Value <> FInverted then
  begin
    FChangingLocalProperty := True;
    try
      FInverted := Value;
      ApplyChanges;
    finally
      FChangingLocalProperty := False;
    end;
  end;
end;

procedure TJvSpecialImage.SetMirrored(const Value: Boolean);
begin
  if Value <> FMirrored then
  begin
    FChangingLocalProperty := True;
    try
      FMirrored := Value;
      ApplyChanges;
    finally
      FChangingLocalProperty := False;
    end;
  end;
end;

procedure TJvSpecialImage.SetPicture(const Value: TPicture);
begin
  FOriginal.Assign(Value);
  inherited Picture := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
