{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpecialImage.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-03-17

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpecialImage;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  JvTypes, JVCLVer;

type
  TJvBright = 0..200;

  TJvSpecialImage = class(TImage)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FInverted: Boolean;
    FFlipped: Boolean;
    FBrightness: TJvBright;
    FOriginal: TPicture;
    FMirrored: Boolean;
    FWorking: Boolean;
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
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Brightness: TJvBright read FBrightness write SetBright default 100;
    property Inverted: Boolean read FInverted write SetInverted default False;
    property Flipped: Boolean read FFlipped write SetFlipped default False;
    property Mirrored: Boolean read FMirrored write SetMirrored default False;
    property Picture: TPicture read GetPicture write SetPicture;
    procedure FadeIn;
    procedure FadeOut;
    procedure Reset;
  end;

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
  Line, Line2: PRGBarray;
  Dest: TBitmap;
  Val: Integer;
  Tmp: TRgbTriple;
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
      //Set brightness
      Val := (FBrightness - 100) * 255 div 100;
      if Val > 0 then
      begin
        for I := 0 to Dest.Height - 1 do
        begin
          Line := Dest.ScanLine[I];
          for J := 0 to Dest.Width - 1 do
            with Line[J] do
            begin
              if rgbtBlue + Val > 255 then
                rgbtBlue := 255
              else
                rgbtBlue := rgbtBlue + Val;
              if rgbtGreen + Val > 255 then
                rgbtGreen := 255
              else
                rgbtGreen := rgbtGreen + Val;
              if rgbtRed + Val > 255 then
                rgbtRed := 255
              else
                rgbtRed := rgbtRed + Val;
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
              if rgbtBlue + Val < 0 then
                rgbtBlue := 0
              else
                rgbtBlue := rgbtBlue + Val;
              if rgbtGreen + Val < 0 then
                rgbtGreen := 0
              else
                rgbtGreen := rgbtGreen + Val;
              if rgbtRed + Val < 0 then
                rgbtRed := 0
              else
                rgbtRed := rgbtRed + Val;
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
              rgbtBlue := not rgbtBlue;
              rgbtGreen := not rgbtGreen;
              rgbtRed := not rgbtRed;
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
  FWorking := true;
  Brightness := 100;
  Inverted := False;
  Flipped := False;
  Mirrored := False;
  FWorking := False;
  Picture.Assign(FOriginal);
end;

procedure TJvSpecialImage.SetBright(Value: TJvBright);
begin
  FBrightness := Value;
  ApplyChanges;
end;

procedure TJvSpecialImage.SetFlipped(const Value: Boolean);
begin
  if Value <> FFlipped then
  begin
    FFlipped := Value;
    ApplyChanges;
  end;
end;

procedure TJvSpecialImage.SetInverted(const Value: Boolean);
begin
  if Value <> FInverted then
  begin
    FInverted := Value;
    ApplyChanges;
  end;
end;

procedure TJvSpecialImage.SetMirrored(const Value: Boolean);
begin
  if Value <> FMirrored then
  begin
    FMirrored := Value;
    ApplyChanges;
  end;
end;

procedure TJvSpecialImage.SetPicture(const Value: TPicture);
begin
  FOriginal.Assign(Value);
  inherited Picture := Value;
end;

end.

