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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpecialImage;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms, JvTypes, JVCLVer;

type
  TJvSpecialImage = class(TImage)
  private
    FInverted: Boolean;
    FFlipped: Boolean;
    FBright: TBright;
    FOriginal: TPicture;
    FMirrored: Boolean;
    FWorking: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetBright(Value: TBright);
    procedure SetFlipped(const Value: Boolean);
    procedure SetInverted(const Value: Boolean);
    procedure SetMirrored(const Value: Boolean);
    procedure PictureChanged(Sender: TObject);
    procedure ApplyChanges;
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Brightness: TBright read FBright write SetBright default 100;
    property Inverted: Boolean read FInverted write SetInverted default False;
    property Flipped: Boolean read FFlipped write SetFlipped default False;
    property Mirrored: Boolean read FMirrored write SetMirrored default False;
    property Picture: TPicture read GetPicture write SetPicture;
    procedure FadeIn;
    procedure FadeOut;
    procedure Reset;
  end;

implementation

{************************************************************}

procedure TJvSpecialImage.ApplyChanges;
var
  i, j: Integer;
  Line, Line2: PRGBarray;
  dest: TBitmap;
  val: Integer;
  tmp: TRgbTriple;
begin
  if FWorking or (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FWorking := True;
  dest := TBitmap.Create;
  try

    //Copy original bitmap
    dest.Width := FOriginal.Width;
    dest.Height := FOriginal.Height;
    dest.Canvas.Draw(0, 0, FOriginal.Graphic);
    dest.PixelFormat := pf24Bit;

    if not dest.Empty then
    begin
      //Set brightness
      Val := (FBright - 100) * 255 div 100;
      if Val > 0 then
      begin
        for i := 0 to dest.Height - 1 do
        begin
          Line := dest.ScanLine[i];
          for j := 0 to dest.Width - 1 do
            with Line[j] do
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
      else if Val < 0 then
      begin
        for i := 0 to dest.Height - 1 do
        begin
          Line := dest.ScanLine[i];
          for j := 0 to dest.Width - 1 do
            with Line[j] do
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
        for i := 0 to (dest.Height - 1) div 2 do
        begin
          Line := dest.ScanLine[i];
          Line2 := dest.ScanLine[dest.Height - i - 1];
          for j := 0 to dest.Width - 1 do
          begin
            tmp := Line[j];
            Line[j] := Line2[j];
            Line2[j] := tmp;
          end;
        end;
      end;

      //Set inverted
      if FInverted then
      begin
        for i := 0 to dest.Height - 1 do
        begin
          Line := dest.ScanLine[i];
          for j := 0 to dest.Width - 1 do
            with Line[j] do
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
        for i := 0 to dest.Height - 1 do
        begin
          Line := Dest.ScanLine[i];
          for j := 0 to (Dest.Width - 1) div 2 do
          begin
            tmp := Line[j];
            Line[j] := Line[dest.Width - j - 1];
            Line[dest.Width - j - 1] := tmp;
          end;
        end;
      end;
    end;
    inherited Picture.Assign(dest);
  finally
    dest.Free;
    FWorking := False;
  end;
end;

{************************************************************}

constructor TJvSpecialImage.Create(AOwner: TComponent);
begin
  inherited;
  FOriginal := TPicture.Create;
  FBright := 100;
  FInverted := False;
  FFlipped := False;
  FMirrored := False;
  FWorking := False;
  Picture.OnChange := PictureChanged;
end;

{************************************************************}

destructor TJvSpecialImage.Destroy;
begin
  Picture.Assign(FOriginal);
  FOriginal.Free;
  inherited;
end;

{************************************************************}

procedure TJvSpecialImage.FadeIn;
var
  i: Integer;
begin
  for i := 0 to 50 do
  begin
    SetBright(i * 2);
    Application.ProcessMessages;
  end;
end;

{************************************************************}

procedure TJvSpecialImage.FadeOut;
var
  i: Integer;
begin
  for i := 50 downto 0 do
  begin
    SetBright(i * 2);
    Application.ProcessMessages;
  end;
end;

{************************************************************}

function TJvSpecialImage.GetPicture: TPicture;
begin
  Result := inherited Picture;
end;

procedure TJvSpecialImage.PictureChanged(Sender: TObject);
begin
  if FWorking = False then
    ApplyChanges; // SetBright(FBright);
  Invalidate;
end;

{************************************************************}

procedure TJvSpecialImage.Reset;
begin
  FWorking := true;
  Brightness := 100;
  Inverted := False;
  Flipped := False;
  Mirrored := False;
  FWorking := false;
  Picture.Assign(FOriginal);
end;

{************************************************************}

procedure TJvSpecialImage.SetBright(Value: TBright);
begin
  FBright := Value;
  ApplyChanges;
end;

{************************************************************}

procedure TJvSpecialImage.SetFlipped(const Value: Boolean);
begin
  if Value <> FFlipped then
  begin
    FFlipped := Value;
    ApplyChanges;
  end;
end;

{************************************************************}

procedure TJvSpecialImage.SetInverted(const Value: Boolean);
begin
  if Value <> FInverted then
  begin
    FInverted := Value;
    ApplyChanges;
  end;
end;

{************************************************************}

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

