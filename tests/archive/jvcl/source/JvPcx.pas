{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPcx.PAS, released on 2001-02-28.

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

unit JvPcx;

// (rom) Warning! This file seems riddled with bugs
// (rom) Warning! My changes to Rle handling are completely untested

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms,
  JvTypes, JvRle;

type
  TJvPcx = class(TGraphic)
  private
    FBitmap: TBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetBitmap: TBitmap;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetPixelFormat: TPixelFormat;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    function HasBitmap: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: PChar);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ResType: PChar);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
  end;

  EPcxError = class(EJVCLException);

implementation

resourcestring
  RC_PcxUnknownFormat = 'PCX: Unknown format';
  RC_PcxPaletteProblem = 'PCX: Unable to retrieve palette';
  RC_PcxInvalid = 'PCX: Invalid PCX file';

  RC_PcxExtension = 'pcx';
  RC_PcxFilterName = 'PCX Image';

constructor TJvPcx.Create;
begin
  inherited Create;
  // (rom) should the bitmap be nonempty on creation?
  FBitmap := TBitmap.Create;
  FBitmap.Width := 0;
  FBitmap.Height := 0;
  FBitmap.Palette := 0;
end;

destructor TJvPcx.Destroy;
begin
  if FBitmap <> nil then
  begin
    // (rom) isnt this handled by TBitmap?
    if FBitmap.Palette <> 0 then
      DeleteObject(FBitmap.Palette);
    FBitmap.Free;
  end;
  inherited Destroy;
end;

procedure TJvPcx.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source = nil then
    FreeAndNil(FBitmap)
  else
  if (Source is TJvPcx) and (Source <> Self) then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;

    Stream := TMemoryStream.Create;
    TJvPcx(Source).SaveToStream(Stream);
    Stream.Position := 0;
    LoadFromStream(Stream);
  end
  else
  if Source is TBitmap then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;
    FBitmap.Assign(TBitmap(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TJvPcx.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvPcx then
    Dest.Assign(Self)
  else
  if Dest is TGraphic then
  begin
    if Empty then
      Dest.Assign(nil)
    else
      (Dest as TGraphic).Assign(FBitmap);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJvPcx.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FBitmap <> nil then
    ACanvas.StretchDraw(ARect, FBitmap);
end;

function TJvPcx.Equals(Graphic: TGraphic): Boolean;
begin
  if FBitmap = nil then
    Result := Graphic = nil
  else
    Result := (Graphic is TJvPcx) and (FBitmap = TJvPcx(TJvPcx).FBitmap);
end;

function TJvPcx.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TJvPcx.GetEmpty: Boolean;
begin
  // (rom) does this test match the constructor?
  Result := FBitmap = nil;
end;

function TJvPcx.GetHeight: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Height
  else
    Result := -1;
end;

function TJvPcx.GetPalette: HPALETTE;
begin
  if FBitmap <> nil then
    Result := FBitmap.Palette
  else
    Result := 0;
end;

function TJvPcx.GetPixelFormat: TPixelFormat;
begin
  if FBitmap <> nil then
    Result := FBitmap.PixelFormat
  else
    Result := pfCustom;
end;

function TJvPcx.GetTransparent: Boolean;
begin
  if FBitmap <> nil then
    Result := FBitmap.Transparent
  else
    Result := False;
end;

function TJvPcx.GetWidth: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Width
  else
    // (rom) is this a good value?
    Result := -1;
end;

function TJvPcx.HasBitmap: Boolean;
begin
  Result := FBitmap <> nil;
end;

procedure TJvPcx.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // (rom) shouldnt this create a bitmap if needed?
  if FBitmap <> nil then
    FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
end;

procedure TJvPcx.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Self.LoadFromStream(Stream);
  Stream.Free;
end;

procedure TJvPcx.LoadFromResourceID(Instance: THandle; ResID: Integer;
  ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResId, ResType);
  Self.LoadFromStream(Stream);
  Stream.Free;
end;

procedure TJvPcx.LoadFromResourceName(Instance: THandle;
  const ResName: string; ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  Self.LoadFromStream(Stream);
  Stream.Free;
end;

procedure TJvPcx.LoadFromStream(Stream: TStream);
var
  I, J, K, L, M: Integer;
  W1, W2, W3, W4: Word;
  BytesPerLine: Integer;
  Bpp, Planes: Integer;
  Decompressed: TMemoryStream;
  Buf: array [0..128] of Byte;
  IBuf2: PRGBArray;
  IBuf: array [0..MaxPixelCount - 1] of Byte;
  IBufTmp: PByteArray;
  RPal: TMaxLogPalette;
begin
  // (rom) changed from <> to =
  if FBitmap = nil then
    FBitmap := TBitmap.Create;

  I := Stream.Read(Buf, 128);

  FBitmap.Width := 0;
  FBitmap.Height := 0;
  if I = 128 then
  begin
    if Buf[0] = 10 then
    begin
      W1 := Buf[4] + Buf[5] * 256;
      W2 := Buf[6] + Buf[7] * 256;
      W3 := Buf[8] + Buf[9] * 256;
      W4 := Buf[10] + Buf[11] * 256;
      FBitmap.Width := (W3 - W1) + 1;
      FBitmap.Height := (W4 - W2) + 1;
      // (rom) FloodFill seems silly
      FBitmap.Canvas.FloodFill(0, 0, clWhite, fsSurface);
      BytesPerLine := (Buf[66] + Buf[67] * 256) * Buf[65];
      L := FBitmap.Width * 2;
      M := FBitmap.Width;
      Bpp := Buf[3];
      Planes := Buf[65];
      case Bpp of
        1:
          case Planes of
            1:
              begin
                FBitmap.PixelFormat := pf1bit;
                FBitmap.Monochrome := True;
                FBitmap.IgnorePalette := True;
                FBitmap.Palette := 0;
              end;
            4:
              begin
                FBitmap.PixelFormat := pf4bit;
                FBitmap.Monochrome := False;
                FBitmap.IgnorePalette := False;
                FBitmap.Palette := 0;
              end;
          end;
        8:
          case Planes of
            1:
              begin //256 colors
                FBitmap.PixelFormat := pf8bit;
                Fbitmap.Monochrome := False;
                Fbitmap.IgnorePalette := False;
                FBitmap.Palette := 0;
              end;
            3:
              begin //16 millions colors
                FBitmap.PixelFormat := pf24bit;
                Fbitmap.Monochrome := False;
                Fbitmap.IgnorePalette := True;
                FBitmap.Palette := 0;
              end;
          else
            raise EPcxError.Create(RC_PcxUnknownFormat);
          end;
      else
        raise EPcxError.Create(RC_PcxUnknownFormat);
      end;

      Decompressed := TMemoryStream.Create;
      Decompressed.CopyFrom(Stream, Stream.Size - Stream.Position);
      RleDecompress(Decompressed);

      if (Bpp = 1) and (Planes = 1) then
      begin
        //monochrome okay
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBufTmp := FBitmap.ScanLine[I];
          Decompressed.Read(IBuf, BytesPerLine);
          CopyMemory(IBufTmp, @IBuf, BytesPerLine);
        end;
      end
      else
      if (Bpp = 1) and (Planes = 4) then
      begin
        //16 colors
        //palette
        Stream.Position := 16;
        if Stream.Read(IBuf, 48) <> 48 then
          raise EPcxError.Create(RC_PcxPaletteProblem)
        else
        begin
          RPal.palVersion := $300;
          RPal.palNumEntries := 16;
          for M := 0 to 15 do
          begin
            I := M * 3;
            RPal.palPalEntry[M].peRed := IBuf[I];
            RPal.palPalEntry[M].peGreen := IBuf[I + 1];
            RPal.palPalEntry[M].peBlue := IBuf[I + 2];
            RPal.palPalEntry[M].peFlags := 0;
          end;
          // (rom) why not like in the following 256 color block?
          FBitmap.Palette := CreatePalette(PLogPalette(@RPal)^);
          PaletteModified := True;
        end;

        //Reading data
        for I := 0 to FBitmap.Height - 1 do
        begin
          Decompressed.Read(IBuf, BytesPerLine);
          IBufTmp := FBitmap.Scanline[I];
          FillChar(IBufTmp^, (FBitmap.Width div 2) + 1, #0);

          L := 0;
          for K := 0 to FBitmap.Width - 1 do
            if ((IBuf[L + K div 8]) and (1 shl (7 - (K mod 8)))) <> 0 then
              if K mod 2 <> 0 then
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $01
              else
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $10;

          L := BytesPerLine div 4;
          for K := 0 to FBitmap.Width - 1 do
            if ((IBuf[L + K div 8]) and (1 shl (7 - (K mod 8)))) <> 0 then
              if K mod 2 <> 0 then
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $02
              else
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $20;

          L := (BytesPerLine div 4) * 2;
          for K := 0 to FBitmap.Width - 1 do
            if ((IBuf[L + K div 8]) and (1 shl (7 - (K mod 8)))) <> 0 then
              if K mod 2 <> 0 then
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $04
              else
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $40;

          L := (BytesPerLine div 4) * 3;
          for K := 0 to FBitmap.Width - 1 do
            if ((IBuf[L + K div 8]) and (1 shl (7 - (K mod 8)))) <> 0 then
              if K mod 2 <> 0 then
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $08
              else
                IBufTmp^[K div 2] := IBufTmp^[K div 2] + $80;
        end;
      end
      else
      if (Bpp = 8) and (Planes = 1) then
      begin
        //256 Colors. Okay
        Stream.Position := Stream.Size - (256 * 3 + 1);
        Stream.Read(IBuf, 1000);
        if IBuf[0] = 12 then
        begin
          RPal.palVersion := $300;
          RPal.palNumEntries := 256;
          for M := 0 to 255 do
          begin
            I := M * 3 + 1;
            RPal.palPalEntry[M].peRed := IBuf[I];
            RPal.palPalEntry[M].peGreen := IBuf[I + 1];
            RPal.palPalEntry[M].peBlue := IBuf[I + 2];
            RPal.palPalEntry[M].peFlags := 0;
          end;
          FBitmap.Palette := CreatePalette(PLogPalette(@RPal)^);
          FBitmap.PaletteModified := True;
          // (rom) this is also done at function exit
          PaletteModified := True;
          Changed(Self);
        end;
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBufTmp := FBitmap.ScanLine[I];
          Decompressed.Read(IBuf, BytesPerLine);
          CopyMemory(IBufTmp, @IBuf, BytesPerLine);
        end;
      end
      else
      if (Bpp = 8) and (Planes = 3) then
      begin
        //24 bit. Okey
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBuf2 := Fbitmap.ScanLine[I];
          Decompressed.Read(IBuf, BytesPerLine);
          for J := 0 to FBitmap.Width - 1 do
          begin
            IBuf2[J].rgbtRed := IBuf[J];
            IBuf2[J].rgbtGreen := IBuf[J + M];
            IBuf2[J].rgbtBlue := IBuf[J + L];
          end;
        end;
        // (rom) why not remove the palette here?
      end;
      Decompressed.Free;
    end
    else
      raise EPcxError.Create(RC_PcxInvalid);
  end
  else
    raise EPcxError.Create(RC_PcxInvalid);

  PaletteModified := True;
  Changed(Self);
end;

procedure TJvPcx.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  if FBitmap <> nil then
    FBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
end;

procedure TJvPcx.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TJvPcx.SaveToStream(Stream: TStream);
var
  I, J, K, L: Integer;
  BytesPerLine: Word;
  MemStream: TMemoryStream;
  B: Byte;
  IBuf2: PRGBArray;
  IBuf: array [0..MaxPixelCount - 1] of Byte;
  IBufTmp: PByteArray;
  WPal: array [0..255] of PALETTEENTRY;
begin
  if FBitmap = nil then
    Exit;
  if (FBitmap.PixelFormat <> pf1Bit) and (FBitmap.PixelFormat <> pf4Bit) and (FBitmap.PixelFormat <> pf8Bit)
    and (FBitmap.PixelFormat <> pf24Bit) then
    FBItmap.PixelFormat := pf24bit;

  // Stream is the temporary Stream to put the data
  MemStream := TMemoryStream.Create;

  // Creating header first
  FillChar(IBuf, 128, 0);
  IBuf[0] := $A;
  IBuf[1] := 5;
  IBuf[2] := 1;
  IBuf[8] := Lo(FBitmap.Width) - 1;
  IBuf[9] := Hi(FBitmap.Width);
  IBuf[10] := Lo(FBitmap.Height) - 1;
  IBuf[11] := Hi(FBitmap.Height);
  IBuf[12] := 1;
  IBuf[13] := 300 - 256;
  IBuf[14] := 1;
  IBuf[15] := 300 - 256;
  IBuf[69] := 1;
  IBuf[70] := Hi(Screen.Height);
  IBuf[71] := Lo(Screen.Height);
  IBuf[72] := Hi(Screen.Width);
  IBuf[73] := Lo(Screen.Width);

  case FBitmap.PixelFormat of
    pf1bit:
      begin
        IBuf[3] := 1;
        IBuf[65] := 1;
        BytesPerLine := FBitmap.Width div 8;
        if FBitmap.Width mod 8 <> 0 then
          Inc(BytesPerLine);
        IBuf[66] := Lo(BytesPerLine);
        IBuf[67] := Hi(BytesPerLine);
        Stream.Write(IBuf, 128);

        //Write data
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBufTmp := FBitmap.ScanLine[I];
          CopyMemory(@IBuf, IBufTmp, BytesPerLine);
          MemStream.Write(IBuf, BytesPerLine);
        end;
      end;
    pf4bit:
      begin
        IBuf[3] := 1;
        IBuf[65] := 4;
        BytesPerLine := FBitmap.Width div 8;
        if FBitmap.Width mod 8 <> 0 then
          Inc(BytesPerLine);
        I := BytesPerLine;
        BytesPerLine := BytesPerLine * 4;
        IBuf[66] := Lo(I);
        IBuf[67] := Hi(I);

        //Write palette
        if FBitmap.Palette <> 0 then
        begin
          GetPaletteEntries(FBitmap.Palette, 0, 16, WPal);
          for I := 0 to 15 do
          begin
            IBuf[16 + I * 3] := WPal[I].peRed;
            IBuf[16 + I * 3 + 1] := WPal[I].peGreen;
            IBuf[16 + I * 3 + 2] := WPal[I].peBlue;
          end;
        end;
        Stream.Write(IBuf, 128);

        //Write data
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBufTmp := FBitmap.Scanline[I];
          FillChar(IBuf, BytesPerLine, #0);

          //Red
          L := 0;
          for J := 0 to FBitmap.Width - 1 do
            if (J mod 2) = 0 then
            begin
              if (IBufTmp^[J div 2] and $10) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end
            else
            begin
              if (IBufTmp^[J div 2] and $01) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end;

          //Green
          L := BytesPerLine div 4;
          for J := 0 to FBitmap.Width - 1 do
            if (J mod 2) = 0 then
            begin
              if (IBufTmp^[J div 2] and $20) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end
            else
            begin
              if (IBufTmp^[J div 2] and $02) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end;

          //Blue
          L := BytesPerLine div 2;
          for J := 0 to FBitmap.Width - 1 do
            if (J mod 2) = 0 then
            begin
              if (IBufTmp^[J div 2] and $40) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end
            else
            begin
              if (IBufTmp^[J div 2] and $04) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end;

          //Intensity
          L := (BytesPerLine div 4) * 3;
          for J := 0 to FBitmap.Width - 1 do
            if (J mod 2) = 0 then
            begin
              if (IBufTmp^[J div 2] and $80) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end
            else
            begin
              if (IBufTmp^[J div 2] and $08) <> 0 then
                IBuf[L + J div 8] := IBuf[L + J div 8] + (1 shl (7 - (J mod 8)));
            end;

          MemStream.Write(IBuf, BytesPerLine);
        end;
      end;
    pf8bit:
      begin
        IBuf[3] := 8;
        IBuf[65] := 1;
        BytesPerLine := FBitmap.Width;
        IBuf[66] := Lo(BytesPerLine);
        IBuf[67] := Hi(BytesPerLine);
        Stream.Write(IBuf, 128);

        //Write Data
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBufTmp := Fbitmap.ScanLine[I];
          CopyMemory(@IBuf, IBufTmp, BytesPerLine);
          MemStream.Write(IBuf, BytesPerLine);
        end;
      end;
    pf24bit:
      begin
        IBuf[3] := 8;
        IBuf[65] := 3;
        BytesPerLine := FBitmap.Width * 3;
        I := FBitmap.Width;
        IBuf[66] := lo(I);
        IBuf[67] := hi(I);
        Stream.Write(IBuf, 128);

        //Write data
        for I := 0 to FBitmap.Height - 1 do
        begin
          IBuf2 := FBitmap.ScanLine[I];

          for J := 0 to FBitmap.Width - 1 do
            IBuf[J] := IBuf2[J].rgbtRed;

          K := FBitmap.Width;
          for J := 0 to FBitmap.Width - 1 do
            IBuf[J + K] := IBuf2[J].rgbtGreen;

          K := FBitmap.Width * 2;
          for J := 0 to FBitmap.Width - 1 do
            IBuf[J + K] := IBuf2[J].rgbtBlue;

          MemStream.Write(IBuf, BytesPerLine);
        end;
      end;
  end;

  //RLE Compress temporary Stream
  // (rom) palette stays uncompressed because it has been written to Stream
  RleCompress(MemStream);
  //Copy temporary Stream to final Stream
  MemStream.Position := 0;
  Stream.CopyFrom(MemStream, MemStream.Size);

  //Write palette if mode 256 color.
  if (FBitmap.PixelFormat = pf8bit) and (FBitmap.Palette <> 0) then
  begin
    GetPaletteEntries(FBitmap.Palette, 0, 256, WPal);
    for I := 0 to 255 do
    begin
      IBuf[I * 3] := WPal[I].peRed;
      IBuf[I * 3 + 1] := WPal[I].peGreen;
      IBuf[I * 3 + 2] := WPal[I].peBlue;
    end;
    B := 12;
    Stream.Write(B, 1);
    Stream.Write(IBuf, 256 * 3);
  end;

  MemStream.Free;
end;

procedure TJvPcx.SetHeight(Value: Integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Height := Value;
    Changed(Self);
  end;
end;

procedure TJvPcx.SetWidth(Value: Integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Width := Value;
    Changed(Self);
  end;
end;

procedure TJvPcx.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

initialization
  RegisterClass(TJvPcx);
  TPicture.RegisterFileFormat(RC_PcxExtension, RC_PcxFilterName, TJvPcx);

finalization
  TPicture.UnRegisterGraphicClass(TJvPcx);

end.

