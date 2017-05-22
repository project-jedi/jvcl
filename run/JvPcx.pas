{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPcx.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                Andreas Hausladen [Andreas dott Hausladen att gmx dott de] (complete rewrite)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPcx;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows,
  {$ENDIF UNIX}
  Graphics, Controls, Forms,
  SysUtils, Classes,
  JvTypes, JvJCLUtils;

type
  EPcxError = class(EJVCLException);

  TJvPcx = class(TBitmap)
  public
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ResType: PChar);
    {$IFDEF MSWINDOWS}
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: PChar);
    {$ENDIF MSWINDOWS}
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
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
  JvResources;

procedure TJvPcx.LoadFromResourceName(Instance: THandle;
  const ResName: string; ResType: PChar);
var
  Stream: TStream;
begin
  Assign(nil); // fixes GDI resource leak
  if ResType = RT_BITMAP then
    inherited LoadFromResourceName(Instance, ResName)
  else
  begin
    Stream := TResourceStream.Create(Instance, ResName, ResType);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TJvPcx.LoadFromResourceID(Instance: THandle; ResID: Integer;
  ResType: PChar);
var
  Stream: TStream;
begin
  Assign(nil); // fixes GDI resource leak
  if ResType = RT_BITMAP then
    inherited LoadFromResourceID(Instance, ResID)
  else
  begin
    Stream := TResourceStream.CreateFromID(Instance, ResID, ResType);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;
{$ENDIF MSWINDOWS}

type
  PPcxPalette = ^TPcxPalette;
  TPcxPalette = packed record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
  end;
  PPcxPaletteArray = ^TPcxPaletteArray;
  TPcxPaletteArray = array [0..255] of TPcxPalette;

  TPcxPalette256 = packed record
    Id: Byte; // $0C
    Items: array [0..255] of TPcxPalette;
  end;

  TPcxHeader = packed record
    Id: Byte; // $0A
    Version: Byte; // 5 = 3.0
    Compressed: Boolean;
    Bpp: Byte;
    x0, y0: Word;
    x1, y1: Word;
    dpiX: Word;
    dpiY: Word;
    Palette16: array [0..15] of TPcxPalette;
    Reserved1: Byte;
    Planes: Byte;
    BytesPerLine: Word;
    PaletteType: Word; // 1: color or s/w   2: grayscaled
    ScreenWidth: Word; // 0
    ScreenHeight: Word; // 0
    Reserved2: array [0..53] of Byte;
  end;



procedure ReadPalette(Bitmap: TJvPcx; ColorNum: Integer; PcxPalette: PPcxPalette);
var
  I: Integer;
  P: PPcxPaletteArray;
  RPal: TMaxLogPalette;
begin
  P := PPcxPaletteArray(PcxPalette);
  RPal.palVersion := $300;
  RPal.palNumEntries := ColorNum;
  for I := 0 to ColorNum - 1 do
  begin
    RPal.palPalEntry[I].peRed := P[I].Red;
    RPal.palPalEntry[I].peGreen := P[I].Green;
    RPal.palPalEntry[I].peBlue := P[I].Blue;
    RPal.palPalEntry[I].peFlags := 0;
  end;
  Bitmap.Palette := CreatePalette(PLogPalette(@RPal)^);
  Bitmap.PaletteModified := True;
end;

procedure WritePalette(Bitmap: TJvPcx; ColorNum: Integer; PcxPalette: PPcxPalette);
var
  I: Integer;
  P: PPcxPaletteArray;
  RPal: array [0..256] of TPaletteEntry;
begin
  P := PPcxPaletteArray(PcxPalette);
  FillChar(P[0], ColorNum * SizeOf(TPcxPalette), 0);
  if Bitmap.Palette <> 0 then
  begin
    GetPaletteEntries(Bitmap.Palette, 0, ColorNum, RPal);
    for I := 0 to ColorNum - 1 do
    begin
      P[I].Red := RPal[I].peRed;
      P[I].Green := RPal[I].peGreen;
      P[I].Blue := RPal[I].peBlue;
    end;
  end;
end;

procedure TJvPcx.LoadFromStream(Stream: TStream);
var
  Header: TPcxHeader;
  BytesRead, BytesPerRasterLine: Integer;
  Decompressed: TMemoryStream;
  ByteLine: PByteArray;
  Line: PJvRGBArray;
  Palette256: TPcxPalette256;
  Buffer: array [0..MaxPixelCount] of Byte;
  Buffer2, Buffer3, Buffer4: PByteArray; // position in Buffer
  B: Byte;
  ByteNum, BitNum: Integer;
  X, Y: Integer;
begin
  Width := 0;
  Height := 0;
  Palette := 0;
  IgnorePalette := False;
  Monochrome := False;

  BytesRead := Stream.Read(Header, SizeOf(Header));
  // is it a valid header
  if (BytesRead <> SizeOf(Header)) or (Header.Id <> $0A) or
     (Header.BytesPerLine mod 2 = 1) then // BytesPerLine must be even
    raise EPcxError.CreateRes(@RsEPcxInvalid);

  // set pixel format before resizing the bitmap to reduce bitmap reallocations
  case Header.Bpp of
    1:
      case Header.Planes of
        1:
          begin
            PixelFormat := pf1bit;
            Monochrome := True;
            IgnorePalette := True;
          end;
        4:
          PixelFormat := pf4bit; // VisualCLX: redirected const
      else
        raise EPcxError.CreateRes(@RsEPcxUnknownFormat);
      end;
    8:
      case Header.Planes of
        1:
          PixelFormat := pf8bit;
        3:
          begin
            PixelFormat := pf24bit; // VisualCLX: redirected const
            IgnorePalette := True;
          end;
      else
        raise EPcxError.CreateRes(@RsEPcxUnknownFormat);
      end;
  end;

  Width := Header.x1 - Header.x0 + 1;
  Height := Header.y1 - Header.y0 + 1;
  if (Width = 0) or (Height = 0) then
    Exit; // nothing to do
  BytesPerRasterLine := Header.BytesPerLine * Header.Planes;

  Decompressed := TMemoryStream.Create;
  try
    if (Header.Bpp = 8) and (Header.Planes = 1) then
     // do not uncompress the appended (uncompressed) palette
      Decompressed.CopyFrom(Stream, Stream.Size - Stream.Position - SizeOf(TPcxPalette256))
    else
      Decompressed.CopyFrom(Stream, Stream.Size - Stream.Position);
    // decompress data stream
    if Header.Compressed then
      RleDecompress(Decompressed);
    if (Header.Bpp = 8) and (Header.Planes = 1) then
    // append the uncompressed palette
      Decompressed.CopyFrom(Stream, SizeOf(TPcxPalette256));

    // create palette (if necessary)
    if (Header.Bpp = 1) and (Header.Planes = 4) then
    begin
      ReadPalette(Self, 16, @Header.Palette16[0]);
    end
    else
    if (Header.Bpp = 8) and (Header.Planes = 1) then
    begin
      Decompressed.Seek(-SizeOf(TPcxPalette256), soFromEnd);
      if Decompressed.Read(Palette256, SizeOf(TPcxPalette256)) <> SizeOf(TPcxPalette256) then
        raise EPcxError.CreateRes(@RsEPcxPaletteProblem);
      if Palette256.Id = $0C then
        ReadPalette(Self, 256, @Palette256.Items[0])
      else
        raise EPcxError.CreateRes(@RsEPcxPaletteProblem);
    end;

    Decompressed.Position := 0;

   // read data
    for Y := 0 to Height - 1 do
    begin
      ByteLine := ScanLine[Y];
      if Decompressed.Read(Buffer, BytesPerRasterLine) <> BytesPerRasterLine then
        raise EPcxError.CreateRes(@RsEPcxUnknownFormat);

      // write data to the ScanLine
      if ((Header.Bpp = 1) and (Header.Planes = 1)) or // 1bit
        ((Header.Bpp = 8) and (Header.Planes = 1)) then // 8bit
        // just copy the data
        Move(Buffer[0], ByteLine[0], Header.BytesPerLine)
      else
      if (Header.Bpp = 8) and (Header.Planes = 3) then // 24bit
      begin
        Line := Pointer(ByteLine);
        Buffer2 := @Buffer[Header.BytesPerLine];
        Buffer3 := @Buffer[Header.BytesPerLine * 2];
        for X := 0 to Width - 1 do
          with Line[X] do
          begin
            rgbRed := Buffer[X];
            rgbGreen := Buffer2[X];
            rgbBlue := Buffer3[X];
          end;
      end
      else
      if (Header.Bpp = 1) and (Header.Planes = 4) then // 4bit
      begin
        Buffer2 := @Buffer[Header.BytesPerLine];
        Buffer3 := @Buffer[Header.BytesPerLine * 2];
        Buffer4 := @Buffer[Header.BytesPerLine * 3];
        FillChar(ByteLine[0], BytesPerRasterLine, 0);
        for X := 0 to Width - 1 do
        begin
          B := 0;
          ByteNum := X div 8;
          BitNum := 7 - (X mod 8);
          if (Buffer[ByteNum] shr BitNum) and $1 <> 0 then
            B := B or $01;
          if (Buffer2[ByteNum] shr BitNum) and $1 <> 0 then
            B := B or $02;
          if (Buffer3[ByteNum] shr BitNum) and $1 <> 0 then
            B := B or $04;
          if (Buffer4[ByteNum] shr BitNum) and $1 <> 0 then
            B := B or $08;

          if X mod 2 = 0 then // BIG ENDIAN
            B := B shl 4;
          ByteLine[X div 2] := ByteLine[X div 2] or B;
        end;
      end;
    end;
  finally
    Decompressed.Free;
  end;
  PaletteModified := True;
  Changed(Self);
end;

procedure TJvPcx.SaveToStream(Stream: TStream);
var
  CompressStream: TMemoryStream;
  Header: TPcxHeader;
  X, Y: Integer;
  ByteLine: PByteArray;
  Line: PJvRGBArray;
  Buffer: array [0..MaxPixelCount] of Byte;
  Buffer2, Buffer3, Buffer4: PByteArray; // position in Buffer
  Palette256: TPcxPalette256;
  BytesPerRasterLine: Integer;
  B: Byte;
  ByteNum, BitNum: Integer;
begin
  if PixelFormat in [pfDevice, pfCustom, pf15bit, pf16bit] then
    PixelFormat := pf24bit;

  FillChar(Header, SizeOf(Header), 0);
  Header.Id := $0A;
  Header.Version := 5; // = 3.0
  Header.Compressed := True;
  Header.dpiX := 72;
  Header.dpiY := 72;
  Header.x1 := Width - 1;
  Header.y1 := Height - 1;
  Header.PaletteType := 1;

  CompressStream := TMemoryStream.Create;
  try
    // complete header
    case PixelFormat of
      pf1bit:
        begin
          Header.Bpp := 1;
          Header.Planes := 1;
          Header.BytesPerLine := (Width + 7) div 8;
          Header.Palette16[1].Red := 255;
          Header.Palette16[1].Green := 255;
          Header.Palette16[1].Blue := 255;
        end;
      pf4bit:
        begin
          Header.Bpp := 1;
          Header.Planes := 4;
          Header.BytesPerLine := (Width + 1) div 2;
        end;
      pf8bit:
        begin
          begin
            Header.Bpp := 8;
            Header.Planes := 1;
            Header.BytesPerLine := Width;
          end;
        end;
      pf24bit:
        begin
          Header.Bpp := 8;
          Header.Planes := 3;
          Header.BytesPerLine := Width;
        end;
    end;

    // round BytesPerPixel to even
    BytesPerRasterLine := Header.BytesPerLine; // save it
    if Header.BytesPerLine mod 2 = 1 then
      Inc(Header.BytesPerLine);

    if (PixelFormat = pf8bit) or (PixelFormat = pf4bit) then
      // copy first 16 palette entries into the header (also for pf8bit)
      WritePalette(Self, 16, @Header.Palette16[0]);
    // write header
    Stream.Write(Header, SizeOf(Header));

    // compress data
    for Y := 0 to Height - 1 do
    begin
      ByteLine := ScanLine[Y];

      case Header.Planes * Header.Bpp of // reduces VisualCLX IFDEFs
        1, 8:
          begin
            if Header.BytesPerLine <> BytesPerRasterLine then
            begin
              Move(ByteLine[0], Buffer, BytesPerRasterLine);
              Buffer[BytesPerRasterLine] := 0;
              ByteLine := @Buffer[0];
            end;
            CompressStream.Write(ByteLine[0], Header.BytesPerLine);
          end;
        4:
          begin
            BytesPerRasterLine := Header.BytesPerLine * 4;
            Buffer2 := @Buffer[Header.BytesPerLine];
            Buffer3 := @Buffer[Header.BytesPerLine * 2];
            Buffer4 := @Buffer[Header.BytesPerLine * 3];
            FillChar(Buffer[0], BytesPerRasterLine, 0);
            for X := 0 to Width - 1 do
            begin
              B := ByteLine[X div 2];
              if X mod 2 = 0 then // BIG ENDIAN
                B := B shr 4
              else
                B := B and $0F;

              ByteNum := X div 8;
              BitNum := 7 - (X mod 8);
              if B and $01 <> 0 then
                Buffer[ByteNum] := Buffer[ByteNum] or (1 shl BitNum);
              if B and $02 <> 0 then
                Buffer2[ByteNum] := Buffer2[ByteNum] or (1 shl BitNum);
              if B and $04 <> 0 then
                Buffer3[ByteNum] := Buffer3[ByteNum] or (1 shl BitNum);
              if B and $08 <> 0 then
                Buffer4[ByteNum] := Buffer4[ByteNum] or (1 shl BitNum);
            end;
            CompressStream.Write(Buffer, BytesPerRasterLine);
          end;
        24:
          begin
            Line := ScanLine[Y];
            Buffer2 := @Buffer[Header.BytesPerLine];
            Buffer3 := @Buffer[Header.BytesPerLine * 2];
            for X := 0 to Width - 1 do
            begin
              with Line[X] do
              begin
                Buffer[X] := rgbRed;
                Buffer2[X] := rgbGreen;
                Buffer3[X] := rgbBlue;
              end;
            end;
            CompressStream.Write(Buffer, Header.BytesPerLine * 3);
          end;
      end;
      RleCompressTo(CompressStream, Stream);
      CompressStream.Size := 0;
    end;

    // write palette
    if PixelFormat = pf8bit then
    begin
      Palette256.Id := $0C;
      WritePalette(Self, 256, @Palette256.Items[0]);
      Stream.Write(Palette256, SizeOf(Palette256));
    end;
  finally
    CompressStream.Free;
  end;
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvPcx, TControl);
  {$ENDIF COMPILER7_UP}
  RegisterClass(TJvPcx);
  TPicture.RegisterFileFormat(RsPcxExtension, RsPcxFilterName, TJvPcx);

finalization
  TPicture.UnregisterGraphicClass(TJvPcx);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
