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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvPcx;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  JvTypes, JvRle;

// (rom) Warning. File seems buggy.

type
  TJvPcx = class(TGraphic)
  private
    FBitmap: TBitmap;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
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

  EPcxError = class(Exception);

implementation

resourcestring
  RC_PcxUnknownFormat = 'PCX: Unknown format';
  RC_PcxPaletteProblem = 'PCX: Unable to retrieve palette';
  RC_PcxInvalid = 'PCX: Invalid PCX file';

  RC_PcxExtension = 'pcx';
  RC_PcxFilterName = 'PCX Image';

  {**************************************************}

procedure TJvPcx.Assign(Source: TPersistent);
var
  ts: TStream;
begin
  if Source = nil then
    FreeAndNil(FBitmap)
  else if (Source is TJvPcx) and (Source <> Self) then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;

    ts := TMemoryStream.Create;
    TJvPcx(Source).SaveToStream(ts);
    ts.Position := 0;
    LoadFromStream(ts);
  end
  else if Source is TBitmap then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;
    FBitmap.Assign(TBitmap(Source));
  end
  else
    inherited Assign(Source);
end;

{**************************************************}

procedure TJvPcx.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvPcx then
    Dest.Assign(Self)
  else if Dest is TGraphic then
  begin
    if Empty then
      Dest.Assign(nil)
    else
      (Dest as TGraphic).Assign(FBitmap);
  end
  else
    inherited AssignTo(Dest);
end;

{**************************************************}

constructor TJvPcx.Create;
begin
  inherited Create;
  // (rom) should the bitmap be nonempty on creation?
  FBitmap := TBitmap.Create;
  FBitmap.Width := 0;
  FBitmap.Height := 0;
  FBitmap.Palette := 0;
end;

{**************************************************}

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

{**************************************************}

procedure TJvPcx.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FBitmap <> nil then
    ACanvas.StretchDraw(ARect, FBitmap);
end;

{**************************************************}

function TJvPcx.Equals(Graphic: TGraphic): Boolean;
begin
  if FBitmap = nil then
    Result := Graphic = nil
  else
    Result := (Graphic is TJvPcx) and (FBitmap = TJvPcx(TJvPcx).FBitmap);
end;

{**************************************************}

function TJvPcx.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

{**************************************************}

function TJvPcx.GetEmpty: Boolean;
begin
  // (rom) does this test match the constructor?
  Result := FBitmap = nil;
end;

{**************************************************}

function TJvPcx.GetHeight: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Height
  else
    Result := -1;
end;

{**************************************************}

function TJvPcx.GetPalette: HPALETTE;
begin
  if FBitmap <> nil then
    Result := FBitmap.Palette
  else
    Result := 0;
end;

{**************************************************}

function TJvPcx.GetPixelFormat: TPixelFormat;
begin
  if FBitmap <> nil then
    Result := FBitmap.PixelFormat
  else
    Result := pfCustom;
end;

{**************************************************}

function TJvPcx.GetTransparent: Boolean;
begin
  if FBitmap <> nil then
    Result := FBitmap.Transparent
  else
    Result := False;
end;

{**************************************************}

function TJvPcx.GetWidth: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Width
  else
    // (rom) is this a good value?
    Result := -1;
end;

{**************************************************}

function TJvPcx.HasBitmap: Boolean;
begin
  Result := FBItmap <> nil;
end;

{**************************************************}

procedure TJvPcx.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // (rom) shouldnt this create a bitmap if needed?
  if FBitmap <> nil then
    FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
end;

{**************************************************}

procedure TJvPcx.LoadFromFile(const FileName: string);
var
  t: TFileStream;
begin
  t := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Self.LoadFromStream(t);
  t.Free;
end;

{**************************************************}

procedure TJvPcx.LoadFromResourceID(Instance: THandle; ResID: Integer;
  ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResId, ResType);
  Self.LoadFromStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvPcx.LoadFromResourceName(Instance: THandle;
  const ResName: string; ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  Self.LoadFromStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvPcx.LoadFromStream(Stream: TStream);
var
  i, j, k, l, m: Integer;
  w1, w2, w3, w4: Word;
  BytesPerLine: Integer;
  Bpp, Planes: Integer;
  Decompressed: TStream;
  Buf: array[0..128] of Byte;
  ibuf2: PRGBArray;
  ibuf: array[0..MaxPixelCount - 1] of Byte;
  ibuftmp: PByteArray;
  rpal: TMaxLogPalette;
begin
  // (rom) changed from <> to =
  if FBitmap = nil then
    FBitmap := TBitmap.Create;

  i := Stream.Read(Buf, 128);

  FBitmap.Width := 0;
  FBitmap.Height := 0;
  if i = 128 then
  begin
    if Buf[0] = 10 then
    begin
      w1 := Buf[4] + Buf[5] * 256;
      w2 := Buf[6] + Buf[7] * 256;
      w3 := Buf[8] + Buf[9] * 256;
      w4 := Buf[10] + Buf[11] * 256;
      FBitmap.Width := (w3 - w1) + 1;
      FBitmap.Height := (w4 - w2) + 1;
      // (rom) FloodFill seems silly
      FBitmap.Canvas.FloodFill(0, 0, clWhite, fsSurface);
      BytesPerLine := (Buf[66] + Buf[67] * 256) * Buf[65];
      l := FBitmap.Width * 2;
      m := FBitmap.Width;
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
      Decompressed.Position := 0;
      with TJvRle.Create(nil) do
      begin
        Decompressed := Decompress(Decompressed);
        Free;
      end;

      if (Bpp = 1) and (Planes = 1) then
      begin
        //monochrome okay
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuftmp := FBitmap.ScanLine[i];
          Decompressed.Read(ibuf, BytesPerLine);
          CopyMemory(ibuftmp, @ibuf, BytesPerLine);
        end;
      end
      else if (Bpp = 1) and (Planes = 4) then
      begin
        //16 colors
        //palette
        Stream.Position := 16;
        if Stream.Read(ibuf, 48) <> 48 then
          raise EPcxError.Create(RC_PcxPaletteProblem)
        else
        begin
          rpal.palVersion := $300;
          rpal.palNumEntries := 16;
          for m := 0 to 15 do
          begin
            i := m * 3;
            rpal.palPalEntry[m].peRed := ibuf[i];
            rpal.palPalEntry[m].peGreen := ibuf[i + 1];
            rpal.palPalEntry[m].peBlue := ibuf[i + 2];
            rpal.palPalEntry[m].peFlags := 0;
          end;
          // (rom) why not like in the following 256 color block?
          FBitmap.Palette := CreatePalette(tagLogPalette((@rpal)^));
          PaletteModified := True;
        end;

        //Reading data
        for i := 0 to FBitmap.Height - 1 do
        begin
          Decompressed.Read(ibuf, BytesPerLine);
          ibuftmp := FBitmap.Scanline[i];
          FillChar(ibuftmp^, (FBitmap.Width div 2) + 1, #0);

          l := 0;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $01
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $10;

          l := BytesPerLine div 4;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $02
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $20;

          l := (BytesPerLine div 4) * 2;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $04
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $40;

          l := (BytesPerLine div 4) * 3;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $08
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $80;
        end;
      end
      else if (Bpp = 8) and (Planes = 1) then
      begin
        //256 Colors. Okay
        Stream.Position := Stream.Size - (256 * 3 + 1);
        Stream.Read(ibuf, 1000);
        if ibuf[0] = 12 then
        begin
          rpal.palVersion := $300;
          rpal.palNumEntries := 256;
          for m := 0 to 255 do
          begin
            i := m * 3 + 1;
            rpal.palPalEntry[m].peRed := ibuf[i];
            rpal.palPalEntry[m].peGreen := ibuf[i + 1];
            rpal.palPalEntry[m].peBlue := ibuf[i + 2];
            rpal.palPalEntry[m].peFlags := 0;
          end;
          FBitmap.Palette := CreatePalette(tagLogPalette((@rpal)^));
          FBitmap.PaletteModified := True;
          // (rom) this is also done at function exit
          PaletteModified := True;
          Changed(Self);
        end;
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuftmp := FBitmap.ScanLine[i];
          Decompressed.Read(ibuf, BytesPerLine);
          CopyMemory(ibuftmp, @ibuf, BytesPerLine);
        end;
      end
      else if (Bpp = 8) and (Planes = 3) then
      begin
        //24 bit. Okey
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuf2 := Fbitmap.ScanLine[i];
          Decompressed.Read(ibuf, BytesPerLine);
          for j := 0 to FBitmap.Width - 1 do
          begin
            ibuf2[j].rgbtRed := ibuf[j];
            ibuf2[j].rgbtGreen := ibuf[j + m];
            ibuf2[j].rgbtBlue := ibuf[j + l];
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

{**************************************************}

procedure TJvPcx.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  if FBitmap <> nil then
    FBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
end;

{**************************************************}

procedure TJvPcx.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvPcx.SaveToStream(Stream: TStream);
var
  i, j, k, l: Integer;
  BytesPerLine: Word;
  st: TStream;
  b: Byte;
  ibuf2: PRGBArray;
  ibuf: array[0..MaxPixelCount - 1] of Byte;
  ibuftmp: PByteArray;
  wpal: array[0..255] of PALETTEENTRY;
begin
  if FBitmap = nil then
    Exit;
  if (FBitmap.PixelFormat <> pf1Bit) and (FBitmap.PixelFormat <> pf4Bit) and (FBitmap.PixelFormat <> pf8Bit)
    and (FBitmap.PixelFormat <> pf24Bit) then
    FBItmap.PixelFormat := pf24bit;

  //St is the temporary Stream to put the data
  st := TMemoryStream.Create;

  //Creating header first
  FillChar(ibuf, 128, 0);
  ibuf[0] := $A;
  ibuf[1] := 5;
  ibuf[2] := 1;
  ibuf[8] := Lo(FBitmap.Width) - 1;
  ibuf[9] := Hi(FBitmap.Width);
  ibuf[10] := Lo(FBitmap.Height) - 1;
  ibuf[11] := Hi(FBitmap.Height);
  ibuf[12] := 1;
  ibuf[13] := 300 - 256;
  ibuf[14] := 1;
  ibuf[15] := 300 - 256;
  ibuf[69] := 1;
  ibuf[70] := Hi(Screen.Height);
  ibuf[71] := Lo(Screen.Height);
  ibuf[72] := Hi(Screen.Width);
  ibuf[73] := Lo(Screen.Width);

  case FBitmap.PixelFormat of
    pf1bit:
      begin
        ibuf[3] := 1;
        ibuf[65] := 1;
        BytesPerLine := FBitmap.Width div 8;
        if FBitmap.Width mod 8 <> 0 then
          Inc(BytesPerLine);
        ibuf[66] := Lo(BytesPerLine);
        ibuf[67] := Hi(BytesPerLine);
        Stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuftmp := FBitmap.ScanLine[i];
          CopyMemory(@ibuf, ibuftmp, BytesPerLine);
          st.Write(ibuf, BytesPerLine);
        end;
      end;
    pf4bit:
      begin
        ibuf[3] := 1;
        ibuf[65] := 4;
        BytesPerLine := FBitmap.Width div 8;
        if FBitmap.Width mod 8 <> 0 then
          Inc(BytesPerLine);
        i := BytesPerLine;
        BytesPerLine := BytesPerLine * 4;
        ibuf[66] := Lo(i);
        ibuf[67] := Hi(i);

        //Write palette
        if FBitmap.Palette <> 0 then
        begin
          GetPaletteEntries(FBitmap.Palette, 0, 16, wpal);
          for i := 0 to 15 do
          begin
            ibuf[16 + i * 3] := wpal[i].peRed;
            ibuf[16 + i * 3 + 1] := wpal[i].peGreen;
            ibuf[16 + i * 3 + 2] := wpal[i].peBlue;
          end;
        end;
        Stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuftmp := FBitmap.Scanline[i];
          FillChar(ibuf, BytesPerLine, #0);

          //Red
          l := 0;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $10) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $01) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Green
          l := BytesPerLine div 4;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $20) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $02) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Blue
          l := BytesPerLine div 2;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $40) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $04) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Intensity
          l := (BytesPerLine div 4) * 3;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $80) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $08) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          st.Write(ibuf, BytesPerLine);
        end;
      end;
    pf8bit:
      begin
        ibuf[3] := 8;
        ibuf[65] := 1;
        BytesPerLine := FBitmap.Width;
        ibuf[66] := Lo(BytesPerLine);
        ibuf[67] := Hi(BytesPerLine);
        Stream.Write(ibuf, 128);

        //Write Data
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuftmp := Fbitmap.ScanLine[i];
          CopyMemory(@ibuf, ibuftmp, BytesPerLine);
          st.Write(ibuf, BytesPerLine);
        end;
      end;
    pf24bit:
      begin
        ibuf[3] := 8;
        ibuf[65] := 3;
        BytesPerLine := FBitmap.Width * 3;
        i := FBitmap.Width;
        ibuf[66] := lo(i);
        ibuf[67] := hi(i);
        Stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.Height - 1 do
        begin
          ibuf2 := FBitmap.ScanLine[i];

          for j := 0 to FBitmap.Width - 1 do
            ibuf[j] := ibuf2[j].rgbtRed;

          k := FBitmap.Width;
          for j := 0 to FBitmap.Width - 1 do
            ibuf[j + k] := ibuf2[j].rgbtGreen;

          k := FBitmap.Width * 2;
          for j := 0 to FBitmap.Width - 1 do
            ibuf[j + k] := ibuf2[j].rgbtBlue;

          st.Write(ibuf, BytesPerLine);
        end;
      end;
  end;

  //RLE Compress temporary Stream
  st.Position := 0;
  with TJvRle.Create(nil) do
  begin
    st := Compress(st);
    Free;
  end;
  //Copy temporary Stream to final Stream
  st.Position := 0;
  Stream.CopyFrom(st, st.Size);

  //Write palette if mode 256 color.
  if (FBitmap.PixelFormat = pf8bit) and (FBitmap.Palette <> 0) then
  begin
    GetPaletteEntries(FBitmap.Palette, 0, 256, wpal);
    for i := 0 to 255 do
    begin
      ibuf[i * 3] := wpal[i].peRed;
      ibuf[i * 3 + 1] := wpal[i].peGreen;
      ibuf[i * 3 + 2] := wpal[i].peBlue;
    end;
    b := 12;
    Stream.Write(b, 1);
    Stream.Write(ibuf, 256 * 3);
  end;

  st.Free;
end;

{**************************************************}

procedure TJvPcx.SetHeight(Value: Integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Height := Value;
    Changed(Self);
  end;
end;

{**************************************************}

procedure TJvPcx.SetWidth(Value: Integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Width := Value;
    Changed(Self);
  end;
end;

{**************************************************}

procedure TJvPcx.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

{**************************************************}

initialization
  RegisterClass(TJvPcx);
  TPicture.RegisterFileFormat(RC_PcxExtension, RC_PcxFilterName, TJvPcx);

finalization
  TPicture.UnRegisterGraphicClass(TJvPcx);

end.
