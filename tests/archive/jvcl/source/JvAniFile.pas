{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAniFile.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvAniFile;

interface

uses SysUtils, Windows,

{$IFDEF Delphi6_Up}
RTLConsts, 
{$ENDIF}
Classes, Graphics;

type
  TFourCC = array[0..3] of Char;

  PAniTag = ^TAniTag;
  TAniTag = packed record
    ckID: TFourCC;
    ckSize: Longint;
  end;

  TAniHeader = packed record
    cbSizeOf: Longint;
    cSteps: Longint;
    cFrames: Longint;
    cReserved: array[0..3] of Longint;
    jifRate: Longint; { 1 Jiffy = 1/60 sec }
    fl: Longint;
  end;

const
  AF_ICON     = $00000001;
  AF_SEQUENCE = $00000002;

{ TJvIconFrame }

type
  TJvIconFrame = class(TPersistent)
  private
    FIcon: TIcon;
    FIsIcon: Boolean;
    FTag: TAniTag;
    FHotSpot: TPoint;
    FJiffRate: Longint;
    FSeq: Integer;
  public
    constructor Create(Index: Integer; Jiff: Longint);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property JiffRate: Longint read FJiffRate;
    property Seq: Integer read FSeq;
  end;

{ TJvAnimatedCursorImage }

  TANINAME = array[0..255] of Char;

  TJvAnimatedCursorImage = class(TPersistent)
  private
    FHeader: TAniHeader;
    FTitle: TANINAME;
    FCreator: TANINAME;
    FIcons: TList;
    FOriginalColors: Word;
    procedure NewImage;
    procedure RiffReadError;
    function ReadCreateIcon(Stream: TStream; ASize: Longint;
      var HotSpot: TPoint; var IsIcon: Boolean): TIcon;
    function GetIconCount: Integer;
    function GetIcon(Index: Integer): TIcon;
    function GetFrame(Index: Integer): TJvIconFrame;
    function GetTitle: string;
    function GetCreator: string;
    function GetDefaultRate: Longint;
    procedure ReadAniStream(Stream: TStream);
    procedure ReadStream(Size: Longint; Stream: TStream);
    procedure WriteStream(Stream: TStream; WriteSize: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
      DecreaseColors, Vertical: Boolean);
    property DefaultRate: Longint read GetDefaultRate;
    property IconCount: Integer read GetIconCount;
    property Icons[Index: Integer]: TIcon read GetIcon;
    property Frames[Index: Integer]: TJvIconFrame read GetFrame;
    property Title: string read GetTitle;
    property Creator: string read GetCreator;
    property OriginalColors: Word read FOriginalColors;
  end;

implementation

{ This implementation based on animated cursor editor source code
  (ANIEDIT.C, copyright (C) Microsoft Corp., 1993-1996) }

uses Consts, JvVCLUtils, JvMaxMin, JvGraph, JvIcoList, JvClipIcon;

const
  FOURCC_ACON = 'ACON';
  FOURCC_RIFF = 'RIFF';
  FOURCC_INFO = 'INFO';
  FOURCC_INAM = 'INAM';
  FOURCC_IART = 'IART';
  FOURCC_LIST = 'LIST';
  FOURCC_anih = 'anih';
  FOURCC_rate = 'rate';
  FOURCC_seq  = 'seq ';
  FOURCC_fram = 'fram';
  FOURCC_icon = 'icon';

function PadUp(Value: Longint): Longint;
  { Up Value to nearest word boundary }
begin
  Result := Value + (Value mod 2);
end;

procedure DecreaseBMPColors(Bmp: TBitmap; Colors: Integer);
var
  Stream: TStream;
begin
  if (Bmp <> nil) and (Colors > 0) then begin
    Stream := BitmapToMemory(Bmp, Colors);
    try
      Bmp.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
    else Result := 0;
  end;
end;

{ ReadTag, ReadChunk, SkipChunk. Some handy functions for reading RIFF files. }

function ReadTag(S: TStream; pTag: PAniTag): Boolean;
begin
  pTag^.ckID := #0#0#0#0;
  pTag^.ckSize := 0;
  Result := S.Read(pTag^, SizeOf(TAniTag)) = SizeOf(TAniTag);
end;

function ReadChunk(S: TStream; pTag: PAniTag; Data: Pointer): Boolean;
begin
  Result := S.Read(Data^, pTag^.ckSize) = pTag^.ckSize;
  if Result then
    Result := S.Seek(pTag^.ckSize mod 2, soFromCurrent) <> -1;
end;

function ReadChunkN(S: TStream; pTag: PAniTag; Data: Pointer;
  cbMax: Longint): Boolean;
var
  cbRead: Longint;
begin
  cbRead := pTag^.ckSize;
  if cbMax < cbRead then cbRead := cbMax;
  Result := S.Read(Data^, cbRead) = cbRead;
  if Result then begin
    cbRead := PadUp(pTag^.ckSize) - cbRead;
    Result := S.Seek(cbRead, soFromCurrent) <> -1;
  end;
end;

function SkipChunk(S: TStream; pTag: PAniTag): Boolean;
begin
  { Round pTag^.ckSize up to nearest word boundary to maintain alignment }
  Result := S.Seek(PadUp(pTag^.ckSize), soFromCurrent) <> -1;
end;

{ Icon and cursor types }

const
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;

type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    xHotspot: Word;
    yHotspot: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

{ TJvIconFrame }

constructor TJvIconFrame.Create(Index: Integer; Jiff: Longint);
begin
  inherited Create;
  FSeq := Index;
  FJiffRate := Jiff;
end;

destructor TJvIconFrame.Destroy;
begin
  if FIcon <> nil then FIcon.Free;
  inherited Destroy;
end;

procedure TJvIconFrame.Assign(Source: TPersistent);
begin
  if Source is TJvIconFrame then begin
    with TJvIconFrame(Source) do begin
      if Self.FIcon = nil then Self.FIcon := TIcon.Create;
      Self.FIcon.Assign(FIcon);
      Self.FIsIcon := FIsIcon;
      Move(FTag, Self.FTag, SizeOf(TAniTag));
      Self.FHotSpot.X := FHotSpot.X;
      Self.FHotSpot.Y := FHotSpot.Y;
      Self.FJiffRate := FJiffRate;
      Self.FSeq := FSeq;
    end;
  end
  else inherited Assign(Source);
end;

{ TJvAnimatedCursorImage }

constructor TJvAnimatedCursorImage.Create;
begin
  inherited Create;
  FIcons := TList.Create;
end;

destructor TJvAnimatedCursorImage.Destroy;
begin
  NewImage;
  FIcons.Free;
  inherited Destroy;
end;

procedure TJvAnimatedCursorImage.Clear;
begin
  NewImage;
end;

procedure TJvAnimatedCursorImage.NewImage;
var
  I: Integer;
begin
  for I := 0 to FIcons.Count - 1 do TJvIconFrame(FIcons[I]).Free;
  FIcons.Clear;
  FillChar(FTitle, SizeOf(FTitle), 0);
  FillChar(FCreator, SizeOf(FCreator), 0);
  FillChar(FHeader, SizeOf(FHeader), 0);
  FOriginalColors := 0;
end;

procedure TJvAnimatedCursorImage.RiffReadError;
begin
  raise EReadError.Create(ResStr(SReadError));
end;

function TJvAnimatedCursorImage.GetTitle: string;
begin
  Result := StrPas(FTitle);
end;

function TJvAnimatedCursorImage.GetCreator: string;
begin
  Result := StrPas(FCreator);
end;

function TJvAnimatedCursorImage.GetIconCount: Integer;
begin
  Result := FIcons.Count;
end;

function TJvAnimatedCursorImage.GetIcon(Index: Integer): TIcon;
begin
  Result := TJvIconFrame(FIcons[Index]).FIcon;
end;

function TJvAnimatedCursorImage.GetFrame(Index: Integer): TJvIconFrame;
begin
  Result := TJvIconFrame(FIcons[Index]);
end;

function TJvAnimatedCursorImage.GetDefaultRate: Longint;
begin
  Result := Max(0, Min((FHeader.jifRate * 100) div 6, High(Result)));
end;

procedure TJvAnimatedCursorImage.Assign(Source: TPersistent);
var
  I: Integer;
  Frame: TJvIconFrame;
begin
  if Source = nil then begin
    Clear;
  end
  else if Source is TJvAnimatedCursorImage then begin
    NewImage;
    try
      with TJvAnimatedCursorImage(Source) do begin
        Move(FHeader, Self.FHeader, SizeOf(FHeader));
        Self.FTitle := FTitle;
        Self.FCreator := FCreator;
        Self.FOriginalColors := FOriginalColors;
        for I := 0 to FIcons.Count - 1 do begin
          Frame := TJvIconFrame.Create(-1, FHeader.jifRate);
          try
            Frame.Assign(TJvIconFrame(FIcons[I]));
            Self.FIcons.Add(Frame);
          except
            Frame.Free;
            raise;
          end;
        end;
      end;
    except
      NewImage;
      raise;
    end;
  end
  else inherited Assign(Source);
end;

procedure TJvAnimatedCursorImage.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TIcon then begin
    if IconCount > 0 then Dest.Assign(Icons[0])
    else Dest.Assign(nil);
  end
  else if Dest is TBitmap then begin
    if IconCount > 0 then
      AssignToBitmap(TBitmap(Dest), TBitmap(Dest).Canvas.Brush.Color,
        True, False)
    else Dest.Assign(nil);
  end
  else if Dest is TJvIconList then begin
    TJvIconList(Dest).BeginUpdate;
    try
      TJvIconList(Dest).Clear;
      for I := 0 to IconCount - 1 do TJvIconList(Dest).Add(Icons[I]);
    finally
      TJvIconList(Dest).EndUpdate;
    end;
  end
  else inherited AssignTo(Dest);
end;

function TJvAnimatedCursorImage.ReadCreateIcon(Stream: TStream; ASize: Longint;
  var HotSpot: TPoint; var IsIcon: Boolean): TIcon;
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array[0..300] of TIconRec;
var
  List: PIconRecArray;
  Mem: TMemoryStream;
  HeaderLen, I: Integer;
  BI: PBitmapInfoHeader;
begin
  Result := nil;
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(ASize);
    Mem.CopyFrom(Stream, Mem.Size);
    HotSpot := Point(0, 0);
    IsIcon := PCursorOrIcon(Mem.Memory)^.wType = RC3_ICON;
    if PCursorOrIcon(Mem.Memory)^.wType = RC3_CURSOR then
      PCursorOrIcon(Mem.Memory)^.wType := RC3_ICON;
    if PCursorOrIcon(Mem.Memory)^.wType = RC3_ICON then begin
      { determinate original icon color }
      HeaderLen := PCursorOrIcon(Mem.Memory)^.Count * SizeOf(TIconRec);
      GetMem(List, HeaderLen);
      try
        Mem.Position := SizeOf(TCursorOrIcon);
        Mem.Read(List^, HeaderLen);
        for I := 0 to PCursorOrIcon(Mem.Memory)^.Count - 1 do
          with List^[I] do begin
            GetMem(BI, DIBSize);
            try
              Mem.Seek(DIBOffset, soFromBeginning);
              Mem.Read(BI^, DIBSize);
              FOriginalColors := Max(GetDInColors(BI^.biBitCount), FOriginalColors);
              HotSpot := Point(xHotspot, yHotspot);
            finally
              FreeMem(BI, DIBSize)
            end;
          end;
      finally
        FreeMem(List, HeaderLen);
      end;
      { return to start of stream }
      Mem.Position := 0;
      Result := TIcon.Create;
      try
        Result.LoadFromStream(Mem);
        if IsIcon then
          HotSpot := Point(Result.Width div 2, Result.Height div 2);
      except
        Result.Free;
        Result := nil;
      end;
    end;
  finally
    Mem.Free;
  end;
end;

{ Loads an animatied cursor from a RIFF file. The RIFF file format for
  animated cursors looks like this:

  RIFF('ACON'
    LIST('INFO'
          INAM(<name>)
          IART(<artist>))
      anih(<anihdr>)
      [rate(<rateinfo>)]
      ['seq '( <seq_info>)]
      LIST('fram' icon(<icon_file>)))
}

procedure TJvAnimatedCursorImage.ReadAniStream(Stream: TStream);
var
  iFrame, iRate, iSeq, I: Integer;
  Tag: TAniTag;
  Frame: TJvIconFrame;
  cbChunk, cbRead, Temp: Longint;
  Icon: TIcon;
  bFound, IsIcon: Boolean;
  HotSpot: TPoint;
begin
  iFrame := 0; iRate := 0; iSeq := 0;
  { Make sure it's a RIFF ANI file }
  if not ReadTag(Stream, @Tag) or (Tag.ckID <> FOURCC_RIFF) then
    RiffReadError;
  if (Stream.Read(Tag.ckID, SizeOf(Tag.ckID)) < SizeOf(Tag.ckID)) or
    (Tag.ckID <> FOURCC_ACON) then RiffReadError;
  NewImage;
  { look for 'anih', 'rate', 'seq ', and 'icon' chunks }
  while ReadTag(Stream, @Tag) do begin
    if Tag.ckID = FOURCC_anih then begin
      if not ReadChunk(Stream, @Tag, @FHeader) then Break;
      if ((FHeader.fl and AF_ICON) <> AF_ICON) or
        (FHeader.cFrames = 0) then RiffReadError;
      for I := 0 to FHeader.cSteps - 1 do begin
        Frame := TJvIconFrame.Create(I, FHeader.jifRate);
        FIcons.Add(Frame);
      end;
    end
    else if Tag.ckID = FOURCC_rate then begin
      { If we find a rate chunk, read it into its preallocated space }
      if not ReadChunkN(Stream, @Tag, @Temp, SizeOf(Longint)) then
        Break;
      if iRate < FIcons.Count then
        TJvIconFrame(FIcons[iRate]).FJiffRate := Temp;
      Inc(iRate);
    end
    else if Tag.ckID = FOURCC_seq then begin
      { If we find a seq chunk, read it into its preallocated space }
      if not ReadChunkN(Stream, @Tag, @Temp, SizeOf(Longint)) then
        Break;
      if iSeq < FIcons.Count then
        TJvIconFrame(FIcons[iSeq]).FSeq := Temp;
      Inc(iSeq);
    end
    else if Tag.ckID = FOURCC_LIST then begin
      cbChunk := PadUp(Tag.ckSize);
      { See if this list is the 'fram' list of icon chunks }
      cbRead := Stream.Read(Tag.ckID, SizeOf(Tag.ckID));
      if cbRead < SizeOf(Tag.ckID) then Break;
      Dec(cbChunk, cbRead);
      if (Tag.ckID = FOURCC_fram) then begin
        while (cbChunk >= SizeOf(Tag)) do begin
          if not ReadTag(Stream, @Tag) then Break;
          Dec(cbChunk, SizeOf(Tag));
          if (Tag.ckID = FOURCC_icon) then begin
            { Ok, load the icon/cursor bits }
            Icon := ReadCreateIcon(Stream, Tag.ckSize, HotSpot, IsIcon);
            if Icon = nil then Break;
            bFound := False;
            for I := 0 to FIcons.Count - 1 do begin
              if TJvIconFrame(FIcons[I]).FSeq = iFrame then begin
                TJvIconFrame(FIcons[I]).FIcon := Icon;
                TJvIconFrame(FIcons[I]).FTag := Tag;
                TJvIconFrame(FIcons[I]).FHotSpot := HotSpot;
                TJvIconFrame(FIcons[I]).FIsIcon := IsIcon;
                bFound := True;
              end;
            end;
            if not bFound then begin
              Frame := TJvIconFrame.Create(-1, FHeader.jifRate);
              Frame.FIcon := Icon;
              Frame.FIsIcon := IsIcon;
              Frame.FTag := Tag;
              Frame.FHotSpot := HotSpot;
              FIcons.Add(Frame);
            end;
            Inc(iFrame);
          end
          else begin
            { Unknown chunk in fram list, just ignore it }
            SkipChunk(Stream, @Tag);
          end;
          Dec(cbChunk, PadUp(Tag.ckSize));
        end;
      end
      else if (Tag.ckID = FOURCC_INFO) then begin
        { now look for INAM and IART chunks }
        while (cbChunk >= SizeOf(Tag)) do begin
          if not ReadTag(Stream, @Tag) then Break;
          Dec(cbChunk, SizeOf(Tag));
          if Tag.ckID = FOURCC_INAM then begin
            if (cbChunk < Tag.ckSize) or not
              ReadChunkN(Stream, @Tag, @FTitle, SizeOf(TANINAME) - 1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
          end
          else if Tag.ckID = FOURCC_IART then begin
            if (cbChunk < Tag.ckSize) or not
              ReadChunkN(Stream, @Tag, @FCreator, SizeOf(TANINAME) - 1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
          end
          else begin
            if not SkipChunk(Stream, @Tag) then Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
          end;
        end;
      end
      else begin
        { Not the fram list or the INFO list. Skip the rest of this
          chunk. (Don't forget that we have already skipped one dword) }
        Tag.ckSize := cbChunk;
        SkipChunk(Stream, @Tag);
      end;
    end
    else begin { We're not interested in this chunk, skip it. }
      if not SkipChunk(Stream, @Tag) then Break;
    end;
  end; { while }
  { Update the frame count incase we coalesced some frames while reading
    in the file. }
  for I := FIcons.Count - 1 downto 0 do begin
    if TJvIconFrame(FIcons[I]).FIcon = nil then begin
      TJvIconFrame(FIcons[I]).Free;
      FIcons.Delete(I);
    end;
  end;
  FHeader.cFrames := FIcons.Count;
  if FHeader.cFrames = 0 then RiffReadError;
end;

procedure TJvAnimatedCursorImage.ReadStream(Size: Longint; Stream: TStream);
var
  Data: TMemoryStream;
begin
  Data := TMemoryStream.Create;
  try
    Data.SetSize(Size);
    Stream.ReadBuffer(Data.Memory^, Size);
    if Size > 0 then begin
      Data.Position := 0;
      ReadAniStream(Data);
    end;
  finally
    Data.Free;
  end;
end;

procedure TJvAnimatedCursorImage.WriteStream(Stream: TStream;
  WriteSize: Boolean);
begin
  NotImplemented;
end;

procedure TJvAnimatedCursorImage.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream.Size - Stream.Position, Stream);
end;

procedure TJvAnimatedCursorImage.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TJvAnimatedCursorImage.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead + fmShareDenyNone);
  try
    try
      LoadFromStream(Stream);
    except
      NewImage;
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TJvAnimatedCursorImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FIcons.Count > 0 then
    DrawRealSizeIcon(ACanvas, Icons[0], ARect.Left, ARect.Top);
end;

procedure TJvAnimatedCursorImage.AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
  DecreaseColors, Vertical: Boolean);
var
  I: Integer;
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    if FIcons.Count > 0 then begin
      with Temp do begin
        Monochrome := False;
        Canvas.Brush.Color := BackColor;
        if Vertical then begin
          Width := Icons[0].Width;
          Height := Icons[0].Height * FIcons.Count;
        end
        else begin
          Width := Icons[0].Width * FIcons.Count;
          Height := Icons[0].Height;
        end;
        Canvas.FillRect(Bounds(0, 0, Width, Height));
        for I := 0 to FIcons.Count - 1 do begin
          if Icons[I] <> nil then
            Canvas.Draw(Icons[I].Width * I * Ord(not Vertical),
              Icons[I].Height * I * Ord(Vertical), Icons[I]);
        end;
      end;
      if DecreaseColors then
        DecreaseBMPColors(Temp, Max(OriginalColors, 16));
    end;
    Bitmap.Assign(Temp);
  finally
    Temp.Free;
  end;
end;

end.
