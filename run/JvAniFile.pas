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

Last Modified: 2003-10-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvAniFile;

interface

uses
  SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  Windows, Graphics, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWindows, QGraphics, QForms, Types,
  {$ENDIF VisualCLX}
  JvTypes;

type
  TJvIconFrame = class(TPersistent)
  private
    FIcon: TIcon;
    FIsIcon: Boolean;
    FHotSpot: TPoint;
    FJiffRate: Longint;
  public
    constructor Create(Jiff: Longint);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property JiffRate: Longint read FJiffRate;
    property Icon: TIcon read FIcon;
    property HotSpot: TPoint read FHotSpot;
  end;

  TJvAnimatedCursorImage = class(TPersistent)
  private
    FHeader: TJvAniHeader;
    FTitle: string;
    FCreator: string;
    FIcons: TList;
    FOriginalColors: Word;
    FIndex: Integer;
    FRates: array of Longint;
    FSequence: array of Longint;
    FFrameCount: Integer;
    procedure NewImage;
    procedure RiffReadError;
    function ReadCreateIcon(Stream: TStream; ASize: Longint;
      var HotSpot: TPoint; var IsIcon: Boolean): TIcon;
    function GetIconCount: Integer;
    function GetFrameCount: Integer;
    function GetIcon(Index: Integer): TIcon;
    function GetFrame(Index: Integer): TJvIconFrame;
    function GetDefaultRate: Longint;
    procedure SetIndex(Value: Integer);
    procedure ReadAniStream(Stream: TStream);
    procedure WriteAniStream(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect);
    procedure Clear;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    {$IFDEF VCL} // DecreaseBMPColors is not VCLX compatible
    procedure AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
      DecreaseColors, Vertical: Boolean);
    {$ENDIF VCL}
    property DefaultRate: Longint read GetDefaultRate;
    property IconCount: Integer read GetIconCount;
    property FrameCount: Integer read GetFrameCount;
    property Icons[Index: Integer]: TIcon read GetIcon;
    property Frames[Index: Integer]: TJvIconFrame read GetFrame;
    property Title: string read FTitle write FTitle;
    property Creator: string read FCreator write FCreator;
    property OriginalColors: Word read FOriginalColors;
    property Header: TJvAniHeader read FHeader;
    property Index: Integer read FIndex write SetIndex;
  end;

implementation

uses
  Consts, Math,
  JvJVCLUtils, JvJCLUtils, JvIconList, JvConsts, JvResources;

function PadUp(Value: Longint): Longint;
begin
  Result := Value + (Value mod 2); // Up Value to nearest word boundary
end;

{$IFDEF VCL}
procedure DecreaseBMPColors(Bmp: TBitmap; Colors: Integer);
var
  Stream: TStream;
begin
  if (Bmp <> nil) and (Colors > 0) then
  begin
    Stream := BitmapToMemory(Bmp, Colors);
    try
      Bmp.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;
{$ENDIF VCL}

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8:
      Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

{ ReadTag, ReadChunk, SkipChunk. Some handy functions for reading RIFF files. }

function ReadTag(S: TStream; var Tag: TJvAniTag): Boolean;
begin
  Tag.ckID := #0#0#0#0;
  Tag.ckSize := 0;
  Result := S.Read(Tag, SizeOf(TJvAniTag)) = SizeOf(TJvAniTag);
end;

function ReadChunk(S: TStream; const Tag: TJvAniTag; var Data): Boolean;
begin
  Result := S.Read(Data, Tag.ckSize) = Tag.ckSize;
  if Result then
    Result := S.Seek(Tag.ckSize mod 2, soFromCurrent) <> -1;
end;

function ReadChunkN(S: TStream; const Tag: TJvAniTag; var Data;
  cbMax: Longint): Boolean;
var
  cbRead: Longint;
begin
  FillChar(Data, cbMax, #0);
  cbRead := Tag.ckSize;
  if cbMax < cbRead then
    cbRead := cbMax;
  Result := S.Read(Data, cbRead) = cbRead;
  if Result then
  begin
    cbRead := PadUp(Tag.ckSize) - cbRead;
    Result := S.Seek(cbRead, soFromCurrent) <> -1;
  end;
end;

function SkipChunk(S: TStream; const Tag: TJvAniTag): Boolean;
begin
  // Round pTag^.ckSize up to nearest word boundary to maintain alignment
  Result := S.Seek(PadUp(Tag.ckSize), soFromCurrent) <> -1;
end;

{ Icon and cursor types }

const
  RC3_STOCKICON = 0;
  RC3_ICON = 1;
  RC3_CURSOR = 2;

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

//=== TJvIconFrame ===========================================================

constructor TJvIconFrame.Create(Jiff: Longint);
begin
  inherited Create;
  FJiffRate := Jiff;
end;

destructor TJvIconFrame.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TJvIconFrame.Assign(Source: TPersistent);
begin
  if Source is TJvIconFrame then
    with Source as TJvIconFrame do
    begin
      if Self.FIcon = nil then
        Self.FIcon := TIcon.Create;
      Self.FIcon.Assign(Icon);
      Self.FIsIcon := FIsIcon;
      Self.FHotSpot := HotSpot;
      Self.FJiffRate := JiffRate;
    end
  else
    inherited Assign(Source);
end;

//=== TJvAnimatedCursorImage =================================================

constructor TJvAnimatedCursorImage.Create;
begin
  inherited Create;
  FIcons := TList.Create;
  FIndex := 0;
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
  for I := 0 to FIcons.Count - 1 do
    TJvIconFrame(FIcons[I]).Free;
  FIcons.Clear;
  SetLength(FRates, 0);
  SetLength(FSequence, 0);
  FFrameCount := 0;
  FTitle := '';
  FCreator := '';
  FillChar(FHeader, SizeOf(FHeader), 0);
  FOriginalColors := 0;
end;

procedure TJvAnimatedCursorImage.RiffReadError;
begin
  raise EReadError.Create(SReadError);
end;

function TJvAnimatedCursorImage.GetIconCount: Integer;
begin
  Result := FIcons.Count;
end;

function TJvAnimatedCursorImage.GetFrameCount: Integer;
begin
  Result := FFrameCount;
end;

function TJvAnimatedCursorImage.GetIcon(Index: Integer): TIcon;
begin
  if (Index >= 0) and (Index < IconCount) then
    Result := TJvIconFrame(FIcons[Index]).FIcon
  else
    Result := nil;
end;

function TJvAnimatedCursorImage.GetFrame(Index: Integer): TJvIconFrame;
begin
  if (Index >= 0) and (Index < FrameCount) then
    Result := TJvIconFrame(FIcons[FSequence[Index]])
  else
    Result := nil;
end;

function TJvAnimatedCursorImage.GetDefaultRate: Longint;
begin
  Result := Max(0, Min((FHeader.dwJIFRate * 100) div 6, High(Result)));
end;

procedure TJvAnimatedCursorImage.SetIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FrameCount) then
    FIndex := Value;
end;

procedure TJvAnimatedCursorImage.Assign(Source: TPersistent);
var
  I: Integer;
  Frame: TJvIconFrame;
begin
  if Source = nil then
    Clear
  else
  if Source is TJvAnimatedCursorImage then
  begin
    NewImage;
    try
      with TJvAnimatedCursorImage(Source) do
      begin
        Move(FHeader, Self.FHeader, SizeOf(FHeader));
        Self.FTitle := Title;
        Self.FCreator := Creator;
        Self.FOriginalColors := FOriginalColors;
        Self.FFrameCount := FrameCount;
        SetLength(Self.FRates, Length(FRates));
        Move(FRates[0], Self.FRates[0], Length(FRates)*SizeOf(Longint));
        SetLength(Self.FSequence, Length(FSequence));
        Move(FSequence[0], Self.FSequence[0], Length(FSequence)*SizeOf(Longint));
        for I := 0 to FIcons.Count - 1 do
        begin
          Frame := TJvIconFrame.Create(FHeader.dwJIFRate);
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
  else
    inherited Assign(Source);
end;

procedure TJvAnimatedCursorImage.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TIcon then
  begin
    if IconCount > 0 then
      Dest.Assign(Icons[Index])
    else
      Dest.Assign(nil);
  end
  else
  if Dest is TBitmap then
  begin
    {$IFDEF VCL}
    if IconCount > 0 then
      AssignToBitmap(TBitmap(Dest), TBitmap(Dest).Canvas.Brush.Color, True, False)
    else
    {$ENDIF VCL}
      Dest.Assign(nil);
  end
  else
  if Dest is TJvIconList then
  begin
    TJvIconList(Dest).BeginUpdate;
    try
      TJvIconList(Dest).Clear;
      for I := 0 to FrameCount - 1 do
        TJvIconList(Dest).Add(Frames[I].Icon);
    finally
      TJvIconList(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

function TJvAnimatedCursorImage.ReadCreateIcon(Stream: TStream; ASize: Longint;
  var HotSpot: TPoint; var IsIcon: Boolean): TIcon;
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array [0..300] of TIconRec;
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
    Mem.CopyFrom(Stream, ASize);
    HotSpot := Point(0, 0);
    IsIcon := PCursorOrIcon(Mem.Memory)^.wType = RC3_ICON;
    if PCursorOrIcon(Mem.Memory)^.wType = RC3_CURSOR then
      PCursorOrIcon(Mem.Memory)^.wType := RC3_ICON;
    if PCursorOrIcon(Mem.Memory)^.wType = RC3_ICON then
    begin
      { determinate original icon color }
      HeaderLen := PCursorOrIcon(Mem.Memory)^.Count * SizeOf(TIconRec);
      GetMem(List, HeaderLen);
      try
        Mem.Position := SizeOf(TCursorOrIcon);
        Mem.Read(List^, HeaderLen);
        for I := 0 to PCursorOrIcon(Mem.Memory)^.Count - 1 do
          with List^[I] do
          begin
            GetMem(BI, DIBSize);
            try
              Mem.Seek(DIBOffset, soFromBeginning);
              Mem.Read(BI^, DIBSize);
              FOriginalColors := Max(GetDInColors(BI^.biBitCount), FOriginalColors);
              HotSpot := Point(xHotspot, yHotspot);
            finally
              FreeMem(BI, DIBSize);
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

{ Loads an animated cursor from a RIFF file. The RIFF file format for
  animated cursors looks like this:

"RIFF" [Length of File]
    "ACON"
        "LIST" [Length of List]
            "INAM" [Length of Title] [Data]
            "IART" [Length of Author] [Data]
        "fram"
            "icon" [Length of Icon][Data]      ; 1st in list
            ...
            "icon" [Length of Icon] [Data]      ; Last in list  (1 to cFrames)
    "anih" [Length of ANI header (36 bytes)] [Data]   ; (see ANI Header TypeDef)
    "rate" [Length of rate block] [Data]      ; ea. rate is a long (length is 1 to cSteps)
    "seq " [Length of sequence block] [Data] ; ea. seq is a long (length is 1 to cSteps)
}

procedure TJvAnimatedCursorImage.ReadAniStream(Stream: TStream);
var
  I: Integer;
  Tag: TJvAniTag;
  Frame: TJvIconFrame;
  cbChunk, cbRead: Longint;
  Icon: TIcon;
  IsIcon: Boolean;
  HotSpot: TPoint;
  Buffer: array [0..255] of Char;
begin
  { Make sure it's a RIFF ANI file }
  if not ReadTag(Stream, Tag) or (Tag.ckID <> FOURCC_RIFF) then
    RiffReadError;
  if (Stream.Read(Tag.ckID, SizeOf(Tag.ckID)) < SizeOf(Tag.ckID)) or
    (Tag.ckID <> FOURCC_ACON) then
    RiffReadError;
  NewImage;
  { look for 'anih', 'rate', 'seq ', and 'icon' chunks }
  while ReadTag(Stream, Tag) do
  begin
    if Tag.ckID = FOURCC_anih then
    begin
      if not ReadChunk(Stream, Tag, FHeader) then
        Break;
      if ((FHeader.dwFlags and AF_ICON) <> AF_ICON) or
        (FHeader.dwFrames = 0) then
        RiffReadError;
    end
    else
    if Tag.ckID = FOURCC_rate then
    begin
      { If we find a rate chunk, read it into its preallocated space }
      SetLength(FRates, Tag.ckSize div SizeOf(Longint));
      if not ReadChunkN(Stream, Tag, FRates[0], Tag.ckSize) then
        Break;
    end
    else
    if Tag.ckID = FOURCC_seq then
    begin
      { If we find a seq chunk, read it into its preallocated space }
      FFrameCount := Tag.ckSize div SizeOf(Longint);
      SetLength(FSequence, FFrameCount);
      if not ReadChunkN(Stream, Tag, FSequence[0], Tag.ckSize) then
        Break;
    end
    else
    if Tag.ckID = FOURCC_LIST then
    begin
      cbChunk := PadUp(Tag.ckSize);
      { See if this list is the 'fram' list of icon chunks }
      cbRead := Stream.Read(Tag.ckID, SizeOf(Tag.ckID));
      if cbRead < SizeOf(Tag.ckID) then
        Break;
      Dec(cbChunk, cbRead);
      if Tag.ckID = FOURCC_fram then
      begin
        while cbChunk >= SizeOf(Tag) do
        begin
          if not ReadTag(Stream, Tag) then
            Break;
          Dec(cbChunk, SizeOf(Tag));
          if Tag.ckID = FOURCC_icon then
          begin
            { Ok, load the icon/cursor bits }
            Icon := ReadCreateIcon(Stream, Tag.ckSize, HotSpot, IsIcon);
            if Icon = nil then
              Break;
            Frame := TJvIconFrame.Create(FHeader.dwJIFRate);
            Frame.FIcon := Icon;
            Frame.FHotSpot := HotSpot;
            Frame.FIsIcon := IsIcon;
            FIcons.Add(Frame);
          end
          else
            { Unknown chunk in fram list, just ignore it }
            SkipChunk(Stream, Tag);
          Dec(cbChunk, PadUp(Tag.ckSize));
        end;
      end
      else
      if Tag.ckID = FOURCC_INFO then
      begin
        { now look for INAM and IART chunks }
        while cbChunk >= SizeOf(Tag) do
        begin
          if not ReadTag(Stream, Tag) then
            Break;
          Dec(cbChunk, SizeOf(Tag));
          if Tag.ckID = FOURCC_INAM then
          begin
            if (cbChunk < Tag.ckSize) or
              not ReadChunkN(Stream, Tag, Buffer[0], SizeOf(Buffer)-1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
            FTitle := Buffer;
          end
          else
          if Tag.ckID = FOURCC_IART then
          begin
            if (cbChunk < Tag.ckSize) or
              not ReadChunkN(Stream, Tag, Buffer[0], SizeOf(Buffer)-1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
            FCreator := Buffer;
          end
          else
          begin
            if not SkipChunk(Stream, Tag) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
          end;
        end;
      end
      else
      begin
        { Not the fram list or the INFO list. Skip the rest of this
          chunk. (Do not forget that we have already skipped one dword) }
        Tag.ckSize := cbChunk;
        SkipChunk(Stream, Tag);
      end;
    end
    else
    begin
      { We are not interested in this chunk, skip it. }
      if not SkipChunk(Stream, Tag) then
        Break;
    end;
  end;
  { Update the frame count in case we coalesced some frames while reading
    in the file. }
  for I := FIcons.Count - 1 downto 0 do
  begin
    if TJvIconFrame(FIcons[I]).FIcon = nil then
    begin
      TJvIconFrame(FIcons[I]).Free;
      FIcons.Delete(I);
    end;
  end;
  FHeader.dwFrames := FIcons.Count;
  if Length(FSequence) = 0 then
  begin
    FFrameCount := FIcons.Count;
    SetLength(FSequence, FFrameCount);
    for I := 0 to FFrameCount - 1 do
      FSequence[I] := I;
  end;
  if Length(FRates) = 0 then
  begin
    SetLength(FRates, FFrameCount);
    for I := 0 to FFrameCount - 1 do
      FRates[I] := FHeader.dwJIFRate;
  end;
  if FHeader.dwFrames = 0 then
    RiffReadError;
end;

{
procedure SetFOURCC(var FourCC: TJvFourCC; ID: string);
begin
  FourCC[0] := ID[1];
  FourCC[1] := ID[2];
  FourCC[2] := ID[3];
  FourCC[3] := ID[4];
end;

procedure StartWriteChunk(Stream: TStream; var Tag: TJvAniTag; ID: string);
begin
  SetFOURCC(Tag.ckID, ID);
  Tag.ckSize := Stream.Position;
  Stream.Write(Tag, SizeOf(Tag));
end;

procedure EndWriteChunk(Stream: TStream; var Tag: TJvAniTag);
var
  Pos: Int64;
  B: Byte;
begin
  Pos := Stream.Position;
  Tag.ckSize := Pos - Tag.ckSize;
  Stream.Seek(-Tag.ckSize, soFromCurrent);
  Stream.Write(Tag, SizeOf(Tag));
  Stream.Seek(Pos, soFromBeginning);
  if Odd(Tag.ckSize) then
  begin
    B := 0;
    Stream.Write(B, 1);
  end;
end;

procedure TJvAnimatedCursorImage.WriteAniStream(Stream: TStream);
var
  I: Integer;
  MemStream: TMemoryStream;
  TagRIFF, TagLIST, Tag: TJvAniTag;
  Id: TJvFourCC;
begin
  MemStream := TMemoryStream.Create;
  try
    StartWriteChunk(MemStream, TagRIFF, FOURCC_RIFF);

    SetFOURCC(Id, FOURCC_ACON);
    MemStream.Write(Id, SizeOf(TJvFourCC));

    if (Title <> '') or (Creator <> '') then
    begin
      StartWriteChunk(MemStream, TagLIST, FOURCC_LIST);
      SetFOURCC(Id, FOURCC_INFO);
      MemStream.Write(Id, SizeOf(TJvFourCC));
      if Title <> '' then
      begin
        StartWriteChunk(MemStream, Tag, FOURCC_INAM);
        MemStream.Write(PChar(Title)^, Length(Title));
        EndWriteChunk(MemStream, Tag);
      end;
      if Creator <> '' then
      begin
        StartWriteChunk(MemStream, Tag, FOURCC_IART);
        MemStream.Write(PChar(Creator)^, Length(Creator));
        EndWriteChunk(MemStream, Tag);
      end;
      EndWriteChunk(MemStream, TagLIST);
    end;
    SetFOURCC(Id, FOURCC_anih);
    MemStream.Write(Id, SizeOf(TJvFourCC));
    I := SizeOf(TJvAniHeader);
    MemStream.Write(I, SizeOf(Integer));
    FHeader.dwFrames := IconCount;
    MemStream.Write(FHeader, SizeOf(TJvAniHeader));
    StartWriteChunk(MemStream, Tag, FOURCC_rate);
    MemStream.Write(Frames[0].FJiffRate, SizeOf(Integer));
    EndWriteChunk(MemStream, Tag);
    StartWriteChunk(MemStream, Tag, FOURCC_seq);
    I := IconCount;
    MemStream.Write(I, SizeOf(Integer));
    EndWriteChunk(MemStream, Tag);

    StartWriteChunk(MemStream, TagLIST, FOURCC_LIST);
    SetFOURCC(Id, FOURCC_fram);
    MemStream.Write(Id, SizeOf(TJvFourCC));
    for I := 0 to IconCount - 1 do
    begin
      StartWriteChunk(MemStream, Tag, FOURCC_icon);
      Icons[I].SaveToStream(MemStream);
      EndWriteChunk(MemStream, Tag);
    end;
    EndWriteChunk(MemStream, TagLIST);

    EndWriteChunk(MemStream, TagRIFF);
    Stream.CopyFrom(MemStream, 0);
  finally
    MemStream.Free;
  end;
end;
}

procedure TJvAnimatedCursorImage.WriteAniStream(Stream: TStream);
begin
  raise EInvalidGraphicOperation.Create('writing of ANI files not yet supported');
end;

procedure TJvAnimatedCursorImage.LoadFromStream(Stream: TStream);
var
  Data: TMemoryStream;
  Size: Longint;
begin
  Size := Stream.Size - Stream.Position;
  Data := TMemoryStream.Create;
  try
    Data.SetSize(Size);
    Stream.ReadBuffer(Data.Memory^, Size);
    if Size > 0 then
    begin
      Data.Position := 0;
      ReadAniStream(Data);
    end;
  finally
    Data.Free;
  end;
end;

procedure TJvAnimatedCursorImage.SaveToStream(Stream: TStream);
begin
  if IconCount = 0 then
    raise EInvalidGraphicOperation.Create(SInvalidImage);
  WriteAniStream(Stream);
end;

procedure TJvAnimatedCursorImage.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
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
    if (Frames[Index] <> nil) and not Frames[Index].Icon.Empty then
      {$IFDEF VCL}
      DrawRealSizeIcon(ACanvas, Frames[Index].Icon, ARect.Left, ARect.Top);
      {$ELSE}
      ACanvas.Draw(ARect.Left, ARect.Top, Frames[Index].Icon);
      {$ENDIF VCL}
end;

{$IFDEF VCL}
procedure TJvAnimatedCursorImage.AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
  DecreaseColors, Vertical: Boolean);
var
  I: Integer;
  Temp: TBitmap;
  Idx: Integer;
  R: TRect;
begin
  Temp := TBitmap.Create;
  try
    if FIcons.Count > 0 then
    begin
      with Temp do
      begin
        Monochrome := False;
        Canvas.Brush.Color := BackColor;
        if Vertical then
        begin
          Width := Icons[0].Width;
          Height := Icons[0].Height * FrameCount;
        end
        else
        begin
          Width := Icons[0].Width * FrameCount;
          Height := Icons[0].Height;
        end;
        Canvas.FillRect(Bounds(0, 0, Width, Height));
        Idx := Index;
        for I := 0 to FrameCount - 1 do
        begin
          Index := I;
          R := Rect(Frames[I].Icon.Width * I * Ord(not Vertical),
            Frames[I].Icon.Height * I * Ord(Vertical), 0, 0);
          Draw(Canvas, R);
        end;
        Index := Idx;
      end;
      if DecreaseColors then
        DecreaseBMPColors(Temp, Max(OriginalColors, 16));
    end;
    Bitmap.Assign(Temp);
  finally
    Temp.Free;
  end;
end;
{$ENDIF VCL}

end.
