{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAni.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

The Original Code is: JvAniFile.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvAni;

interface

uses
  SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  Windows, Graphics, Controls, ExtCtrls, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QExtCtrls, QDialogs, Types, QTypes, QWindows,
  {$ENDIF VisualCLX}
  JvTypes;

type
  TJvIconFrame = class(TPersistent)
  private
    FIcon: TIcon;
    FIsIcon: Boolean;
    FHotSpot: TPoint;
    FRate: Longint;
  public
    constructor Create(JifRate: Longint);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Icon: TIcon read FIcon;
    property HotSpot: TPoint read FHotSpot;
    property Rate: Longint read FRate;
  end;

  TJvAni = class(TGraphic)
  private
    FHeader: TJvAniHeader;
    FTitle: string;
    FAuthor: string;
    FIcons: TList;
    FOriginalColors: Word;
    FIndex: Integer;
    FRates: array of Longint;
    FSequence: array of Longint;
    FFrameCount: Integer;
    FFrameResult: TJvIconFrame;
    FTimer: TTimer;
    procedure RiffReadError;
    function ReadCreateIcon(Stream: TStream; ASize: Longint;
      var HotSpot: TPoint; var IsIcon: Boolean): TIcon;
    procedure ReadAniStream(Stream: TStream);
    procedure WriteAniStream(Stream: TStream);
    procedure Clear;
    procedure NewImage;
    function GetAnimated: Boolean;
    function GetAuthor: string;
    function GetTitle: string;
    function GetIconCount: Integer;
    function GetFrameCount: Integer;
    function GetIcons(Index: Integer): TIcon;
    function GetFrames(Index: Integer): TJvIconFrame;
    procedure SetIndex(const Value: Integer);
    procedure SetAnimated(const Value: Boolean);
    procedure CalcDelay;
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure Animate(Sender: TObject);
    procedure SetTransparent(Value: Boolean); override;
    function GetTransparent: Boolean; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    {$IFDEF VCL}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure LoadFromMimeSource(MimeSource: TMimeSource); override;
    procedure SaveToMimeSource(MimeSource: TClxMimeSource); override;
    {$ENDIF VisualCLX}
    procedure AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
      DecreaseColors, Vertical: Boolean);
    procedure AssignIconsToBitmap(Bitmap: TBitmap; BackColor: TColor;
      DecreaseColors, Vertical: Boolean);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property Author: string read GetAuthor;
    property IconCount: Integer read GetIconCount;
    property FrameCount: Integer read GetFrameCount;
    property Frames[Index: Integer]: TJvIconFrame read GetFrames;
    property Header: TJvAniHeader read FHeader;
    property Icons[Index: Integer]: TIcon read GetIcons;
    property Index: Integer read FIndex write SetIndex;
    property OriginalColors: Word read FOriginalColors;
    property Title: string read GetTitle;
  end;

function LoadJvAniDialog: TJvAni;

implementation

uses
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  Math,
  JvJVCLUtils, JvJCLUtils, JvIconList, JvConsts, JvResources;

//=== TJvAnimatedCursorImage helper ==========================================

// (rom) created because JvAnimatedEditor.pas and JvIconListForm.pas contained the same code

function LoadJvAniDialog: TJvAni;
var
  CurDir: string;
begin
  Result := nil;
  CurDir := GetCurrentDir;
  with TOpenDialog.Create(nil) do
  try
    Options := [{$IFDEF VCL} ofHideReadOnly, {$ENDIF} ofFileMustExist];
    DefaultExt := RsAniExtension;
    Filter := RsAniCurFilter;
    if Execute then
    begin
      Result := TJvAni.Create;
      try
        Result.LoadFromFile(FileName);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  finally
    Free;
    SetCurrentDir(CurDir);
  end;
end;

function PadUp(Value: Longint): Longint;
begin
  Result := Value + (Value mod 2); // Up Value to nearest word boundary
end;

procedure DecreaseBMPColors(Bmp: TBitmap; Colors: Integer);
{$IFDEF VCL}
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
{$IFDEF VisualCLX}
begin
  // TODO
end;
{$ENDIF VisualCLX}

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

constructor TJvIconFrame.Create(JifRate: Longint);
begin
  inherited Create;
  FIcon := nil;
  FRate := JifRate;
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
      Self.FRate := Rate;
    end
  else
    inherited Assign(Source);
end;

//=== TJvAni =================================================================

constructor TJvAni.Create;
begin
  inherited Create;
  FIcons := TList.Create;
  FIndex := -1;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := Animate;
  FTimer.Enabled := False;
  FFrameResult := TJvIconFrame.Create(0);
end;

destructor TJvAni.Destroy;
begin
  NewImage;
  FreeAndNil(FIcons);
  FreeAndNil(FTimer);
  FFrameResult.FIcon := nil;
  FreeAndNil(FFrameResult);
  inherited Destroy;
end;

procedure TJvAni.Clear;
begin
  if FIcons.Count > 0 then
  begin
    NewImage;
    Changed(Self);
  end;
end;

procedure TJvAni.NewImage;
var
  I: Integer;
begin
  if Assigned(FIcons) then
    for I := 0 to FIcons.Count - 1 do
      TJvIconFrame(FIcons[I]).Free;
  FreeAndNil(FIcons);
  SetLength(FRates, 0);
  SetLength(FSequence, 0);
  FFrameCount := 0;
  FTitle := '';
  FAuthor := '';
  FillChar(FHeader, SizeOf(FHeader), 0);
  FOriginalColors := 0;
  FIndex := -1;
end;

procedure TJvAni.Assign(Source: TPersistent);
var
  I: Integer;
  Frame: TJvIconFrame;
begin
  if Source = nil then
    Clear
  else
  if Source is TJvAni then
  begin
    Clear;
    try
      with TJvAni(Source) do
      begin
        Move(FHeader, Self.FHeader, SizeOf(FHeader));
        Self.FTitle := Title;
        Self.FAuthor := Author;
        Self.FOriginalColors := FOriginalColors;
        Self.FFrameCount := FrameCount;
        SetLength(Self.FRates, Length(FRates));
        if Length(FRates) <> 0 then
          Move(FRates[0], Self.FRates[0], Length(FRates) * SizeOf(Longint));
        SetLength(Self.FSequence, Length(FSequence));
        if Length(FSequence) <> 0 then
          Move(FSequence[0], Self.FSequence[0], Length(FSequence) * SizeOf(Longint));
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
        Self.FIndex := Index;
        Self.Animated := Animated;
      end;
    except
      NewImage;
      raise;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvAni.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TIcon then
  begin
    if FrameCount > 0 then
      Dest.Assign(Frames[Index].Icon)
    else
      Dest.Assign(nil);
  end
  else
  if Dest is TBitmap then
  begin
    if FrameCount > 0 then
      AssignToBitmap(TBitmap(Dest), TBitmap(Dest).Canvas.Brush.Color, True, False)
    else
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

function TJvAni.GetEmpty: Boolean;
begin
  Result := (FrameCount = 0);
end;

procedure TJvAni.SetHeight(Value: Integer);
begin
  raise EInvalidGraphicOperation.CreateRes(@SChangeIconSize);
end;

procedure TJvAni.SetWidth(Value: Integer);
begin
  raise EInvalidGraphicOperation.CreateRes(@SChangeIconSize);
end;

function TJvAni.GetWidth: Integer;
begin
  Result := Frames[Index].Icon.Width;
end;

function TJvAni.GetHeight: Integer;
begin
  Result := Frames[Index].Icon.Height;
end;

{$IFDEF VCL}

procedure TJvAni.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.CreateRes(@SIconToClipboard);
end;

procedure TJvAni.SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.CreateRes(@SIconToClipboard);
end;

{$ENDIF VCL}

procedure TJvAni.SetIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FrameCount) and (FIndex <> Value) then
  begin
    FIndex := Value;
    Changed(Self);
  end;
end;

function TJvAni.GetAuthor: string;
begin
  Result := FAuthor;
end;

function TJvAni.GetTitle: string;
begin
  Result := FTitle;
end;

function TJvAni.GetIconCount: Integer;
begin
  Result := FIcons.Count;
end;

function TJvAni.GetFrameCount: Integer;
begin
  Result := FFrameCount;
end;

function TJvAni.GetAnimated: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TJvAni.SetAnimated(const Value: Boolean);
begin
  if Value <> FTimer.Enabled then
    FTimer.Enabled := Value;
end;

procedure TJvAni.Animate(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FrameCount > 0 then
    Index := (Index + 1) mod Integer(FrameCount);
  CalcDelay;
  FTimer.Enabled := True;
end;

procedure TJvAni.CalcDelay;
begin
  if Index = -1 then
    Animated := False
  else
  begin
    FTimer.Interval := (Cardinal(Frames[Index].Rate) * 100) div 6;
    if FTimer.Interval = 0 then
      FTimer.Interval := 100;
  end;
end;

procedure TJvAni.SetTransparent(Value: Boolean);
begin
  // Icons are always transparent so animations also
end;

function TJvAni.GetTransparent: Boolean;
begin
  Result := True;
end;

procedure TJvAni.RiffReadError;
begin
  raise EReadError.CreateRes(@SReadError);
end;

function TJvAni.GetIcons(Index: Integer): TIcon;
begin
  if (Index >= 0) and (Index < IconCount) then
    Result := TJvIconFrame(FIcons[Index]).FIcon
  else
    Result := nil;
end;

function TJvAni.GetFrames(Index: Integer): TJvIconFrame;
var
  N: Integer;
begin
  if (Index >= 0) and (Index < FrameCount) then
  begin
    if Length(FSequence) <> 0 then
      N := FSequence[Index]
    else
      N := Index;
    FFrameResult.FIcon := TJvIconFrame(FIcons[N]).FIcon;
    FFrameResult.FIsIcon := TJvIconFrame(FIcons[N]).FIsIcon;
    FFrameResult.FHotSpot := TJvIconFrame(FIcons[N]).FHotSpot;
    if Length(FRates) <> 0 then
      FFrameResult.FRate := FRates[Index]
    else
      FFrameResult.FRate := FHeader.dwJIFRate;
    Result := FFrameResult;
  end
  else
    Result := nil;
end;

function TJvAni.ReadCreateIcon(Stream: TStream; ASize: Longint;
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

procedure TJvAni.ReadAniStream(Stream: TStream);
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
  Clear;
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
              not ReadChunkN(Stream, Tag, Buffer[0], SizeOf(Buffer) - 1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
            FTitle := Buffer;
          end
          else
          if Tag.ckID = FOURCC_IART then
          begin
            if (cbChunk < Tag.ckSize) or
              not ReadChunkN(Stream, Tag, Buffer[0], SizeOf(Buffer) - 1) then
              Break;
            Dec(cbChunk, PadUp(Tag.ckSize));
            FAuthor := Buffer;
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
  if FrameCount = 0 then
    FFrameCount := FIcons.Count;
  FHeader.dwFrames := FIcons.Count;
  if FHeader.dwFrames = 0 then
    RiffReadError;
end;

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

procedure EndWriteChunk(Stream: TStream; var Tag: TJvAniTag; AddSize: Integer);
var
  Pos: Int64;
  B: Byte;
begin
  Pos := Stream.Position;
  Tag.ckSize := Pos - Tag.ckSize;
  Stream.Seek(-Tag.ckSize, soFromCurrent);
  Dec(Tag.ckSize, SizeOf(TJvAniTag));
  Inc(Tag.ckSize, AddSize);
  Stream.Write(Tag, SizeOf(Tag));
  Stream.Seek(Pos, soFromBeginning);
  if Odd(Tag.ckSize) then
  begin
    B := 0;
    Stream.Write(B, 1);
  end;
end;

procedure TJvAni.WriteAniStream(Stream: TStream);
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

    if (Title <> '') or (Author <> '') then
    begin
      StartWriteChunk(MemStream, TagLIST, FOURCC_LIST);
      SetFOURCC(Id, FOURCC_INFO);
      MemStream.Write(Id, SizeOf(TJvFourCC));
      if Title <> '' then
      begin
        StartWriteChunk(MemStream, Tag, FOURCC_INAM);
        MemStream.Write(PChar(Title)^, Length(Title) + 1);
        EndWriteChunk(MemStream, Tag, 0);
      end;
      if Author <> '' then
      begin
        StartWriteChunk(MemStream, Tag, FOURCC_IART);
        MemStream.Write(PChar(Author)^, Length(Author) + 1);
        EndWriteChunk(MemStream, Tag, 0);
      end;
      EndWriteChunk(MemStream, TagLIST, 0);
    end;
    StartWriteChunk(MemStream, Tag, FOURCC_anih);
    FHeader.dwFrames := IconCount;
    MemStream.Write(FHeader, SizeOf(TJvAniHeader));
    EndWriteChunk(MemStream, Tag, 0);
    if Length(FRates) <> 0 then
    begin
      StartWriteChunk(MemStream, Tag, FOURCC_rate);
      MemStream.Write(FRates, Length(FRates) * SizeOf(Longint));
      EndWriteChunk(MemStream, Tag, 0);
    end;
    if Length(FSequence) <> 0 then
    begin
      StartWriteChunk(MemStream, Tag, FOURCC_seq);
      MemStream.Write(FSequence[0], Length(FSequence) * SizeOf(Longint));
      EndWriteChunk(MemStream, Tag, 0);
    end;

    StartWriteChunk(MemStream, TagLIST, FOURCC_LIST);
    SetFOURCC(Id, FOURCC_fram);
    MemStream.Write(Id, SizeOf(TJvFourCC));
    for I := 0 to IconCount - 1 do
    begin
      StartWriteChunk(MemStream, Tag, FOURCC_icon);
      Icons[I].SaveToStream(MemStream);
      EndWriteChunk(MemStream, Tag, 0);
    end;
    EndWriteChunk(MemStream, TagLIST, 0);

    EndWriteChunk(MemStream, TagRIFF, SizeOf(TJvAniTag));
    Stream.CopyFrom(MemStream, 0);
  finally
    MemStream.Free;
  end;
end;

procedure TJvAni.LoadFromStream(Stream: TStream);
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
  if FrameCount > 0 then
    Index := 0;
end;

procedure TJvAni.SaveToStream(Stream: TStream);
begin
  if IconCount = 0 then
    raise EInvalidGraphicOperation.CreateRes(@SInvalidImage);
  WriteAniStream(Stream);
end;

procedure TJvAni.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
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

procedure TJvAni.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvAni.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if Assigned(FIcons) and (FIcons.Count > 0) then
    if (Frames[Index] <> nil) and not Frames[Index].Icon.Empty then
      {$IFDEF VCL}
      DrawRealSizeIcon(ACanvas, Frames[Index].Icon, ARect.Left, ARect.Top);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      ACanvas.Draw(ARect.Left, ARect.Top, Frames[Index].Icon);
      {$ENDIF VisualCLX}
end;

procedure TJvAni.AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
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

procedure TJvAni.AssignIconsToBitmap(Bitmap: TBitmap; BackColor: TColor;
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
          Height := Icons[0].Height * IconCount;
        end
        else
        begin
          Width := Icons[0].Width * IconCount;
          Height := Icons[0].Height;
        end;
        Canvas.FillRect(Bounds(0, 0, Width, Height));
        Idx := Index;
        for I := 0 to IconCount - 1 do
        begin
          Index := I;
          R := Rect(Icons[I].Width * I * Ord(not Vertical),
            Icons[I].Height * I * Ord(Vertical), 0, 0);
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

{$IFDEF VisualCLX}

procedure TJvAni.LoadFromMimeSource(MimeSource: TMimeSource);
begin
  raise EInvalidGraphicOperation.CreateRes(@RsENotSupported);
end;

procedure TJvAni.SaveToMimeSource(MimeSource: TClxMimeSource);
begin
  raise EInvalidGraphicOperation.CreateRes(@RsENotSupported);
end;

{$ENDIF VisualCLX}

initialization
  {$IFDEF VCL}
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvAni, TControl);
  {$ENDIF COMPILER7_UP}
  Classes.RegisterClass(TJvAni);
  {$ENDIF VCL}
  TPicture.RegisterFileFormat(RsAniExtension, RsAniFilterName, TJvAni);

finalization
  TPicture.UnregisterGraphicClass(TJvAni);

end.

