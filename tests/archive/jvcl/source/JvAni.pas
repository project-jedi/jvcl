{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAni.PAS, released on 2001-02-28.

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

unit JvAni;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Consts;

type
  TAniHeader = record
    dwSizeof: Longint;
    dwFrames: Longint;
    dwSteps: Longint;
    dwCX: Longint;
    dwCY: Longint;
    dwBitCount: Longint;
    dwPlanes: Longint;
    dwJIFRate: Longint;
    dwFlags: Longint;
  end;
{$EXTERNALSYM TAniHeader}

  TJvAni = class(TGraphic)
  private
    FAuthor: string;
    FTitle: string;
    FNumberFrames: Cardinal;
    FHeader: TAniHeader;
    FCurrentIcon: TIcon;
    FImage: TMemoryStream;
    FRate: TList;
    FSequence: TList;
    FImages: TList;
    FIndex: Integer;
    FTimer: TTimer;
    procedure Clear;
    procedure SetIndex(const Value: Integer);
    function GetAnimated: Boolean;
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
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    property Author: string read FAuthor;
    property Title: string read FTitle;
    property Icon: TIcon read FCurrentIcon;
    property FramesCount: Cardinal read FNumberFrames default 0;
    property Index: Integer read FIndex write SetIndex default -1;
    property Animated: Boolean read GetAnimated write SetAnimated default False;
  end;

implementation

resourcestring
  RC_AniExtension = 'ani';
  RC_AniFilterName = 'ANI Image';

  {**************************************************}

constructor TJvAni.Create;
begin
  inherited Create;
  FAuthor := '';
  FTitle := '';
  FNumberFrames := 0;
  FImage := TMemoryStream.Create;
  FRate := TList.Create;
  FSequence := TList.Create;
  FImages := TList.Create;
  FCurrentIcon := TIcon.Create;
  FIndex := -1;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := Animate;
  FTimer.Enabled := False;
end;

{**************************************************}

destructor TJvAni.Destroy;
begin
  FTimer.Free;
  Clear;
  FImage.Free;
  FRate.Free;
  FSequence.Free;
  FImages.Free;
  FCurrentIcon.Free;

  inherited Destroy;
end;
{**************************************************}

procedure TJvAni.Clear;
begin
  FImages.Clear;
  FAuthor := '';
  FTitle := '';
  FNumberFrames := 0;
  FRate.Clear;
  FSequence.Clear;
  FImage.Size := 0;
  if FCurrentIcon.Handle <> 0 then
    DestroyIcon(FCurrentIcon.Handle);
  FCurrentIcon.Handle := 0;
  FIndex := -1;

  if not (csDestroying in Application.ComponentState) then
    Changed(Self);
end;

{**************************************************}

procedure TJvAni.Assign(Source: TPersistent);
var
  Stream: TStream;
begin
  if Source = nil then
    Clear
  else if Source is TJvAni then
  begin
    Stream := TMemoryStream.Create;
    TJvAni(Source).SaveToStream(Stream);
    Stream.Position := 0;
    LoadFromStream(Stream);
    Stream.Free;
    Animated := TJvAni(Source).Animated;
  end
  else
    inherited Assign(Source);
end;

{**************************************************}

function TJvAni.GetEmpty: Boolean;
begin
  Result := (FNumberFrames = 0);
end;

{**************************************************}

procedure TJvAni.SetHeight(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(sChangeIconSize);
end;

{**************************************************}

procedure TJvAni.SetWidth(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(sChangeIconSize);
end;

{**************************************************}

function TJvAni.GetWidth: Integer;
begin
  Result := FHeader.dwCX;
end;

{**************************************************}

function TJvAni.GetHeight: Integer;
begin
  Result := FHeader.dwCY;
end;

{**************************************************}

procedure TJvAni.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.Create(sIconToClipboard);
end;

{**************************************************}

procedure TJvAni.SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.Create(sIconToClipboard);
end;

{**************************************************}

procedure TJvAni.LoadFromStream(Stream: TStream);
const
  an_RIFF = $46464952;
  an_ACON = $4E4F4341;
  an_LIST = $5453494C;
  an_INFO = $4F464E49;
  an_INAM = $4D414E49;
  an_IART = $54524149;
  an_anih = $68696E61;
  an_rate = $65746172;
  an_fram = $00617266;
  an_icon = $6E6F6369;
  an_seq = $20716573;
var
  dw, dw2: Integer;

  procedure Error;
  begin
    raise EInvalidGraphic.Create('Animated icon image is not valid');
  end;

  function ReadByte: Byte;
  begin
    Dec(dw, SizeOf(Byte));
    if FImage.Read(Result, SizeOf(Result)) = 0 then
      Result := 0;
  end;

  function ReadDWord: DWord;
  begin
    Dec(dw, SizeOf(DWord));
    if FImage.Read(Result, SizeOf(Result)) < SizeOf(DWord) then
      Result := 0;
  end;

  function ReadString: string;
  var
    p: PChar;
    l: Integer;
  begin
    l := ReadDWord;

    //Check for integrity of the data
    if (l > 0) and (l < FImage.Size - FImage.Position) then
    begin
      Dec(dw, l);
      GetMem(p, l + 1);
      FillChar(p^, l + 1, 0);
      FImage.Read(p^, l);
      Result := StrPas(p);
      FreeMem(p);
      if ReadByte <> $00 then
        FImage.Position := FImage.Position - 1;
    end
    else
      Result := '';
  end;

  procedure ReadList(List: TList);
  var
    Len: Integer;
  begin
    Len := ReadDWord div 4;
    if (Len = FHeader.dwSteps) then
    begin
      for Len := 1 to Len do
        List.Add(Pointer(ReadDWord));
    end
    else
      Error;
  end;

  procedure ReadFrames;
  var
    i, j, k: Integer;
  begin
    for i := 0 to FHeader.dwFrames - 1 do
      if ReadDWord <> an_icon then
        Error
      else
      begin
        FImages.Add(Pointer(FImage.Position));
        k := ReadDWord;
        Dec(dw, k);
        j := FImage.Position + k;
        if (FHeader.dwCX = 0) or (FHeader.dwCY = 0) then
        begin
          FImage.Position := FImage.Position + 6;
          FHeader.dwCX := ReadByte;
          FHeader.dwCY := ReadByte;
        end;
        FImage.Position := j;
      end;
  end;

begin
  Clear;
  FImage.CopyFrom(Stream, Stream.Size - Stream.Position);
  FImage.Position := 0;
  if ReadDWord <> an_RIFF then
    Error;
  FImage.Size := ReadDWord;
  if ReadDWord <> an_ACON then
    Error;

  while FImage.Position < FImage.Size do
    case ReadDWord of
      an_LIST:
        begin
          dw := ReadDWord;
          while dw > 0 do
          begin
            dw2 := ReadDWord;
            case dw2 of
              an_INAM:
                FTitle := ReadString;
              an_IART:
                FAuthor := ReadString;
            else
              if (dw2 and $00FFFFFF) = an_fram then
                ReadFrames;
            end;
          end;
        end;
      an_anih:
        FImage.Read(FHeader, ReadDWord);
      an_rate:
        ReadList(FRate);
      an_seq:
        ReadList(FSequence);
    else
      FImage.Position := FImage.Position + Integer(ReadDWord);
    end;

  FNumberFrames := FHeader.dwFrames;
  FIndex := -1;
  SetIndex(0);
end;

{**************************************************}

procedure TJvAni.SaveToStream(Stream: TStream);
begin
  if GetEmpty then
    raise EInvalidGraphicOperation.Create(sInvalidImage);
  Stream.Write(FImage.Memory^, FImage.Size)
end;

{**************************************************}

procedure TJvAni.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FCurrentIcon.Handle <> 0 then
    DrawIcon(ACanvas.Handle, Rect.Left, Rect.Top, FCurrentIcon.Handle);
end;

{**************************************************}

procedure TJvAni.SetIndex(const Value: Integer);
type
  TIconHeader = packed record
    AlwaysZero: Word;
    CursorType: Word;
    NumIcons: Word;
  end;

  TIconDirEntry = packed record
    Width: Byte;
    Height: Byte;
    Colors: Byte;
    Reserved: Byte;
    dwReserved: LongInt;
    dwBytesInRes: LongInt;
    dwImageOffset: LongInt;
  end;
var
  p: Integer;
  Len: Integer;
  IconHeader: TIconHeader;
begin
  if (FImages.Count > 0) and (Value >= 0) and (Value < FHeader.dwFrames) then
    if FIndex <> Value then
    begin
      FIndex := Value;
      if FCurrentIcon.Handle <> 0 then
        DestroyIcon(FCurrentIcon.Handle);

      p := Integer(FImage.Memory);
      FImage.Position := Integer(FImages[FIndex]);
      FImage.Read(Len, SizeOf(Len));
      FImage.Read(IconHeader, SizeOf(IconHeader));
      FImage.Position := FImage.Position + (SizeOf(TIconDirEntry) * IconHeader.NumIcons);
      Dec(Len, SizeOf(IconHeader) + (SizeOf(TIconDirEntry) * IconHeader.NumIcons));
      Inc(p, FImage.Position);

      p := CreateIconFromResource(Pointer(p), Len, True, $30000);
      FCurrentIcon.Handle := p;
      Changed(Self);
    end;
end;

{**************************************************}

function TJvAni.GetAnimated: Boolean;
begin
  Result := FTimer.Enabled;
end;

{**************************************************}

procedure TJvAni.SetAnimated(const Value: Boolean);
begin
  if Value <> FTimer.Enabled then
    FTimer.Enabled := Value;
end;

{**************************************************}

procedure TJvAni.Animate(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FNumberFrames > 0 then
    SetIndex((FIndex + 1) mod Integer(FNumberFrames));
  CalcDelay;
  FTimer.Enabled := True;
end;

{**************************************************}

procedure TJvAni.CalcDelay;
begin
  if FIndex = -1 then
    SetAnimated(False)
  else
  begin
    if (FIndex > 0) and (FRate.Count > FIndex) then
      FTimer.Interval := Cardinal(FRate[FIndex]) * (1000 div 60)
    else
    begin
      FTimer.Interval := FHeader.dwJIFRate * (1000 div 60);
      if FTimer.Interval = 0 then
        FTimer.Interval := 100;
    end
  end;
end;

{**************************************************}

procedure TJvAni.SetTransparent(Value: Boolean);
begin
  // Icons are always transparent so animations also
end;

{**************************************************}

function TJvAni.GetTransparent: Boolean;
begin
  Result := True;
end;

{**************************************************}

initialization
  RegisterClass(TJvAni);
  TPicture.RegisterFileFormat(RC_AniExtension, RC_AniFilterName, TJvAni);

finalization
  TPicture.UnregisterGraphicClass(TJvAni);

end.
