{-------------------------------------------------------------------------------------------------}
{ TWinCursor                                                                                      }
{                                                                                                 }
{ Copyright (c) 2002, Matthias Thoma (ma.thoma@gmx.de)                                                             }
{ All rights reserved.                                                                            }
{                                                                                                 }
{ Version 0.6                                                                                     }
{ Supported:     - Traditional cursors                                                            }
{ Not supported: - Multicolor cursors (as soon as QT3 is supported)                               }
{                - Animated cursors (maybe in feature)                                            }
{                                                                                                 }
{ Thanks to Christoph Federer for Beta testing.                                                   }
{                                                                                                 }
{ Permission is hereby granted, free of charge, to any person obtaining a copy of this software   }
{ and associated documentation files(the "Software"), to deal in the Software without restriction,}
{ including without limitation the rights to use, copy, modify, merge, publish, distribute,       }
{ sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                                        }
{                                                                                                 }
{ The above copyright notice and this permission notice shall be included in all copies or        }
{ substantial portions of the Software.                                                           }
{                                                                                                 }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING   }
{ BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      }
{ NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,    }
{ DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  }
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         }
{-------------------------------------------------------------------------------------------------}

unit QWinCursors;
{$R-}

interface
uses
  Classes, SysUtils, Types,
  Qt, QGraphics, QTypes;

type
  TCurInvMode = (invBlack, invWhite, invTransparent);

type
  TWinCursor = class(TGraphic)
  private
    FHandle: QCursorH;
    FWidth: Integer;
    FHeight: Integer;
    FBytesPerRow: Word;
    FOwnsHandle: Boolean;
    FInvMode: TCurInvMode;
    FHotspot: TPoint;
    FCustomCursor: record
      Bits: array of Byte;
      Mask: array of Byte;
    end;
  protected
    procedure ConvertDIB(Stream: TStream);
    procedure CreateCursor;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure FreeCursor;
    function GetHotSpot: TPoint;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure SetHeight(Value: Integer); override;
    procedure SetHotspot(const Value: TPoint); virtual;
    procedure SetWidth(Value: Integer); override;
  public
    property Handle: QCursorH read FHandle;
    property Height: Integer read FHeight;
    property Hotspot: TPoint read GetHotspot write SetHotspot;
    property InvMode: TCurInvMode read FInvMode write FInvMode;
    property Width: Integer read FWidth;

    constructor Create;  reintroduce; overload;
    constructor Create(AHandle: QCursorH); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromMimeSource(MimeSource: TMimeSource); override;
    procedure LoadFromResourceName(Instance: Cardinal; ResourceName: string);
    procedure OwnHandle;
    procedure SaveToMimeSource(MimeSource: TClxMimeSource); override;
    procedure SaveToStream(Stream: TStream); override;

    function ReleaseHandle: QCursorH;
  end;

function LoadCursor(Instance: Cardinal; CursorName: string): QCursorH;
function LoadCursorFromFile(CursorFileName: string): QCursorH;

type
  EWinCursor = class(Exception);

implementation

resourcestring
  RsUnsupported = 'Unsupported or illegal format.';
  RsInvalidOperation = 'Invalid operation.';

type
   _CURSORDIRENTRY = packed record
     bWidth: Byte;
     bHeight: Byte;
     bColorCount: Byte;
     bReserved: Byte;
     wXHotspot: Word;
     wYHotspot: Word;
     lBytesInRes: DWORD;
     dwImageOffset: DWORD;
   end;

   TCURSORDIRENTRY = _CURSORDIRENTRY;
   PCURSORDIRENTRY = ^_CURSORDIRENTRY;

   _CURSORDIR =  packed record
    cdReserved: WORD;
    cdType: WORD;
    cdCount: WORD;
  end;

  TCURSORDIR = _CURSORDIR;
  PCURSORDIR = ^_CURSORDIR;

  TResCursorDir = packed record
     Width: Word;
     Height: Word;
     Planes: Word;
     BitCount: Word;
     BytesInRes: DWORD;
     IconCursorId: Word;
  end;

type
  TCustomCursor = record
      Bits: array of Byte;
      Mask: array of Byte;
    end;

type
  tagLocalHeader = packed record
     XHotSpot: Word;
     YHotSpot: Word;
     Reserved: Int64;
  end;

//=== TWinCursor =============================================================

constructor TWinCursor.Create;
begin
  inherited Create;

  FHandle := nil;
  FWidth := 0;
  FHeight := 0;
  FBytesPerRow := 0;
  FOwnsHandle := False;
  FInvMode := InvTransparent;
end;

constructor TWinCursor.Create(AHandle: QCursorH);
begin
  inherited Create;

  FHandle := AHandle;
  FOwnsHandle := False;
end;

destructor TWinCursor.Destroy;
begin
  if FOwnsHandle then
    FreeCursor;

  inherited Destroy;
end;

procedure TWinCursor.LoadFromResourceName(Instance: Cardinal; ResourceName: String);
var
  ResourceStream: TResourceStream;
  CURSORDIR: TCURSORDIR;
  ResDir: TResCursorDir;
  BmpInfo: TBITMAPINFOHEADER;
  localHeader: tagLocalHeader;
begin
  ResourceStream := TResourceStream.Create(Instance, ResourceName, PChar(12));

  try
    ResourceStream.ReadBuffer(CursorDir, SizeOf(TCursorDir));

    if (CursorDir.cdReserved <> 0) or (CursorDir.cdType <> 2) or (CursorDir.cdCount <> 1) then
      raise EWinCursor.Create(RsUnsupported);

    ResourceStream.ReadBuffer(ResDir, SizeOf(TResCursorDir));

    FWidth := ResDir.Width;
    FHeight := ResDir.Height div 2;
  finally
    ResourceStream.Free;
  end;

  ResourceStream := TResourceStream.CreateFromID(HInstance, ResDir.IconCursorId, PChar(1));
  try
    ResourceStream.Position := 0;
    ResourceStream.Read(LocalHeader, SizeOf(tagLocalHeader));

    FBytesPerRow := FWidth div 8;

    if (FWidth mod 8) <> 0 then
      Inc(FBytesPerRow);

    ResourceStream.Read(BmpInfo, SizeOf(BmpInfo)); // Ignore BmpInfo
    SetLength(FCustomCursor.Bits, FBytesPerRow * FHeight);
    SetLength(FCustomCursor.Mask, FBytesPerRow * FHeight);

    ConvertDIB(ResourceStream);
    CreateCursor;
  finally
      ResourceStream.Free;
      SetLength(FCustomCursor.Bits, 0);
      SetLength(FCustomCursor.Mask, 0);
  end;
end;

procedure TWinCursor.LoadFromStream(Stream: TStream);
var
  CURSORDIR: TCURSORDIR;
  Entry: TCURSORDIRENTRY;
  BitmapInfo: TBITMAPINFOHEADER;
begin
  Stream.ReadBuffer(CursorDir, SizeOf(TCursorDir));

  if (CursorDir.cdReserved <> 0) or (CursorDir.cdType <> 2) or (CursorDir.cdCount <> 1) then
    raise EWinCursor.Create(RsUnsupported);

  Stream.Read(Entry, SizeOf(TCURSORDIRENTRY));
  Stream.Seek(Entry.dwImageOffset, soFromBeginning);
  Stream.Read(BitmapInfo, SizeOf(TBITMAPINFOHEADER));

  with Entry do
  begin
    FWidth := bWidth;
    FHeight := bHeight;
    FHotspot.X := wXHotspot;
    FHotspot.Y := wYHotspot
  end;

  Stream.Seek(8, soFromCurrent);
  FBytesPerRow := FWidth div 8;

  if (FWidth mod 8) <> 0 then
    Inc(FBytesPerRow);

  SetLength(FCustomCursor.Bits, FBytesPerRow * FHeight);
  SetLength(FCustomCursor.Mask, FBytesPerRow * FHeight);

  ConvertDib(Stream);
  CreateCursor;
end;

{------------------------------------------------------------------------------}
{                                                                              }
{ Convert Table                                                                }
{                                                                              }
{                   And   Xor          Bitmap    Mask                          }
{ Black              0     0             1         1                           }
{ White              0     1      =>     0         1                           }
{ Transparent        1     0             0         0                           }
{                                                                              }
{ Inverse            1     1             0         0  Transparent              }
{ Inverse            1     1             1         1  Black                    }
{ Inverse            1     1             0         1  White                    }
{                                                                              }
{ Inv > Transparent: => Bitmap := not("AND" or "XOR")                          }
{                    => Mask   := not "AND"                                    }
{                                                                              }
{ Inv > Black:       => Bitmap := not("AND" xor "XOR")                         }
{                    => Mask   := not "AND" or "XOR"                           }
{                                                                              }
{ Inv > White:       => Bitmap := not("AND" or "XOR")                          }
{                    => Mask   := not(("XOR" xor "AND") and "AND")             }
{                                                                              }
{------------------------------------------------------------------------------}

procedure TWinCursor.ConvertDIB(Stream: TStream);
var
  TempCursor: TCustomCursor;
  AndByte, XorByte: Byte;
  I: Integer;
  T: Integer;
begin
  SetLength(TempCursor.Bits, FBytesPerRow * FHeight);
  SetLength(TempCursor.Mask, FBytesPerRow * FHeight);

  Stream.ReadBuffer(TempCursor.Mask[0], FHeight*FBytesPerRow);
  Stream.ReadBuffer(TempCursor.Bits[0], FHeight*FBytesPerRow);

  for I := 0 to FHeight - 1 do
  begin
    for T := 0 to FBytesPerRow - 1 do
    begin
      AndByte := TempCursor.Bits[I*FBytesPerRow+T];
      XorByte := TempCursor.Mask[I*FBytesPerRow+T];

      case FInvMode of
      invBlack:
        begin
          FCustomCursor.Bits[(FHeight-1-I) * FBytesPerRow + T] := not(XorByte xor AndByte);
          FCustomCursor.Mask[(FHeight-1-I) * FBytesPerRow + T] := not AndByte or XorByte;
        end;
      invWhite:
        begin
          FCustomCursor.Bits[(FHeight-1-I) * FBytesPerRow + T] := not(XorByte or AndByte);
          FCustomCursor.Mask[(FHeight-1-I) * FBytesPerRow + T] := not((XorByte xor AndByte) and AndByte);
        end;
      invTransparent:
        begin
          FCustomCursor.Bits[(FHeight-1-I) * FBytesPerRow + T] := not(XorByte or AndByte);
          FCustomCursor.Mask[(FHeight-1-I) * FBytesPerRow + T] := not(AndByte);
        end;
      end;
    end;
  end;
end;

procedure TWinCursor.CreateCursor;
var
  Bitmap: QBitmapH;
  Mask: QBitmapH;
begin
  if Assigned(FHandle) and FOwnsHandle then
    QCursor_destroy(FHandle);

  Bitmap := QBitmap_create(FBytesPerRow*8,FHeight, @FCustomCursor.Bits[0], False);
  Mask   := QBitmap_create(FBytesPerRow*8,FHeight, @FCustomCursor.Mask[0], False);

  if (FWidth mod 8) <> 0 then
  begin
    QPixmap_resize(Bitmap, FWidth, FHeight);
    QPixmap_resize(Mask, FWidth, FHeight);
  end;

  FHandle := QCursor_create(Bitmap, Mask, FHotspot.X, FHotspot.Y);

  QBitmap_Destroy(Bitmap);
  QBitmap_Destroy(Mask);

  Changed(Self);
end;

procedure TWinCursor.OwnHandle;
begin
  FOwnsHandle := True;
end;

function TWinCursor.ReleaseHandle: QCursorH;
begin
  Result := FHandle;
  FHandle := nil;
  Changed(Self);
end;

procedure TWinCursor.HandleNeeded;
begin
  if FHandle = nil then
  begin
    FHandle := QCursor_create;
    OwnHandle;
  end;
end;

function TWinCursor.GetHotSpot: TPoint;
begin
  Result := Point(0, 0);

  if Assigned(FHandle) then
    QCursor_hotSpot(FHandle, @Result);
end;

procedure TWinCursor.SetHotspot(const Value: TPoint);
var
  TempHandle: QCursorH;
begin
  if Assigned(FHandle) then
  begin
    TempHandle := QCursor_create(QCursor_bitmap(FHandle), QCursor_bitmap(FHandle), Value.X, Value.Y);

    if FOwnsHandle then
      QCursor_destroy(FHandle);

    FHandle := TempHandle;
    OwnHandle;
  end;
end;

procedure TWinCursor.FreeCursor;
begin
  if Assigned(FHandle) then
  begin
    QCursor_destroy(FHandle);
    FHandle := nil;

    FWidth := 0;
    FHeight := 0;
    FBytesPerRow := 0;

    SetLength(FCustomCursor.Bits, 0);
    SetLength(FCustomCursor.Mask, 0);
  end;
end;

procedure TWinCursor.Assign(Source: TPersistent);
begin
  if FOwnsHandle then
    FreeCursor;

  if Source is TWinCursor then
  begin
     FHandle := QCursor_create((Source as TWinCursor).Handle);
     OwnHandle;
  end
  else
    inherited Assign(Source);
end;

procedure TWinCursor.LoadFromMimeSource(MimeSource: TMimeSource);
begin
  raise EInvalidGraphicOperation.Create(RsInvalidOperation);
end;

procedure TWinCursor.SaveToMimeSource(MimeSource: TClxMimeSource);
begin
  raise EInvalidGraphicOperation.Create(RsInvalidOperation);
end;

procedure TWinCursor.SaveToStream(Stream: TStream);
begin
  raise EInvalidGraphicOperation.Create(RsInvalidOperation);
end;

function TWinCursor.GetEmpty: Boolean;
begin
  Result := not Assigned(FHandle);
end;

function TWinCursor.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TWinCursor.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TWinCursor.SetHeight(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(RsInvalidOperation);
end;

procedure TWinCursor.SetWidth(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(RsInvalidOperation);
end;

type
  TCrackBitmap = class(TBitmap);

procedure TWinCursor.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Bitmap: TCrackBitmap;
  Pixmap: QPixmapH;
begin
  if not Empty then
  begin
    Bitmap := TCrackBitmap.Create;
    try
      Bitmap.Width := FWidth;
      Bitmap.Height := FHeight;
      Pixmap := QPixmap_create(QCursor_bitmap(FHandle));
      QPixmap_setMask(Pixmap,QCursor_mask(FHandle));
      Bitmap.Handle := Pixmap;
      Bitmap.Draw(ACanvas, Rect);
    finally
      Bitmap.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Helper functions
//------------------------------------------------------------------------------

{ LoadCursor Helper function                                                   }
{ ==========================                                                   }
{                                                                              }
{ If you are using this function with D6/CLX please be aware that it might     }
{ collide with the Windows LoadCursor function. In such cases reference it     }
{ directly using QWinCursors.LoadCursor                                        }

function LoadCursor(Instance: Cardinal; CursorName: string): QCursorH;
var
  WinCursor: TWinCursor;
begin
  WinCursor := TWinCursor.Create;
  try
    WinCursor.LoadFromResourceName(Instance, CursorName);
    Result := WinCursor.ReleaseHandle;
  finally
    WinCursor.Free;
  end;
end;

function LoadCursorFromFile(CursorFileName: string): QCursorH;
var
  WinCursor: TWinCursor;
begin
  WinCursor := TWinCursor.Create;
  try
    WinCursor.LoadFromFile(CursorFileName);
    Result := WinCursor.ReleaseHandle;
  finally
    WinCursor.Free;
  end;
end;

end.
