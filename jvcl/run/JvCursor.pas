{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCursor.PAS, released on 2004-03-14.

The Initial Developer of the Original Code is Peter Thornqvist
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

Description:
  A TGraphic that can display cursors
-----------------------------------------------------------------------------}
// $Id$

unit JvCursor;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Graphics;

type
  TJvCursorImage = class(TGraphic)
  private
    FHandle: HCURSOR;
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); virtual;
    procedure LoadFromResourceName(Instance: THandle; const ResName: string); virtual;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    property Handle: HCURSOR read FHandle;
  end;

var
  CF_CURSOR: UINT; { Clipboard format for cursor }

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
  SysUtils, Consts,
  JvResources;

function CopyCursor(pcur: HCURSOR): HCURSOR;
begin
 Result := HCURSOR(CopyIcon(HICON(pcur)));
end;

procedure DestroyAndNilCursor(var AHandle: HCURSOR);
begin
  if AHandle <> 0 then
    DestroyCursor(AHandle);
  AHandle := 0;
end;

destructor TJvCursorImage.Destroy;
begin
  DestroyAndNilCursor(FHandle);
  inherited Destroy;
end;

// Cursor are *not* always transparent: it depends on how you draw them ;)

procedure TJvCursorImage.Draw(ACanvas: TCanvas; const Rect: TRect);
const
  cTransparent: array [Boolean] of DWORD = (DI_IMAGE, DI_NORMAL);
begin
  with Rect do
    DrawIconEx(ACanvas.Handle, Left, Top, Handle, Right - Left, Bottom - Top,
      0, 0, cTransparent[Transparent]);
end;

function TJvCursorImage.GetEmpty: Boolean;
begin
  Result := (FHandle = 0);
end;

function TJvCursorImage.GetHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYCURSOR);
end;

function TJvCursorImage.GetWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXCURSOR);
end;

procedure TJvCursorImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
var
  Hnd: HCURSOR;
begin
  if AFormat <> CF_CURSOR then
    raise EInvalidGraphic.CreateRes(@SUnknownClipboardFormat);
  Hnd := CopyCursor(AData);
  if Hnd <> 0 then
  begin
    DestroyAndNilCursor(FHandle);
    FHandle := Hnd;
  end;
end;

procedure TJvCursorImage.LoadFromFile(const FileName: string);
var
  Hnd: HCURSOR;
begin
  Hnd := LoadCursorFromFile(PChar(FileName));
  if Hnd <> 0 then
  begin
    DestroyAndNilCursor(FHandle);
    FHandle := Hnd;
  end;
end;

procedure TJvCursorImage.LoadFromStream(Stream: TStream);
begin
  raise Exception.CreateRes(@RsECursorLoadFromStream);
end;

procedure TJvCursorImage.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Hnd: HCURSOR;
begin
  Hnd := LoadCursor(Instance, PChar(ResID));
  if Hnd <> 0 then
  begin
    DestroyAndNilCursor(FHandle);
    FHandle := Hnd;
  end;
end;

procedure TJvCursorImage.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Hnd: HCURSOR;
begin
  Hnd := LoadCursor(Instance, PChar(ResName));
  if Hnd <> 0 then
  begin
    DestroyAndNilCursor(FHandle);
    FHandle := Hnd;
  end;
end;

procedure TJvCursorImage.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  if Handle <> 0 then
  begin
    AFormat := CF_CURSOR;
    APalette := 0;
    AData := CopyCursor(Handle);
  end;
end;

procedure TJvCursorImage.SaveToStream(Stream: TStream);
begin
  raise Exception.CreateRes(@RsECursorSaveToStream);
end;

procedure TJvCursorImage.SetHeight(Value: Integer);
begin
  // just ignore
end;

procedure TJvCursorImage.SetWidth(Value: Integer);
begin
  // just ignore
end;

procedure TJvCursorImage.Assign(Source: TPersistent);
begin
  if Source = nil then
    DestroyAndNilCursor(FHandle)
  else
  if Source is TJvCursorImage then
  begin
    DestroyAndNilCursor(FHandle);
    if TJvCursorImage(Source).Handle <> 0 then
      FHandle := CopyImage(TJvCursorImage(Source).Handle, IMAGE_CURSOR, Width, Height, 0);
  end
  else
    inherited Assign(Source);
end;

procedure TJvCursorImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TIcon then
  begin
    TIcon(Dest).ReleaseHandle;
    if Handle <> 0 then
      TIcon(Dest).Handle := CopyImage(Handle, IMAGE_CURSOR, Width, Height, 0);
  end
  else
  if Dest is TBitmap then
    with TBitmap(Dest) do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Transparent := Self.Transparent;
      Draw(Canvas, Rect(0, 0, Width, Height));
    end
  else
    inherited AssignTo(Dest);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterClass(TJvCursorImage);
  CF_CURSOR := RegisterClipboardFormat(PChar(RsCursor));
  TPicture.RegisterFileFormat(RsCurExtension, RsCurDescription, TJvCursorImage);
  TPicture.RegisterClipboardFormat(CF_CURSOR, TJvCursorImage);

finalization
  TPicture.UnregisterGraphicClass(TJvCursorImage);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.