{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClipMon.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvClipboardMonitor;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages,
  {$IFDEF VCL}
  Clipbrd,
  {$ENDIF VCL}
  Classes,
  JvComponent;

type
  TJvClipboardMonitor = class(TJvComponent)
  private
    FWindowHandle: HWND;
    FNextWindow: HWND;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure ForwardMessage(var Msg: TMessage);
    procedure SetEnabled(Value: Boolean);
    procedure WndProc(var AMsg: TMessage);
    procedure ClipboardChanged;
  protected
    procedure Change; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure SaveClipboardToStream(Format: Word; Stream: TStream);
function LoadClipboardFromStream(Stream: TStream): Word;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms, JvJVCLUtils;

procedure SaveClipboardToStream(Format: Word; Stream: TStream);
var
  Buffer: Pointer;
  Data: THandle;
  Size: Longint;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then
      Exit;
    Buffer := GlobalLock(Data);
    try
      // (rom) added handling of Format and Size!
      Size := GlobalSize(Data);
      Stream.Write(Format, SizeOf(Word));
      Stream.Write(Size, SizeOf(Longint));
      Stream.Write(Buffer^, Size);
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

function LoadClipboardFromStream(Stream: TStream): Word;
var
  Size: Longint;
  Buffer: Pointer;
  Data: THandle;
begin
  Result := 0;
  Clipboard.Open;
  try
    // (rom) added handling of Format and Size!
    if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word) then
      Exit;
    if Stream.Read(Size, SizeOf(Longint)) <> SizeOf(Longint) then
      Exit;
    Data := GlobalAlloc(HeapAllocFlags, Size);
    try
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          if Stream.Read(Buffer^, Size) <> Size then
            Exit;
          SetClipboardData(Result, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

constructor TJvClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowHandle := AllocateHWndEx(WndProc);
  Enabled := True;
end;

destructor TJvClipboardMonitor.Destroy;
begin
  FOnChange := nil;
  Enabled := False;
  DeallocateHWndEx(FWindowHandle);
  inherited Destroy;
end;

procedure TJvClipboardMonitor.ForwardMessage(var Msg: TMessage);
begin
  if FNextWindow <> 0 then
    with Msg do
      SendMessage(FNextWindow, Msg, WParam, LParam);
end;

procedure TJvClipboardMonitor.WndProc(var AMsg: TMessage);
begin
  with AMsg do
  begin
    Result := 0;
    case Msg of
      WM_DESTROYCLIPBOARD:
        ClipboardChanged;
      WM_CHANGECBCHAIN:
        if HWND(WParam) = FNextWindow then
          FNextWindow := HWND(LParam)
        else
          ForwardMessage(AMsg);
      WM_DRAWCLIPBOARD:
        begin
          ForwardMessage(AMsg);
          ClipboardChanged;
        end;
      WM_DESTROY:
        Enabled := False;
    else
      Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
    end;
  end;
end;

procedure TJvClipboardMonitor.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if Value then
      FNextWindow := SetClipboardViewer(FWindowHandle)
    else
    begin
      ChangeClipboardChain(FWindowHandle, FNextWindow);
      FNextWindow := 0;
    end;
    FEnabled := Value;
  end;
end;

procedure TJvClipboardMonitor.ClipboardChanged;
begin
  try
    Change;
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvClipboardMonitor.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
