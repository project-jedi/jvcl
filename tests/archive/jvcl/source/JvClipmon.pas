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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}



unit JvClipMon;

interface


uses Messages, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  SysUtils, Classes{, JvComponent};

type
  TJvClipboardMonitor = class(TComponent)
  private
    FWindowHandle: HWnd;
    FNextWindow: HWnd;
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
procedure LoadClipboardFromStream(Format: Word; Stream: TStream; Size: Longint);

implementation

uses Forms, Clipbrd;

{ Stream routines }

procedure SaveClipboardToStream(Format: Word; Stream: TStream);
var
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    Buffer := GlobalLock(Data);
    try
      Stream.Write(Buffer^, GlobalSize(Data));
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure LoadClipboardFromStream(Format: Word; Stream: TStream; Size: Longint);
var
  Len: Longint;
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Len := Stream.Size - Stream.Position;
    if Len > Size then Len := Size;
    Data := GlobalAlloc(HeapAllocFlags, Len);
    try
      if Data <> 0 then begin
        Buffer := GlobalLock(Data);
        try
          Stream.Read(Buffer^, Len);
          SetClipboardData(Format, Data);
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

{ TJvClipboardMonitor }

constructor TJvClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowHandle := {$IFDEF Delphi6_Up}Classes.{$ENDIF}AllocateHWnd(WndProc);
  SetEnabled(True);
end;

destructor TJvClipboardMonitor.Destroy;
begin
  FOnChange := nil;
  SetEnabled(False);
  {$IFDEF Delphi6_Up}Classes.{$ENDIF}DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

procedure TJvClipboardMonitor.ForwardMessage(var Msg: TMessage);
begin
  if FNextWindow <> 0 then
    with Msg do SendMessage(FNextWindow, Msg, WParam, LParam);
end;

procedure TJvClipboardMonitor.WndProc(var AMsg: TMessage);
begin
  with AMsg do begin
    Result := 0;
    case Msg of
      WM_DESTROYCLIPBOARD:
        ClipboardChanged;
      WM_CHANGECBCHAIN:
        if HWnd(WParam) = FNextWindow then FNextWindow := HWnd(LParam)
        else ForwardMessage(AMsg);
      WM_DRAWCLIPBOARD:
        begin
          ForwardMessage(AMsg);
          ClipboardChanged;
        end;
      WM_DESTROY:
        SetEnabled(False);
      else Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
    end;
  end;
end;

procedure TJvClipboardMonitor.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    if Value then begin
      FNextWindow := SetClipboardViewer(FWindowHandle);
      FEnabled := True;
    end
    else begin
      ChangeClipboardChain(FWindowHandle, FNextWindow);
      FEnabled := False;
      FNextWindow := 0;
    end;
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
  if Assigned(FOnChange) then FOnChange(Self);
end;

end.
