{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingClip.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
unit JvDesignClip;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes;

type
  TJvDesignComponentClipboard = class
  protected
    Stream: TMemoryStream;
    procedure Close;
    procedure Open;
    procedure ReadError(Reader: TReader; const Message: string;
      var Handled: Boolean);
  public
    function GetComponent: TComponent;
    procedure CloseRead;
    procedure CloseWrite;
    procedure OpenRead;
    procedure OpenWrite;
    procedure SetComponent(inComponent: TComponent);
  end;

function DesignLoadComponentFromBinaryStream(inStream: TStream;
  inComp: TComponent; inOnError: TReaderError): TComponent;
procedure DesignSaveComponentToBinaryStream(inStream: TStream;
  inComp: TComponent);
procedure DesignCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);
procedure DesignCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Clipbrd, JvDesignUtils;

var
  CF_COMPONENTSTREAM: Cardinal;

procedure DesignSaveComponentToBinaryStream(inStream: TStream;
  inComp: TComponent);
var
  ms: TMemoryStream;
  sz: Int64;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(inComp);
    ms.Position := 0;
    sz := ms.Size;
    inStream.Write(sz, 8);
    inStream.CopyFrom(ms, sz);
  finally
    ms.Free;
  end;
end;

function DesignLoadComponentFromBinaryStream(inStream: TStream;
  inComp: TComponent; inOnError: TReaderError): TComponent;
var
  ms: TMemoryStream;
  sz: Int64;
begin
  inStream.Read(sz, 8);
  ms := TMemoryStream.Create;
  try
    ms.CopyFrom(inStream, sz);
    ms.Position := 0;
    with TReader.Create(ms, 4096) do
    try
      OnError := inOnError;
      Result := ReadRootComponent(inComp);
    finally
      Free;
    end;
  finally
    ms.Free;
  end;
end;

procedure DesignCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  inS.Position := 0;
  hMem := GlobalAlloc(GHND or GMEM_DDESHARE, inS.Size);
  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      inS.Read(pMem^, inS.Size);
      inS.Position := 0;
      GlobalUnlock(hMem);
      Clipboard.Open;
      try
        Clipboard.SetAsHandle(inFmt, hMem);
      finally
        Clipboard.Close;
      end;
    end
    else begin
      GlobalFree(hMem);
      OutOfMemoryError;
    end;
  end else
    OutOfMemoryError;
end;

procedure DesignCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  hMem := Clipboard.GetAsHandle(inFmt);
  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      inS.Write(pMem^, GlobalSize(hMem));
      inS.Position := 0;
      GlobalUnlock(hMem);
    end;
  end;
end;

{ TJvDesignComponentClipboard }

procedure TJvDesignComponentClipboard.Close;
begin
  Stream.Free;
  Clipboard.Close;
end;

procedure TJvDesignComponentClipboard.CloseRead;
begin
  Close;
end;

procedure TJvDesignComponentClipboard.CloseWrite;
begin
  DesignCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
  Close;
end;

function TJvDesignComponentClipboard.GetComponent: TComponent;
begin
  if Stream.Position < Stream.Size then
    Result := DesignLoadComponentFromBinaryStream(Stream, nil, ReadError)
  else
    Result := nil;
end;

procedure TJvDesignComponentClipboard.Open;
begin
  Clipboard.Open;
  Stream := TMemoryStream.Create;
end;

procedure TJvDesignComponentClipboard.OpenRead;
begin
  Open;
  DesignCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
end;

procedure TJvDesignComponentClipboard.OpenWrite;
begin
  Open;
end;

procedure TJvDesignComponentClipboard.ReadError(Reader: TReader;
  const Message: string; var Handled: Boolean);
begin
  Handled := true;
end;

procedure TJvDesignComponentClipboard.SetComponent(inComponent: TComponent);
begin
  DesignSaveComponentToBinaryStream(Stream, inComponent);
end;

initialization
  { The following string should not be localized }
  CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

