{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGraphics.PAS, released on 2004-04-09.

The Initial Developers of the Original Code is
Sergio Samayoa <sergiosamayoa att icon dott com dott gt> and Peter Thornqvist <peter att users dott sourceforge dott net>
Portions created by Sergio Samayoa are Copyright (C) 2004 Sergio Samayoa.
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.

All Rights Reserved.

Contributor(s):
       ZioNemo  (rework to be used both in JvImage and JvDBImage)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{
Documentation:
*************

WHAT IT IS:
   This unit contains helper functions to register graphic formats than can
   later be recognized from a stream, thus allowing to rely on the actual
   content of a file rather than from its filename extension.
   This is used in TJvDBImage and TJvImage.

IMAGE FORMATS:
   The implementation is simple: Just register image signatures with
   RegisterGraphicSignature procedure and the methods takes care
   of the correct instantiation of the TGraphic object. The signatures
   register at unit's initialization are: BMP, WMF, EMF, ICO, JPG.
   If you got some other image library (such as GIF, PCX, TIFF, ANI or PNG),
   just register the signature:

     RegisterGraphicSignature(<string value>, <offset>, <class>)

     or

     RegisterGraphicSignature([<byte values>], <offset>, <class>)

   This means:
     When <string value> (or byte values) found at <offset> the graphic
     class to use is <class>

   For example (actual code of the initialization section):

     RegisterGraphicSignature([$D7, $CD], 0, TMetaFile); // WMF
     RegisterGraphicSignature([1, 0], 0, TMetaFile);     // EMF
     RegisterGraphicSignature('JFIF', 6, TJPEGImage);

   You can also unregister signature. IF you want use TGIFImage instead of
   TJvGIFImage, you can unregister with:

     UnregisterGraphicSignature('GIF', 0);

     or just

     UnregisterGraphicSignature(TJvGIFImage); // must add JvGIF unit in uses clause

   then:
     RegisterGraphicSignature('GIF', 0, TGIFImage); // must add GIFImage to uses clause

   If you dont like the signature registration there is a new event called
   OnGetGraphicClass. The event gets the following parameters:

    Sender: TObject;
    Stream: TMemoryStream;
    var GraphicClass: TGraphicClass)

   The memory stream containing the blob data is sent in Stream to allow the user
   to inspect the contents and figure out which graphic class is.

   The graphic class to be used must implement LoadFromStream and SaveToStream
   methods in order to work properly.
}

unit JvGraphics;

interface

uses
  Classes, Graphics, Contnrs;

type
  TJvGetGraphicClassEvent = procedure(Sender: TObject; Stream: TMemoryStream;
    var GraphicClass: TGraphicClass) of object;

procedure RegisterGraphicSignature(const ASignature: string; AOffset: Integer;
  AGraphicClass: TGraphicClass); overload;
procedure RegisterGraphicSignature(const ASignature: array of Byte;
  AOffset: Integer; AGraphicClass: TGraphicClass); overload;

procedure UnregisterGraphicSignature(AGraphicClass: TGraphicClass); overload;
procedure UnregisterGraphicSignature(const ASignature: string; AOffset: Integer); overload;
procedure UnregisterGraphicSignature(const ASignature: array of Byte;
  AOffset: Integer); overload;

function GetGraphicClass(Stream: TStream): TGraphicClass;
function GetGraphicObject(aStream: TStream): TGraphic; overload;
function GetGraphicObject(aStream: TStream; aSender: TObject; aOnProc: TJvGetGraphicClassEvent): TGraphic; overload;

implementation

uses
 {JvGIF,
 PNGImage,}
 jpeg,
// GraphicEx,
 SysUtils;

//=== { TGraphicSignature } ==================================================

// Code to manage graphic's signatures.
type
  TGraphicSignature = class(TObject)
  public
    Signature: string;
    Offset: Integer;
    GraphicClass: TGraphicClass;
    constructor Create(const ASignature: string; AOffset: Integer;
      AGraphicClass: TGraphicClass);
    function IsThisSignature(Stream: TStream): Boolean;
  end;

constructor TGraphicSignature.Create(const ASignature: string; AOffset: Integer;
  AGraphicClass: TGraphicClass);
begin
  inherited Create;
  Signature := ASignature;
  Offset := AOffset;
  GraphicClass := AGraphicClass;
end;

function TGraphicSignature.IsThisSignature(Stream: TStream): Boolean;
var
  Buffer: string;
  Count: Integer;
  BytesRead: Integer;
begin
  Result := False;
  try
    Count := Length(Signature);
    SetLength(Buffer, Count);
    Stream.Position := Offset;
    BytesRead := Stream.Read(Buffer[1], Count);
    Result := (BytesRead = Count) and (Buffer = Signature);
  except
    // Ignore any error...
  end;
end;

var
  GraphicSignatures: TObjectList = nil;

procedure GraphicSignaturesNeeded;
begin
  if not Assigned(GraphicSignatures) then
  begin
    GraphicSignatures := TObjectList.Create;

    RegisterGraphicSignature('BM', 0, TBitmap);
    RegisterGraphicSignature([0, 0, 1, 0], 0, TIcon);
    RegisterGraphicSignature([$D7, $CD], 0, TMetafile); // WMF
    RegisterGraphicSignature([1, 0], 0, TMetafile); // EMF
    RegisterGraphicSignature('JFIF', 6, TJPEGImage);
    RegisterGraphicSignature('Exif', 6 , TJPEGImage);
    // NB! Registering these will add a requirement on having the JvMM package installed
    // Let users register these manually
    // RegisterGraphicSignature([$0A], 0, TJvPcx);
    // RegisterGraphicSignature('ACON', 8, TJvAni);
    // JvCursorImage cannot be registered because it doesn't support
    // LoadFromStream/SaveToStream but here's the signature for future reference:
    // RegisterGraphicSignature([0, 0, 2, 0], 0, TJvCursorImage);
    {$IFDEF USE_JV_GIF}
    // RegisterGraphicSignature('GIF', 0, TJvGIFImage);
    {$ENDIF USE_JV_GIF}
//    RegisterGraphicSignature('GIF', 0, TGIFGraphic);
//    RegisterGraphicSignature('PNG', 1, TPNGGraphic);
  end;
end;

procedure RegisterGraphicSignature(const ASignature: string; AOffset: Integer;
  AGraphicClass: TGraphicClass);
var
  GraphicSignature: TGraphicSignature;
begin
  GraphicSignaturesNeeded;
  // Avoid bad signatures
  if (ASignature = '') or (AOffset < 0) or (AGraphicClass = nil) then
    raise Exception.Create('Error: Bad Graphic Signature');
  // Should raise an exception if empty signature, negative offset or null class.
  GraphicSignature := TGraphicSignature.Create(ASignature, AOffset, AGraphicClass);
  try
    GraphicSignatures.Add(GraphicSignature)
  except
    GraphicSignature.Free;
  end;
end;

procedure RegisterGraphicSignature(const ASignature: array of Byte;
  AOffset: Integer; AGraphicClass: TGraphicClass);
var
  Signature: string;
  I: Integer;
begin
  SetLength(Signature, Length(ASignature));
  for I := Low(ASignature) to High(ASignature) do
    Signature[I + 1] := Char(ASignature[I]);
  RegisterGraphicSignature(Signature, AOffset, AGraphicClass);
end;

procedure UnregisterGraphicSignature(AGraphicClass: TGraphicClass); overload;
var
  I: Integer;
begin
  if Assigned(GraphicSignatures) then
    for I := GraphicSignatures.Count - 1 downto 0 do
      if TGraphicSignature(GraphicSignatures[I]).GraphicClass = AGraphicClass then
        GraphicSignatures.Delete(I);
end;

procedure UnregisterGraphicSignature(const ASignature: string; AOffset: Integer);
var
  I: Integer;
begin
  if Assigned(GraphicSignatures) then
    for I := GraphicSignatures.Count - 1 downto 0 do
      with TGraphicSignature(GraphicSignatures[I]) do
        if (Signature = ASignature) and (Offset = AOffset) then
          GraphicSignatures.Delete(I);
end;

procedure UnregisterGraphicSignature(const ASignature: array of Byte; AOffset: Integer);
var
  Signature: string;
  I: Integer;
begin
  SetLength(Signature, Length(ASignature));
  for I := Low(ASignature) to High(ASignature) do
    Signature[I + 1] := Char(ASignature[I]);
  UnregisterGraphicSignature(Signature, AOffset);
end;

function GetGraphicClass(Stream: TStream): TGraphicClass;
var
  P: Integer;
  I: Integer;
  S: TGraphicSignature;
begin
  Result := nil;
  GraphicSignaturesNeeded;
  if Assigned(GraphicSignatures) then
  begin
    P := Stream.Position;
    try
      for I := 0 to GraphicSignatures.Count - 1 do
      begin
        S := TGraphicSignature(GraphicSignatures[I]);
        if S.IsThisSignature(Stream) then
        begin
          Result := S.GraphicClass;
          Exit;
        end;
      end;
    finally
      Stream.Position := P;
    end;
  end;
end;

function GetGraphicObject(aStream: TStream): TGraphic;
var
  lOnProc: TJvGetGraphicClassEvent;
begin
  lOnProc := nil;
  Result := GetGraphicObject(aStream, nil, lOnProc)
end;

function GetGraphicObject(aStream: TStream; aSender: TObject; aOnProc: TJvGetGraphicClassEvent): TGraphic; overload;
var
  GraphicClass: TGraphicClass;
begin
  // Figure out which Graphic class is...
  GraphicClass := GetGraphicClass(aStream);
  // Call user event
  if Assigned(aOnProc) and (aStream is TMemoryStream) then
    aOnProc(aSender, aStream as TMemoryStream, GraphicClass);
  // If we got one, load it..
  if Assigned(GraphicClass) then
  begin
    Result := GraphicClass.Create;
    Result.LoadFromStream(aStream);
  end
  else // nope.
    Result := nil;
end;

initialization
  { registration happens in GraphicSignatures Needed() }

finalization
  FreeAndNil(GraphicSignatures);

end.

