{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBImage.PAS, released on 2004-04-09.

The Initial Developers of the Original Code is
Sergio Samayoa <sergiosamayoa att icon dott com dott gt> and Peter Thornqvist <peter att users dott sourceforge dott net>
Portions created by Sergio Samayoa are Copyright (C) 2004 Sergio Samayoa.
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.

All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

{
Documentation:
*************

WHAT IT IS:
   This component is a TDBImage replacement that supports other image
   formats than bitmap, a limitation of TDBImage since D1.

IMAGE FORMATS:
   The implementation is simple: Just register image signatures with
   RegisterGraphicSignature procedure and the component takes care
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
     RegisterGraphicSignature([0, 1], 0, TMetaFile);     // EMF
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

   If the component can't find the graphic class and the user doesn't provide it
   in the OnGetGraphicClass event no graphic object is created, the default
   behavior is used (Picture.Assign(Field)). This might raise an exception
   ('Bitmap image is not valid').

   The graphic class to be used must implement LoadFromStream and SaveToStream
   methods in order to work properly.

SUPPORT FOR TDBCtrlGrid:
   You can safely put an TJvDBImage in TDBCtrlGrid.
}

unit JvDBImage;

interface

uses
  Windows, Classes, Clipbrd, Controls, DB,
  DBCtrls, Forms, Graphics, Messages;

type
  TJvGetGraphicClassEvent = procedure(Sender: TObject; Stream: TMemoryStream;
    var GraphicClass: TGraphicClass) of object;

  TJvDBImage = class(TDBImage)
  private
    FAutoDisplay: Boolean;
    FDataLink: TFieldDataLink;
    FOldPictureChange: TNotifyEvent;
    FPictureLoaded: Boolean;
    FProportional: Boolean;
    FOnGetGraphicClass: TJvGetGraphicClassEvent;
    FTransparent: Boolean;
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure DataChange(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure CheckFieldType;
    function CreateGraphic: TGraphic;
    function DestRect(W, H, CW, CH: Integer): TRect;
    procedure Paint; override;

    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure KeyPress(var Key: Char); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadPicture;
    procedure PasteFromClipboard;
  published
    property AutoSize;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnGetGraphicClass: TJvGetGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  end;

procedure RegisterGraphicSignature(const ASignature: string; AOffset: Integer;
  AGraphicClass: TGraphicClass); overload;
procedure RegisterGraphicSignature(const ASignature: array of Byte;
  AOffset: Integer; AGraphicClass: TGraphicClass); overload;

procedure UnregisterGraphicSignature(AGraphicClass: TGraphicClass); overload;
procedure UnregisterGraphicSignature(const ASignature: string; AOffset: Integer); overload;
procedure UnregisterGraphicSignature(const ASignature: array of Byte;
  AOffset: Integer); overload;

function GetGraphicClass(Stream: TStream): TGraphicClass;

implementation

uses
  Contnrs,
  DBConsts, jpeg, SysUtils,
  JvConsts, JvResources, JvFinalize;

const
  sUnitName = 'JvDBImage';

//=== TGraphicSignature ======================================================

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
    RegisterGraphicSignature([0, 1], 0, TMetafile); // EMF
    RegisterGraphicSignature('JFIF', 6, TJPEGImage);
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
    raise Exception.CreateRes(@RsEBadGraphicSignature);
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
  begin
    for I := GraphicSignatures.Count - 1 downto 0 do
      if TGraphicSignature(GraphicSignatures[I]).GraphicClass = AGraphicClass then
        GraphicSignatures.Delete(I);
  end;
end;

procedure UnregisterGraphicSignature(const ASignature: string; AOffset: Integer);
var
  I: Integer;
begin
  if Assigned(GraphicSignatures) then
  begin
    for I := GraphicSignatures.Count - 1 downto 0 do
      with TGraphicSignature(GraphicSignatures[I]) do
        if (Signature = ASignature) and (Offset = AOffset) then
          GraphicSignatures.Delete(I);
  end;
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
  I: Integer;
  S: TGraphicSignature;
begin
  Result := nil;
  if Assigned(GraphicSignatures) then
  begin
    for I := 0 to GraphicSignatures.Count - 1 do
    begin
      S := TGraphicSignature(GraphicSignatures[I]);
      if S.IsThisSignature(Stream) then
      begin
        Result := S.GraphicClass;
        Exit;
      end;
    end;
  end;
end;

//=== TJvDBImage =============================================================

constructor TJvDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // we cannot use the inherited AutoDisplay - it raises an "Invalid Bitmap" if
  // the first record in a table is an image type not supported by TDBImage
  inherited AutoDisplay := False;
  FAutoDisplay := True;
  FOldPictureChange := Picture.OnChange;
  Picture.OnChange := PictureChanged;
end;

procedure TJvDBImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    Invalidate;
  end;
end;

procedure TJvDBImage.CheckFieldType;
begin
  if Field = nil then
    Exit;
  with Field do
    if not IsBlob then
      DatabaseErrorFmt(SFieldTypeMismatch,
        [DisplayName, FieldTypeNames[ftBlob], FieldTypeNames[DataType]]);
end;

procedure TJvDBImage.CreateHandle;
begin
  inherited CreateHandle;
  if FDataLink = nil then
  begin
    // (p3) get a pointer to the datalink (it is private in TDBImage):
    FDataLink := TFieldDataLink(SendMessage(Handle, CM_GETDATALINK, 0, 0));
    if FDataLink <> nil then
    begin
      FDataLink.OnDataChange := DataChange;
      FDataLink.OnUpdateData := UpdateData;
      // (p3) it is now safe to call LoadPicture because we have control over the datalink:
      if FAutoDisplay then
        LoadPicture
      else
        Invalidate;
    end;
  end;
end;

function TJvDBImage.CreateGraphic: TGraphic;
var
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Result := nil;

  // If nil field or null field just exit
  if (Field = nil) or (Field.IsNull) then
    Exit;

  CheckFieldType;

  GraphicClass := nil;
  Stream := TMemoryStream.Create;
  try
    // Move blob data to Stream
    TBlobField(Field).SaveToStream(Stream);
    // Figure out which Graphic class is...
    GraphicClass := GetGraphicClass(Stream);
    // Call user event
    if Assigned(FOnGetGraphicClass) then
      FOnGetGraphicClass(Self, Stream, GraphicClass);
    // If we got one, load it..
    if GraphicClass <> nil then
    begin
      Result := GraphicClass.Create;
      try
        Stream.Position := 0;
        Result.LoadFromStream(Stream);
      except
        Result.Free;
        raise;
      end;
    end
    else // try the old fashioned way
    begin
      Picture.Assign(Field);
      Result := Picture.Graphic;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TJvDBImage.PictureChanged(Sender: TObject);
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
    SetBounds(Left, Top, Picture.Width, Picture.Height);
  FOldPictureChange(Sender);
  FPictureLoaded := Picture.Graphic <> nil;
end;

procedure TJvDBImage.DataChange(Sender: TObject);
begin
  Picture.Graphic := nil;
  FPictureLoaded := False;
  if FAutoDisplay then
    LoadPicture;
end;

function TJvDBImage.DestRect(W, H, CW, CH: Integer): TRect;
var
  XYAspect: Double;
begin
  if AutoSize then
  begin
    Result := ClientRect;
    Exit;
  end;
  if Stretch or (Proportional and ((W > CW) or (H > CH))) then
  begin
    if Proportional and (W > 0) and (H > 0) then
    begin
      XYAspect := W / H;
      if W > H then
      begin
        W := CW;
        H := Trunc(CW / XYAspect);
        if H > CH then // woops, too big
        begin
          H := CH;
          W := Trunc(CH * XYAspect);
        end;
      end
      else
      begin
        H := CH;
        W := Trunc(CH * XYAspect);
        if W > CW then // woops, too big
        begin
          W := CW;
          H := Trunc(CW / XYAspect);
        end;
      end;
    end
    else
    begin
      W := CW;
      H := CH;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := W;
    Bottom := H;
  end;

  if Center then
    OffsetRect(Result, (CW - W) div 2, (CH - H) div 2);
end;

procedure TJvDBImage.Paint;
var
  Size: TSize;
  R: TRect;
  S: string;
  DrawPict: TPicture;
  Form: TCustomForm;
  Pal: HPalette;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    if FPictureLoaded or (csPaintCopy in ControlState) and Assigned(FDataLink) then
    begin
      DrawPict := TPicture.Create;
      Pal := 0;
      try
        if (csPaintCopy in ControlState) and Assigned(FDataLink.Field) and
          FDataLink.Field.IsBlob then
        begin
          DrawPict.Graphic := CreateGraphic;
          if DrawPict.Graphic is TBitmap then
            DrawPict.Bitmap.IgnorePalette := QuickDraw;
        end
        else
        begin
          DrawPict.Assign(Picture);
          if Focused and (DrawPict.Graphic <> nil) and
            (DrawPict.Graphic.Palette <> 0) then
          begin
            Pal := SelectPalette(Handle, DrawPict.Graphic.Palette, False);
            RealizePalette(Handle);
          end;
        end;
        FillRect(ClientRect); // (p3) always fill or the text might be visible through the control
        if (DrawPict.Graphic <> nil) and not DrawPict.Graphic.Empty then
        begin
          DrawPict.Graphic.Transparent := Self.Transparent;
          // (p3) DestRect adjusts the rect according to the values of Stretch, Center and Proportional
          R := DestRect(DrawPict.Width, DrawPict.Height, Self.Width, Self.Height);
          StretchDraw(R, DrawPict.Graphic);
          ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
          FillRect(ClientRect);
          SelectClipRgn(Handle, 0);
        end;
      finally
        if Pal <> 0 then
          SelectPalette(Handle, Pal, True);
        DrawPict.Free;
      end;
    end
    else
    begin
      Font := Self.Font;
      if (FDataLink <> nil) and (FDataLink.Field <> nil) then
        S := FDataLink.Field.DisplayLabel
      else
        S := Name;
      if S = '' then
        S := Self.ClassName;
      S := '(' + S + ')';
      Size := TextExtent(S);
      R := ClientRect;
      TextRect(R, (R.Right - Size.cx) div 2, (R.Bottom - Size.cy) div 2, S);
    end;
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl = Self) and not
      (csDesigning in ComponentState) and not (csPaintCopy in ControlState) then
    begin
      Brush.Color := clWindowFrame;
      FrameRect(ClientRect);
    end;
  end;
end;

procedure TJvDBImage.LoadPicture;
begin
  try
    Picture.Graphic := CreateGraphic;
  except
    Picture.Graphic := nil;
    raise;
  end;
end;

procedure TJvDBImage.UpdateData(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  CheckFieldType;

  // If there is no graphic just clear field and exit
  if Picture.Graphic = nil then
  begin
    Field.Clear;
    Exit;
  end;

  Stream := TMemoryStream.Create;
  try
    Picture.Graphic.SaveToStream(Stream);
    Stream.Position := 0;
    TBlobField(Field).LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvDBImage.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadPicture;
  end;
end;

procedure TJvDBImage.PasteFromClipboard;
begin
  if FDataLink.Edit then
  begin
    if Clipboard.HasFormat(CF_BITMAP) then
      Picture.Bitmap.Assign(Clipboard)
    else
    if Clipboard.HasFormat(CF_METAFILEPICT) or
      Clipboard.HasFormat(CF_ENHMETAFILE) then
      Picture.Metafile.Assign(Clipboard)
    else
    if Clipboard.HasFormat(CF_PICTURE) then
      Picture.Assign(Clipboard);
  end;
end;

function ControlCursorPos(Control: TControl): TPoint;
begin
  GetCursorPos(Result);
  Result := Control.ScreenToClient(Result);
end;

procedure TJvDBImage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  // we can't call inherited because TDBImage loads the image there as well
  // and will get mighty upset if it's not a BMP, so we have to redo the
  // code in TControl as closely as we can
  SendCancelMode(Self);
  // inherited;
  if csCaptureMouse in ControlStyle then
    MouseCapture := True;
  if csClickEvents in ControlStyle then
    DblClick;
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with ControlCursorPos(Self) do
          MouseDown(mbLeft, KeysToShiftState(Keys), X, Y)
      else
        MouseDown(mbLeft, KeysToShiftState(Keys), Message.XPos, Message.YPos);
  LoadPicture;
end;

procedure TJvDBImage.KeyPress(var Key: Char);
begin
  case Key of
    CtrlC:
      CopyToClipBoard;
    CtrlV:
      PasteFromClipBoard;
    CtrlX:
      CutToClipBoard;
    Cr:
      LoadPicture;
    Esc:
      if FDataLink <> nil then
        FDataLink.Reset;
  else // this should be safe, TDBImage doesn't handle any other keys
    inherited KeyPress(Key);
  end;
end;

procedure TJvDBImage.WMPaste(var Message: TWMPaste);
begin
  PasteFromClipboard;
end;

procedure TJvDBImage.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

function TJvDBImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width + Ord(BorderStyle = bsSingle) * 4;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height + Ord(BorderStyle = bsSingle) * 4;
  end;
end;

initialization
  { registration happens in GraphicSignatures Needed() }
  
finalization
  FinalizeUnit(sUnitName);

end.

