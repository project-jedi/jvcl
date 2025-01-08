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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{
Documentation:
*************

WHAT IT IS:
   This component is a TDBImage replacement that supports other image
   formats than bitmap, a limitation of TDBImage since D1.

IMAGE FORMATS:
   See JvGraphics.pas for details

SUPPORT FOR TDBCtrlGrid:
   You can safely put an TJvDBImage in TDBCtrlGrid.
}

unit JvDBImage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls,
  Clipbrd, DB, DBCtrls, Forms,
  JvJVCLUtils;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBImage = class(TDBImage)
  private
    FAutoDisplay: Boolean;
    FDataLink: TFieldDataLink;
    FOldPictureChange: TNotifyEvent;
    FPictureLoaded: Boolean;
    FProportional: Boolean;
    FOnGetGraphicClass: TJvGetGraphicClassEvent;
    FTransparent: Boolean;
    FShowNameIfEmpty: Boolean;
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure DataChange(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure SetTransparent(const Value: Boolean);
    procedure SetShowNameIfEmpty(const Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure CheckFieldType;
    procedure AssignGraphicTo(Picture: TPicture);
    function DestRect(W, H, CW, CH: Integer): TRect;
    procedure Paint; override;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure KeyPress(var Key: Char); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadPicture;
    procedure PasteFromClipboard;
  published
    property AutoSize;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property ShowNameIfEmpty: Boolean read FShowNameIfEmpty write SetShowNameIfEmpty default True;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property OnGetGraphicClass: TJvGetGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  end;

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
  DBConsts, SysUtils,
  JvConsts;

//=== { TJvDBImage } =========================================================

constructor TJvDBImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // we cannot use the inherited AutoDisplay - it raises an "Invalid Bitmap" if
  // the first record in a table is an image type not supported by TDBImage
  inherited AutoDisplay := False;
  FAutoDisplay := True;
  FOldPictureChange := Picture.OnChange;
  Picture.OnChange := PictureChanged;
  FShowNameIfEmpty := True;
end;

procedure TJvDBImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    Invalidate;
  end;
end;

procedure TJvDBImage.SetShowNameIfEmpty(const Value: Boolean);
begin
  if FShowNameIfEmpty <> Value then
  begin
    FShowNameIfEmpty := Value;
    Invalidate;
  end;
end;

procedure TJvDBImage.CheckFieldType;
begin
  if Field = nil then
    Exit;
  with Field do
    if not IsBlob then
      DatabaseErrorFmt(SFieldTypeMismatch, [DisplayName, FieldTypeNames[ftBlob], FieldTypeNames[DataType]]);
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

procedure TJvDBImage.AssignGraphicTo(Picture: TPicture);
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  // If nil field or null field just exit
  if (Field = nil) or Field.IsNull then
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
      Graphic := GraphicClass.Create;
      try
        Stream.Position := 0;
        Graphic.LoadFromStream(Stream);
        Picture.Graphic := Graphic;
      finally
        Graphic.Free;
      end;
    end
    else // try the old fashioned way
      Picture.Assign(Field);
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

  Result := Rect(0, 0, W, H);
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
          AssignGraphicTo(DrawPict);
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
    else if ShowNameIfEmpty then
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
  if not FPictureLoaded then
  try
    AssignGraphicTo(Picture);
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

procedure TJvDBImage.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
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
    with Msg do
      if (Width > 32768) or (Height > 32768) then
        with ControlCursorPos(Self) do
          MouseDown(mbLeft, KeysToShiftState(Keys), X, Y)
      else
        MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  LoadPicture;
end;

procedure TJvDBImage.KeyPress(var Key: Char);
begin
  case Key of
    CtrlC:
      CopyToClipboard;
    CtrlV:
      PasteFromClipboard;
    CtrlX:
      CutToClipboard;
    Cr:
      LoadPicture;
    Esc:
      if FDataLink <> nil then
        FDataLink.Reset;
  else // this should be safe, TDBImage doesn't handle any other keys
    inherited KeyPress(Key);
  end;
end;

procedure TJvDBImage.WMPaste(var Msg: TWMPaste);
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
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  { registration happens in GraphicSignatures Needed() }

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
