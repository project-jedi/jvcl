{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbImage.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer@Excite.com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Changes form the previous Version:

Converted the rotation Functions to use scanlines for faster results
  I have converted the movement from an array of TRGBTriple to an
  an array of bytes. Right now it must rotate the following formats
  without big speed differences and problems pf8bit,pf24bit,pf32bit
  the pf4bit,pf1bit is converted to pf8bit.
  The Pfdevice,pfcustom is converted into pf24bit.
  all the Color convertion do not revert to the primary state after the
  rotation

Added the Mirror routines
Removed the 180 degree rotation and replaced by the mirror(mtBoth) call.
 this let the GDI engine to make the rotation and it is faster than any
 rotation I have tested until now I have tested this routine with
 and image of 2300x3500x24bit without any problems on Win2K.
 I must test it on Win98 before release.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvThumbImage;

interface
uses
  Windows, Classes, Controls, ExtCtrls, SysUtils, Messages, Graphics, Forms,
  jpeg, Dialogs, JvTypes, JvBaseThumbnail;

type
  TAngle = (AT0, AT90, AT180, AT270);

  // (rom) renamed elements
  TMirror = (mtHorizontal, mtVertical, mtBoth);

  TRGB = array [0..32767] of TRGBTriple;
  PRGB = ^TRGB;
  TCurveArray = array [0..255] of Byte;
  TRotateNotify = procedure(Sender: TObject; Percent: Byte; var Cancel: Boolean) of object;
  TFilterEmpty = function: Byte;
  TFilterArray = array [1..9] of Byte;

  TJvThumbImage = class(TJvBaseThumbImage)
  private
    FAngle: TAngle;
    FModified: Boolean;
    FOnRotate: TRotateNotify;
    FZoom: Word;
    FOnLoad: TNotifyEvent;
    FFileName: string;
    FClass: TGraphicClass;
    FOnInvalidImage: TInvalidImageEvent;
    procedure Rotate90;
    //procedure Rotate180;
    procedure Rotate270;
    procedure SetAngle(AAngle: TAngle);
    function GetModify: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Mirror(MirrorType: TMirror);
    procedure ChangeRGB(R, G, B: Longint);
    procedure ChangeRGBCurves(R, G, B: TCurveArray);
    procedure ScaleDown(MaxW, MaxH: Longint);
    procedure LoadFromFile(AFile: string); //virtual;
    procedure LoadFromStream(AStream: TStream; AType: TGRFKind); // needs more tests
    procedure SaveToStream(AStream: TStream; AType: TGRFKind); // testing it
    procedure SaveToFile(AFile: string);
    procedure Save;
    procedure BitmapNeeded;
    //    Procedure FilterFactory(Filter:TFilterArray;Divider:Byte);
    procedure Invert;
    procedure Contrast(const Percent: TPercent);
    procedure Lightness(const Percent: TPercent);
    procedure Grayscale;
    procedure Rotate(AAngle: TAngle);
    function GetFilter: string;
    //Property JpegScale : TJPegScale read vJPegScale write vJpegScale;
  published
    { Published declarations }
    property Angle: TAngle read FAngle write SetAngle;
    property Modified: Boolean read FModified;
    //Property OnRelease : TdestroyNotify read EVonrelease write Evonrelease;
    property CanModify: Boolean read GetModify;
    property Zoom: Word read FZoom write FZoom;
    // (rom) should be called in the implementation more often
    property OnRotate: TRotateNotify read FOnRotate write FOnRotate;
    property OnLoaded: TNotifyEvent read FOnLoad write FOnLoad;
    property OnInvalidImage: TInvalidImageEvent read FOnInvalidImage write FOnInvalidImage;
  end;

implementation

uses
  JvThumbnails;

type
  TRGBTripleArray = array [0..MaxListSize] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

constructor TJvThumbImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAngle := AT0;
//  FClass := Graphics.TBitmap;
  FModified := False;
end;

destructor TJvThumbImage.Destroy;
begin
  inherited Destroy;
end;

procedure TJvThumbImage.Lightness(const Percent: TPercent);
var
  Amount: Integer;
  RCurve: TCurveArray;
  I: Integer;
begin
  Amount := Round((255 / 100) * Percent);
  if Amount > 0 then
    for I := 0 to 255 do
      RCurve[I] := BoundByte(0, 255, I + ((Amount * (I xor 255)) shr 8))
  else
    for I := 0 to 255 do
      RCurve[I] := BoundByte(0, 255, I - ((Abs(Amount) * I) shr 8));
  ChangeRGBCurves(RCurve, RCurve, RCurve);
end;

procedure TJvThumbImage.Rotate(AAngle: TAngle);
begin
  case AAngle of
    AT90:
      Rotate90;
    At180:
      Mirror(mtBoth);
    AT270:
      Rotate270;
  end;
end;

function TJvThumbImage.GetFilter: string;
var
  //  a: string;
  P: Longint;
begin
  Result := Graphics.GraphicFilter(TGraphic);
  P := Pos('(', Result);
  InsertStr(Result, '*.pcx;*.tga;', p);
  P := Pos('|', Result);
  InsertStr(Result, '*.pcx;*.tga;', p);
  Result := Result + '|PCX Files(*.pcx)|*.pcx|Targa Files(*.tga)|*.tga';
    //Graphics.GraphicFilter(TGraphic)+'|PCX File|*.PCX|Targa File|*.TGA';
  { TODO : Add in the filter the rest of the images we support but are not registered to the Graphics unit }
end;

procedure TJvThumbImage.Contrast;
var
  Amount: Integer;
  Counter: Integer;
  Colors: TCurveArray;
begin
  Amount := Round((256 / 100) * Percent);
  for Counter := 0 to 127 do
    Colors[Counter] := BoundByte(0, 255, Counter - ((Abs(128 - Counter) * Amount) div 256));
  for Counter := 127 to 255 do
    Colors[Counter] := BoundByte(0, 255, Counter + ((Abs(128 - Counter) * Amount) div 256));
  ChangeRGBCurves(Colors, Colors, Colors);
end;

procedure TJvThumbImage.LoadFromStream(AStream: TStream; AType: TGRFKind);
var
  Bmp: Graphics.TBitmap;
  Jpg: TJpegImage;
  Wmf: TMetafile;
  Ico: TIcon;
begin
  //testing the stream load capabilities;
  // (rom) deactivated because LoadFromStream is not defined that way
  //AStream.Seek(0, soFromBeginning); //most of the stream error are generated because this is not at the proper position
  Bmp := nil;
  Jpg := nil;
  Wmf := nil;
  Ico := nil;
  try
    case AType of
      grBMP:
        begin
          Bmp := Graphics.TBitmap.Create;
          Bmp.LoadFromStream(AStream);
          Bmp.PixelFormat := pf24bit;
          Picture.Assign(Bmp);
        end;
      grJPG:
        begin
          Jpg := TJpegImage.Create;
          Jpg.LoadFromStream(AStream);
          Picture.Assign(Jpg);
        end;
      grWMF, grEMF:
        begin
          Wmf := Graphics.TMetafile.Create;
          Wmf.LoadFromStream(AStream);
          Picture.Assign(Wmf);
        end;
      grICO:
        begin
          Ico := Graphics.TIcon.Create;
          Ico.LoadFromStream(AStream);
          Picture.Assign(Ico);
        end;
    end;
  finally
    if Assigned(Bmp) then
      FreeAndNil(Bmp);
    if Assigned(Jpg) then
      FreeAndNil(Jpg);
    if Assigned(Wmf) then
      FreeAndNil(Wmf);
    if Assigned(Ico) then
      FreeAndNil(Ico);
  end;
end;

procedure TJvThumbImage.SaveToStream(AStream: TStream; AType: TGRFKind);
var
  Bmp: Graphics.TBitmap;
  Jpg: TJpegImage;
  Wmf: TMetafile;
  Ico: TIcon;
begin
  //testing the stream Save capabilities;
  // (rom) deactivated because SaveToStream is not defined that way
  //AStream.Seek(0, soFromBeginning); //most of the stream error are generated because this is not at the proper position
  case AType of
    grBMP:
      begin
        Bmp := Graphics.TBitmap.Create;
        // (rom) secured
        try
          Bmp.Assign(Picture.Graphic);
          Bmp.PixelFormat := pf24bit;
          Bmp.SaveToStream(AStream);
        finally
          FreeAndNil(Bmp);
        end;
      end;
    grJPG:
      begin
        Jpg := TJpegImage.Create;
        try
          Jpg.Assign(Picture.Graphic);
          Jpg.SaveToStream(AStream);
        finally
          FreeAndNil(Jpg);
        end;
      end;
    grWMF, grEMF:
      begin
        Wmf := Graphics.TMetafile.Create;
        try
          Wmf.Assign(Picture.Graphic);
          Wmf.SaveToStream(AStream);
        finally
          FreeAndNil(Wmf);
        end;
      end;
    grICO:
      begin
        Ico := Graphics.TIcon.Create;
        try
          Ico.Assign(Picture.Graphic);
          Ico.SaveToStream(AStream);
        finally
          FreeAndNil(Ico);
        end;
      end;
  end;
end;

procedure TJvThumbImage.LoadFromFile(AFile: string);
var
  JpegImage: TJpegImage;
  Fl: TFileStream;
begin
  try
    if UpperCase(ExtractFileExt(AFile)) = '.JPG' then
    begin
      JpegImage := TJpegImage.Create;

      if Parent is TJVThumbnail then
      begin
        Fl := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
        // (rom) this is idiotic
        try
          case Fl.Size of
            0..1000000:
              JpegImage.Scale := jsFullSize;
            1000001..4000000:
              JpegImage.Scale := jsHalf;
            4000001..7000000:
              JpegImage.Scale := jsQuarter;
          else
            JpegImage.Scale := jsEighth;
          end;
        finally
          Fl.Free;
        end;
      end
      else
        JpegImage.Scale := jsFullSize;
      JPegImage.LoadFromFile(AFile);
      Picture.Bitmap := Graphics.TBitmap.Create;
      with Picture.Bitmap do
      begin
        Width := JpegImage.Width;
        Height := JpegImage.Height;
        Picture.Bitmap.Canvas.Draw(0, 0, JpegImage);
        Self.FClass := TJpegImage;
        FreeAndNil(JpegImage);
      end;
    end
    else
    begin
      try
        Picture.LoadFromFile(AFile);
      except
        if Assigned(FOnInvalidImage) then
        begin
          FOnInvalidImage(Self, AFile);
          Exit;
        end
        else
          raise;
      end;
      Self.FClass := TGraphicClass(Picture.Graphic.ClassType);
    end;
    FFileName := AFile;
    FAngle := AT0;
    if Assigned(FOnLoad) then
      FOnLoad(Self);
  except
    on E: Exception do
    begin
      FFileName := '';
      Self.FClass := nil;
      raise;
    end;
  end;
end;

procedure TJvThumbImage.SaveToFile(AFile: string);
var
  Ext: string;
  Gr: TGraphic;
begin
  // (rom) enforcing a file extension is bad style
  Ext := UpperCase(ExtractFileExt(AFile));
  if (Ext = '.JPG') or (Ext = '.JPEG') then
  begin
    Gr := TJpegImage.Create;
    TJpegImage(Gr).Assign(Picture.Graphic);
    TJpegImage(Gr).CompressionQuality := 75;
    TJpegImage(Gr).Compress;
    Gr.SaveToFile(AFile);
  end
  else
  if Ext = '.BMP' then
  begin
    Gr := Graphics.TBitmap.Create;
    Gr.Assign(Picture.Graphic);
    Graphics.TBitmap(Gr).Canvas.Draw(0, 0, Picture.Graphic);
    Gr.SaveToFile(AFile);
  end
  else
  if Ext = '.WMF' then
  begin
    Gr := TMetafile.Create;
    Gr.Assign(Picture.Graphic);
    TMetafile(Gr).Enhanced := False;
    Gr.SaveToFile(AFile);
  end
  else
  if Ext = '.EMF' then
  begin
    Gr := Graphics.TMetafile.Create;
    Gr.Assign(Picture.Graphic);
    TMetafile(Gr).Enhanced := True;
    Gr.SaveToFile(AFile);
  end
  else
    raise Exception.Create('Unknown file extension ' + Ext);
end;

procedure TJvThumbImage.Save;
var
  Temp: TGraphic;
begin
  if FClass <> nil then
  begin
    Temp := FClass.Create;
    Temp.Assign(Self.Picture.Graphic);
    Temp.SaveToFile(FFileName);
    FreeAndNil(Temp);
  end
  else
    SaveToFile(FFileName);
end;

procedure TJvThumbImage.BitmapNeeded;
var
  Bmp: Graphics.TBitmap;
begin
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    //    Bmp.PixelFormat := pf24Bit;
    //    Bmp.Width := Picture.Graphic.Width;
    //    Bmp.Height := Picture.Graphic.Height;
    //    Bmp.Canvas.Draw(0,0,Picture.Graphic);
    Bmp.Assign(Picture.Graphic);
    Picture.Graphic.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJvThumbImage.ScaleDown(MaxW, MaxH: Longint);
var
  NewSize: TPoint;
  Bmp: Graphics.TBitmap;
begin
  NewSize := ProportionalSize(Point(Picture.Width, Picture.Height), Point(MaxW, MaxH));
  if (NewSize.X > Picture.Width) and (NewSize.Y > Picture.Height) then
    Exit;
  // SomeTimes when the resize is bigger than 1600% then the strechDraw
  // doesn't produce any results at all so do it more than once to make
  // absolutly sure the will have an image in any case.
  if ((Picture.Width div NewSize.X) > 16) or ((Picture.Height div NewSize.Y) > 16) then
    ScaleDown(2 * MaxW, 2 * MaxH);
  Bmp := Graphics.TBitmap.Create;
  Bmp.Width := NewSize.X;
  Bmp.Height := NewSize.Y;
  Bmp.HandleType := bmDIB;
  Bmp.PixelFormat := pf24bit;
  Bmp.Canvas.StretchDraw(Rect(0, 0, Bmp.Width, Bmp.Height), Picture.Graphic);
  Picture.Assign(Bmp);
  Picture.Bitmap.Dormant;
  Picture.Bitmap.FreeImage;
  FreeAndNil(Bmp);
  FModified := True;
end;

function TJvThumbImage.GetModify: Boolean;
begin
  Result := False;
  if not Assigned(Picture) or not Assigned(Picture.Graphic) then
    Exit;
  if Picture.Graphic.Empty then
    Result := False
  else
  if Picture.Graphic is Graphics.TMetafile then
    Result := False
  else
    Result := not (Picture.Graphic is Graphics.TIcon);
end;

procedure TJvThumbImage.Grayscale;
{At this point I would like to thanks The author of the EFG's computer lab
 (I don't Recall His name Right now) for the fantastic job he has
 done gathering all this info}
var
  Color: PRGB;
  MemBmp: Graphics.TBitmap;
  Row, Col: Word;
  Intens: Byte;
begin
  if CanModify then
  begin
    MemBmp := Graphics.TBitmap.Create;
    MemBmp.Width := Picture.Width;
    MemBmp.Height := Picture.Height;
    MemBmp.Assign(Picture.Graphic);
    MemBmp.PixelFormat := pf24bit;
    MemBmp.HandleType := bmDIB;
    for Row := 0 to MemBmp.Height - 1 do
    begin
      Color := MemBmp.ScanLine[Row];
      for Col := 0 to MemBmp.Width - 1 do
      begin
        Intens := (Color[Col].rgbtRed + Color[Col].rgbtGreen + Color[Col].rgbtBlue)
          div 3;
        Color[Col].rgbtRed := Intens;
        Color[Col].rgbtGreen := Intens;
        Color[Col].rgbtBlue := Intens;
      end;
    end;
    if Picture.Graphic is TJpegImage then
      TJpegImage(Picture.Graphic).Assign(MemBmp);
    if Picture.Graphic is Graphics.TBitmap then
      Picture.Bitmap.Assign(MemBmp);
    MemBmp.Free;
  end;
  Invalidate;
end;

procedure TJvThumbImage.Invert;
var
  R: TCurveArray;
  I: Byte;
begin
  for I := 0 to 255 do
    R[I] := 255 - I;
  ChangeRGBCurves(R, R, R);
end;

procedure TJvThumbImage.ChangeRGBCurves(R, G, B: TCurveArray);
var
  Color: PRGB;
  MemBmp: Graphics.TBitmap;
  Row, Col: Word;
begin
  {
  This procedure substitutes the values of R,G,B acordinally to the arrays the
  user passes in it. This is the simplest way to change the curve of a Color
  depending on an algorith created by the user.
  The substitute value of a red 0 is the value which lies in the R[0] position.
  for a simple example have a look at the invert procedure above
  }
  if CanModify then
  begin
    MemBmp := Graphics.TBitmap.Create;
    MemBmp.Width := Picture.Width;
    MemBmp.Height := Picture.Height;
    MemBmp.Assign(Picture.Graphic);
    MemBmp.PixelFormat := pf24bit;
    MemBmp.HandleType := bmDIB;
    for Row := 0 to MemBmp.Height - 1 do
    begin
      Color := MemBmp.ScanLine[Row];
      for Col := 0 to MemBmp.Width - 1 do
      begin
        Color[Col].rgbtRed := R[Color[Col].rgbtRed];
        Color[Col].rgbtGreen := G[Color[Col].rgbtGreen];
        Color[Col].rgbtBlue := B[Color[Col].rgbtBlue];
      end;
    end;
    if Picture.Graphic is TJpegImage then
      TJpegImage(Picture.Graphic).Assign(MemBmp);
    if Picture.Graphic is Graphics.TBitmap then
      Picture.Bitmap.Assign(MemBmp);
    FreeAndNil(MemBmp);
  end;
  Invalidate;
end;

procedure TJvThumbImage.Mirror(MirrorType: TMirror);
var
  MemBmp: Graphics.TBitmap;
  //  RotateBmp: Graphics.TBitmap;
  Dest: TRect;
  Crsr: TCursor;
begin
  Crsr := Screen.Cursor;
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      if not Assigned(FOnRotate) then
        Screen.Cursor := crHourGlass;
      MemBmp := Graphics.TBitmap.Create;
      MemBmp.PixelFormat := pf24bit;
      MemBmp.HandleType := bmDIB;
      MemBmp.Width := Self.Picture.Graphic.Width;
      MemBmp.Height := Self.Picture.Height;
      MemBmp.Canvas.Draw(0, 0, Picture.Graphic);
      try
        //MemBmp.Assign(Picture.Graphic);
        case MirrorType of
          mtHorizontal:
            begin
              //SpiegelnVertikal(MemBmp);
//                          SpiegelnHorizontal(MemBmp);
              Dest.Left := MemBmp.Width;
              Dest.Top := 0;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := MemBmp.Height;
            end;
          mtVertical:
            begin
              //                           SpiegelnVertikal(MemBmp);
                                         //SpiegelnHorizontal(MemBmp);
              Dest.Left := 0;
              Dest.Top := MemBmp.Height;
              Dest.Right := MemBmp.Width;
              Dest.Bottom := -MemBmp.Height;
            end;
          mtBoth:
            begin
              Dest.Left := MemBmp.Width;
              Dest.Top := MemBmp.Height;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := -MemBmp.Height;
            end;
        end;
        {    stretchblt(RotateBmp.Canvas.Handle,Dest.Left,Dest.Top,Dest.Right,Dest.Bottom,
             MemBmp.Canvas.Handle,0,0,MemBmp.Width,MemBmp.Height,SRCCOPY);{}
        {procedure Rotate180Grad(Bitmap:Graphics.TBitmap);Forward;
        procedure Rotate90Grad(Bitmap:Graphics.TBitmap);Forward;
        procedure Rotate270Grad(Bitmap:Graphics.TBitmap);Forward;{}
        stretchblt(MemBmp.Canvas.Handle, Dest.Left, Dest.Top, Dest.Right, Dest.Bottom,
          MemBmp.Canvas.Handle, 0, 0, MemBmp.Width, MemBmp.Height, SRCCOPY);
        Picture.Graphic.Assign(MemBmp);
        Invalidate;
        //    FreeAndNil(RotateBmp);
      finally
        FreeAndNil(MemBmp);
      end;
      if not Assigned(FOnRotate) then
        Screen.Cursor := Crsr;
    end;
end;

procedure TJvThumbImage.ChangeRGB(R, G, B: Longint);
{
Just a simple procedure to increase or decrease the values of the each channel
in the image idependendly from each other. E.G.
lets say the R,G,B vars have the values of 5,-3,7 this means that the red
channel should be increased buy 5 points in all the image the green value will
be decreased by 3 points and the blue value will be increased by 7 points.
This will happen to all the image by the same value no Color limunocity is
been preserved or values calculations depenting on the current channel values;
}
var
  PixColor: PRGB;
  InBmp: Graphics.TBitmap;
  Row, Col: Integer;
begin
  if not CanModify then
    Exit;
  Screen.Cursor := crHourGlass;
  InBmp := Graphics.TBitmap.Create;
  InBmp.Width := Picture.Width;
  InBmp.Height := Picture.Height;
  InBmp.Assign(Picture.Graphic);
  InBmp.HandleType := bmDIB;
  InBmp.PixelFormat := Pf24bit;
  for Row := 0 to InBmp.Height - 1 do
  begin
    PixColor := InBmp.ScanLine[Row];
    for Col := 0 to InBmp.Width - 1 do
    begin
      PixColor[Col].rgbtRed := BoundByte(0, 255, PixColor[Col].rgbtRed + R);
      PixColor[Col].rgbtGreen := BoundByte(0, 255, PixColor[Col].rgbtGreen + G);
      PixColor[Col].rgbtBlue := BoundByte(0, 255, PixColor[Col].rgbtBlue + B);
    end;
  end;
  {  if Picture.Graphic is TJpegImage then
       TJpegImage(Picture.Graphic).Assign(InBmp){}
  //  else
  Picture.Graphic.Assign(InBmp);
  Invalidate;
  InBmp.Free;
  FModified := True;
  Screen.Cursor := crArrow;
end;

procedure TJvThumbImage.SetAngle(AAngle: TAngle);
begin
  { Procedure to actually decide wich should be the rotation in conjuction with the
    image's phisical Angle}
  if Assigned(Picture.Graphic) then
    if CanModify then
      if AAngle <> FAngle then
      begin
        if FAngle = AT0 then
        begin
          if AAngle = AT90 then
          begin
            Rotate90;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
          if AAngle = AT180 then
          begin
            //rotate180;
            Mirror(mtBoth);
          end;
          if AAngle = AT270 then
          begin
            Rotate270;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
        end;
        if FAngle = AT90 then
        begin
          if AAngle = AT180 then
          begin
            Rotate90;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
          if AAngle = AT270 then
          begin
            //rotate180;
            Mirror(mtBoth);
          end;
          if AAngle = at0 then
          begin
            Rotate270;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
        end;
        if FAngle = AT180 then
        begin
          if AAngle = AT90 then
          begin
            Rotate270;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
          if AAngle = AT0 then
          begin
            //rotate180;
            Mirror(mtBoth);
          end;
          if AAngle = AT270 then
          begin
            Rotate90;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
        end;
        if FAngle = AT270 then
        begin
          if AAngle = AT90 then
          begin
            //rotate180;
            Mirror(mtBoth);
          end;
          if AAngle = AT0 then
          begin
            Rotate90;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
          if AAngle = AT180 then
          begin
            Rotate270;
            if Parent is TJVThumbnail then
              SendMessage(TJVThumbnail(Parent).Handle, TH_IMAGESIZECHANGED, 0, 0);
          end;
        end;
        FAngle := AAngle;
        FModified := FAngle <> AT0;
      end;
end;

procedure TJvThumbImage.Rotate270;
var
  MemBmp: Graphics.TBitmap;
  PByte1: PRGBTripleArray;
  PByte2: PRGBTripleArray;
  //  Stp: Byte;
  RotateBmp: Graphics.TBitmap;
  I, J: Longint;
  Crsr: TCursor;
begin
  Crsr := Screen.Cursor;
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      if not Assigned(FOnRotate) then
        Screen.Cursor := crHourGlass;
      MemBmp := Graphics.TBitmap.Create;
      MemBmp.Assign(Picture.Graphic);
      MemBmp.HandleType := bmDIB;
      MemBmp.PixelFormat := PF24Bit;
      RotateBmp := Graphics.TBitmap.Create;
      RotateBmp.PixelFormat := MemBmp.PixelFormat;
      RotateBmp.HandleType := MemBmp.HandleType;
      RotateBmp.Width := MemBmp.Height;
      RotateBmp.Height := MemBmp.Width; {}
      try
        I := 0; //RotateBmp.Height-1;
        while I < RotateBmp.Height {-1} do
        begin
          PByte1 := RotateBmp.ScanLine[I];
          J := 0;
          while J < MemBmp.Height {-1} do
          begin
            PByte2 := MemBmp.ScanLine[J];
            PByte1[J] := PByte2[RotateBmp.Height - 1 - I];
            Inc(J);
          end;
          Inc(I);
        end;
        Picture.Bitmap.Assign(RotateBmp);
        Invalidate;
      finally
        FreeAndNil(RotateBmp);
        FreeAndNil(MemBmp);
      end;
      if not Assigned(FOnRotate) then
        Screen.Cursor := Crsr;
    end;
end;

(*
procedure TJvThumbImage.Rotate180;
var
  MemBmp: Graphics.TBitmap;
  RotateBmp: Graphics.TBitmap;
  I, J: Longint;
  Brake: Boolean;
begin
  //Procedure to rotate the image at 180d cw or ccw is the same

  { TODO : Removed the 180 degree rotation and replaced by the mirror(mtBoth) call.
    this let the GDI engine to make the rotation and it is faster than any
    rotation I have tested until now I have tested this routine with
    and image of 2300x3500x24bit with out any problems on Win2K.
    I must test it on Win98 before release. }
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      if not Assigned(FOnRotate) then
        Screen.Cursor := crHourGlass;
      MemBmp := Graphics.TBitmap.Create;
      MemBmp.Width := Picture.Width;
      MemBmp.Height := Picture.Height;
      MemBmp.canvas.Draw(0, 0, Picture.Graphic);
      MemBmp.Palette := Picture.Graphic.Palette;
      RotateBmp := Graphics.TBitmap.Create;
      RotateBmp.Assign(MemBmp);
      with MemBmp.Canvas.ClipRect do
        for I := Left to Right do
          for J := Top to Bottom do
          begin
            RotateBmp.Canvas.Pixels[Right - I - 1, Bottom - J - 1] :=
              MemBmp.Canvas.Pixels[I, J];
            if Assigned(FOnRotate) then
            begin
              Brake := False;
              FOnRotate(Self, Trunc(((I * J) / (Right * Bottom)) * 100), Brake);
              if Brake then
              begin
                RotateBmp.Free;
                MemBmp.Free;
                // (rom) AAAAHHHRRRGGG Exit was missing
                Exit;
              end;
            end;
          end;
      Picture.Bitmap.Assign(RotateBmp);
      Invalidate;
      RotateBmp.Free;
      MemBmp.Free;
      if not Assigned(FOnRotate) then
        Screen.Cursor := crArrow;
    end;
end;
*)

procedure TJvThumbImage.Rotate90;
var
  MemBmp: Graphics.TBitmap;
  PByte1: PRGBtripleArray;
  PByte2: PRGBtripleArray;
  //  Stp: Byte;
  RotateBmp: Graphics.TBitmap;
  I, J {, C}: Longint;
  Crsr: TCursor;
begin
  //Procedure to rotate an image at 90D clockwise or 270D ccw
  Crsr := Screen.Cursor;
  if Assigned(Picture.Graphic) then
    if CanModify then
    begin
      if not Assigned(FOnRotate) then
        Screen.Cursor := crHourGlass;
      MemBmp := Graphics.TBitmap.Create;
      MemBmp.Assign(Picture.Graphic);
      MemBmp.HandleType := bmDIB;
      //MemBmp.PixelFormat := pf24bit;
    {  Case MemBmp.PixelFormat of
        pf4bit,pf1bit   : begin MemBmp.PixelFormat := pf8bit; Stp := 1; end;
        pf8bit          : Stp := 1;
        pf16bit,PF15Bit : Stp := 2;
        pf24bit         : Stp := 3;
        pf32bit         : Stp := 4;
        pfDevice,
        pfCustom        : begin
                            MemBmp.PixelFormat := PF24bit;
                            Stp:=3;
                          end;
      else Exit;
      end;{}
      MemBmp.PixelFormat := PF24bit;
      //      Stp := 3;
      RotateBmp := Graphics.TBitmap.Create;
      RotateBmp.FreeImage;
      RotateBmp.PixelFormat := MemBmp.PixelFormat;
      RotateBmp.HandleType := MemBmp.HandleType;
      RotateBmp.Width := MemBmp.Height;
      RotateBmp.Height := MemBmp.Width;
      try
        I := RotateBmp.Height - 1;
        while I  >= 0 do
        begin
          PByte1 := RotateBmp.ScanLine[I];
          J := 0;
          while J < MemBmp.Height do
          begin
            PByte2 := MemBmp.ScanLine[MemBmp.Height - 1 - J];
            PByte1[J] := PByte2[I];
            Inc(J);
          end;
          Dec(I);
        end;
        Picture.Bitmap.Assign(RotateBmp);
      finally
        FreeAndNil(RotateBmp);
        FreeAndNil(MemBmp);
      end;
      if not Assigned(FOnRotate) then
        Screen.Cursor := Crsr;
    end;
end;

end.

