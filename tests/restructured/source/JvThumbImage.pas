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

Changes form the previus Version:

Converted the rotation Functions to use scanlines for faster results
  I have converted the movement from an array of TrgbTriple to an
  an array of bytes. Right now it must rotate the following formats
  with out big speed differrences and problems pf8bit,pf24bit,pf32bit
  the pf4bit,pf1bit is been converted to pf8bit.
  The Pfdevice,pfcustom is converted into pf24bit.
  all the color convertion do not revert to the primary state after the
  rotation

Added the Mirror routines
Removed the 180 degree rotation and replaced by the mirror(mtBoth) call.
 this let the GDI engine to make the rotation and it is faster than any
 rotation I have tested until now I have tested this routine with
 and image of 2300x3500x24bit with out any problems on Win2K.
 I must test it on Win98 before rellease.
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvThumbImage;

interface
uses
  Classes, Controls, ExtCtrls, sysutils, messages, Graphics, windows, forms,
  jpeg, Dialogs, JvBaseThumbnail;

type
  Tangle = (AT0, AT90, AT180, AT270);
  TRGBArray = array[0..32767] of TRGBTriple;
  pRGBArray = ^TRGBArray;

  TMirror = (MT_Horizontal, MT_Vertical, MT_Both);

  TRGB = array[0..32767] of TRGBTriple;
  PRGB = ^TRGB;
  TCurveArray = array[0..255] of byte;
  TRotateNotify = procedure(sender: Tobject; percent: byte;
    var break: boolean) of object;
  TFilterEmpty = function: Byte;
  TFilterArray = array[1..9] of byte;

  PJvThumbImage = ^TJvThumbImage;
  TJvThumbImage = class(TJvBaseThumbImage)
  private
    { Private declarations }
    VAngle: TAngle;
    PChanged: Boolean;
    TBrake: Boolean;
    P_onrotate: TRotateNotify;
    VZoom: Word;
    VOnLoad: TNotifyEvent;
    VFileName: string;
    VClass: TGraphicClass;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;


    procedure SetAngle(AnAngle: Tangle);
    function GetModify: boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure Mirror(MirrorType: TMirror);
    procedure ChangeRGB(r, g, b: longint);
    procedure ChangeRGBCurves(R, G, B: TCurvearray);
    procedure ScaleDown(MaxW, MaxH: longint);
    procedure LoadFromFile(AFile: string); //virtual;
    procedure LoadFromStream(AStream: TStream; Atype: TGRF_type); // needs more tests
    procedure SaveToStream(Astream: Tstream; AType: Tgrf_Type); // testing it
    procedure SaveToFile(aFile: string);
    procedure Save;
    procedure BitmapNeeded;
    //    Procedure FilterFactory(Filter:TFilterArray;Divider:Byte);
    procedure invert;
    procedure Contrast(const Percent: Tpercent);
    procedure Lightness(const Percent: Tpercent); //
    procedure Grayscale;
    procedure Rotate(AnAngle: TAngle);
    function GetFilter: string;
    //Property JpegScale : TJPegScale read vJPegScale write vJpegScale;
  published
    { Published declarations }
    property Angle: TAngle read VAngle write SetAngle;
    property Modified: boolean read PChanged;
    //Property OnRelease : TdestroyNotify read EVonrelease write Evonrelease;
    property CanModify: Boolean read getmodify;
    property Zoom: Word read VZoom write VZoom;
    property OnRotate: TRotateNotify read P_OnRotate write P_OnRotate;
    property OnLoaded: TNotifyEvent read VOnLoad write VOnLoad;
  end;


implementation
uses jvThumbnails;
//***********************THumbimage procedure and functions;****************
type
  TRgbTripleArray = array[0..maxlistsize] of TrgbTriple;
  PRgbTripleArray = ^TRgbTripleArray;

procedure TJvThumbImage.Lightness; //(const Percent:Tpercent);
var
  Amount: Integer;
  RCurve: TCurveArray;
  I: integer;
begin
  amount := round((255 / 100) * Percent);
  if Amount > 0 then
    for I := 0 to 255 do
      Rcurve[I] := boundbyte(0, 255, I + ((Amount * (I xor 255)) shr 8))
  else
    for I := 0 to 255 do
      RCurve[I] := boundbyte(0, 255, I - ((Abs(Amount) * I) shr 8));
  changergbcurves(rcurve, rcurve, rcurve);
end;

procedure TJvThumbImage.Rotate(AnAngle: TAngle);
begin
  case AnAngle of
    AT90: Rotate90;
    At180: Mirror(MT_Both);
    AT270: Rotate270;
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
  Result := Result + '|PCX Files(*.pcx)|*.pcx|Targa Files(*.tga)|*.tga'; //Graphics.GraphicFilter(TGraphic)+'|PCX File|*.PCX|Targa File|*.TGA';
  { TODO : Add in the filter the rest of the images we support but are not registered to the graphics unit }
end;

procedure TJvThumbImage.Contrast;
var
  amount: integer;
  counter: integer;
  Colors: TcurveArray;
begin
  amount := round((256 / 100) * Percent);
  for Counter := 0 to 127 do
    colors[Counter] := boundbyte(0, 255, counter - ((Abs(128 - counter) * amount) div 256));
  for counter := 127 to 255 do
    colors[counter] := boundbyte(0, 255, counter + ((Abs(128 - counter) * amount) div 256));
  Changergbcurves(colors, colors, colors);
end;

procedure TJvThumbImage.LoadfromStream(AStream: TStream; Atype: TGRF_Type);
var
  BMP: Graphics.TBitmap;
  JPG: TJpegImage;
  WMF: TMetafile;
  ICO: TIcon;
begin
  //testing the stream load capabilities;
  Astream.Seek(0, 0); //most of the stream error are generated because this is not at the proper position
  try
    case AType of
      GR_BMP:
        begin
          Bmp := graphics.Tbitmap.create;
          bmp.LoadFromStream(Astream);
          bmp.PixelFormat := pf24bit;
          picture.Assign(Bmp);
        end;
      GR_JPG:
        begin
          JPG := TJpegImage.Create;
          JPG.LoadFromStream(Astream);
          picture.Assign(JPG);
        end;
      GR_WMF,
        GR_EMF:
        begin
          WMF := graphics.TMetafile.create;
          WMF.LoadFromStream(Astream);
          picture.Assign(WMF);
        end;
      GR_ICO:
        begin
          ICO := graphics.TIcon.create;
          ICO.LoadFromStream(Astream);
          picture.Assign(ICO);
        end;
    end; //case
  finally
    if Assigned(BMP) then FreeAndNil(Bmp);
    if Assigned(JPG) then FreeAndNil(JPG);
    if Assigned(WMF) then FreeAndNil(WMF);
    if Assigned(ICO) then FreeAndNil(ICO);
  end;
end;

procedure TJvThumbImage.SaveToStream(Astream: TStream; Atype: TGrf_Type);
var
  BMP: Graphics.TBitmap;
  JPG: TJpegImage;
  WMF: TMetafile;
  ICO: TIcon;
begin
  //testing the stream Save capabilities;
  Astream.Seek(0, 0); //most of the stream error are generated because this is not at the proper position
  case AType of
    GR_BMP:
      begin
        Bmp := graphics.Tbitmap.create;
        Bmp.Assign(Picture.graphic);
        bmp.PixelFormat := pf24bit;
        Bmp.SaveToStream(AStream);
        FreeAndNil(Bmp);
      end;
    GR_JPG:
      begin
        JPG := TJpegImage.Create;
        Jpg.Assign(Picture.graphic);
        Jpg.SaveToStream(Astream);
        FreeAndNil(Jpg);
      end;
    GR_WMF,
      GR_EMF:
      begin
        WMF := graphics.TMetafile.create;
        WMF.Assign(Picture.Graphic);
        WMF.SaveToStream(AStream);
        FreeAndNil(Wmf);
      end;
    GR_ICO:
      begin
        ICO := graphics.TIcon.create;
        Ico.Assign(Picture.Graphic);
        Ico.SaveToStream(AStream);
        FreeAndNil(ICO);
      end;
  end; //case
end;

procedure TJvThumbImage.Loadfromfile(afile: string);
var
  JpegImage: TJpegImage;
  FL: TFileStream;
begin
  try
    if UpperCase(ExtractFileExt(Afile)) = '.JPG' then
    begin
      JpegImage := TJpegImage.Create;

      if Parent is TJVThumbNail then
      begin
        FL := TFileStream.create(Afile, fmopenRead or fmShareDenyNone);
        try
          case Fl.Size of
            0..1000000: JpegImage.Scale := jsFullSize;
            1000001..4000000: JpegImage.Scale := jsHalf;
            4000001..7000000: JpegImage.Scale := jsQuarter;
          else
            JpegImage.Scale := jsEighth;
          end; //CASE
        finally
          fl.free;
        end;
      end
      else
        JpegImage.Scale := jsFullSize;
      JPegImage.LoadFromFile(Afile);
      picture.Bitmap := graphics.Tbitmap.create;
      with picture.bitmap do
      begin
        width := JpegImage.width;
        Height := JpegImage.height;
        Picture.bitmap.Canvas.draw(0, 0, JpegImage);
        Self.VClass := TJpegImage;
        FreeAndNil(JpegImage);
      end;
    end
    else
    begin
      picture.loadfromfile(afile);
      Self.VClass := TGraphicClass(Picture.Graphic.ClassType);
    end;
    vFileName := aFile;
    VAngle := AT0;
    if Assigned(VOnLoad) then VOnLoad(Self);
  except
    on E: Exception do
    begin
      VFileName := '';
      Self.VClass := nil;
      raise;
    end;
  end;
end;

procedure TJvThumbImage.SaveToFile(aFile: string);
var
  Ext: ShortString;
  Gr: TGraphic;
begin
  ext := UpperCase(ExtractFileExt(aFile));
  if (Ext = '.JPG') or (Ext = '.JPEG') then
  begin
    gr := TJpegImage.Create;
    TJpegImage(gr).assign(Picture.Graphic);
    TJpegImage(gr).CompressionQuality := 75;
    TJpegImage(gr).Compress;
    gr.SaveToFile(aFile);
  end
  else
    if (Ext = '.BMP') then
    begin
      gr := Graphics.TBitmap.Create;
      gr.assign(Picture.Graphic);
      graphics.Tbitmap(gr).Canvas.Draw(0, 0, Picture.graphic);
      gr.SaveToFile(aFile);
    end
    else
      if (Ext = '.WMF') then
      begin
        gr := TMetafile.Create;
        gr.assign(Picture.Graphic);
        TMetafile(gr).Enhanced := False;
        gr.SaveToFile(aFile);
      end
      else
        if (Ext = '.EMF') then
        begin
          gr := graphics.TMetafile.Create;
          gr.assign(Picture.Graphic);
          TMetafile(gr).Enhanced := true;
          gr.SaveToFile(aFile);
        end
        else
          raise exception.Create('Unknown file extension ' + Ext);
end;

procedure TJvThumbImage.Save;
var
  Temp: TGraphic;
begin
  if VClass <> nil then
  begin
    Temp := VClass.Create;
    Temp.Assign(Self.Picture.Graphic);
    Temp.SaveToFile(VFileName);
    FreeandNil(Temp);
  end
  else
    SaveToFile(VFileName);
end;

procedure TJvThumbImage.BitmapNeeded;
var
  Bmp: Graphics.TBitmap;
begin
  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.HandleType := bmDib;
    //    Bmp.PixelFormat := pf24Bit;
    //    Bmp.Width := Picture.Graphic.Width;
    //    Bmp.Height := Picture.Graphic.Height;
    //    Bmp.Canvas.Draw(0,0,Picture.Graphic);
    Bmp.Assign(Picture.Graphic);
    Picture.Graphic.assign(BMP);
  finally
    BMP.Free;
  end;
end;

constructor TJvThumbImage.create(Aowner: Tcomponent);
begin
  inherited;
  VAngle := AT0;
  Pchanged := false;
end;

destructor TJvThumbImage.destroy;
begin
  inherited;
end;

procedure TJvThumbImage.ScaleDown(MaxW, MaxH: longint);
var
  newsize: tpoint;
  bmp: Graphics.tbitmap;
begin
  Newsize := ProportionalSize(Point(Picture.Width, Picture.Height), point(MaxW, MaxH));
  if (NewSize.x > picture.Width) and (NewSize.y > Picture.Height) then exit;
  // SomeTimes when the resize is bigger than 1600% then the strechDraw
  // doesn't produce any results at all so do it more than once to make
  // absolutly sure the will have an image in any case.
  if ((Picture.Width div NewSize.X) > 16) or ((Picture.Height div NewSize.Y) > 16) then
    ScaleDown(2 * MaxW, 2 * MaxH);
  bmp := Graphics.TBitmap.Create;
  bmp.Width := NewSize.X;
  bmp.height := NewSize.Y;
  bmp.handletype := bmdib;
  bmp.pixelformat := pf24bit;
  bmp.Canvas.Stretchdraw(Rect(0, 0, Bmp.Width, Bmp.Height), picture.graphic);
  picture.Assign(bmp);
  picture.Bitmap.Dormant;
  Picture.Bitmap.FreeImage;
  FreeAndNil(bmp);
  PChanged := true;
end;

function TJvThumbImage.getmodify: boolean;
begin
  if picture.graphic.empty then
    result := false
  else
    if picture.graphic is graphics.TMetafile then
      result := false
    else
      if picture.graphic is graphics.TIcon then
        result := false
      else
        result := true;
end;

procedure TJvThumbImage.grayscale;
{At this point I would like to thanks The author of the EFG's computer lab
 (I don't Recall His name right now) for the fantastic job he has
 done gathering all this info}
var
  color: prgb;
  membmp: GRaphics.Tbitmap;
  row, col: word;
  intens: byte;
begin
  if canmodify then
  begin
    membmp := Graphics.TBitmap.create;
    membmp.width := picture.width;
    membmp.height := picture.Height;
    membmp.Assign(picture.graphic);
    membmp.PixelFormat := pf24bit;
    membmp.HandleType := bmdib;
    for Row := 0 to membmp.Height - 1 do
    begin
      Color := membmp.ScanLine[Row];
      for Col := 0 to membmp.Width - 1 do
      begin
        intens := (Color[Col].rgbtRed + Color[Col].rgbtGreen + Color[Col].rgbtBlue)
          div 3;
        Color[Col].rgbtRed := intens;
        Color[Col].rgbtGreen := intens;
        Color[Col].rgbtBlue := intens;
      end;
    end;
    if picture.graphic is Tjpegimage then
      Tjpegimage(picture.graphic).assign(membmp);
    if picture.graphic is Graphics.Tbitmap then
      picture.bitmap.assign(membmp);
    membmp.free;
  end;
  invalidate;
end;



procedure TJvThumbImage.invert;
var
  r: Tcurvearray;
  i: byte;
begin
  for i := 0 to 255 do
    r[i] := 255 - i;
  changergbcurves(r, r, r);
end;

procedure TJvThumbImage.ChangeRGBCurves(r, g, b: TcurveArray);
var
  color: prgb;
  membmp: GRaphics.Tbitmap;
  row, col: word;
begin
  {
  This procedure substitutes the values of r,g,b acordinally to the arrays the
  user passes in it. This is the simplest way to change the curve of a color
  depending on an algorith created by the user.
  The substitute value of a red 0 is the value which lies in the r[0] position.
  for a simple example have a look at the invert procedure above
  }
  if canmodify then
  begin
    membmp := GRaphics.TBitmap.create;
    membmp.width := picture.width;
    membmp.height := picture.Height;
    membmp.Assign(picture.graphic);
    membmp.PixelFormat := pf24bit;
    membmp.HandleType := bmdib;
    for Row := 0 to membmp.Height - 1 do
    begin
      Color := membmp.ScanLine[Row];
      for Col := 0 to membmp.Width - 1 do
      begin
        Color[Col].rgbtRed := r[Color[Col].rgbtRed];
        Color[Col].rgbtGreen := g[Color[Col].rgbtGreen];
        Color[Col].rgbtBlue := b[Color[Col].rgbtBlue];
      end;
    end;
    if picture.graphic is Tjpegimage then
      Tjpegimage(picture.graphic).assign(membmp);
    if picture.graphic is Graphics.Tbitmap then
      picture.bitmap.assign(membmp);
    FreeAndNil(membmp);
  end;
  invalidate;
end;

procedure TJvThumbImage.Mirror(MirrorType: TMirror);
var
  membmp: graphics.Tbitmap;
  //  rotatebmp: graphics.Tbitmap;
  Dest: TRect;
  Crsr: TCursor;
begin
  Crsr := screen.cursor;
  if assigned(picture.graphic) then
    if canmodify then
    begin
      if not assigned(OnRotate) then
      begin
        screen.cursor := crhourglass;
      end;
      MemBmp := Graphics.TBitmap.Create;
      Membmp.PixelFormat := pf24bit;
      MemBmp.HandleType := BMDib;
      MemBmp.Width := self.Picture.Graphic.Width;
      MemBmp.Height := Self.Picture.Height;
      MemBmp.Canvas.Draw(0, 0, Picture.Graphic);
      try
        //MemBmp.Assign(Picture.Graphic);
        case MirrorType of
          MT_Horizontal:
            begin
              //SpiegelnVertikal(MemBmp);
//                          SpiegelnHorizontal(MemBmp);
              Dest.Left := MemBmp.Width;
              Dest.Top := 0;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := MemBmp.Height; {}
            end;
          MT_Vertical:
            begin
              //                           SpiegelnVertikal(MemBmp);
                                         //SpiegelnHorizontal(MemBmp);
              Dest.Left := 0;
              Dest.Top := MemBmp.Height;
              Dest.Right := MemBmp.Width;
              Dest.Bottom := -MemBmp.Height; {}
            end;
          MT_Both:
            begin
              Dest.Left := MemBmp.Width;
              Dest.Top := MemBmp.Height;
              Dest.Right := -MemBmp.Width;
              Dest.Bottom := -MemBmp.Height; {}
            end;
        end;
        {    stretchblt(Rotatebmp.Canvas.Handle,Dest.Left,Dest.Top,Dest.Right,Dest.Bottom,
             MemBmp.Canvas.Handle,0,0,MemBmp.Width,membmp.Height,SRCCOPY);{}
        {procedure Rotate180Grad(Bitmap:Graphics.TBitmap);Forward;
        procedure Rotate90Grad(Bitmap:Graphics.TBitmap);Forward;
        procedure Rotate270Grad(Bitmap:Graphics.TBitmap);Forward;{}
        stretchblt(MemBmp.Canvas.Handle, Dest.Left, Dest.Top, Dest.Right, Dest.Bottom,
          MemBmp.Canvas.Handle, 0, 0, MemBmp.Width, membmp.Height, SRCCOPY);
        Picture.Graphic.Assign(MemBmp);
        Invalidate;
        //    FreeAndNil(RotateBmp);
      finally
        FreeAndNil(MemBmp);
      end;
      if not assigned(OnRotate) then screen.cursor := Crsr;
    end;
end;

procedure TJvThumbImage.ChangeRGB(r, g, b: longint);

{
Just a simple procedure to increase or decrease the values of the each channel
in the image idependendly from each other. E.G.
lets say the r,g,b vars have the values of 5,-3,7 this means that the red
channel should be increased buy 5 points in all the image the green value will
be decreased by 3 points and the blue value will be increased by 7 points.
This will happen to all the image by the same value no color limunocity is
been preserved or values calculations depenting on the current channel values;
}

var
  PixColor: PRGB;
  inbmp: Graphics.tbitmap;
  Row, Col: integer;
begin
  if not CanModify then exit;
  screen.cursor := crhourglass;
  inbmp := Graphics.Tbitmap.create;
  inbmp.width := picture.width;
  inbmp.height := picture.height;
  inbmp.assign(picture.graphic);
  Inbmp.HandleType := bmDib;
  inbmp.PixelFormat := Pf24bit;
  for Row := 0 to inbmp.Height - 1 do
  begin
    PixColor := inbmp.ScanLine[Row];
    for Col := 0 to inbmp.Width - 1 do
    begin
      PixColor[Col].rgbtRed := boundbyte(0, 255, pixcolor[Col].rgbtRed + r);
      PixColor[Col].rgbtGreen := boundbyte(0, 255, PixColor[Col].rgbtGreen + g);
      PixColor[Col].rgbtBlue := boundbyte(0, 255, PixColor[Col].rgbtBlue + b);
    end;
  end;
  {  if picture.graphic is Tjpegimage then
       tJpegimage(picture.graphic).assign(inbmp){}
  //  else
  picture.Graphic.assign(inbmp);
  invalidate;
  inbmp.free;
  Pchanged := true;
  Screen.cursor := crArrow;
end;

procedure TJvThumbImage.SetAngle(AnAngle: Tangle);
begin
  { Procedure to actually decide wich should be the rotation in conjuction with the
    image's phisical Angle}
  if assigned(picture.graphic) then
    if canmodify then
      if AnAngle <> Vangle then
      begin
        if Vangle = At0 then
        begin
          if AnAngle = AT90 then
          begin
            rotate90;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
          if AnAngle = AT180 then
          begin
            //rotate180;
            Mirror(MT_Both);
          end;
          if AnAngle = AT270 then
          begin
            Rotate270;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
        end;
        if Vangle = AT90 then
        begin
          if AnAngle = AT180 then
          begin
            rotate90;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
          if AnAngle = AT270 then
          begin
            //rotate180;
            Mirror(MT_Both);
          end;
          if AnAngle = at0 then
          begin
            rotate270;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
        end;
        if Vangle = AT180 then
        begin
          if AnAngle = AT90 then
          begin
            rotate270;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
          if AnAngle = AT0 then
          begin
            //rotate180;
            Mirror(MT_Both);
          end;
          if AnAngle = AT270 then
          begin
            Rotate90;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
        end;
        if Vangle = AT270 then
        begin
          if AnAngle = AT90 then
          begin
            //rotate180;
            Mirror(MT_Both);
          end;
          if AnAngle = AT0 then
          begin
            rotate90;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
          if AnAngle = AT180 then
          begin
            Rotate270;
            if parent is TJVThumbNail then
              sendmessage(TJVThumbNail(parent).handle, TH_Imagesizechanged, 0, 0);
          end;
        end;
        VAngle := AnAngle;
        if VAngle <> At0 then Pchanged := true
        else Pchanged := false;
      end;
end;

procedure TJvThumbImage.rotate270;
var
  membmp: graphics.Tbitmap;
  PByte1: PRGBTripleArray;
  PByte2: PRGBTripleArray;
  //  Stp: Byte;
  rotatebmp: graphics.Tbitmap;
  i, j: longint;
  Crsr: TCursor;
begin
        Crsr := screen.cursor;
  if assigned(picture.graphic) then
    if canmodify then
    begin
      if not assigned(OnRotate) then
      begin
        screen.cursor := crhourglass;
      end;
      membmp := Graphics.tbitmap.create;
      MemBmp.Assign(Picture.Graphic);
      MemBmp.HandleType := BMDib;
      MemBmp.PixelFormat := PF24Bit;
      RotateBmp := Graphics.Tbitmap.create;
      RotateBmp.PixelFormat := MemBmp.PixelFormat;
      RotateBmp.HandleType := MemBmp.HandleType;
      RotateBmp.width := membmp.height;
      RotateBmp.height := membmp.width; {}
      try
        I := 0; //RotateBmp.Height-1;
        while I {:= >=0{} {} < RotateBmp.Height {-1} do
        begin
          PByte1 := RotateBmp.ScanLine[I];
          j := 0;
          while j {:= {} { 0{} < MemBmp.Height {-1} do
          begin
            PByte2 := MemBmp.ScanLine[j];
            PByte1[j] := PByte2[RotateBmp.Height - 1 - i];
            Inc(j)
          end;
          Inc(I)
        end;
        picture.bitmap.assign(rotatebmp);
        invalidate;
      finally
        FreeAndNil(Rotatebmp);
        FreeAndNil(MemBmp);
      end;
      if not assigned(OnRotate) then screen.cursor := Crsr;
    end;
end;

procedure TJvThumbImage.Rotate180;
var
  membmp: graphics.Tbitmap;
  rotatebmp: graphics.Tbitmap;
  i, j: longint;
begin
  {
  Procedure to rotate the image at 180d cw or ccw is the same
  }

  { TODO : Removed the 180 degree rotation and replaced by the mirror(mtBoth) call.
 this let the GDI engine to make the rotation and it is faster than any
 rotation I have tested until now I have tested this routine with
 and image of 2300x3500x24bit with out any problems on Win2K.
 I must test it on Win98 before release. }

  if assigned(picture.graphic) then
    if canmodify then
    begin
      if not assigned(OnRotate) then screen.cursor := crhourglass;
      membmp := Graphics.tbitmap.create;
      membmp.Width := picture.width;
      membmp.Height := picture.Height;
      membmp.canvas.Draw(0, 0, picture.graphic);
      membmp.Palette := picture.graphic.Palette;
      rotatebmp := Graphics.Tbitmap.create;
      rotatebmp.assign(membmp);
      with membmp.Canvas.ClipRect do
        for i := Left to Right do
          for j := Top to Bottom do
          begin
            rotatebmp.Canvas.Pixels[Right - i - 1, Bottom - j - 1] :=
              membmp.Canvas.Pixels[i, j];
            if assigned(onrotate) then
            begin
              Tbrake := false;
              onrotate(self, trunc(((i * j) / (right * bottom)) * 100), Tbrake);
              if Tbrake then
              begin
                rotatebmp.free;
                membmp.free;
              end;
            end;
          end;
      picture.bitmap.assign(rotatebmp);
      invalidate;
      rotatebmp.free;
      membmp.free;
      if not assigned(OnRotate) then screen.cursor := crArrow;
    end;
end;

procedure TJvThumbImage.Rotate90;
var
  membmp: graphics.Tbitmap;
  PByte1: PRGBtripleArray;
  PByte2: PRGBtripleArray;
  //  Stp: Byte;
  rotatebmp: graphics.Tbitmap;
  i, j {, C}: longint;
  Crsr: TCursor;
begin
  //Procedure to rotate an image at 90D clockwise or 270D ccw
     Crsr := screen.cursor;
  if assigned(picture.graphic) then
    if canmodify then
    begin
      if not assigned(OnRotate) then
      begin
        screen.cursor := crhourglass;
      end;
      membmp := Graphics.tbitmap.create;
      MemBmp.Assign(Picture.Graphic);
      MemBmp.HandleType := BMDib;
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
      RotateBmp := Graphics.Tbitmap.create;
      RotateBmp.FreeImage;
      RotateBmp.PixelFormat := MemBmp.PixelFormat;
      RotateBmp.HandleType := MemBmp.HandleType; {}
      RotateBmp.width := membmp.height;
      RotateBmp.height := membmp.width; {}
      try
        I := RotateBmp.Height - 1;
        while I {:= {} >= 0 {} { < RotateBmp.Height {-1} do
        begin
          PByte1 := RotateBmp.ScanLine[I];
          j := 0;
          while j {:= {} { 0{} < MemBmp.Height {-1} do
          begin
            PByte2 := MemBmp.ScanLine[MemBmp.Height - 1 - j];
            PByte1[j] := PByte2[i];
            Inc(j)
          end;
          Dec(I)
        end;
        picture.bitmap.assign(rotatebmp);
      finally
        FreeAndNil(Rotatebmp);
        FreeAndNil(MemBmp);
      end;
      if not assigned(OnRotate) then screen.cursor := Crsr;
    end;
end;

end.

