{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Component   : TJvEditor
Description : 'Delphi IDE'-like Editor

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvaScrollText;

interface

uses Windows, SysUtils, Classes, Controls, Forms, ExtCtrls, Graphics;

type

  TJvaScrollText = class(TCustomControl)
  private
    ImageFon: TImage;
    ImagePattern: TImage;
    ImageFontMask: TImage;
    ImageFont: TImage;
    ImageScroll: TImage;
    FStrings : TStrings;

    FStop : boolean;
    FScrollBottom, FScrollTop  : integer;
    FLeftMargin, FRightMargin : integer;
    FMaxFontSize: integer;
    DelayC: integer;
    Pic: integer;

    procedure SetImageFon(Value: TPicture);
    procedure SetImagePattern(Value: TPicture);
    function GetImageFon: TPicture;
    function GetImagePattern: TPicture;
    procedure SetStrings(Value: TStrings);
  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Scroll;
    procedure Stop;
  published
    property ForeImage: TPicture read GetImageFon write SetImageFon;
    property BackImage: TPicture read GetImagePattern write SetImagePattern;
    property Lines: TStrings read FStrings write SetStrings;
    property ScrollBottom: integer read FScrollBottom write FScrollBottom default -1;
    property ScrollTop: integer read FScrollTop write FScrollTop default -1;
    property LeftMargin: integer read FLeftMargin write FLeftMargin default -1;
    property RightMargin: integer read FRightMargin write FRightMargin default -1;
    property MaxFontSize: integer read FMaxFontSize write FMaxFontSize default 48;
    property Font;
    property Speed: integer read DelayC write DelayC default 25;
  end;

implementation

uses JvStrUtil, JvDsgnIntf;

{$IFNDEF COMPILER4_UP}
type
  longword = Integer;
{$ENDIF}

const
  constDelayIncrement = 50;

  IntToStyle : array[0..3] of TFontStyles = ([], [fsBold], [fsItalic], [fsBold, fsItalic]);


constructor TJvaScrollText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageFon := TImage.Create(Self);
  ImagePattern := TImage.Create(Self);
  ImageFontMask := TImage.Create(Self);
  ImageFont := TImage.Create(Self);
  ImageScroll := TImage.Create(Self);
  FStrings := TStringList.Create;
  FScrollBottom := -1;
  FScrollTop := -1;
  FLeftMargin := -1;
  FMaxFontSize := 48;
  Speed := 25;
  Width := 150;
  Height := 150; 
end;    { Create }

destructor TJvaScrollText.Destroy;
begin
  ImageFon.Free;
  ImagePattern.Free;
  ImageFontMask.Free;
  ImageFont.Free;
  ImageScroll.Free;
  FStrings.Free;
  inherited Destroy;
end;    { Destroy }

procedure TJvaScrollText.SetImageFon(Value: TPicture);
begin
  ImageFon.Picture.Assign(Value);
  Invalidate;
end;

procedure TJvaScrollText.SetImagePattern(Value: TPicture);
begin
  ImagePattern.Picture.Assign(Value);
  Invalidate;
end;

function TJvaScrollText.GetImageFon: TPicture;
begin
  Result := ImageFon.Picture;
end;

function TJvaScrollText.GetImagePattern: TPicture;
begin
  Result := ImagePattern.Picture;
end;

procedure TJvaScrollText.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TJvaScrollText.Paint;
begin
  inherited Paint; {?}
  if csDesigning in ComponentState then
  begin
    DrawDesignFrame(Canvas, ClientRect);
		Canvas.Draw(0, 0, ImageFon.Picture.Graphic);
  end
  else
    Canvas.Draw(0, 0, ImageScroll.Picture.Graphic);
end;

procedure TJvaScrollText.Scroll;
var
  j  : integer;
  H : integer;
  RecTmp    : TRect;
  DelayMsec : longword;
	DelayPause : longword;
	DelayPause2 : longword;
	Pixels  : 1..4;
	Pixels2 : 1..4;
	Pix : array [1..4] of integer;
 // DrawInfo : boolean;
	Line  : integer;
	H2, Popr, LastLine : integer;
	Dest   : TRect;
	Source : TRect;
	SourceFon : TRect;
	FontHeight : integer;

  procedure Delay(msec : longword);
  var
    DelayM : longword;
  begin
    DelayM := GetTickCount;
    repeat
      Application.ProcessMessages;
      if FStop then exit;
    until GetTickCount - DelayM > msec;
  end;

  function ChangeFont(S : string) : boolean;
  var
  	msec : string[10];
  begin
  	Result := true;
  	if StrLComp('$Font:', PChar(S), 6) = 0 then
    	with imageFont.Canvas.Font do
      begin
    		S := PChar(S)+6;
    		Name := SubStr(S, 0, ';');
    		Size := StrToInt(SubStr(S, 1, ';'));
    		Style := IntToStyle[StrToInt(SubStr(S, 2, ';'))];
    	end
    else
    if StrLComp('$Pause', PChar(S), 6) = 0 then
    begin
  		msec := Copy(S, 7, 16);
  		try
  			Delay(StrToInt(msec));
  		except
  		end
  	end else
      Result := false;
  	FontHeight := -imageFont.Canvas.Font.Height +3;
  end;

  procedure InitAll;
  begin
    FStop := false;
    Pixels := 1;
    DelayPause := DelayC;
    Pixels2 := 1;
    DelayPause2 := DelayC;
   // DrawInfo := false;
    imageScroll.Picture.Assign(imageFon.Picture);
    imageFontMask.Picture.Assign(imageFon.Picture);
    imageFont.Picture.Assign(imageFon.Picture);
    imageScroll.BoundsRect := BoundsRect;
    imageFon.BoundsRect := BoundsRect;
    imageFontMask.BoundsRect := BoundsRect;
    imageFont.BoundsRect := BoundsRect;
    Canvas.Font.Size := MaxFontSize;
    imageFont.Picture.Bitmap.Height := Height + Canvas.TextHeight('W');

    imageScroll.Picture.Assign(imageFon.Picture);
    SourceFon.Top := 0;
    SourceFon.Left := 0;
    SourceFon.Right := imageFon.Width-1;
    SourceFon.Bottom := imageFon.Height-1;
    Source.Top := 0;
    Source.Left := 0;
    Source.Right := imageScroll.Picture.Width-1;
    Dest := Source;
    imageFont.Canvas.Brush.Color := clBlack;
    Source.Bottom := imageFont.Picture.Height-1;
    imageFont.Canvas.FillRect(Source);
    imageFont.Canvas.Font.Color := clWhite;
    imageFontMask.Canvas.Brush.Color := clWhite;
    imageFontMask.Canvas.FillRect(SourceFon);

    FStop := false;
   // ChangeFont('$Font:Times New Roman;12;0');
    imageFont.Canvas.Font := Font;
    imageFont.Canvas.Font.Color := clWhite;
    FontHeight := imageFont.Canvas.TextHeight('W') + 3;

    if FScrollTop < 0 then FScrollTop := 2;
    if FScrollBottom < 0 then FScrollBottom := Height - 2;
    if FLeftMargin < 0 then FLeftMargin := 2;
    if FRightMargin < 0 then FRightMargin := Width - 2;
    H2 := ScrollBottom;
    Popr := 0;
    LastLine := 0;
    Line := -1;
  end;

  procedure DelayBegin;
  begin
    DelayMsec := GetTickCount;
  end;

  procedure DelayEnd;
  var
    DelayFact: longword;
  begin
    DelayFact := GetTickCount - DelayMsec;
    repeat
      Application.ProcessMessages;
      if FStop then exit;
    until GetTickCount - DelayMsec > DelayPause;
   {************* Коррекция скорости *************}
    inc(Pic);
    if Pic > 11 then begin
        {Подкорректировать скорость - сделать рывками}
        Pixels := 1;
        if Pix[2] > Pix[Pixels] then Pixels := 2;
        if Pix[3] > Pix[Pixels] then Pixels := 3;
        if Pix[4] > Pix[Pixels] then Pixels := 4;
        DelayPause := DelayC + (Pixels-1)*constDelayIncrement;
      DelayPause2 := DelayPause;
      Pixels2 := Pixels;
      Pix[1] := 0;
      Pix[2] := 0;
      Pix[3] := 0;
      Pix[4] := 0;
      Pic    := 0;
    end else begin
      if (DelayFact > DelayPause2) and (Pixels2 < 4) then begin
        {Подкорректировать скорость - сделать рывками}
        inc(Pixels2);
        inc(DelayPause2, constDelayIncrement);
      end else if Pixels2 > 1 then begin
        {Подкорректировать скорость - сделать плавнее - машина успевает}
        dec(Pixels2);
        dec(DelayPause2, constDelayIncrement);
      end;
    end;
    inc(Pix[Pixels2]);
   { if DrawInfo then
      lblInfo.Caption := 'P='+IntToStr(Pixels)
       +' P2='+IntToStr(Pixels2)+' D='+IntToStr(DelayFact)
       +' DP='+IntToStr(DelayPause)+' DP2='+IntToStr(DelayPause2); }
   {############# Коррекция скорости #############}
  end;

  procedure CopyAll;
  begin
    imageFontMask.Canvas.FillRect(SourceFon);
    {перенести текст}
    imageFontMask.Canvas.CopyMode := cmNotSrcCopy;
    imageFontMask.Canvas.CopyRect(Dest, imageFont.Canvas, Source);
    {Корректировка верхней границы}
    RecTmp := SourceFon;
    RecTmp.Bottom := FScrollTop;
    imageFontMask.Canvas.FillRect(RecTmp);
    {Корректировка правой границы}
    RecTmp := SourceFon;
    RecTmp.Left := FRightMargin;
    imageFontMask.Canvas.FillRect(RecTmp);
    {наложить маску на фон}
    imageScroll.Canvas.CopyMode := cmSrcCopy;
    imageScroll.Canvas.CopyRect(SourceFon, imageFon.Canvas, SourceFon);
    imageScroll.Canvas.CopyMode := cmSrcAnd;
    imageScroll.Canvas.CopyRect(SourceFon, imageFontMask.Canvas, SourceFon);
    {наложить маску}
    imageFontMask.Canvas.CopyMode := cmSrcErase;
    imageFontMask.Canvas.CopyRect(SourceFon, imagePattern.Canvas, SourceFon);
    {наложить текст на фон}
    imageScroll.Canvas.CopyMode := cmSrcPaint;
    imageScroll.Canvas.CopyRect(SourceFon, imageFontMask.Canvas, SourceFon);
  end;

begin
  InitAll;
  while True do
  begin
  	inc(Line);
  	if Line = FStrings.Count then Line := 0;
  	{Вывести строку}
  	if ChangeFont(FStrings[Line]) then continue;
  	H := LastLine - Popr;
  	LastLine := LastLine + FontHeight;
  	{H := Line * FontHeight - Popr;}
  	imageFont.Canvas.TextOut(FLeftMargin, H, FStrings[Line]);
  	{Прокрутить строку}
  	for j := 1 to FontHeight do
    begin
  		dec(H2);
  		if (j mod Pixels) <> 0 then continue;
  		Source.Bottom := H+j;{H1}
  		Source.Left := FLeftMargin;
  		SourceFon.Left := FLeftMargin;
  		Dest.Left := FLeftMargin;
  		Dest.Top := H2;
  		Dest.Bottom := H2 + H+j;{H2+H1}
  	  DelayBegin;
  			CopyAll;
  			Canvas.Draw(0, 0, ImageScroll.Picture.Graphic);
  	  DelayEnd;
  		if FStop then exit;
  	end;
  	if (Source.Bottom - imageScroll.Height) > FontHeight then
    begin
  		inc(H2, FontHeight);
  		inc(Popr, FontHeight);
  		Dest.Top      := 0;
  		Dest.Bottom   := imageFont.Picture.Height-1 - FontHeight;
  		Source.Top := FontHeight;
  		Source.Bottom := imageFont.Picture.Height-1;
  		imageFont.Canvas.CopyRect(Dest, imageFont.Canvas, Source);
  		Source.Top := 0;
  	end;
  end;    { while }
end;

procedure TJvaScrollText.Loaded;
begin
  inherited Loaded;
  imageScroll.BoundsRect := BoundsRect;
  imageScroll.Picture.Assign(imageFon.Picture);
end;    { Loaded }

procedure TJvaScrollText.Stop;
begin
  FStop := True;
end;    { Stop }

end.
