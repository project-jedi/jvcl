{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaScrollText.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvaScrollText;

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Controls, ExtCtrls, Graphics,
  {$IFDEF VisualCLX}
  Types, QWindows,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvaScrollText = class(TJvCustomControl)
  private
    FForeImage: TImage;
    FBackImage: TImage;
    FFontMaskImage: TImage;
    FFontImage: TImage;
    FScrollImage: TImage;
    FStrings: TStringList;
    FStop: Boolean;
    FScrollBottom: Integer;
    FScrollTop: Integer;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FMaxFontSize: Integer;
    FSpeed: Integer;
    FPics: Integer;
    procedure SetForeImage(Value: TPicture);
    procedure SetBackImage(Value: TPicture);
    function GetForeImage: TPicture;
    function GetBackImage: TPicture;
    function GetStrings: TStrings;
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
    property ForeImage: TPicture read GetForeImage write SetForeImage;
    property BackImage: TPicture read GetBackImage write SetBackImage;
    property Height default 150;
    property Lines: TStrings read GetStrings write SetStrings;
    property ScrollBottom: Integer read FScrollBottom write FScrollBottom default -1;
    property ScrollTop: Integer read FScrollTop write FScrollTop default -1;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default -1;
    property RightMargin: Integer read FRightMargin write FRightMargin default -1;
    property MaxFontSize: Integer read FMaxFontSize write FMaxFontSize default 48;
    property Font;
    property Speed: Integer read FSpeed write FSpeed default 25;
    property Width default 150;
  end;

implementation

uses
  SysUtils, Forms,
  JvJCLUtils, JvDsgnIntf, JvThemes;

const
  cDelayIncrement = 50;
  cIntToStyle: array [0..3] of TFontStyles =
    ([], [fsBold], [fsItalic], [fsBold, fsItalic]);

constructor TJvaScrollText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
  FForeImage := TImage.Create(nil);
  FBackImage := TImage.Create(nil);
  FFontMaskImage := TImage.Create(nil);
  FFontImage := TImage.Create(nil);
  FScrollImage := TImage.Create(nil);
  FStrings := TStringList.Create;
  FScrollBottom := -1;
  FScrollTop := -1;
  FLeftMargin := -1;
  FRightMargin := -1;
  FMaxFontSize := 48;
  Speed := 25;
  Width := 150;
  Height := 150;
end;

destructor TJvaScrollText.Destroy;
begin
  FForeImage.Free;
  FBackImage.Free;
  FFontMaskImage.Free;
  FFontImage.Free;
  FScrollImage.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TJvaScrollText.GetForeImage: TPicture;
begin
  Result := FForeImage.Picture;
end;

function TJvaScrollText.GetBackImage: TPicture;
begin
  Result := FBackImage.Picture;
end;

procedure TJvaScrollText.SetForeImage(Value: TPicture);
begin
  FForeImage.Picture.Assign(Value);
  Invalidate;
end;

procedure TJvaScrollText.SetBackImage(Value: TPicture);
begin
  FBackImage.Picture.Assign(Value);
  Invalidate;
end;

function TJvaScrollText.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TJvaScrollText.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TJvaScrollText.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    DrawDesignFrame(Canvas, ClientRect);
    Canvas.Draw(0, 0, FForeImage.Picture.Graphic);
  end
  else
    Canvas.Draw(0, 0, FScrollImage.Picture.Graphic);
end;

procedure TJvaScrollText.Scroll;
var
  J: Integer;
  H: Integer;
  RecTmp: TRect;
  DelayMsec: Longword;
  DelayPause: Longword;
  DelayPause2: Longword;
  Pixels: 1..4;
  Pixels2: 1..4;
  Pix: array [1..4] of Integer;
 // DrawInfo: Boolean;
  Line: Integer;
  H2, Popr, LastLine: Integer;
  Dest: TRect;
  Source: TRect;
  SourceFon: TRect;
  FontHeight: Integer;

  // (rom) the Delay implementation is crude. Better use a Timer

  procedure Delay(MSecs: Longword);
  var
    DelayM: Longword;
  begin
    DelayM := GetTickCount;
    repeat
      Application.ProcessMessages;
      if FStop then
        Exit;
    until GetTickCount - DelayM > MSecs;
  end;

  function ChangeFont(S: string): Boolean;
  var
    msec: string[10];
  begin
    Result := True;
    if StrLComp('$Font:', PChar(S), 6) = 0 then
      with FFontImage.Canvas.Font do
      begin
        S := PChar(S) + 6;
        Name := SubStr(S, 0, ';');
        Size := StrToInt(SubStr(S, 1, ';'));
        Style := cIntToStyle[StrToInt(SubStr(S, 2, ';'))];
      end
    else
    if StrLComp('$Pause', PChar(S), 6) = 0 then
    begin
      msec := Copy(S, 7, 16);
      try
        Delay(StrToInt(msec));
      except
      end;
    end
    else
      Result := False;
    FontHeight := abs(FFontImage.Canvas.Font.Height) + 3;
  end;

  procedure InitAll;
  begin
    FStop := False;
    Pixels := 1;
    DelayPause := Speed;
    Pixels2 := 1;
    DelayPause2 := Speed;
   // DrawInfo := False;
    FScrollImage.Picture.Assign(FForeImage.Picture);
    FFontMaskImage.Picture.Assign(FForeImage.Picture);
    FFontImage.Picture.Assign(FForeImage.Picture);
    FScrollImage.BoundsRect := BoundsRect;
    FForeImage.BoundsRect := BoundsRect;
    FFontMaskImage.BoundsRect := BoundsRect;
    FFontImage.BoundsRect := BoundsRect;
    Canvas.Font.Size := MaxFontSize;
    FFontImage.Picture.Bitmap.Height := Height + Canvas.TextHeight('W');

    FScrollImage.Picture.Assign(FForeImage.Picture);
    SourceFon.Top := 0;
    SourceFon.Left := 0;
    SourceFon.Right := FForeImage.Width - 1;
    SourceFon.Bottom := FForeImage.Height - 1;
    Source.Top := 0;
    Source.Left := 0;
    Source.Right := FScrollImage.Picture.Width - 1;
    Dest := Source;
    FFontImage.Canvas.Brush.Color := clBlack;
    Source.Bottom := FFontImage.Picture.Height - 1;
    FFontImage.Canvas.FillRect(Source);
    FFontImage.Canvas.Font.Color := clWhite;
    FFontMaskImage.Canvas.Brush.Color := clWhite;
    FFontMaskImage.Canvas.FillRect(SourceFon);

    FStop := False;
   // ChangeFont('$Font:Times New Roman;12;0');
    FFontImage.Canvas.Font := Font;
    FFontImage.Canvas.Font.Color := clWhite;
    FontHeight := FFontImage.Canvas.TextHeight('W') + 3;

    if ScrollTop < 0 then
      ScrollTop := 2;
    if ScrollBottom < 0 then
      ScrollBottom := Height - 2;
    if LeftMargin < 0 then
      LeftMargin := 2;
    if RightMargin < 0 then
      RightMargin := Width - 2;
    H2 := ScrollBottom;
    Popr := 0;
    LastLine := 0;
    Line := -1;
  end;

  // (rom) the Delay implementation is crude. Better use a Timer or multimedia timer

  procedure DelayBegin;
  begin
    DelayMsec := GetTickCount;
  end;

  procedure DelayEnd;
  var
    DelayFact: Longword;
  begin
    DelayFact := GetTickCount - DelayMsec;
    repeat
      Application.ProcessMessages;
      if FStop then
        Exit;
    until GetTickCount - DelayMsec > DelayPause;
    {************* Correction of speed [translated] *************}
    Inc(FPics);
    if FPics > 11 then
    begin
      { To recorrect speed - to make by the jerks [translated] }
      Pixels := 1;
      if Pix[2] > Pix[Pixels] then
        Pixels := 2;
      if Pix[3] > Pix[Pixels] then
        Pixels := 3;
      if Pix[4] > Pix[Pixels] then
        Pixels := 4;
      DelayPause := Speed + (Pixels - 1) * cDelayIncrement;
      DelayPause2 := DelayPause;
      Pixels2 := Pixels;
      Pix[1] := 0;
      Pix[2] := 0;
      Pix[3] := 0;
      Pix[4] := 0;
      FPics := 0;
    end
    else
    begin
      if (DelayFact > DelayPause2) and (Pixels2 < 4) then
      begin
        { To recorrect speed - to make by the jerks [translated] }
        Inc(Pixels2);
        Inc(DelayPause2, cDelayIncrement);
      end
      else
      if Pixels2 > 1 then
      begin
        { To recorrect speed - to make more smoothly - the computer has time [translated] }
        Dec(Pixels2);
        Dec(DelayPause2, cDelayIncrement);
      end;
    end;
    Inc(Pix[Pixels2]);
   { if DrawInfo then
      lblInfo.Caption := 'P='+IntToStr(Pixels)
       +' P2='+IntToStr(Pixels2)+' D='+IntToStr(DelayFact)
       +' DP='+IntToStr(DelayPause)+' DP2='+IntToStr(DelayPause2); }
    {############# Correction of speed [translated] #############}
  end;

  procedure CopyAll;
  begin
    FFontMaskImage.Canvas.FillRect(SourceFon);
    { To transfer the text [translated] }
    FFontMaskImage.Canvas.CopyMode := cmNotSrcCopy;
    FFontMaskImage.Canvas.CopyRect(Dest, FFontImage.Canvas, Source);
    { Adjustment of a high bound [translated] }
    RecTmp := SourceFon;
    RecTmp.Bottom := ScrollTop;
    FFontMaskImage.Canvas.FillRect(RecTmp);
    { Adjustment of the right boundary [translated] }
    RecTmp := SourceFon;
    RecTmp.Left := RightMargin;
    FFontMaskImage.Canvas.FillRect(RecTmp);
    { To put a mask on a background [translated] }
    FScrollImage.Canvas.CopyMode := cmSrcCopy;
    FScrollImage.Canvas.CopyRect(SourceFon, FForeImage.Canvas, SourceFon);
    FScrollImage.Canvas.CopyMode := cmSrcAnd;
    FScrollImage.Canvas.CopyRect(SourceFon, FFontMaskImage.Canvas, SourceFon);
    { To put the mask [translated] }
    FFontMaskImage.Canvas.CopyMode := cmSrcErase;
    FFontMaskImage.Canvas.CopyRect(SourceFon, FBackImage.Canvas, SourceFon);
    { To put text on the background [translated] }
    FScrollImage.Canvas.CopyMode := cmSrcPaint;
    FScrollImage.Canvas.CopyRect(SourceFon, FFontMaskImage.Canvas, SourceFon);
  end;

begin
  InitAll;
  while True do
  begin
    Inc(Line);
    if Line = FStrings.Count then
      Line := 0;
    { To output the line [translated] }
    if ChangeFont(FStrings[Line]) then
      Continue;
    H := LastLine - Popr;
    LastLine := LastLine + FontHeight;
    {H := Line * FontHeight - Popr;}
    FFontImage.Canvas.TextOut(LeftMargin, H, FStrings[Line]);
    { To scroll line [translated] }
    for J := 1 to FontHeight do
    begin
      Dec(H2);
      if (J mod Pixels) <> 0 then
        Continue;
      Source.Bottom := H + J; {H1}
      Source.Left := LeftMargin;
      SourceFon.Left := LeftMargin;
      Dest.Left := LeftMargin;
      Dest.Top := H2;
      Dest.Bottom := H2 + H + J; {H2+H1}
      DelayBegin;
      CopyAll;
      Canvas.Draw(0, 0, FScrollImage.Picture.Graphic);
      DelayEnd;
      if FStop then
        Exit;
    end;
    if (Source.Bottom - FScrollImage.Height) > FontHeight then
    begin
      Inc(H2, FontHeight);
      Inc(Popr, FontHeight);
      Dest.Top := 0;
      Dest.Bottom := FFontImage.Picture.Height - 1 - FontHeight;
      Source.Top := FontHeight;
      Source.Bottom := FFontImage.Picture.Height - 1;
      FFontImage.Canvas.CopyRect(Dest, FFontImage.Canvas, Source);
      Source.Top := 0;
    end;
  end;
end;

procedure TJvaScrollText.Loaded;
begin
  inherited Loaded;
  FScrollImage.BoundsRect := BoundsRect;
  FScrollImage.Picture.Assign(FForeImage.Picture);
end;

procedure TJvaScrollText.Stop;
begin
  FStop := True;
end;

end.

