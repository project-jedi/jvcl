{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbNail.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer@Excite.com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):


Last Modified: 2002-07-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvThumbnails;

interface

{Thumbimage, ThumbNail components
 Thumbimage is a TImage descentant wich passes the control of the mouse events
 to the ThumbNail and have the ability to change an images look buy changing
 the rgb values with the changergb,changergbcurve procedures.
 You can have precise control over the images look.
 The changergb procedure just adds the values you pass to its rgb variables to
 the actual values of the image.
 The Changergbcurves procedure just replaces the value of the rgb values
 accordingly with the values that passed in the the arrays.
 e.g.
 the r array in the position 15 has a value of 35 this meens that wherever in
 the picture there is a pixels which has a red value equall to 15 it will be ]
 replaced with the value 35.

 ThumbNail is what the name says a component to simply shrink an image
 proportionaly to fit in a portion of the screen with some extra mouse handling
 to create a button like effect.Just give it a filename and it will do the work
 for you.
}
uses
  Classes, Controls, ExtCtrls, sysutils, messages, graphics, windows, forms,
  JvThumbImage, JvBaseThumbnail, Dialogs;

const
  TH_ImageSizeChanged = WM_user + 1;
type
  TTitlePos = (T_Up, T_Down, T_None);

  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbNailTitle: string) of object;



  PJvThumbNail = ^TJVThumbNail;
  TJVThumbNail = class(TJvBaseThumbNail)
  private
    { Private declarations }
    V_Title: string;
    V_TitlePanel: TJvThumbTitle;
    V_TitlePanelColor: TColor;
    V_TitlePanelFont: TFont;
    TStreamfiletype: Tgrf_type;
    FDFilecreated: string;
    FDFilechanged: string;
    FDFileAccessed: string;
    TShowTitle: Boolean;
    FDFileSize: Longint;
    Pstream: TStream;
    IGWidth: longint;
    IGHeight: longint;
    THClientHeight: word;
    THClientWidth: word;
    VShadowObj: TShape;
    {Other}
    V_Updated: boolean;
    V_ImageReady: boolean;
    V_TitlePos: TTitlePos;
    VPhotoName: TFilename;
    VPhoto: TJvThumbimage;
    {Event Related}
    V_OnGetTitle: TTitleNotify;
    MousePressed: Boolean;
    Destroying: Boolean;
    PAsButton: Boolean;
    VMinimize: Boolean;
    VAutoLoad: Boolean; // if true then load the image either from a thum file or create it from the filename
    VShadowColor: TColor;
    VShowShadow: Boolean;
    VHShadowOffset: Word;
    VVShadowOffset: Word;
    {Event Related}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string);
    procedure RefreshFont(Sender: TObject);
    {Normal Procedures}
    procedure SetFile(const Afile: string);
    function LoadFile(Afile: string): string;
    function GetFilename: string;
    procedure CalculateImageSize; virtual;
    procedure setClientWidth(Awidth: word);
    procedure setdummystr(AnStr: string);
    procedure Setminimize(Min: boolean);
    procedure setdummyCard(AnInt: Longint);
    procedure SetClientHeight(aheight: word);
    procedure SetVisibleTitle(const Astate: boolean);
    procedure SetTitlePos(const Astate: TtitlePos);
    {Property Procedures}
    procedure PSetV_Title(const Value: string);
    procedure PSetV_TitlePanelColor(const Value: TColor);
    procedure SetStream(const AStream: Tstream);
    procedure PSetV_TitlePanelFont(const Value: TFont);
    procedure GetFileInfo(Aname: string);
    procedure SetShowShadow(Show: Boolean);
//    procedure SetShadowColor(aColor: TColor);
  protected
    procedure THSizechanged(var Message: tmessage); message TH_ImagesizeChanged;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize; override;

  public
    { Public declarations }
    constructor Create(Aowner: Tcomponent); override;
    destructor Destroy; override;
    procedure SetTitlePanel(P_Title: string; P_Font: TFont; P_Color: TColor);
    procedure Refresh;
    property Stream: Tstream read Pstream write setstream;
    property Photo: TJvTHumbImage read VPhoto write VPhoto;
  published
    { Published declarations }
    property Filename: string read GetFileName write SetFile;
    property Title: string read V_Title write PSetV_Title;
    property TitleColor: TColor read V_TitlePanelColor write PSetV_TitlePanelColor;
    property TitleFont: TFont read V_TitlePanelFont write PSetV_TitlePanelFont;
    property ImageReady: boolean read V_ImageReady;
    property OnGetTitle: TTitleNotify read V_OnGetTitle write V_OnGetTitle;
    property ClientWidth: word read THClientWidth write setClientWidth;
    property ClientHeight: word read THClientHeight write setClientHeight;
    property FileSize: Longint read FDFileSize write SetDummyCard;
    property FileAccessed: string read FDFileAccessed write SetDummyStr;
    property FileCreated: string read FDFileCreated write SetDummyStr;
    property FileChanged: string read FDFileChanged write SetDummyStr;
    property ImageWidth: longint read IGWIdth default 0;
    property ImageHeight: Longint read IGHeight default 0;
    property Asbutton: boolean read PAsbutton write PAsbutton;
    property MinimizeMemory: Boolean read VMinimize write SetMinimize;
    property StreamFileType: Tgrf_type read Tstreamfiletype write tstreamfiletype;
    property ShowTitle: boolean read TshowTitle write SetVisibleTitle;
    property TitlePlacement: TTitlePos read V_titlePos write SetTitlePos;
    property Autoload: Boolean read VAutoload write VAutoload;
    property ShadowColor: TColor read VShadowColor write VShadowColor;
    property ShowShadow: Boolean read VShowShadow write SetShowShadow;
  end;



implementation

uses jvThumbViews, jpeg;
//uses {$IFNDEF COMPILER6_UP}Gifimage,{$ENDIF} Pcx_unit, Targa, PngImage, jpeg;


//***********************ThumbNail procedure and functions;****************

procedure TJVThumbNail.SetVisibleTitle(const AState: boolean);
begin
  if astate <> Tshowtitle then
  begin
    TshowTitle := astate;
    v_titlePanel.Visible := astate;
  end
end;

procedure TJVThumbNail.resize;
begin
  Calculateimagesize;
end;

procedure TJVThumbNail.SetStream(const Astream: Tstream);
var
  bmp: Graphics.Tbitmap;
  size: tpoint;
  img2: Tjpegimage;
begin
  case StreamFileType of
    gr_bmp: photo.Picture.bitmap.LoadFromStream(Astream);
    gr_emf,
      gr_wmf: photo.Picture.metafile.LoadFromStream(Astream);
    gr_jpg:
      begin
        img2 := Tjpegimage.create;
        img2.LoadFromStream(Astream);
        photo.picture.assign(img2);
        FreeAndNil(img2);
      end;
  end;

  if VMinimize then
  begin
    bmp := GRaphics.TBitmap.Create;
    if parent is TJvTHumbview then
      size := proportionalsize(point(photo.picture.width, photo.picture.height),
        point(TJvThumbView(parent).maxWidth, TJvThumbView(parent).MAxheight))
    else
      size := proportionalsize(point(photo.picture.width, photo.picture.height),
        point(width, height)); {}
    bmp.width := size.x;
    bmp.Height := size.y;
    bmp.handletype := bmDib;
    bmp.pixelformat := pf24bit;
    bmp.canvas.stretchdraw(rect(0, 0, bmp.width, bmp.height),
      photo.picture.graphic);
    photo.picture.graphic.Free;
    photo.picture.graphic := nil;
    photo.picture.Assign(bmp);
    bmp.free;
  end;
end;


procedure TJVThumbNail.setClientWidth(Awidth: word);
begin
  THClientWidth := (width - (borderwidth * 2)) - 8;
end;

procedure TJVThumbNail.SetClientHeight(aheight: word);
begin
  if assigned(V_titlepanel) then
    THClientHeight := Height - (V_TitlePanel.Height + 8)
  else THClientHeight := Height - 8;
end;

// dummy property functions to alow the object inspector to
// show the properties and there values

procedure TJVThumbNail.setdummystr(AnStr: string);
begin
end;

procedure TJVThumbNail.SetDummyCard(AnInt: Longint);
begin
end;
// Dummy Properties Values end;

procedure TJVThumbNail.PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Stage = psEnding then
  begin
    V_ImageReady := True;
//    RedrawNow := true;
    exit;
  end;
  V_ImageReady := False;
end;

procedure TJVThumbNail.mousedown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if asbutton then
    if button = mbleft then
    begin
      MousePressed := True;
      BevelOuter := bvlowered;
      V_TitlePanel.BevelOuter := BVRaised;
    end;
  inherited;
end;

procedure TJVThumbNail.SetShowShadow(Show: Boolean);
begin
  VShadowObj.Visible := Show;
  VShowShadow := Show;
end;

{procedure TJVThumbNail.SetShadowColor(aColor: TColor);
begin
  VShadowObj.Brush.Color := aColor;
  VShadowColor := aColor;
end;}

procedure TJVThumbNail.mousemove(Shift: TShiftState; X, Y: Integer);
begin
  if asbutton then
    if MousePressed then
    begin
      if (x < 0) or (x > width) or (y < 0) or (y > height) then
      begin
        bevelouter := bvraised;
        V_TitlePanel.BevelOuter := bvlowered
      end
      else
      begin
        bevelouter := bvlowered;
        V_TitlePanel.BevelOuter := BVRaised;
      end;
    end;
  inherited;
end;

procedure TJVThumbNail.mouseup(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if asbutton then
    if button = mbleft then
    begin
      MousePressed := false;
      bevelouter := bvraised;
      V_TitlePanel.BevelOuter := bvlowered;
    end;
  inherited;
end;

procedure TJVThumbNail.Getfileinfo(Aname: string);
var
  FileInfo: Twin32finddata;
  h: Thandle;
  dft: dword;
  lft: tfiletime;
begin
  h := windows.findfirstfile(pchar(Aname), fileinfo);
  if (invalid_handle_value <> h) then
  begin
    windows.findclose(h);
    filetimetolocalfiletime(fileinfo.ftLastAccessTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    try
      fdFileAccessed := datetimetostr(filedatetodatetime(dft));
    except
      fdFileAccessed := 'Unknown'
    end;
    filetimetolocalfiletime(fileinfo.ftLastwriteTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    try
      fdFilechanged := datetimetostr(filedatetodatetime(dft));
    except
      fdFileChanged := 'Unknown';
    end;
    filetimetolocalfiletime(fileinfo.ftCreationTime, lft);
    filetimetodosdatetime(lft, longrec(dft).hi, longrec(dft).lo);
    try
      fdFileCreated := datetimetostr(filedatetodatetime(dft));
    except
      fdFilecreated := 'Unknown';
    end;
    FDfilesize := (fileinfo.nfilesizehigh * maxdword) + fileinfo.nfilesizelow;
  end;
end;

function TJVThumbNail.GetFilename: string;
begin
  REsult := VPhotoName.Filename;
end;

function TJVThumbNail.LoadFile(Afile: string): string;
var
  Fname: string;
begin
  try
    Fname := Afile;
    photo.LoadFromFile(afile);
    Igwidth := photo.picture.width;
    IGHeight := photo.picture.height;
    v_updated := false;
    Calculateimagesize;
    photo.visible := true;
  except
    on E: Exception do
    begin
      Fname := '';
      ShowMessage(E.Message);
    end;
  end;
  if minimizememory and (VPhotoName.Filename <> '') then
  begin
    if owner is TJvThumbView then
      photo.scaledown(TJvThumbView(owner).maxWidth, TJvThumbView(owner).maxheight)
    else
      Photo.scaledown(width, height); {}
  end;
  Result := Fname;
end;

procedure TJVThumbNail.SetFile(const Afile: string);
var
  Fname: string;
//  Pos: Longint;
//  tmp: TJvThumbImage;
//  D1, D2: TdateTime;
begin
  if afile <> '' then
  begin
    getfileinfo(Afile);
    if Vautoload then Fname := LoadFile(Afile);
  end
  else
    fname := ''; {}
  if fname = afile then
    if (title = extractfilename(VPhotoName.filename)) or (title = '') then
      title := extractfilename(Fname);
  VPhotoName.filename := Fname;
end;

procedure TJVThumbNail.CalculateImageSize;
var
  Percent: Byte;
  tempx, tempy: Single;
begin
  setclientheight(15);
  setclientwidth(15);
  if (photo.picture.width > clientwidth) or (photo.picture.height > clientheight)
    then
  begin
    tempx := ((clientwidth) / photo.picture.width) * 100;
    tempy := ((clientheight) / photo.picture.height) * 100;
  end
  else
  begin
    tempx := 100;
    tempy := 100;
  end;
  if tempx <= tempy then
    percent := trunc(tempx)
  else
    percent := trunc(tempy);
  photo.width := trunc((photo.picture.width / 100) * percent);
  photo.height := trunc((photo.picture.height / 100) * percent);
  photo.Left := trunc(width / 2 - photo.width / 2);
  photo.top := (Height div 2) - (photo.height div 2);
  case V_TitlePos of
    T_UP: Photo.top := Photo.Top + (v_TitlePanel.height div 2);
    T_Down: photo.top := Photo.Top - (v_TitlePanel.height div 2);
  end;
  VShadowObj.Setbounds(Photo.Left + VHShadowOffset, Photo.Top + VVShadowOffset,
    Photo.Width, Photo.Height);
end;

constructor TJVThumbNail.Create(Aowner: Tcomponent);
begin
  inherited create(Aowner);
  VPhotoName := TfileName.create;
  VHShadowOffset := 3;
  VVShadowOffset := 3;
  VShowShadow := False;
  VShadowColor := clsilver;
  VShadowObj := Tshape.Create(Self);
  VShadowObj.Visible := VShowShadow;
  VshadowObj.Brush.Color := VShadowColor;
  VshadowObj.Parent := Self;
  VShadowObj.Pen.Style := psClear;
  Photo := tJvThumbimage.Create(self);
  Photo.AutoSize := False;
  Photo.Align := alNone;
  Photo.Stretch := True;
  Photo.OnProgress := PhotoOnProgress;

  VShadowObj.Width := Photo.Width;
  VShadowObj.Height := Photo.Height;
  VShadowObj.Left := Photo.Left + VHShadowOffset;
  VShadowObj.Top := Photo.Top + VVShadowOffset;
  V_TitlePanel := TJvThumbTitle.Create(Self);
  V_TitlePanel.Align := alTop;
  V_TitlePanel.Height := 15;
  V_TitlePanel.Alignment := taCenter;
  V_TitlePanelColor := clBtnFace;
  V_TitlePanel.Color := V_TitlePanelColor;
  V_TitlePanelFont := TFont.Create;
  V_TitlePanelFont.OnChange := RefreshFont;
  v_titlepanel.bevelouter := bvlowered;
  V_titlePanel.ParentColor := true;
  V_titlePanel.Color := self.Color;
  if V_titlepos = T_None then V_titlePanel.visible := false;
  V_Title := '';
  V_Updated := False;
  InsertControl(Photo);
  InsertControl(V_TitlePanel);
  Align := alNone;
  if aowner is TJvThumbView then
  begin
    width := TJvThumbView(owner).maxwidth;
    height := TJvThumbView(owner).maxheight;
  end
  else
  begin
    width := 120;
    height := 120;
  end; {}
  VMinimize := true;
  Asbutton := false;
  Left := 10;
  Top := 10;
  Visible := True;
  Bevelouter := bvraised;
  streamfiletype := gr_bmp;
  VAutoLoad := True;
end;

procedure TJVThumbNail.THSizechanged(var Message: tmessage);
begin
  calculateimagesize;
end;

destructor TJVThumbNail.destroy;
begin
  destroying := true;
  Photo.OnProgress := nil;
  VPhotoName.Free;
  V_TitlePanelFont.Onchange := nil;
  V_TitlePanelFont.Free;
  inherited destroy;
end;

procedure TJVThumbNail.PSetV_Title(const Value: string);
begin
  if Value <> V_Title then
  begin
    V_Title := Value;
    V_TitlePanel.Caption := Value;
  end;
end;

procedure TJVThumbNail.WMPaint(var Message: TWMPaint);
var
  ThumbNailTitle: string;
begin
  if not V_Updated then
  begin
    ThumbNailTitle := Title;
    if Assigned(V_OnGetTitle) then
    begin
      V_OnGetTitle(Self, filename, ThumbNailTitle);
      PSetV_Title(ThumbNailTitle);
    end
    else
    begin
      if ThumbNailtitle = '' then PSetV_Title(ExtractFileName(filename))
      else PSetV_Title(ThumbNailtitle);
    end;
    V_Updated := True;
  end;
  inherited;
end;

procedure TJVThumbNail.PSetV_TitlePanelColor(const Value: TColor);
begin
  if Value <> V_TitlePanelColor then
  begin
    V_TitlePanelColor := Value;
    V_TitlePanel.Color := Value;
  end;
end;

procedure TJVThumbNail.PSetV_TitlePanelFont(const Value: TFont);
begin
  V_TitlePanelFont.Assign(Value);
end;

procedure TJVThumbNail.Refreshfont(Sender: TObject);
begin
  V_TitlePanel.Font.Assign(V_TitlePanelFont);
end;

procedure TJVThumbNail.SetTitlePanel(P_Title: string; P_Font: TFont;
  P_Color: TColor);
begin
  PSetV_TitlePanelFont(P_Font);
  PSetV_TitlePanelColor(P_Color);
  PSetV_Title(P_Title);
  V_Updated := True;
end;

procedure TJVThumbNail.SetTitlePos(const aState: Ttitlepos);
begin
  if astate <> V_TitlePos then case astate of
      T_up: V_TitlePanel.Align := alTop;
      T_Down: V_TitlePanel.Align := alBottom;
      T_None:
        begin
          V_TitlePanel.Visible := False;
        end;
    end;
  if V_TitlePos = T_None then
    v_TitlePanel.Visible := True;
  V_titlePos := AState;
  CalculateImageSize;
end;

procedure TJVThumbNail.setminimize(min: boolean);
begin
  if assigned(photo.picture.graphic) then
  begin
    if VMinimize <> min then
    begin
      if (min) then
      begin
        if owner is TJvThumbView then
          photo.ScaleDown(TJvThumbView(owner).MaxWidth, TJvThumbView(owner).MaxHeight)
        else
          photo.ScaleDown(Width, Height); {}
      end
      else
        if VMinimize then photo.picture.loadfromfile(filename);
      VMinimize := min;
    end;
  end
  else VMinimize := min;
end;

procedure TJVThumbNail.Refresh;
begin
  CalculateImageSize;
  inherited refresh;
end;


end.

