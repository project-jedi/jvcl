{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glPage Unit 0x.1998			    component TglPageControl

 PageControl  component  that can  display  its  pages	captions  in
 3D styles with 3D borders.  Component	can display  glyphs  on  own
 captions and fill background with bitmap.  You  can  set  different
 fonts for selected page caption and for other captions.
 ===================================================================
}
unit glPage;
{$I glDEF.INC}
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, glTypes, glUtils, DrawTab, TabComm, ExtCtrls, glCommCl {$IFDEF GLVER_D4},Imglist{$ENDIF};

const
  TCM_SETTEXTCOLOR  =	(TCM_FIRST + 36);
type

  TglPageControl = class (TPageControl)
   private
     FGlyphs		: TImageList;
     FSingleGlyph	: boolean;
     FTabStyle	        : TTabStyle;
     FTabSelectedStyle  : TTabStyle;
     FWallpaper 	: TTabsWallpaper;
     FDrawGlyphsOption	: TglWallpaperOption;
     FLookLikeButtons	: boolean;
     FTabsPosition	: TglSide;
     FOptions		: TglTabOptions;
     FFontDirection	: TglLabelDir;
     FOnGetItemColor    : TglOnGetItemColorEvent;
     FOnGetItemFontColor: TglOnGetItemColorEvent;
     FOnGetGradientColors: TglOnGetGradientColors;

     GlyphsChangeLink	: TChangeLink;
     DrawTabStr 	: TDRAWTABSTRUCT;
     GlyphTmpBitmap	: TBitmap;
     FontNormal         : TFont;
     FontSelected       : TFont;
     fNotFirst	: boolean;
     aTabColors         : array[0..100] of TColor;

     function GetGlyphIndex(Index: Integer): Integer;
     procedure SetGlyphIndex(Index: Integer; imgIndex: Integer);
     procedure SetGlyphs(Value: TImageList);
     procedure SetSingleGlyph(Value: boolean);
     procedure SetDrawGlyphsOption(Value: TglWallpaperOption);
     procedure SetLookLikeButtons(Value: boolean);
     procedure SetTabsPosition(Value: TglSide);
     procedure SetOptions(Value: TglTabOptions);
     procedure SetFontDirection(Value: TglLabelDir);
     function GetFont: TFont;
     procedure SetFont(Value: TFont);
     function GetTabColor( Index: integer ): TColor;
     procedure SetTabColor(Index: integer; Value: TColor);

     procedure SmthChanged(Sender: TObject);
     procedure FontsChanged(Sender: TObject);
     procedure DrawItem(lpDrawItemStr:PDRAWITEMSTRUCT);
     procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
     procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetTabStyle(const Value: TTabStyle);
    procedure SetTabSelectedStyle(const Value: TTabStyle);
   protected
     procedure GlyphsListChanged(Sender: TObject);
     procedure WndProc(var Message: TMessage); override;
     procedure CreateParams(var Params: TCreateParams); override;
     procedure Loaded; override;
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
     fSupressDraw       : boolean;
     procedure RemakeFonts;
     constructor Create (AOwner: TComponent); override;
     destructor Destroy; override;

     property GlyphIndex[Index: Integer]: Integer read GetGlyphIndex write SetGlyphIndex;
     property TabColor[ Index: integer ]: TColor read GetTabColor write SetTabColor;
//     property GlyphState[Index: Integer]: Integer read GetGlyphState write SetGlyphState;
  published
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property SingleGlyph: boolean read FSingleGlyph write SetSingleGlyph
       default false;
    property TabStyle: TTabStyle read FTabStyle write SetTabStyle;
    property TabSelectedStyle: TTabStyle read FTabSelectedStyle write SetTabSelectedStyle;
    property Wallpaper: TTabsWallpaper read FWallpaper write FWallpaper;
    property DrawGlyphsOption: TglWallpaperOption
     read FDrawGlyphsOption write SetDrawGlyphsOption default fwoNone;
    property LookLikeButtons: boolean read FLookLikeButtons write SetLookLikeButtons
      default false;
    property TabsPosition: TglSide read FTabsPosition write SetTabsPosition
      default fsdTop;
    property Options: TglTabOptions read FOptions write SetOptions;
    property FontDirection: TglLabelDir
     read FFontDirection write SetFontDirection default fldLeftRight;
    property Font: TFont read GetFont write SetFont;
    property OnGetItemColor: TglOnGetItemColorEvent read FOnGetItemColor write FOnGetItemColor;
    property OnGetItemFontColor: TglOnGetItemColorEvent read FOnGetItemFontColor write FOnGetItemFontColor;
    property OnGetGradientColors: TglOnGetGradientColors read FOnGetGradientColors write FOnGetGradientColors;
  end;

procedure Register;

implementation
const FontDirs:array[TglSide]of TglLabelDir
	= ( fldDownUp,fldLeftRight, fldUpDown, fldLeftRight );
procedure Register;
begin
  RegisterComponents('Proba', [TglPageControl]);
end;
//*****************************************_____________LowLevel METHODS
constructor TglPageControl.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabStop:=false;
  FTabStyle		:= TTabStyle.Create(self);
  with FTabStyle do
  begin
    BackgrColor := clBtnShadow;
    Font.Color := clBtnHighlight;
    CaptionHAlign := fhaCenter;
  end;
  FTabSelectedStyle	:= TTabStyle.Create(self);
  with FTabSelectedStyle do
  begin
    BackgrColor := clBtnFace;
    Font.Color := clBtnText;
    CaptionHAlign := fhaCenter;
  end;

  FWallpaper		:= TTabsWallpaper.Create;
  FontNormal            := TFont.Create;
  FontSelected          := TFont.Create;
  DrawTabStr.Font_      := TFont.Create;

  FTabStyle.Font.Name:='Arial';
  FTabSelectedStyle.Font.Name:='Arial';

  GlyphTmpBitmap	:= TBitmap.Create;
  GlyphsChangeLink	:= TChangeLink.Create;
  GlyphsChangeLink.OnChange:=GlyphsListChanged;
  DrawTabStr.Gradient := TGradient.Create;
  //...set defaults
  FSingleGlyph:=false;

  FDrawGlyphsOption:=fwoNone;
  FTabsPosition:=fsdTop;
  FOptions:=[ftoAutoFontDirection,ftoExcludeGlyphs];
  FFontDirection:=fldLeftRight;

  FTabStyle.OnChanged	        := SmthChanged;
  FTabSelectedStyle.OnChanged   := SmthChanged;
  FTabStyle.OnFontChanged	:= FontsChanged;
  FTabSelectedStyle.OnFontChanged := FontsChanged;
  FWallpaper.OnChanged		 := SmthChanged;
  FillMemory( @aTabColors, sizeof(aTabColors), $FF );
end;

destructor TglPageControl.Destroy;
begin
  FTabStyle.Free;
  FTabSelectedStyle.Free;
  GlyphTmpBitmap.Free;
  FWallpaper.Free;
  GlyphsChangeLink.Free;
  FontNormal.Free;
  FontSelected.Free;
  DrawTabStr.Font_.Free;
  if Assigned(DrawTabStr.Gradient) then DrawTabStr.Gradient.Free;
  inherited;
end;

procedure TglPageControl.SmthChanged;
begin Invalidate; end;

procedure TglPageControl.FontsChanged;
begin RemakeFonts; Invalidate; end;

//---------------------------------------------------special procs
procedure TglPageControl.CreateParams(var Params: TCreateParams);
const PosStyles : array[TglSide]of DWORD =
 ( TCS_VERTICAL, 0, TCS_VERTICAL or TCS_RIGHT, TCS_BOTTOM {or TCS_SCROLLOPPOSITE or TCS_BUTTONS} );
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if LookLikeButtons then Style := Style or TCS_BUTTONS;
    Style := Style or TCS_OWNERDRAWFIXED or PosStyles[FTabsPosition];
  end;
end;// {TCS_HOTTRACK-?};

procedure TglPageControl.Loaded;
begin
  inherited Loaded; RemakeFonts;
  if Assigned(Wallpaper.Bitmap)and(not Wallpaper.Bitmap.Empty)
   then Wallpaper.bmp := Wallpaper.Bitmap;
end;

procedure TglPageControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Wallpaper)and(AComponent = Wallpaper.Image) and (Operation = opRemove) then Wallpaper.Image := nil;
end;

procedure TglPageControl.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Pointer(Message.DrawItemStruct));
end;

procedure TglPageControl.WndProc(var Message: TMessage);
var
  GlyphID:integer;
begin
  inherited WndProc(Message);
  with Message do
    case Msg of
      TCM_INSERTITEM:
      begin  result:=0;
	if not Assigned(FGlyphs) then exit;
	GlyphID:=-1;
	if FSingleGlyph then GlyphID:=0
	else if wParam < FGlyphs.Count then GlyphID:=wParam;
	if GlyphID=-1 then exit;
	TTCItem(Pointer(Message.lParam)^).iImage:=GlyphID;
	TTCItem(Pointer(Message.lParam)^).Mask:=TCIF_IMAGE;
	SendMessage( handle, TCM_SETITEM, wParam, lParam );
      end;
      TCM_DELETEITEM: begin end;
      TCM_DELETEALLITEMS: begin end;
    end;
end;

procedure TglPageControl.GlyphsListChanged(Sender: TObject);
begin
  if HandleAllocated then SendMessage(Handle, TCM_SETIMAGELIST, 0,
				      Longint(TImageList(Sender).Handle));
end;

procedure TglPageControl.DrawItem(lpDrawItemStr:PDRAWITEMSTRUCT);
//var lpTabBoxStyle:^TTabBoxStyle;
var
  FontColor: TColor;
begin
  if fSupressDraw then exit;
  with lpDrawItemStr^ do
  if CtlType=ODT_TAB then
  begin
    //fLoaded:=true; Options:=NewOptions;
    DrawTabStr.lpDrawItemStr := lpDrawItemStr;
    DrawTabStr.Caption:=Tabs[ItemID];

    if GlyphIndex[ItemID]<>-1 then
    begin
      FGlyphs.GetBitmap( GlyphIndex[ItemID], GlyphTmpBitmap );
      DrawTabStr.Glyph:=GlyphTmpBitmap;
    end else DrawTabStr.Glyph:=nil;

    if (itemState and ODS_DISABLED)<>0 then
    begin
      DrawTabStr.BoxStyle := FTabStyle;
      DrawTabStr.Font_.Assign(FontNormal);
    end
    else if (itemState and ODS_SELECTED)<>0 then begin
      DrawTabStr.BoxStyle := FTabSelectedStyle;
      DrawTabStr.Font_.Assign(FontSelected);
    end
    else begin
      DrawTabStr.BoxStyle := FTabStyle;
      DrawTabStr.Font_.Assign(FontNormal);
    end;

    if Assigned(OnGetItemFontColor) then
    begin
      OnGetItemFontColor( self, ItemID, FontColor );
      DrawTabStr.Font_.Color := FontColor;
    end;

    DrawTabStr.GlyphOption := FDrawGlyphsOption;
    DrawTabStr.Wallpaper:=FWallpaper;
    DrawTabStr.ClientR:=ClientRect;
    DrawTabStr.TabsCount:=Tabs.Count;
    DrawTabStr.fButton:=LookLikeButtons;
    DrawTabStr.Position:=TabsPosition;
    DrawTabStr.Options:=Options;
    DrawTabStr.FontDirection:=FontDirection;

    if Assigned(OnGetGradientColors) then OnGetGradientColors( self, ItemID, DrawTabStr.Gradient);

    if Assigned(OnGetItemColor) then OnGetItemColor( self, ItemID, DrawTabStr.BackgrColor_ )
    else
      if aTabColors[ItemID] <> -1 then DrawTabStr.BackgrColor_ := aTabColors[ItemID]
                                  else DrawTabStr.BackgrColor_ := DrawTabStr.BoxStyle.BackgrColor;

    {$IFDEF GLVER_D4} if Style = tsFlatButtons then DrawTabStr.FlatButtons := true; {$ELSE} DrawTabStr.FlatButtons := false;{$ENDIF}

    DrawOwnTab( DrawTabStr ); //FWallpaper.IncludeBevels
  end;
end;

procedure TglPageControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if ftoInheriteTabFonts in Options then
  begin
    FTabStyle.Font.Assign(inherited Font);
    FTabSelectedStyle.Font.Assign(inherited Font);
    // Disabled.Assign(inherited Font);
    RemakeFonts;
  end;
end;

procedure TglPageControl.RemakeFonts;
const
  RadianEscapments:array [TgllabelDir] of integer = (0,-1800,-900,900);
begin
  if csReading in ComponentState then exit;
  if fNotFirst then DeleteObject( FTabStyle.Font.Handle );
  fNotFirst:=true;

  FontNormal.Handle := CreateRotatedFont( FTabStyle.Font, RadianEscapments[FFontDirection]);
  FontNormal.Color := FTabStyle.Font.Color;
  FontSelected.Handle := CreateRotatedFont( FTabSelectedStyle.Font, RadianEscapments[FFontDirection]);
  FontSelected.Color := FTabSelectedStyle.Font.Color;
end;
//*****************************************_____________PROPERTY METHODS
procedure TglPageControl.SetGlyphs(Value: TImageList);
var i: integer;
label SkipAutoGlypsSet;
begin
  if Assigned(FGlyphs) then  FGlyphs.UnregisterChanges(GlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
  begin
    FGlyphs.RegisterChanges(GlyphsChangeLink);
    SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(FGlyphs.Handle));
    for i:=0 to min( Tabs.Count-1, FGlyphs.Count-1) do
      if GlyphIndex[i]<>-1 then goto SkipAutoGlypsSet;
    SetSingleGlyph(FSingleGlyph);
    SkipAutoGlypsSet:
  end
  else SendMessage(Handle, TCM_SETIMAGELIST, 0, Longint(0));
end;

procedure TglPageControl.SetGlyphIndex( Index: Integer; imgIndex: Integer );
var
  r	: TRect;
  Item	: TTCItem;
begin
  Item.iImage := imgIndex;
  Item.mask := TCIF_IMAGE;
  SendMessage( Handle, TCM_SETITEM, Index, Longint(@Item) );
  SendMessage( Handle, TCM_GETITEMRECT, Index, Longint(@r) );
  InvalidateRect( Handle, @r, true );
end;

function TglPageControl.GetGlyphIndex( Index: Integer ): Integer;
var
  imgItem: TTCItem;
begin
  if Assigned(FGlyphs) then
  begin
    imgItem.mask := TCIF_IMAGE;
    SendMessage( Handle, TCM_GETITEM, Index, Longint(@imgItem) );
    Result := imgItem.iImage;
  end
  else Result := -1;
end;

procedure TglPageControl.SetSingleGlyph(Value: boolean);
var i: integer;
begin
  FSingleGlyph:=Value;
  if (Tabs=nil)or(FGlyphs=nil) then exit;
  if FSingleGlyph then
    for i:=0 to Tabs.Count-1 do GlyphIndex[i]:=0
  else
    for i:=0 to Tabs.Count-1 do
      if FGlyphs.Count >= i then GlyphIndex[i]:=i else break;
end;

procedure TglPageControl.SetDrawGlyphsOption(Value: TglWallpaperOption);
begin
  if FDrawGlyphsOption = Value then exit;
  FDrawGlyphsOption := Value; Invalidate;
end;

procedure TglPageControl.SetLookLikeButtons(Value: boolean);
begin
  if FLookLikeButtons = Value then exit;
  FLookLikeButtons := Value;
  RecreateWnd;
end;

procedure TglPageControl.SetTabsPosition(Value: TglSide);
begin
  if FTabsPosition = Value then exit;
  FTabsPosition := Value; RecreateWnd;
  if (ftoAutoFontDirection in FOptions)and not(csLoading in ComponentState) then
    FontDirection := FontDirs[TabsPosition];
end;

procedure TglPageControl.SetOptions(Value: TglTabOptions);
begin
  if FOptions = Value then exit; FOptions := Value;
  if ftoAutoFontDirection in FOptions then
    FontDirection := FontDirs[TabsPosition];
  Invalidate;
end;

procedure TglPageControl.SetFontDirection(Value: TgllabelDir);
begin
  if FFontDirection = Value then exit;
  FFontDirection := Value; RemakeFonts;
  Invalidate;
end;

function TglPageControl.GetFont: TFont;
begin Result := inherited Font; end;

procedure TglPageControl.SetFont(Value: TFont);
begin
  inherited Font := Value;
  if ftoInheriteTabFonts in Options then
  begin
    FTabStyle.Font.Assign(inherited Font);
    FTabSelectedStyle.Font.Assign(inherited Font);
  end;
end;

function TglPageControl.GetTabColor( Index: integer ): TColor;
begin
  if Index<100 then Result := aTabColors[Index] else Result := -1;
end;

procedure TglPageControl.SetTabColor(Index: integer; Value: TColor);
var
  TCItem: TTCItem;
begin
  if (Index<100)and(TabColor[Index] <> Value) then aTabColors[Index] := Value
                                              else exit;
  if not fSupressDraw then
  begin
//  Repaint;
    TCItem.mask := TCIF_TEXT;
    TCItem.pszText := PChar(Tabs[Index]);
    SendMessage( Handle, TCM_SETITEM, Index, Longint(@TCItem));
  end;
end;

procedure TglPageControl.SetTabStyle(const Value: TTabStyle);
begin
  FTabStyle := Value;
  RemakeFonts;
end;

procedure TglPageControl.SetTabSelectedStyle(const Value: TTabStyle);
begin
  FTabSelectedStyle := Value;
  RemakeFonts;
end;

end.
