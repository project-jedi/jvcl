{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 ...common 'Globus Lib' classes declaration
 ===================================================================
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//...created: 22.07.1998
//...last modified: 26.11.1998
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unit glCommCl;
{$I glDEF.INC}
interface
uses Windows,Graphics,Controls,Classes,ExtCtrls,glTypes;

type
TTwainColors = class;
TCustomGradient = class;
TGradient = class;
TThreeDGradient = class;
T2DAlign = class;
TPointClass = class;
TglBevel = class;
TglExtBevel = class;
TIllumination = class;
TglLabelTextStyles = class;
TglCustomTextColors = class;
TglSimleLabelColors = class;
TglCustomLabelColors = class;
TglLabelColors = class;
TglGrBoxColors = class;
TglListBoxItemStyle  = class;
TglAskListBoxItemStyle	= class;
TglCustomBoxStyle	= class;
TglCustomTextBoxStyle	= class;
TglTextBoxStyle = class;
TglBevelLines = class;
//*************************************{ . TTwainColors . }
TTwainColors = class(TPersistent)
private
  FFromColor	:TColor;
  FToColor	:TColor;
  procedure SeTglomColor( Value :TColor );
  procedure SetToColor( Value :TColor );
public
  FRGBFromColor :Longint;
  FRGBToColor	:Longint;
  OnChanged	:TNotifyEvent;
  constructor Create; virtual;
published
  property FromColor :TColor read FFromColor write SeTglomColor
    default $00808080;
  property ToColor :TColor read FToColor write SetToColor
    default 0;
end;
//*************************************{ . TCustomGradient . }
TCustomGradient = class(TTwainColors)
private
  FBufferedDraw : boolean;
  FSteps	: integer;
  FPercentFilling: TglPercent;
  FBrushStyle   : TBrushStyle;
  procedure SetActive( Value :boolean );
  procedure SetOrientation( Value :TglGradientDir );
  procedure SetSteps( Value: integer );
  procedure SetPercentFilling( Value: TglPercent );
  procedure SetBrushStyle( Value: TBrushStyle );
public
  FOrientation	: TglGradientDir; //...public!
  FActive	: boolean;
  fReverse      : boolean;
  procedure TextOut( DC: HDC; Str:string; TextR:TRect; x, y :integer );
  function GetColorFromGradientLine( GradientLineWidth, Position: word ): COLORREF;

  constructor Create; override;
//  destructor Destroy;override;
protected
  property Active :boolean read FActive write SetActive;
  property BufferedDraw :boolean read FBufferedDraw write FBufferedDraw
    default false;
  property Orientation :TglGradientDir read FOrientation write SetOrientation;
  property Steps: integer read FSteps write SetSteps default 255;
  property PercentFilling: TglPercent read FPercentFilling write SetPercentFilling
   default 100;
  property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle
   default bsSolid;

end;
//*************************************{ . TGradient . }
TGradient = class(TCustomGradient)
private
public
  procedure Draw(DC: HDC; r: TRect; PenStyle, PenWidth: integer);
published
  property Active;
  property BufferedDraw;
  property Orientation;
  property Steps;
  property PercentFilling;
  property BrushStyle;
end;
//*************************************{ . TThreeDGradient . }
TThreeDGradient = class(TCustomGradient)
private
  FDepth :word;
  FGType :TThreeDGradientType;
  procedure SetDepth(Value: word);
  procedure SetGType(Value: TThreeDGradientType);
public
  constructor Create; override;
published
  property Depth: word
   read FDepth write SetDepth default 16;
  property GType :TThreeDGradientType read FGType write SetGType default fgtFlat;
end;
//*************************************{ . T2DAlign . }
 T2DAlign = class(TPersistent)
private
  FHorizontal: TglHorAlign;
  FVertical: TglVertAlign;
  procedure SetHorizontal(Value: TglHorAlign);
  procedure SetVertical(Value: TglVertAlign);
public
  OnChanged	:TNotifyEvent;
  constructor Create;
published
   property Horizontal :TglHorAlign read FHorizontal write SetHorizontal
    default fhaLeft;
   property Vertical :TglVertAlign read FVertical write SetVertical
    default fvaTop;
end;
//*************************************{ . TPointClass . }
 TPointClass = class(TPersistent)
private
  FX: integer;
  FY: integer;
  procedure SetX(Value: integer);
  procedure SetY(Value: integer);
public
  OnChanged	:TNotifyEvent;
published
   property X :integer read FX write SetX;
   property Y :integer read FY write SetY;
end;
//*************************************{ . TglBevel . }
TglBevel = class(TPersistent)
private
  FInner      : TPanelBevel;
  FOuter      : TPanelBevel;
  FSides      : TglSides;
  FBold       : boolean;
  procedure SetInner(Value: TPanelBevel);
  procedure SetOuter(Value: TPanelBevel);
  procedure SetSides(Value: TglSides);
  procedure SetBold(Value: boolean);
public
  OnChanged	:TNotifyEvent;
  constructor Create; virtual;
published
  property Inner: TPanelBevel read FInner write SetInner stored true;//    default bvLowered;
  property Outer: TPanelBevel read FOuter write SetOuter stored true;//    default bvNone;
  property Sides: TglSides read FSides write SetSides stored true default ALLGLSIDES;
  property Bold: boolean read FBold write SetBold stored true;//  default false;
end;
//*************************************{ . TglExtBevel . }
TglExtBevel = class(TglBevel)
private
    FActive		: boolean;
    FBevelPenStyle	: TPenStyle;
    FBevelPenWidth	: word;
    FInteriorOffset	: word;
    procedure SetActive(Value: boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: word);
    procedure SetInteriorOffset(Value: word);
  public
    constructor Create; override;
  published
    property Active: boolean read FActive write SetActive
      default true;
    property BevelPenStyle: TPenStyle read FBevelPenStyle write SetBevelPenStyle
      default psSolid;
    property BevelPenWidth: word read FBevelPenWidth write SetBevelPenWidth
      default 1;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset
      default 0;
  end;
//*************************************{ . TIllumination . }
TIllumination = class(T2DAlign)
private
  procedure SetShadowDepth(Value: integer);
public
  FShadowDepth	: integer;
  OnChanged	:TNotifyEvent;
  constructor Create;
published
  property ShadowDepth: integer
   read FShadowDepth write SetShadowDepth default 2;
end;
//*************************************{ . TglLabelTextStyles . }
TglLabelTextStyles = class(TPersistent)
private
  FPassive  : TglTextStyle;
  FActive   : TglTextStyle;
  FDisabled : TglTextStyle;
  procedure SetPassive(Value: TglTextStyle);
  procedure SetActive(Value: TglTextStyle);
  procedure SetDisabled(Value: TglTextStyle);
public
  OnChanged	:TNotifyEvent;
  constructor Create;
published
  property Passive: TglTextStyle read FPassive write SetPassive
    default fstRaised;
  property Active: TglTextStyle read FActive write SetActive
    default fstRaised;
  property Disabled: TglTextStyle read FDisabled write SetDisabled
    default fstPushed;
end;
//*************************************{ . TglCustomTextColors . }
TglCustomTextColors = class(TPersistent)
private
  FText 	: TColor;
  FTextDisabled : TColor;
  FDelineate	: TColor;
  FBackground	: TColor;
public
  FHighlight		: TColor;
  FShadow		: TColor;
private
  procedure SetText(Value: TColor);
  procedure SetTextDisabled(Value: TColor);
  procedure SetDelineate(Value: TColor);
  procedure SetBackground(Value: TColor);
  procedure SetHighlight(Value: TColor);
  procedure SetShadow(Value: TColor);
public
  OnChanged :TNotifyEvent;
  constructor Create; virtual;
protected
  property Text: TColor read FText write SetText
   default clBlack;
  property TextDisabled: TColor read FTextDisabled write SetTextDisabled
   default clGray;
  property Delineate: TColor read FDelineate write SetDelineate
   default clWhite;
  property Shadow: TColor
   read FShadow write SetShadow default clBtnShadow;
  property Highlight: TColor
   read FHighlight write SetHighlight default clBtnHighlight;
  property Background: TColor
   read FBackground write SetBackground default clBtnFace;

end;
//*************************************{ . TglCustomLabelColors . }
TglSimleLabelColors = class(TglCustomTextColors)
published
//  property Text stored true;
  property Delineate stored true;
  property Shadow stored true;
  property Highlight;
  property Background stored true;
end;

//*************************************{ . TglCustomLabelColors . }
TglCustomLabelColors = class(TglCustomTextColors)
private
  FTextActive		: TColor;
  FDelineateActive	: TColor;
  FAutoHighlight	: boolean;
  FAutoShadow		: boolean;
  FBackgroundActive	: TColor;
public
  FColorHighlightShift	: integer;
  FColorShadowShift	: integer;
private
  procedure SetTextActive(Value: TColor);
  procedure SetDelineateActive(Value: TColor);
  procedure SetBackgroundActive(Value: TColor);
  procedure SetAutoHighlight(Value: boolean);
  procedure SetAutoShadow(Value: boolean);
  procedure SetColorHighlightShift(Value: integer);
  procedure SetColorShadowShift(Value: integer);
public
  OnChanged :TNotifyEvent;
  constructor Create; override;
protected
  property TextActive: TColor read FTextActive write SetTextActive
   default clBlack;
  property DelineateActive: TColor read FDelineateActive write SetDelineateActive
   default clWhite;
  property AutoHighlight: boolean
   read FAutoHighlight write SetAutoHighlight default false;
  property AutoShadow: boolean
   read FAutoShadow write SetAutoShadow default false;
  property ColorHighlightShift: integer
   read FColorHighlightShift write SetColorHighlightShift default 40;
  property ColorShadowShift: integer
   read FColorShadowShift write SetColorShadowShift default 60;
  property BackgroundActive: TColor
   read FBackgroundActive write SetBackgroundActive default clBtnFace;
end;
//*************************************{ . TglLabelColors . }
TglLabelColors = class(TglCustomLabelColors)
published
  property Text;
  property TextDisabled;
  property Delineate;
  property Shadow;
  property Highlight;
  property Background;
  property TextActive;
  property DelineateActive;
  property AutoHighlight;
  property AutoShadow;
  property ColorHighlightShift;
  property ColorShadowShift;
  property BackgroundActive;
end;
//*************************************{ . TglGrBoxColors . }
TglGrBoxColors = class(TglCustomLabelColors)
private
  FCaption       : TColor;
  FCaptionActive : TColor;
  FClient        : TColor;
  FClientActive  : TColor;
  procedure SetCaption(Value: TColor);
  procedure SetCaptionActive(Value: TColor);
  procedure SetClient(Value: TColor);
  procedure SetClientActive(Value: TColor);
public
  constructor Create; override;
published
  property Text;
  property Delineate;
  property Shadow;
  property Highlight;
//  property Background;
  property TextActive;
  property DelineateActive;
//  property AutoHighlight;
//  property AutoShadow;
//  property ColorHighlightShift;
//  property ColorShadowShift;
//  property BackgroundActive;
  property Caption: TColor read FCaption write SetCaption;
  property CaptionActive: TColor read FCaptionActive write SetCaptionActive;
  property Client: TColor read FClient write SetClient;
  property ClientActive: TColor read FClientActive write SetClientActive;
end;
//*************************************{ . TglListItemStyle . }
TglCustomListBoxItemStyle  = class(TPersistent)
  private
    FColor		: TColor;
    FDelineateColor	: TColor;
    FFont		: TFont;
    FBevel		: TglBevel;
    FTextStyle		: TglTextStyle;
    FOnChanged		: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetDelineateColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetTextStyle(Value: TglTextStyle);
  protected
    procedure SetOnChanged(Value: TNotifyEvent); virtual;
  public
    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor;
    property DelineateColor: TColor read FDelineateColor write SetDelineateColor;
    property Font: TFont read FFont write SetFont;
    property Bevel: TglBevel read FBevel write FBevel;
    property TextStyle: TglTextStyle read FTextStyle write SetTextStyle;
end;
//*************************************{ . TglListItemStyle . }
TglListBoxItemStyle  = class(TglCustomListBoxItemStyle)
  private
    FGradient		: TGradient;
  protected
    procedure SetOnChanged(Value: TNotifyEvent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Gradient: TGradient read FGradient write FGradient;
end;
//*************************************{ . TglListItemStyle . }
TglAskListBoxItemStyle	= class(TglCustomListBoxItemStyle)
  private
    FBtnColor		: TColor;
    FBtnFont		: TFont;
    FBtnTextStyle	: TglTextStyle;
    procedure SetBtnColor(Value: TColor);
    procedure SetBtnFont(Value: TFont);
    procedure SetBtnTextStyle(Value: TglTextStyle);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property BtnColor: TColor read FBtnColor write SetBtnColor;
    property BtnFont: TFont read FBtnFont write SetBtnFont;
    property BtnTextStyle: TglTextStyle read FBtnTextStyle write SetBtnTextStyle;
end;
//*************************************{ . TglCustomBoxStyle . }
TglCustomBoxStyle	= class(TglBevel)
private
  FPenStyle		: TPenStyle;
  FHighlightColor	: TColor;
  FShadowColor		: TColor;
  procedure SetPenStyle(Value: TPenStyle);
  procedure SetHighlightColor(Value: TColor);
  procedure SetShadowColor(Value: TColor);
public
  OnChanged		: TNotifyEvent;
  constructor Create; override;
protected
  property PenStyle: TPenStyle read FPenStyle write SetPenStyle
   default psSolid;
  property HighlightColor: TColor read FHighlightColor write SetHighlightColor
   default clBtnHighlight;
  property ShadowColor: TColor read FShadowColor write SetShadowColor
   default clBtnShadow;
end;
//*************************************{ . TglTextBoxStyle . }
TglCustomTextBoxStyle	= class(TglCustomBoxStyle)
private
  FTextColor		: TColor;
  FBackgroundColor	: TColor;
  procedure SetTextColor(Value: TColor);
  procedure SetBackgroundColor(Value: TColor);
public
  constructor Create; override;
protected
  property TextColor: TColor read FTextColor write SetTextColor
   default clBlack;
  property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor
   default clWindow;
end;
//*************************************{ . TglCustomBoxStyle . }
TglTextBoxStyle = class(TglCustomTextBoxStyle)
published
  property Inner;
  property Outer;
  property Sides;
  property Bold;
  property PenStyle;
  property TextColor;
  property BackgroundColor;
  property HighlightColor;
  property ShadowColor;
end;
//*************************************{ .TglBevelLines. }
TglBevelLines = class(TPersistent)
private
    FCount		: cardinal;
    FStep		: cardinal;
    FOrigin		: TglOrigin;
    FStyle		: TPanelBevel;
    FBold		: boolean;
    FThickness		: byte;
    FIgnoreBorder	: boolean;

    procedure SetCount(Value: cardinal);
    procedure SetStep(Value: cardinal);
    procedure SetOrigin(Value: TglOrigin);
    procedure SetStyle(Value: TPanelBevel);
    procedure SetBold(Value: boolean);
    procedure SetThickness(Value: byte);
    procedure SetIgnoreBorder(Value: boolean);
public
  OnChanged: TNotifyEvent;
  constructor Create;
published
  property Count: cardinal read FCount write SetCount
   default 0;
  property Step: cardinal read FStep write SetStep
   default 0;
  property Origin: TglOrigin read FOrigin write SetOrigin
   default forLeftTop;
  property Style: TPanelBevel read FStyle write SetStyle
   default bvLowered;
  property Bold: boolean read FBold write SetBold
   default false;
  property Thickness: byte read FThickness write SetThickness
   default 1;
  property IgnoreBorder: boolean read FIgnoreBorder write SetIgnoreBorder
   default false;
end;
//*************************************{ .. }
implementation
uses glUTILS;
//______________________________________{ . TTwainColors methods . }
constructor TTwainColors.Create;
begin
  inherited Create;
  //...set defaults
  FFromColor	 := $00808080; FRGBFromColor := ColorToRGB( FFromColor );
  FToColor	 := 0;	       FRGBToColor := ColorToRGB( FToColor );
end;

procedure TTwainColors.SeTglomColor( Value :TColor );
begin
  if FFromColor = Value then exit;
  FFromColor := Value; FRGBFromColor := ColorToRGB( Value );
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TTwainColors.SetToColor( Value :TColor );
begin
  if FToColor = Value then exit;
  FToColor := Value; FRGBToColor := ColorToRGB( Value );
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TCustomGradient methods . }
constructor TCustomGradient.Create;
begin
  inherited Create;
  //...set defaults
  FActive	  := false;
  FBufferedDraw   := false;
  FOrientation	  := fgdHorizontal;
  FSteps	  := 255;
  FPercentFilling := 100;
  FBrushStyle     := bsSolid;
end;

//destructor TCustomGradient.Destroy;
//begin inherited Destroy; end;

//procedure TCustomGradient.Free;
//begin if self<>nil then Destroy; end;

procedure TCustomGradient.SetActive( Value :boolean );
begin
  if FActive = Value then exit;
  FActive := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetOrientation( Value :TglGradientDir );
begin
  if FOrientation = Value then exit;
  FOrientation := Value;
  if FActive and Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetSteps( Value: integer );
begin
  if Value > 255 then Value := 255
  else if Value < 1 then Value := 1;
  if FSteps = Value then exit;
  FSteps := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetPercentFilling( Value: TglPercent );
begin
  if FPercentFilling = Value then exit;
  FPercentFilling := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetBrushStyle( Value: TBrushStyle );
begin
  FBrushStyle := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

function TCustomGradient.GetColorFromGradientLine
		   ( GradientLineWidth, Position: word ): COLORREF;
var
  c1F,c2F,c3F	      :byte;
  c1T,c2T,c3T	      :byte;
  Step1,Step2,Step3   :Single;
begin
  c1F := Byte( self.FRGBFromColor ) ;
  c2F := Byte( WORD(self.FRGBFromColor) shr 8);
  c3F := Byte( self.FRGBFromColor shr 16 );
  c1T := Byte( self.FRGBToColor ) ;
  c2T := Byte( WORD(self.FRGBToColor) shr 8);
  c3T := Byte( self.FRGBToColor shr 16 );

  Step1:=(c1T-c1F)/GradientLineWidth;
  Step2:=(c2T-c2F)/GradientLineWidth;
  Step3:=(c3T-c3F)/GradientLineWidth;

  Result:=RGB( trunc(c1F+Step1*Position),
	       trunc(c2F+Step2*Position),
	       trunc(c3F+Step3*Position) );
end;


procedure TCustomGradient.TextOut( DC: HDC; Str:string; TextR:TRect; x, y :integer );
var
  i,Steps	      :integer;
  r		      :TRect;
  c1F,c2F,c3F	      :byte;
  c1T,c2T,c3T	      :byte;
  c1,c2,c3	      :Single;
  Step1,Step2,Step3   :Single;
  OldTextColor	      :TCOLORREF;
begin
  if (not Active)or(GetDeviceCaps( DC, BITSPIXEL )<16) then
  begin Windows.TextOut( DC, x,y, PChar(str), Length(str)); exit; end;
  r := TextR;
  c1F := Byte( FRGBFromColor ) ;
  c2F := Byte( WORD(FRGBFromColor) shr 8);
  c3F := Byte( FRGBFromColor shr 16 );
  c1T := Byte( FRGBToColor ) ;
  c2T := Byte( WORD(FRGBToColor) shr 8);
  c3T := Byte( FRGBToColor shr 16 );

  c1:=c1F; c2:=c2F; c3:=c3F;
  if FOrientation = fgdVertical then Steps:=r.right-r.left
				else Steps:=r.bottom-r.top;
  Step1:=(c1T-c1F)/Steps; Step2:=(c2T-c2F)/Steps; Step3:=(c3T-c3F)/Steps;

  OldTextColor := SetTextColor( DC, 0 );
  Steps := MulDiv( Steps, PercentFilling, 100 );
  for i:=0 to Steps do
  begin
    SetTextColor( DC, RGB( trunc(c1),trunc(c2),trunc(c3)) );

    if FOrientation = fgdVertical then
      begin r.left:=i; r.right:=r.left+1; end
    else
      begin r.top:=i; r.bottom:=r.top+1; end;

      Windows.ExtTextOut( DC, x, y,  ETO_CLIPPED, @r,
			  PChar(str), Length(str), nil);
      c1 := c1 + Step1; c2 := c2 + Step2; c3 := c3 + Step3;
  end;
  SetTextColor( DC, OldTextColor );
end;
//______________________________________{ . TThreeDGradient methods . }
constructor TThreeDGradient.Create;
begin
  inherited Create;
  Depth:=16;
  FGType:=fgtFlat;
  FActive:=true;
end;

procedure TThreeDGradient.SetGType(Value: TThreeDGradientType);
begin
  if FGType = Value then exit;
  FGType := Value; if FActive and Assigned(OnChanged) then OnChanged(self);
end;

procedure TThreeDGradient.SetDepth(Value: word);
begin
  if FDepth = Value then exit;
  FDepth := Value; if FActive and Assigned(OnChanged) then OnChanged(self);
end;

//______________________________________{ . T2DAlign methods . }
constructor T2DAlign.Create;
begin
  inherited Create;
  //...set defaults
  FHorizontal := fhaLeft;
  FVertical := fvaTop;
end;

procedure T2DAlign.SetHorizontal(Value: TglHorAlign);
begin
  if FHorizontal = Value then exit; FHorizontal := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure T2DAlign.SetVertical(Value: TglVertAlign);
begin
  if FVertical = Value then exit; FVertical := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TPointClass methods . }
procedure TPointClass.SetX(Value: integer);
begin
  if FX = Value then exit; FX := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TPointClass.SetY(Value: integer);
begin
  if FY = Value then exit; FY := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglBevel methods . }
constructor TglBevel.Create;
begin
  inherited;
  //..defaults
  FSides := ALLGLSIDES;
end;

procedure TglBevel.SetOuter(Value: TPanelBevel);
begin
  if FOuter=Value then exit; FOuter:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglBevel.SetInner(Value: TPanelBevel);
begin
  if FInner=Value then exit; FInner:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglBevel.SetSides(Value: TglSides);
begin
  if FSides=Value then exit; FSides:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglBevel.SetBold(Value: boolean);
begin
  if FBold=Value then exit; FBold:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

//______________________________________{ . TIllumination methods . }
constructor TIllumination.Create;
begin
  inherited;
  //..defaults
  FShadowDepth		:= 2;
end;

procedure TIllumination.SetShadowDepth(Value: integer);
begin
  if Value < 0 then Value:=0;
  if FShadowDepth=Value then exit; FShadowDepth:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglLabelTextStyles methods . }
constructor TglLabelTextStyles.Create;
begin
  inherited;
  //..defaults
  FActive   := fstRaised;
  FPassive  := fstRaised;
  FDisabled := fstPushed;
end;

procedure TglLabelTextStyles.SetPassive(Value: TglTextStyle);
begin
  if FPassive=Value then exit; FPassive:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglLabelTextStyles.SetActive(Value: TglTextStyle);
begin
  if FActive=Value then exit; FActive:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglLabelTextStyles.SetDisabled(Value: TglTextStyle);
begin
  if FDisabled=Value then exit; FDisabled:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglCustomTextColors methods . }

constructor TglCustomTextColors.Create;
begin
  inherited;
  //..defaults
  FText 	:= clBlack;
  FTextDisabled := clGray;
  FDelineate	:= clWhite;
  FHighlight	:= clBtnHighlight;
  FShadow	:= clBtnShadow;
  FBackground	:= clBtnFace;
end;

procedure TglCustomTextColors.SetText(Value: TColor);
begin
  if FText=Value then exit; FText:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomTextColors.SetTextDisabled(Value: TColor);
begin
  if FTextDisabled=Value then exit; FTextDisabled:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomTextColors.SetDelineate(Value: TColor);
begin
  if FDelineate=Value then exit; FDelineate:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomTextColors.SetHighlight(Value: TColor);
begin
  if FHighlight=Value then exit; FHighlight:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomTextColors.SetShadow(Value: TColor);
begin
  if FShadow=Value then exit; FShadow:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomTextColors.SetBackground(Value: TColor);
begin
  if FBackground=Value then exit; FBackground:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglCustomLabelColors . }
constructor TglCustomLabelColors.Create;
begin
  inherited;
  //..defaults
  FTextActive		:= clBlack;
  FDelineateActive	:= clWhite;
  FAutoHighlight	:= false;
  FAutoShadow		:= false;
  FColorHighlightShift	:= 40;
  FColorShadowShift	:= 60;
  FBackgroundActive	:= clBtnFace;
end;

procedure TglCustomLabelColors.SetTextActive(Value: TColor);
begin
  if FTextActive=Value then exit; FTextActive:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetDelineateActive(Value: TColor);
begin
  if FDelineateActive=Value then exit; FDelineateActive:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetAutoHighlight(Value: boolean);
begin
  if FAutoHighlight=Value then exit; FAutoHighlight:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetAutoShadow(Value: boolean);
begin
  if FAutoShadow=Value then exit; FAutoShadow:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetColorHighlightShift(Value: integer);
begin
  if FColorHighlightShift=Value then exit; FColorHighlightShift:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetColorShadowShift(Value: integer);
begin
  if FColorShadowShift=Value then exit; FColorShadowShift:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomLabelColors.SetBackgroundActive(Value: TColor);
begin
  if FBackgroundActive=Value then exit; FBackgroundActive:=Value;
  if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglGrBoxColors . }
constructor TglGrBoxColors.Create;
begin
  inherited;
  //..defaults
  FCaption := clBtnFace;
  FCaptionActive := clBtnFace;
  FClient := clBtnFace;
  FClientActive := clBtnFace;
end;

procedure TglGrBoxColors.SetCaption(Value: TColor);
begin FCaption := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglGrBoxColors.SetCaptionActive(Value: TColor);
begin FCaptionActive := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglGrBoxColors.SetClient(Value: TColor);
begin FClient := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglGrBoxColors.SetClientActive(Value: TColor);
begin FClientActive := Value; if Assigned(OnChanged) then OnChanged(self); end;
//______________________________________{ . TglExtBevel . }
constructor TglExtBevel.Create;
begin
  inherited;
  //..defaults
  FActive := true;
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
end;

procedure TglExtBevel.SetActive(Value: boolean);
begin
  if FActive = Value then exit;
  FActive := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglExtBevel.SetBevelPenStyle(Value: TPenStyle);
begin
  if FBevelPenStyle = Value then exit;
  FBevelPenStyle := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglExtBevel.SetBevelPenWidth(Value: word);
begin
  if FBevelPenWidth = Value then exit;
  FBevelPenWidth := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglExtBevel.SetInteriorOffset(Value: word);
begin
  if FInteriorOffset = Value then exit;
  FInteriorOffset := Value; if Assigned(OnChanged) then OnChanged(self);
end;

//______________________________________{ . TglCustomListBoxItemStyle . }
constructor TglCustomListBoxItemStyle.Create;
begin
  inherited Create;
  FBevel := TglBevel.Create;
  FFont := TFont.Create;
end;

destructor TglCustomListBoxItemStyle.Destroy;
begin
  FFont.Free; FBevel.Free; inherited;
end;

procedure TglCustomListBoxItemStyle.SetOnChanged(Value: TNotifyEvent);
begin
  FOnChanged := Value; FBevel.OnChanged := Value;
end;

procedure TglCustomListBoxItemStyle.SetColor(Value: TColor);
begin
  if FColor = Value then exit;
  FColor := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomListBoxItemStyle.SetDelineateColor(Value: TColor);
begin
  if FDelineateColor = Value then exit;
  FDelineateColor := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomListBoxItemStyle.SetFont(Value: TFont);
begin
  FFont.Assign(Value); if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglCustomListBoxItemStyle.SetTextStyle(Value: TglTextStyle);
begin
  FTextStyle := Value; if Assigned(OnChanged) then OnChanged(self);
end;
//______________________________________{ . TglListBoxItemStyle . }
constructor TglListBoxItemStyle.Create;
begin
  inherited Create;
  FGradient := TGradient.Create;
end;

destructor TglListBoxItemStyle.Destroy;
begin
  FGradient.Free; inherited;
end;

procedure TglListBoxItemStyle.SetOnChanged(Value: TNotifyEvent);
begin
  inherited SetOnChanged(Value);
  FGradient.OnChanged := Value;
end;

//______________________________________{ . TglAskListBoxItemStyle . }
constructor TglAskListBoxItemStyle.Create;
begin
  inherited Create;
  FBtnFont := TFont.Create;
end;

destructor TglAskListBoxItemStyle.Destroy;
begin
  FBtnFont.Free; inherited;
end;

procedure TglAskListBoxItemStyle.SetBtnColor(Value: TColor);
begin
  if FBtnColor = Value then exit;
  FBtnColor := Value; if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglAskListBoxItemStyle.SetBtnFont(Value: TFont);
begin
  FBtnFont.Assign(Value); if Assigned(OnChanged) then OnChanged(self);
end;

procedure TglAskListBoxItemStyle.SetBtnTextStyle(Value: TglTextStyle);
begin
  FBtnTextStyle := Value; if Assigned(OnChanged) then OnChanged(self);
end;

//______________________________________{ . TglCustomBoxStyle . }
constructor TglCustomBoxStyle.Create;
begin
  inherited;
  FPenStyle	   := psSolid;
  FHighlightColor  := clBtnHighlight;
  FShadowColor	   := clBtnShadow;
end;

procedure TglCustomBoxStyle.SetPenStyle(Value: TPenStyle);
begin  FPenStyle := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglCustomBoxStyle.SetHighlightColor(Value: TColor);
begin  FHighlightColor := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglCustomBoxStyle.SetShadowColor(Value: TColor);
begin  FShadowColor := Value; if Assigned(OnChanged) then OnChanged(self); end;

//______________________________________{ . TglCustomTextBoxStyle . }
constructor TglCustomTextBoxStyle.Create;
begin
  inherited;
  FTextColor		:= clBlack;
  FBackgroundColor	:= clWindow;
end;

procedure TglCustomTextBoxStyle.SetTextColor(Value: TColor);
begin  FTextColor := Value; if Assigned(OnChanged) then OnChanged(self); end;

procedure TglCustomTextBoxStyle.SetBackgroundColor(Value: TColor);
begin  FBackgroundColor := Value; if Assigned(OnChanged) then OnChanged(self); end;

//______________________________________{ . TglBevelLines . }
constructor TglBevelLines.Create;
begin
  inherited;
  FStyle     := bvLowered;
  FThickness := 1;
end;

procedure TglBevelLines.SetCount(Value: cardinal);
begin FCount := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetStep(Value: cardinal);
begin FStep := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetOrigin(Value: TglOrigin);
begin FOrigin := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetStyle(Value: TPanelBevel);
begin FStyle := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetBold(Value: boolean);
begin FBold := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetThickness(Value: byte);
begin FThickness := Value; if Assigned(OnChanged) then OnChanged(self); end;
procedure TglBevelLines.SetIgnoreBorder(Value: boolean);
begin FIgnoreBorder := Value; if Assigned(OnChanged) then OnChanged(self); end;

{ TGradient }

{ отрисовывает градиент }
procedure TGradient.Draw(DC: HDC; r: TRect; PenStyle, PenWidth: integer);
var
  i,j,x,y,x2,y2,h,w,NumberOfColors     :integer;
  c1F,c2F,c3F	      :byte;
  c1T,c2T,c3T	      :byte;
  c1D,c2D,c3D	      :integer;
  _R,_G,_B	      :byte;
  Pen,OldPen	      :HPen;
  FillBrush	      :HBrush;
  BufferBmp, OldBMP   :HBITMAP;
  BufferDC,TargetDC   :HDC;
  ColorR	      :TRect;
  LOGBRUSH            :TLOGBRUSH;

  procedure SwapColors;
  var TempColor: Longint;
  begin
    TempColor := FRGBFromColor; FRGBFromColor := FRGBToColor; FRGBToColor := TempColor;
  end;
begin
  if (not Active)or glGlobalData.fSuppressGradient then exit;
  if (Steps = 1)or(GetDeviceCaps( DC, BITSPIXEL )<16) then
  begin exit;
    FillBrush := CreateSolidBrush( ColorToRGB( FromColor ) );
    FillRect( DC, r, FillBrush );
    DeleteObject( FillBrush );
    exit;
  end;
  x:=r.left; y:=r.top; h:=r.bottom-r.top; w:=r.right-r.left;
  x2 := 0; y2 := 0; pen := 0; oldpen := 0; BufferDC := 0;

  if Orientation = fgdHorzConvergent then
  begin
    FOrientation := fgdHorizontal;
    Draw(DC, Rect(R.Left, R.Top, R.Right, R.Bottom - h div 2), PenStyle, PenWidth);
    SwapColors;
    Draw(DC, Rect(R.Left, R.Top + h div 2, R.Right, R.Bottom), PenStyle, PenWidth);
    SwapColors;
    FOrientation := fgdHorzConvergent;
    exit;
  end;
  if Orientation = fgdVertConvergent then
  begin
    FOrientation := fgdVertical;
    Draw(DC, Rect(R.Left, R.Top, R.Right - w div 2, R.Bottom), PenStyle, PenWidth);
    SwapColors;
    Draw(DC, Rect(R.Left + w div 2, R.Top, R.Right, R.Bottom), PenStyle, PenWidth);
    SwapColors;
    FOrientation := fgdVertConvergent;
    exit;
  end;

  //...r._ data no more useful
  c1F := Byte( FRGBFromColor );
  c2F := Byte( WORD(FRGBFromColor) shr 8);
  c3F := Byte( FRGBFromColor shr 16 );
  c1T := Byte( FRGBToColor );
  c2T := Byte( WORD(FRGBToColor) shr 8);
  c3T := Byte( FRGBToColor shr 16 );
  c1D := c1T - c1F;
  c2D := c2T - c2F;
  c3D := c3T - c3F;

  if BufferedDraw then
  begin
    BufferDC := CreateCompatibleDC(DC);
    BufferBmp := CreateBitmap( w, h, GetDeviceCaps( DC, PLANES ), GetDeviceCaps( DC, BITSPIXEL ), nil );
    OldBMP := SelectObject( BufferDC, BufferBmp );
    SetMapMode( BufferDC, GetMapMode(DC) );
    TargetDC := BufferDC;
  end else TargetDC := DC;

  case Orientation of
    fgdHorizontal:
    begin
      NumberOfColors := min( Steps, h );
      ColorR.Left := r.left; ColorR.Right  := r.right;
    end;
    fgdVertical:
    begin
      NumberOfColors := min( Steps, w );
      ColorR.Top := r.top; ColorR.Bottom  := r.bottom;
    end;
    fgdLeftBias, fgdRightBias:
    begin
      NumberOfColors := min( Steps, w+h );
      if PenStyle=0 then PenStyle:=PS_SOLID; if PenWidth=0 then PenWidth:=1;
      Pen := CreatePen( PenStyle, PenWidth, 0 );
      OldPen := SelectObject( TargetDC, Pen );
      y2 := y;
      if Orientation = fgdLeftBias then x2 := x else
		       begin x := r.right; x2 := r.right; end;
    end;
    else{fgdRectangle}
    begin
      h:=h div 2; w:=w div 2;
      NumberOfColors := min( Steps, min(w,h) );
    end;
  end;
  LOGBRUSH.lbStyle := BS_HATCHED;
  LOGBRUSH.lbHatch := Ord(BrushStyle) - Ord(bsHorizontal);
  for i:=0 to NumberOfColors-1 do
  begin
    _R := c1F + MulDiv(i, c1D, NumberOfColors - 1);
    _G := c2F + MulDiv(i, c2D, NumberOfColors - 1);
    _B := c3F + MulDiv(i, c3D, NumberOfColors - 1);

    case Orientation of
    fgdHorizontal, fgdVertical, fgdRectangle:
    begin
      if BrushStyle = bsSOLID then
        FillBrush := CreateSolidBrush( RGB( _R, _G, _B ) )
      else
      begin
        LOGBRUSH.lbColor := RGB( _R, _G, _B );
        FillBrush := CreateBrushIndirect(LOGBRUSH);
      end;

      case Orientation of
	fgdHorizontal:
	begin
          if fReverse then begin
	    ColorR.Top	  := r.bottom - MulDiv( i    , h, NumberOfColors);
	    ColorR.Bottom := r.bottom - MulDiv( i + 1, h, NumberOfColors);
          end else begin
	    ColorR.Top	  := r.top + MulDiv( i	  , h, NumberOfColors);
	    ColorR.Bottom := r.top + MulDiv( i + 1, h, NumberOfColors);
          end;
	end;
	fgdVertical:
	begin
          if fReverse then begin
	    ColorR.Left  := r.right - MulDiv( i	   , w, NumberOfColors);
	    ColorR.Right := r.right - MulDiv( i + 1, w, NumberOfColors);
          end else begin
            ColorR.Left  := r.left + MulDiv( i	  , w, NumberOfColors);
	    ColorR.Right := r.left + MulDiv( i + 1, w, NumberOfColors);
          end;
	end;
	fgdRectangle:
	begin
	  ColorR.Top	:= r.top     + MulDiv( i, h, NumberOfColors);
	  ColorR.Bottom := r.bottom  - MulDiv( i, h, NumberOfColors);
	  ColorR.Left	:= r.left    + MulDiv( i, w, NumberOfColors);
	  ColorR.Right	:= r.right   - MulDiv( i, w, NumberOfColors);
	end;
      end;
      FillRect( TargetDC, ColorR, FillBrush );
      DeleteObject( FillBrush );
    end;
    else {fgdLeftBias, fgdRightBias:}
    begin
      if Pen <> 0 then
        DeleteObject(SelectObject( TargetDC, OldPen ));//...cant delete selected!

      Pen := CreatePen( PenStyle, PenWidth, RGB( _R, _G, _B ) );

      OldPen := SelectObject( TargetDC, Pen );
      for j := 1 to MulDiv( i + 1, h+w, NumberOfColors) - MulDiv( i , h+w, NumberOfColors) do
      begin
	case Orientation of
	  fgdLeftBias:
	  begin
	    if y >= r.bottom then inc( x, PenWidth ) else y := y + PenWidth;
	    if x2 >= r.right then inc( y2, PenWidth ) else x2 := x2 + PenWidth;
	    MoveToEx( TargetDC, x, y, nil );
	    LineTo( TargetDC, x2, y2 );
	  end;
	  else{fgdRightBias:}
	  begin
	    if x <= r.left then inc( y, PenWidth ) else x := x - PenWidth;
	    if y2 >= r.bottom then dec( x2, PenWidth ) else y2 := y2 + PenWidth;
	    MoveToEx( TargetDC, x, y, nil );
	    LineTo( TargetDC, x2, y2 );
	  end;
	end;{end case}
      end;{end for}
      DeleteObject( SelectObject( TargetDC, OldPen ) );
    end;{end case else}
    end;{end case}
//    if NumberOfColors=0 then exit;
    if i/NumberOfColors*100 > PercentFilling then break;
  end;{end for}


  if BufferedDraw then
  begin
    BitBlt( DC, 0, 0, r.right-r.left, r.bottom-r.top, BufferDC, 0, 0, SRCCOPY );
    DeleteObject( SelectObject( BufferDC, OldBMP ) );
    DeleteDC( BufferDC );
  end;

end;

end.
