{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glBevel Unit 02.1999			       component TglProgress

 ===================================================================
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//...created: 08.1998
//...last modified: 26.11.1998
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unit GlPrgrs;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, SysUtils, glTypes,
  glCommCl, glUtils, ExtCtrls {$IFDEF GLVER_D5},Imglist{$ENDIF};
type

  TglProgress = class(TGraphicControl)
  private
    FBevelInner 	: TPanelBevel;
    FBevelOuter 	: TPanelBevel;
    FBevelBold		: boolean;
    FColors		: TglSimleLabelColors;
    FGradientF		: TGradient;
    FGradientB		: TGradient;
    FPercent		: TglPercent;
    FCaptionAlignment	: TAlignment;
    FCaptionDirection	: TglLabelDir;
    FCaptionStyle	: TglTextStyle;
    FStep		: integer;
    FInterspace 	: integer;
    FOptions		: TglProgressOptions;
    Image		: TBitmap;
    BackImage		: TBitmap;
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelBold(Value: boolean);
    procedure SetPercent(Value: TglPercent);
    procedure SetCaptionAlignment(Value: TAlignment);
    procedure SetCaptionDirection(Value: TglLabelDir);
    procedure SetCaptionStyle(Value: TglTextStyle);
    procedure SetStep(Value: integer);
    procedure SetInterspace(Value: integer);
    procedure SetOptions(Value: TglProgressOptions);

    procedure OnSmthChanged(Sender: TObject);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Loaded; override;
    procedure Paint; override;

  public
    fNeedRebuildBackground :boolean;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

  published
    {$IFDEF GLVER_D5}
    property Anchors;
    {$ENDIF}  
    property Align;
    property Caption;
    property Font;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property DragCursor;
    property DragMode;

    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
      default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
      default bvNone;
    property BevelBold: boolean read FBevelBold write SetBevelBold
      default false;
    property Colors: TglSimleLabelColors read FColors write FColors;
    property Gradient: TGradient read FGradientF write FGradientF;
    property GradientBack: TGradient read FGradientB write FGradientB;
    property Percent: TglPercent read FPercent write SetPercent;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment;
    property CaptionDirection: TglLabelDir read FCaptionDirection write SetCaptionDirection;
    property CaptionStyle: TglTextStyle read FCaptionStyle write SetCaptionStyle;
    property Step: integer read FStep write SetStep;
    property Interspace: integer read FInterspace write SetInterspace;
    property Options: TglProgressOptions read FOptions write SetOptions;

  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglProgress]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglProgress.Create( AOwner : TComponent );
begin
  inherited;
  ControlStyle := [csOpaque, csDoubleClicks];
  FColors    := TglSimleLabelColors.Create;
  FGradientB := TGradient.Create;
  FGradientF := TGradient.Create;
  //..defaults
  if csDesigning in ComponentState then with FColors do
  begin
    FGradientF.Orientation := fgdVertical;
    FGradientB.Orientation := fgdVertical;
    FGradientF.Active := true;
    FGradientB.Active := true;
    FGradientF.FromColor := clGreen;
    FGradientF.ToColor := clYellow;
    FGradientB.FromColor := 0;
    FGradientB.ToColor := clGreen;
    FGradientF.PercentFilling := FPercent;
    Delineate := clGray;
    Shadow := 0;
    Background := 0;
    Caption := 'progress...[%d%%]';
  end;
  FColors.OnChanged := OnSmthChanged;
  FGradientB.OnChanged := OnSmthChanged;
  FGradientF.OnChanged := OnSmthChanged;
  Image := TBitmap.Create;
  BackImage := TBitmap.Create;
  Width := 150; height := 15;

  FCaptionDirection := fldLeftRight;
  FCaptionAlignment := taLeftJustify;
  //...
  FStep := 3; FInterspace := 1;
  FCaptionStyle := fstShadow;
  FCaptionAlignment := taCenter;
  Font.Color := clWhite;
  FBevelInner := bvLowered;
  //...
  Color := clBlack;
end;

destructor TglProgress.Destroy;
begin
  FGradientF.Free;
  FGradientB.Free;
  FColors.Free;
  BackImage.Free;
  Image.Free;
  inherited;
end;

procedure TglProgress.Loaded;
begin
  inherited loaded;
{  Image.Width := Width; Image.Height := Height;
  BackImage.Width := Width; BackImage.Height := Height;
  if fpoTransparent in Options then
  GetParentImageRect( self, Bounds(Left,Top,Width,Height),
	              Image.Canvas.Handle );}

end;

procedure TglProgress.CMTextChanged(var Message: TMessage);
begin Repaint; end;

procedure TglProgress.Paint;
const
  ShadowDepth = 2;
var
  r : TRect;
  i,x,x2,y: integer;
  Size, TextSize: TSize;
  Capt: string;
begin
  if (Image.Width <> Width)or(Image.Height <> Height) then
  begin
    Image.Width := Width; Image.Height := Height;
    BackImage.Width := Width; BackImage.Height := Height;
    fNeedRebuildBackground := true;
  end;
  r := ClientRect;
  if (fpoTransparent in Options)and fNeedRebuildBackground then
  begin
    GetParentImageRect( self, Bounds(Left,Top,Width,Height),
    	                BackImage.Canvas.Handle );
    fNeedRebuildBackground := false;
  end;
  BitBlt(Image.Canvas.Handle, 0, 0, Width, Height, BackImage.Canvas.Handle, 0, 0, SRCCOPY);
  with Image.Canvas do
  begin
    dec(r.bottom); dec(r.right);
    r := DrawBoxEx( Handle, r, [fsdLeft, fsdTop, fsdRight, fsdBottom], FBevelInner, FBevelOuter,
		    FBevelBold, Colors.Background, fpoTransparent in Options );

//    PercentWidth := trunc( Width * Percent / 100 );
//    PercentWidth := Width;
    Brush.Color := Colors.Background; inc(r.top);
    if Percent > 0 then
    begin
    GradientBox( handle, r, FGradientB, integer(psSolid), 1 );
    GradientBox( handle, r, FGradientF, integer(psSolid), 1 );
    x := r.left;
    if not(fpoTransparent in Options) then
    for i:=r.left to Width div (FStep+FInterspace)+1 do
    begin
      x2 := x+FInterspace;
      if x2>r.right then
	if x < r.right then x2 := r.right else break;
      FillRect( Rect( x, r.top, x2, r.Bottom ) );
      inc( x, FStep+FInterspace );
    end;

    end;
//...CALC POSITION
  try Capt := Format( Caption, [Percent] ); except Capt := Caption; end;
  GetTextExtentPoint32( Self.Canvas.Handle, PChar(Capt), length(Capt), Size );

  x:=2; y:=0;
//  Size.cx:=Size.cx+2+trunc(Size.cx*0.01);
//  Size.cy := Size.cy+2;
  TextSize:=Size;
  if (FCaptionStyle=fstShadow)or(FCaptionStyle=fstVolumetric) then
  begin
    inc( Size.cy, ShadowDepth );
    inc( Size.cx, ShadowDepth );
  end;
  if fpoDelineatedText in FOptions then
  begin
    inc( Size.cy, 2 );
    inc( Size.cx, 2 );
  end;

  case FCaptionDirection of
    fldLeftRight: begin
      case FCaptionAlignment of
	taCenter: x:=(Width-Size.cx)div 2;
	taRightJustify: x:=Width-Size.cx;
      end; y := (Height-Size.cy) div 2;
    end;
    fldRightLeft: begin
      case FCaptionAlignment of
	taCenter: x:=(Width+Size.cx)div 2;
	taLeftJustify: x:=Width-(Size.cx-TextSize.cx)-2;
	else x:=TextSize.cx;
      end;
      y:=TextSize.cy;
    end;
    fldDownUp: begin
      case FCaptionAlignment of
	taCenter: y:=(Height+TextSize.cx-(Size.cy-TextSize.cy))div 2;
	taRightJustify: y:=TextSize.cx-4;
	else y := Height-(Size.cy-TextSize.cy)-2;
      end;
    end;
    fldUpDown: begin
      case FCaptionAlignment of
	taCenter: y:=(Height-Size.cx)div 2;
	taRightJustify: y:=Height-Size.cx;
	else y:=1;
      end;
      x:=TextSize.cy;
    end;

  end;
//...CALC POSITION end

    ExtTextOutExt( Handle, x, y, GetClientRect, Capt,
		   FCaptionStyle, fpoDelineatedText in FOptions,
		   false, Self.Font.Color, FColors.Delineate,
		   FColors.Highlight,FColors.Shadow,
		   nil, nil, Self.Font );

  end;
  Canvas.Draw( 0, 0, Image );

end;

procedure TglProgress.OnSmthChanged(Sender: TObject);
begin Repaint; end;
//...______________________________________________PROPERTIES METHODS
procedure TglProgress.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter:=Value; Repaint;
end;

procedure TglProgress.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner:=Value;
  Invalidate;
end;

procedure TglProgress.SetBevelBold(Value: boolean);
begin
  FBevelBold:=Value; Repaint;
end;

procedure TglProgress.SetPercent(Value: TglPercent);
begin
  if FPercent = Value then exit;
  FPercent := Value;
  FGradientF.PercentFilling := FPercent;
end;

procedure TglProgress.SetCaptionAlignment(Value: TAlignment);
begin
  FCaptionAlignment := Value; Repaint;
end;

procedure TglProgress.SetCaptionDirection(Value: TglLabelDir);
begin
  FCaptionDirection := Value; Repaint;
end;

procedure TglProgress.SetCaptionStyle(Value: TglTextStyle);
begin
  FCaptionStyle := Value; Repaint;
end;

procedure TglProgress.SetStep(Value: integer);
begin
  FStep := Value; Repaint;
end;

procedure TglProgress.SetInterspace(Value: integer);
begin
  FInterspace := Value; Repaint;
end;

procedure TglProgress.SetOptions(Value: TglProgressOptions);
begin
  FOptions := Value; Repaint;
end;


end.
