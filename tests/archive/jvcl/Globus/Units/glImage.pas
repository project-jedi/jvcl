{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glImage Unit 10.1998				  component TglImage
 ===================================================================
}
unit glImage;
{$I glDEF.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, glTypes, glUtils, glCommCl;

type
  TglBitmapImage = class(TGraphicControl)
  private
    FAutoSize		: boolean;
    FImageAlign 	: T2DAlign;
    FBitmapOption	: TglWallpaperOption;
    FDrawState		: TglDrawState;
    FTransparent	: boolean;
    FTransparentColor	: TColor;
    FMasked		: boolean;
    FMaskedColor	: TColor;
    FMaskedToColor	: TColor;
    FDisabledMaskColor	: TColor;
    FBitmap		: TBitmap;
    FImage	        : TImage;
    FAutoTrColor	: TglAutoTransparentColor;
    FFastDraw		: boolean;
    Bmp                 : TBitmap;
    fChanged		: boolean;
    OldClientRect	: TRect;
    OldWidth,OldHeight  : integer;
    procedure CreateResBitmap;
    procedure Changed;
    procedure SmthChanged(Sender: TObject);
    function CalcAlignOffset: TPoint;

    procedure SetAutoSize(Value: boolean);
    function  GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetBitmapOption(Value: TglWallpaperOption);
    procedure SetDrawState(Value: TglDrawState);
    procedure SetTransparent(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMasked(Value: boolean);
    procedure SetMaskedColor(Value: TColor);
    procedure SetMaskedToColor(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetFastDraw(Value: boolean);

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    OnChangeParams: TNotifyEvent;
    FResBitmap: TBitmap;//...you can use it!
//    procedure PaintTo(Canvas: TCanvas);
    procedure Paint; override;
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemakeBackground;//...for users
//    procedure RepaintBackground;//...for users

  published
  {$IFDEF GLVER_D5}
    property Anchors;
  {$ENDIF}  
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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

    property AutoSize: boolean read FAutoSize write SetAutoSize
     default false;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property ImageAlign: T2DAlign read FImageAlign write FImageAlign;
    property BitmapOption: TglWallpaperOption read FBitmapOption
     write SetBitmapOption default fwoNone;
    property DrawState: TglDrawState read FDrawState write SetDrawState
     default fdsDefault;
    property Transparent: boolean read FTransparent write SetTransparent
     default false;
    property TransparentColor: TColor read FTransparentColor
     write SetTransparentColor default clOlive;
    property Masked: boolean read FMasked write SetMasked
     default false;
    property MaskedColor: TColor read FMaskedColor
     write SetMaskedColor default clOlive;
    property MaskedToColor: TColor read FMaskedToColor
     write SetMaskedToColor default clBtnFace;
    property DisabledMaskColor: TColor read FDisabledMaskColor
     write SetDisabledMaskColor default clBlack;
    property AutoTransparentColor: TglAutoTransparentColor
     read FAutoTrColor write SetAutoTrColor default ftcLeftBottomPixel;
    property FastDraw: boolean read FFastDraw write SetFastDraw
     default false;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Proba', [TglBitmapImage]);
end;

//*****************************************_____________LowLevel METHODS
//________________________________________________________
constructor TglBitmapImage.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
//  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  {inherited }Width := 105;
  {inherited }Height := 105;

  FResBitmap:=TBitmap.create;
  FImageAlign:=T2DAlign.create;
  FImageAlign.OnChanged:=SmthChanged;
  fChanged:= true;
  OldClientRect:=Rect( left, top, left+width, top+height );
  //...defaults
  FAutoSize	      := false;
  FBitmapOption       := fwoNone;
  FDrawState	      := fdsDefault;
  FTransparent	      := false;
  FTransparentColor   := clOlive;
  FMasked	      := false;
  FMaskedColor	      := clOlive;
  FMaskedToColor      := clBtnFace;
  FDisabledMaskColor  := clBlack;
  FAutoTrColor:= ftcLeftBottomPixel;
  FFastDraw:=false;
  OnChangeParams:=nil;
end;
//________________________________________________________
destructor TglBitmapImage.Destroy;
begin
  FResBitmap.free;
  if Assigned(FBitmap) then FBitmap.Free;
  FImageAlign.Free;
  inherited;
end;
//________________________________________________________
procedure TglBitmapImage.Loaded;
begin
  inherited;
  if Assigned(FBitmap)and(not FBitmap.Empty) then Bmp := FBitmap;
  SetAutoTrColor(FAutoTrColor);
end;
//________________________________________________________
procedure TglBitmapImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Image) and (Operation = opRemove) then Image := nil;
end;
//________________________________________________________
procedure TglBitmapImage.Paint;
var //R,IntersectR: TRect;
    pt: TPoint;
begin
  if Assigned(Bitmap) then bmp := Bitmap;
  if Assigned(Image) then bmp := Image.Picture.Bitmap;

  if Assigned(Bmp)and(Bmp.handle<>0) then
  begin
  if (OldWidth<>Width)or(OldHeight<>Height) then
  begin
    fChanged := true;
    {if (OldLeft=Left)and(OldTop=Top) then
    begin
      R:=Rect( left, top, left+width, top+height );
      IntersectRect( IntersectR, OldClientRect, R );
      InvalidateRect( Parent.Handle, @R, false );
      ValidateRect( Parent.Handle, @IntersectR );
      OldClientRect := R;
     end;}
  end; //OldLeft := Left; OldTop := Top;
  OldWidth := Width; OldHeight := Height;

  if fChanged or not FFastDraw then
  begin
    CreateResBitmap;
    fChanged:=false;
  end;
  pt:=CalcAlignOffset;
  BitBlt( Canvas.Handle, pt.x, pt.y, FResBitmap.Width, FResBitmap.Height,
	  FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
  end;
  if (csDesigning in ComponentState)and(tag<>9999) then with Canvas do
  begin
    Pen.Color := clBlack; Pen.Style := psDash; Brush.Style := bsClear;
    Rectangle( 0, 0, width, height );
  end;

end;

procedure TglBitmapImage.RemakeBackground;
begin fChanged := true; Repaint; end;
//________________________________________________________
//procedure TglBitmapImage.WMSize(var Message: TWMSize);
//var R,IntersectR: TRect;
//begin
{  exit;
  if FAutoSize then
  begin Width:=FResBitmap.Width; Height:=FResBitmap.Height; end;
  if not FTransparent then
  begin
    R:=Rect( left, top, left+width, top+height );
    IntersectRect( IntersectR, OldClientRect, R );
    InvalidateRect( Parent.Handle, @R, false );
    ValidateRect( Parent.Handle, @IntersectR );
    OldClientRect := R;
  end else Invalidate;
  Changed;}
//end;
//________________________________________________________
procedure TglBitmapImage.CreateResBitmap;
var
  pt: TPoint;
//  BmpInfo: Windows.TBitmap;
begin
    if (FBitmapOption = fwoStretch)or(FBitmapOption = fwoPropStretch)or(FBitmapOption = fwoTile) then
    begin FResBitmap.Width:=Width; FResBitmap.Height:=Height; end
    else
    begin FResBitmap.Width:=Bmp.Width; FResBitmap.Height:=Bmp.Height; end;

    with FResBitmap do begin
//	if FTransparent then Canvas.Brush.Color := FTransparentColor
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect( Bounds(0,0,Width,Height) );
    end;

    pt:=CalcAlignOffset;
    if FTransparent then
     GetParentImageRect( self, Bounds(Left+pt.x,Top+pt.y,FResBitmap.Width,FResBitmap.Height),
			 FResBitmap.Canvas.Handle );
    //BringParentWindowToTop(parent);
//    BitBlt( FResBitmap.Canvas.Handle, 0,0, Width, Height, canvas.Handle, 0, 0, SRCCOPY);

    CreateBitmapExt( FResBitmap.Canvas.Handle, Bmp, ClientRect, 0, 0,
		     FBitmapOption, FDrawState,
		     FTransparent, FTransparentColor, FDisabledMaskColor );

    if FMasked then
      ChangeBitmapColor( FResBitmap, FMaskedColor, FMaskedToColor );

{  GetObject( FResBitmap.Handle, sizeof(Windows.TBitmap), @BmpInfo );
  if BmpInfo.bmBitsPixel >= 8 then
  with FResBitmap,BmpInfo do begin
    for i := 1 to bmWidth*bmHeight*(bmBitsPixel div 8)-1 do
      begin
	asm
	 inc BmpInfo.bmBits
	end;
	byte(bmBits^):=1;
      end;
  end;}

end;
//________________________________________________________
procedure TglBitmapImage.Changed;
begin
  fChanged:=true;
  if Assigned(OnChangeParams) then OnChangeParams(self);
end;
//________________________________________________________

procedure TglBitmapImage.SmthChanged(Sender: TObject);
begin  Changed; Invalidate; end;

function TglBitmapImage.CalcAlignOffset: TPoint;
var
  D,D_: double;
  bmp_: TPoint;
begin
  Result.x:=0; Result.y:=0;
  if (FBitmapOption=fwoNone)or(FBitmapOption=fwoPropStretch) then
  begin
    bmp_.x:=Bmp.Width; bmp_.y:=Bmp.Height;
    if FBitmapOption=fwoPropStretch then
    begin
	D_ := Width / bmp_.x; D := Height / bmp_.y;
	if D > D_ then D := D_;//...D == min
	bmp_.x := trunc(bmp_.x * D); bmp_.y := trunc(bmp_.y * D);
    end;
    case ImageAlign.Horizontal of
      fhaCenter:   Result.x:=max(0,(Width-bmp_.x) div 2);
      fhaRight:    Result.x:=max(0,Width-bmp_.x);
    end;
    case ImageAlign.Vertical of
      fvaCenter:   Result.y:=max(0,(Height-bmp_.y) div 2);
      fvaBottom:   Result.y:=max(0,Height-bmp_.y);
    end;
  end;
end;
//*****************************************_____________PROPERTY METHODS
//________________________________________________________
procedure TglBitmapImage.SetAutoSize(Value: boolean);
begin
  if (FAutoSize = Value)or not Assigned(Bmp) then exit;
  FAutoSize := Value;
  if FAutoSize and (FBitmapOption = fwoNone)
  and((Bmp.Width and Bmp.Height)<>0) then
  begin Width:=Bmp.Width; Height:=Bmp.Height; Changed; Invalidate; end;
end;
//________________________________________________________
function  TglBitmapImage.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then FBitmap := TBitmap.Create;
  Result := FBitmap;
end;
//________________________________________________________
procedure TglBitmapImage.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FBitmap:=TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then Bmp := FBitmap else
    if Assigned(FImage)and Assigned(FImage.Picture)and Assigned(FImage.Picture.Bitmap) then
      Bmp := FImage.Picture.Bitmap else Bmp := nil;
  SetAutoTrColor( FAutoTrColor );
  Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage)and Assigned(FImage.Picture)and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap else if Assigned(FBitmap) then Bmp := FBitmap
                                                           else Bmp := nil;
  SetAutoTrColor( FAutoTrColor );
  Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetBitmapOption(Value: TglWallpaperOption);
begin
  if FBitmapOption = Value then exit;
  FBitmapOption := Value; Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetDrawState(Value: TglDrawState);
begin
  if FDrawState = Value then exit;
  FDrawState := Value; Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetTransparent(Value: boolean);
begin
  if FTransparent = Value then exit;
  FTransparent := Value; Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetTransparentColor(Value: TColor);
begin
  if (FAutoTrColor <> ftcUser)or(FTransparentColor = Value) then exit;
  FTransparentColor := Value; Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetMasked(Value: boolean);
begin
  if FMasked = Value then exit;
  FMasked := Value; Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetMaskedColor(Value: TColor);
begin
  if FMaskedColor = Value then exit;
  FMaskedColor := Value; Changed; Invalidate;
end;

procedure TglBitmapImage.SetMaskedToColor(Value: TColor);
begin
  if FMaskedToColor = Value then exit;
  FMaskedToColor := Value; Changed; Invalidate;
end;

procedure TglBitmapImage.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor = Value then exit;
  FDisabledMaskColor := Value; Changed; Invalidate;
end;

//________________________________________________________
procedure TglBitmapImage.SetAutoTrColor(Value: TglAutoTransparentColor);
begin
  FAutoTrColor := Value;
  if not Assigned(bmp) then exit;
  if Value<>ftcUser then
    FTransparentColor := GetTransparentColor( bmp, Value );
  Changed; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetFastDraw(Value: boolean);
begin
  if FFastDraw = Value then exit;
  FFastDraw := Value; Changed; Invalidate;
end;
//________________________________________________________
{procedure TglBitmapImage.SetWidth(Value: integer);
begin
  if FWidth = Value then exit;
  FWidth := Value; Invalidate;
end;
//________________________________________________________
procedure TglBitmapImage.SetHeight(Value: integer);
begin
  if FHeight = Value then exit;
  FHeight := Value; Invalidate;
end;}
//________________________________________________________
end.
