unit glImgGr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, glTypes, glUtils, glCommCl;

type
  TglImageGroup = class(TGraphicControl)
  private
    FImageList		: TImageList;
    FPassiveMask	: TBitmap;
    FActiveMask 	: TBitmap;
    FSelectedMask	: TBitmap;
    FSingleSelected	: boolean;
    FTransparent	: boolean;
    FTransparentColor	: TColor;
    FMasked		: boolean;
    FMaskedColor	: TColor;
    FDisabledMaskColor	: TColor;

    FAutoTrColor	: TglAutoTransparentColor;
    FFastDraw		: boolean;
    fNeedRemakeBackground: boolean;
    Image		: TBitmap;
    OldWidth,OldHeight,
    OldLeft, OldTop	: integer;

    procedure SmthChanged(Sender: TObject);

    procedure SetImageList(Value: TImageList);
    procedure SetTransparent(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMasked(Value: boolean);
    procedure SetMaskedColor(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetAutoTrColor(Value: TglAutoTransparentColor);
    procedure SetFastDraw(Value: boolean);

  protected
    procedure Paint; override;
    procedure WMSize( var Msg: TMessage );
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateResBitmap;
    procedure RemakeBackground;
  published
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
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

    property Images: TImageList read FImageList write SetImageList;
    property Transparent: boolean read FTransparent write SetTransparent
     default false;
    property TransparentColor: TColor read FTransparentColor
     write SetTransparentColor default clOlive;
    property Masked: boolean read FMasked write SetMasked
     default false;
    property MaskedColor: TColor read FMaskedColor
     write SetMaskedColor default clOlive;
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
  RegisterComponents('Proba', [TglImageGroup]);
end;

//*****************************************_____________LowLevel METHODS
//________________________________________________________
constructor TglImageGroup.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
//  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  Width := 105;
  Height := 105;

  Image:=TBitmap.create;
  //...defaults
  FTransparent	      := false;
  FTransparentColor   := clOlive;
  FMasked	      := false;
  FMaskedColor	      := clOlive;
  FDisabledMaskColor  := clBlack;
  FAutoTrColor:= ftcLeftBottomPixel;
  FFastDraw:=false;
end;
//________________________________________________________
destructor TglImageGroup.Destroy;
begin
  Image.free;
  inherited;
end;
//________________________________________________________
procedure TglImageGroup.WMSize( var Msg: TMessage );
begin
  if csDesigning in ComponentState then CreateResBitmap;
end;
//________________________________________________________
procedure TglImageGroup.Paint;
begin
//  if fNeedRebuildImage then
  begin
    CreateResBitmap;
  end;
  BitBlt( Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TglImageGroup.RemakeBackground;
begin fNeedRemakeBackground := true; Repaint; end;
//________________________________________________________
procedure TglImageGroup.CreateResBitmap;
var
  i: integer;
  Bitmap: TBitmap;
begin
    if (FImageList=nil) or (FImageList.Count=0) then exit;

    Bitmap := TBitmap.Create;

    Image.Width := FImageList.Width*FImageList.Count; Image.Height := FImageList.Height;
    Width := max( Image.Width, Width ); Height := max( Image.Height, Height);
    with Image do begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect( Bounds(0,0,Width,Height) );
    end;

    if FTransparent then
     GetParentImageRect( self, Bounds(Left,Top,Image.Width,Image.Height),
			 Image.Canvas.Handle );

    for i:=0 to FImageList.Count-1 do
    begin
      FImageList.GetBitmap( i, Bitmap );

      if FMasked then
	ChangeBitmapColor( Image, FMaskedColor, clBtnFace );

      CreateBitmapExt( Image.Canvas.Handle, Bitmap, ClientRect,
		       i*FImageList.Width, 0,
		       fwoNone, fdsDefault,
		       FTransparent, FTransparentColor, FDisabledMaskColor );

    end;
    Bitmap.Free;
end;
//________________________________________________________
procedure TglImageGroup.SmthChanged(Sender: TObject);
begin  Invalidate; end;

//*****************************************_____________PROPERTY METHODS
//________________________________________________________
procedure TglImageGroup.SetImageList(Value: TImageList);
begin
  FImageList := Value;
//  SetAutoTrColor( FAutoTrColor );
  Invalidate;
end;
//________________________________________________________
procedure TglImageGroup.SetTransparent(Value: boolean);
begin
  if FTransparent = Value then exit;
  FTransparent := Value; Invalidate;
end;
//________________________________________________________
procedure TglImageGroup.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then exit;
//  FAutoTrColor:=ftcUser;
  FTransparentColor := Value; Invalidate;
end;
//________________________________________________________
procedure TglImageGroup.SetMasked(Value: boolean);
begin
  if FMasked = Value then exit;
  FMasked := Value; Invalidate;
end;
//________________________________________________________
procedure TglImageGroup.SetMaskedColor(Value: TColor);
begin
  if FMaskedColor = Value then exit;
  FMaskedColor := Value; Invalidate;
end;

procedure TglImageGroup.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor = Value then exit;
  FDisabledMaskColor := Value; Invalidate;
end;
//________________________________________________________
procedure TglImageGroup.SetAutoTrColor(Value: TglAutoTransparentColor);
//var x,y :integer;
begin{
  FAutoTrColor := Value;
  if (FAutoTrColor=ftcUser)or((FBitmap.Width or FBitmap.Height)=0)then
    exit;
  case FAutoTrColor of
    ftcLeftTopPixel: begin x:=0; y:=0; end;
    ftcLeftBottomPixel: begin x:=0; y:=FBitmap.Height-1; end;
    ftcRightTopPixel: begin x:=FBitmap.Width-1; y:=0; end;
    ftcRightBottomPixel: begin x:=FBitmap.Width-1; y:=FBitmap.Height-1; end;
  end;
  FTransparentColor := GetPixel(FBitmap.Canvas.Handle,x,y);
  Invalidate;}
end;
//________________________________________________________
procedure TglImageGroup.SetFastDraw(Value: boolean);
begin
  FFastDraw := Value;
end;

end.
