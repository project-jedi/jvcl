unit glShape;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, extctrls, glTypes, glUtils;
type

  TglShape = class(TShape)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FWallpaper: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetWallpaper: TBitmap;
  protected
    procedure Paint; override;
  public
    property Wallpaper: TBitmap read GetWallpaper write FWallpaper;
  published
    property PopupMenu;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick;
    property OnDblClick;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Gl Controls', [TglShape]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

procedure TglShape.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TglShape.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
end;

constructor TglShape.Create(AOwner: TComponent);
begin
  inherited;
//  FWallpaper := TBitmap.Create;
  ControlStyle := ControlStyle + [csClickEvents	,csDoubleClicks];
end;

destructor TglShape.Destroy;
begin
  if FWallpaper <> nil then FWallpaper.Free;
  inherited;
end;

function TglShape.GetWallpaper: TBitmap;
begin
  if FWallpaper = nil then FWallpaper := TBitmap.Create;
  Result := FWallpaper;
end;

procedure TglShape.Paint;
var
  R: TRect;
  OldBrush: TBrushStyle;
begin
  if not Assigned(FWallpaper) or FWallpaper.Empty then
    inherited
  else
  begin
    R := ClientRect;
    OldBrush := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen := Pen;
    {$IFDEF GLVER_D5}
    Canvas.Rectangle(R);
    {$ELSE}
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    {$ENDIF}
    Canvas.Brush.Style := OldBrush;
    InflateRect(R, -1, -1);
    DrawBitmapExt(Canvas.Handle, FWallpaper, R, 0, 0, fwoTile, fdsDefault, false, 0, 0);
  end;
end;

end.
