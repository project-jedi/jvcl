{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glEdit Unit 03.1999				 component TglEdit
 ===================================================================
}
unit glEdit;
{$I glDEF.INC}
interface
uses Dialogs,
  Windows, Messages, Classes, Controls, Graphics, forms,
  glTypes, glCommCl, glUtils, StdCtrls, ExtCtrls, SysUtils, Mask, gl3DCol;
type

  TglMaskEdit = class(TCustomMaskEdit)
  private
    FScrollBars : TScrollStyle;
    FAlignment  : TAlignment;
    FMultiline  : boolean;
    FWordWrap   : boolean;
    FAfterPaint : TNotifyEvent;
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetAlignment(Value: TAlignment);
    procedure SetMultiline(Value: boolean);
    procedure SetWordWrap(Value: boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Canvas	: TCanvas;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure Paint(var Message: TWMPaint); message WM_PAINT;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property ScrollBars : TScrollStyle read FScrollBars write SetScrollBars
     default ssNone;
    property Alignment  : TAlignment read FAlignment write SetAlignment
     default taLeftJustify;
    property Multiline  : boolean read FMultiline write SetMultiline
     default false;
    property WordWrap  : boolean read FWordWrap write SetWordWrap
     default false;
    property AfterPaint: TNotifyEvent read FAfterPaint write FAfterPaint;
  end;

procedure Register;

implementation
//{$R glShadow.res}

{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglMaskEdit]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
constructor TglMaskEdit.Create( AOwner : TComponent );
begin
  inherited;
  Canvas := TControlCanvas.Create;
  TControlCanvas(Canvas).Control := Self;//...i can draw now! :)
  {$IFDEF FR_RUS}
  Font.CharSet := RUSSIAN_CHARSET;
  {$ENDIF}
end;

destructor TglMaskEdit.Destroy;
begin
  Canvas.Free;
  inherited;
end;

procedure TglMaskEdit.Paint(var Message: TWMPaint);
begin
  inherited;
  if Assigned(FAfterPaint) then FAfterPaint(self);
end;

procedure TglMaskEdit.CreateParams(var Params: TCreateParams);
const
  aAlignments: array[TAlignment] of DWORD = ( ES_LEFT, ES_RIGHT, ES_CENTER );
  aMultiline: array[boolean] of DWORD = ( 0, ES_MULTILINE );
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or aMultiline[FMultiline] or WS_CLIPCHILDREN
    or aAlignments[FAlignment] or ScrollBar[FScrollBars] or WordWraps[FWordWrap];
end;

procedure TglMaskEdit.SetScrollBars(Value: TScrollStyle);
begin FScrollBars := Value; RecreateWnd; end;
procedure TglMaskEdit.SetAlignment(Value: TAlignment);
begin FAlignment := Value; RecreateWnd; end;
procedure TglMaskEdit.SetMultiline(Value: boolean);
begin FMultiline := Value; RecreateWnd; end;
procedure TglMaskEdit.SetWordWrap(Value: boolean);
begin FWordWrap := Value; RecreateWnd; end;
{
procedure TglMaskEdit.SetText( Value: string );
var
  i: integer;
  fIsDigit: boolean;
begin
  if DigitsOnly then
  begin
    Value := trim( Value );
    fIsDigit := true;
    try
      i := StrToInt( Value );
    except
      fIsDigit := false;
    end;
    if fIsDigit then Control.Text := Value;
  end
 else Control.Text := Value;

end;
}
{procedure TglMaskEdit.SetDigitsOnly( Value: boolean );
var
  Text: string;
  i: integer;
begin
  if DigitsOnly = Value then exit;
  FDigitsOnly := Value;
  if DigitsOnly then
  begin
    Control.Text := trim( Control.Text );
     try
      i := StrToInt( Control.Text );
    except
      Control.Text := '';
    end;
  end;
end;}


end.

