{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 2000 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glWinMask Unit 03.2000			        component TglWinMask
 ===================================================================
}
unit glWinMask;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, extctrls ,glTypes, CommCtrl, glCommCl {$IFDEF GLVER_D5},Imglist{$ENDIF};

//const

type

  TglWinMask = class(TCustomPanel)
  private
    FMask        : TBitmap;
    FMaskBuff    : TBitmap;
    fIgnorePaint : boolean;
  public
    Control      : TWinControl;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent(Value: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Mask: TBitmap read FMask write FMask;// stored fDontUseDefaultImage;
  end;

  procedure Register;

implementation
uses glUtils;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglWinMask]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//___________________________________________________ TglWinMask Methods _
constructor TglWinMask.Create( AOwner : TComponent );
begin
  inherited;
  Height  := 50;  Width  := 100;
  FMask := TBitmap.Create;
  FMaskBuff := TBitmap.Create;
  fIgnorePaint := false;
end;

destructor TglWinMask.Destroy;
begin
  FMask.Free;
  FMaskBuff.Free;
  inherited;
end;

procedure TglWinMask.Loaded;
begin
  inherited;
end;

procedure TglWinMask.Paint;
var
  r: TRect;
  CurrStyle: TglTextBoxStyle;
  OldPointer: Pointer;
  Message: TMessage;
  procedure CreateMaskBuff(R: TRect);
  begin
    FMaskBuff.Width := Width; FMaskBuff.Height := Height;

    FMaskBuff.Canvas.Brush.Color := clBlue;
    FMaskBuff.Canvas.FillRect(R);

    Message.Msg := WM_PAINT;
    SendMessage(Control.Handle, WM_PAINT, FMaskBuff.Canvas.handle, 0);
//    GetWindowImageFrom(Control, 0, 0, true, false, FMaskBuff.Canvas.handle);
//    GetParentImageRect( self, Bounds(Left,Top,Width,Height),
//			  FMaskBuff.Canvas.Handle );

//    BitBlt( FMaskBuff.Canvas.Handle, 0, 0, Width, Height,
//            FMask.Canvas.Handle, 0, 0, SRCPAINT );

    BitBlt( Canvas.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
            FMaskBuff.Canvas.Handle, 0, 0, SRCCOPY );
//    FMaskBuff
  end;
begin
  if fIgnorePaint then exit;
  fIgnorePaint := true;

  r := ClientRect;
  if Enabled then
  begin
    CreateMaskBuff(R);

//    BitBlt( Canvas.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
//            FMaskBuff.Canvas.Handle, 0, 0, SRCCOPY );
  end;
//  if Assigned(FAfterPaint) then FAfterPaint(self);
  fIgnorePaint := false;
end;

procedure TglWinMask.SetParent(Value: TWinControl);
begin
  inherited;
end;

procedure TglWinMask.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

end.
