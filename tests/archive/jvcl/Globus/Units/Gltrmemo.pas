unit glTrMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TglTranspMemo = class(TMemo)
  private
    procedure OnWMPaint( var Msg: TWMPaint ); message WM_PAINT;
  protected
    { Protected declarations }
  public
    constructor Create( AOwner : TComponent ); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Proba', [TglTranspMemo]);
end;

constructor TglTranspMemo.Create( AOwner : TComponent );
begin
  inherited;
  //Canvas.Brush.Style:=bsClear;
end;

procedure TglTranspMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
//if Transparent then
  Params.ExStyle := Params.ExStyle or WS_EX_Transparent;
end;

procedure TglTranspMemo.OnWMPaint( var Msg: TWMPaint );
var dc:HDC;
begin
  dc:=GetDC(handle);
  SetBkMode( dc, TRANSPARENT );
  releaseDC(handle,dc);
  inherited;
end;

end.
