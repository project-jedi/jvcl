unit JvSysLink;

interface
uses
  Windows, SysUtils, Messages, Classes, Controls, Graphics;

const
  ICC_LINK_CLASS       = $00008000;
  {$EXTERNALSYM ICC_LINK_CLASS}
  WC_LINK: PWideChar = 'SysLink';
  {$EXTERNALSYM WC_LINK}

type
  TSysLink = class(TWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color default clWindow;
    property Height default 24;
    property TabStop default true;
    property Width default 200;
    property Caption;
  end;


implementation
uses
  ComCtrls;

resourcestring
  SComCtl32Version6Required = 'This control requires ComCtl32.dll version 6 or later';

{ TSysLink }

constructor TSysLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := true;
  Height := 24;
  Width := 200;
  Color := clWindow;
end;

procedure TSysLink.CreateParams(var Params: TCreateParams);
begin
  if not InitCommonControl(ICC_LINK_CLASS) then
    EComponentError.CreateRes(@SComCtl32Version6Required);
  inherited;
  CreateSubClass(Params,PChar(WC_LINK));
end;


end.
