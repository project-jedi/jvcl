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
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
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

const
  MAX_LINKID_TEXT = 255; //??
  L_MAX_URL_LENGTH = 255; // ??
  LIF_ITEMINDEX = 1;
  LIF_STATE     = 2;
  LIF_ITEMID    = 4;
  LIF_URL       = 8;

  LIS_ENABLED   = 1;
  LIS_ENABLED   = 2;
  LIS_VISITED   = 3;
  LM_GETITEM = 0;

type
  tagLITEM = record
    mask:UINT;
    state:UINT
    stateMask:UINT;
    szID:array [0..MAX_LINKID_TEXT-1] of WideChar;
    szUrl: array [0..L_MAX_URL_LENGTH-1]; of WideChar;
  end;
  {$EXTERNALSYM tagLITEM}
  TLItem = tagLITEM;
  PLItem = ^tagLITEM;

  tagLHITTESTINFO = record
    pt:TPoint;
    item:tagLITEM;
  end;
  {$EXTERNALSYM tagLHITTESTINFO}
  LHITTESTINFO = tagLHITTESTINFO;
  {$EXTERNALSYM LHITTESTINFO}
  PLHITTESTINFO = ^LHITTESTINFO;
  {$EXTERNALSYM PLHITTESTINFO}

  TLHitTestInfo = tagLHITTESTINFO;
  PLHitTestInfo = ^tagLHITTESTINFO;

  tagNMLINK = record
    hdr:NMHDR;
    item:tagLITEM;
  end;
  {$EXTERNALSYM tagNMLINK}
  NMLINK = tagNMLINK;
  {$EXTERNALSYM NMLINK}
  PNMLINK = ^NMLINK;
  {$EXTERNALSYM PNMLINK}
  TNMLink = tagNMLINK;


{ TSysLink }

procedure TSysLink.CNNotify(var Message: TWMNotify);
begin
  inherited;
  
end;

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
