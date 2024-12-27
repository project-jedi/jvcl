{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLED.pas, released on 2005-03-30.

The Initial Developer of the Original Code is Robert Marquardt (robert_marquardt att gmx dott de)
Portions created by Robert Marquardt are Copyright (C) 2005 Robert Marquardt.
All Rights Reserved.

Contributor(s):
- Marc Geldon

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPoweredBy;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Graphics, Controls,
  JvComponent;

type
  TJvPoweredBy = class(TJvGraphicControl)
  private
    FResourceName: string;
    FImage: TBitmap;
    FURLActive: Boolean;
    FURL: string;
    procedure SetURL(const Value: string);
    procedure SetURLActive(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property URLActive: Boolean read FURLActive write SetURLActive stored True default True;
    property URL: string read FURL write SetURL stored True;
    property Image: TBitmap read FImage;
  end;

  // In a sense this component is silly :-). By using it the JVCL gets used.
  // Therefore it gets an exception from the MPL rule of mentioning the JVCL if using a JVCL component.

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvPoweredByJCL = class(TJvPoweredBy)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property Constraints;
    property Cursor default crHandPoint;
    property DragMode;
    property Height default 31;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 195;
    property URLActive;
    property URL;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvPoweredByJVCL = class(TJvPoweredBy)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property Constraints;
    property Cursor default crHandPoint;
    property DragMode;
    property Height default 31;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property URLActive;
    property URL;
    property Visible;
    property Width default 209;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJCLUtils, JvResources;

{$R JvPoweredBy.res}

const
  cPoweredByJCL = 'JvPoweredByJCL';
  cPoweredByJVCL = 'JvPoweredByJVCL';

//=== { TJvPoweredBy } =======================================================

constructor TJvPoweredBy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crHandPoint;
  FImage := TBitmap.Create;
  FImage.LoadFromResourceName(HInstance, FResourceName);
  Width := FImage.Width;
  Height := FImage.Height;
end;

destructor TJvPoweredBy.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TJvPoweredBy.SetURL(const Value: string);
begin
  if Value <> FURL then
  begin
    FURL := Value;
    if FURL = '' then
    begin
      FURLActive := False;
      Cursor := crDefault;
    end
    else
    begin
      FURLActive := True;
      Cursor := crHandPoint;
    end;
  end;
end;

procedure TJvPoweredBy.SetURLActive(const Value: Boolean);
begin
  if Value <> FURLActive then
  begin
    if Value then
    begin
      FURLActive := True;
      Cursor := crHandPoint;
    end
    else
    begin
      FURLActive := False;
      Cursor := crDefault;
    end;
  end;
end;

procedure TJvPoweredBy.Paint;
var
  DestRect, SrcRect: TRect;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
  SrcRect := Rect(0, 0, FImage.Width, FImage.Height);
  DestRect := SrcRect;
  OffsetRect(DestRect, (ClientWidth - FImage.Width) div 2, (ClientHeight - FImage.Height) div 2);
  with Canvas do
  begin
    CopyMode := cmSrcCopy;
    CopyRect( DestRect, FImage.Canvas, SrcRect);
  end;
end;

procedure TJvPoweredBy.Click;
begin
  if not Assigned(OnClick) and (URL <> '') and (URLActive) then
    OpenObject(URL)
  else
    inherited Click;
end;

procedure TJvPoweredBy.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AutoSize and (Align in [alNone, alCustom]) then
    inherited SetBounds(ALeft, ATop, FImage.Width, FImage.Height)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;



//=== { TJvPoweredByJCL } ====================================================

constructor TJvPoweredByJCL.Create(AOwner: TComponent);
begin
  FResourceName := cPoweredByJCL;
  // simple trick with inherited
  inherited Create(AOwner);
  FURLActive := True;
  FURL := RsURLPoweredByJCL;
end;

//=== { TJvPoweredByJVCL } ===================================================

constructor TJvPoweredByJVCL.Create(AOwner: TComponent);
begin
  FResourceName := cPoweredByJVCL;
  inherited Create(AOwner);
  FURLActive := True;
  FURL := RsURLPoweredByJVCL;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
