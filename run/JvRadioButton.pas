{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRadioButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRadioButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JVCLVer,JvTypes;

type
  TJvRadioButton = class(TRadioButton)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FAutoSize: boolean;
    FControlCanvas: TControlCanvas;
    FHotTrackFontOptions: TJvTrackFOntOptions;
    FWordWrap: boolean;
    procedure SetHotFont(const Value: TFont);
    function GetCanvas: TCanvas;
    procedure SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
    procedure SetWordWrap(const Value: boolean);
  protected
    procedure SetAutoSize(Value: boolean);{$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    property Canvas: TCanvas read GetCanvas;
    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CalcAutoSize;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoSize:boolean read FAutoSize write SetAutoSize default true;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotTrackFontOptions: TJvTrackFOntOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;

    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property WordWrap:boolean read FWordWrap write SetWordWrap default true;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation
uses
  JvJVCLUtils;

constructor TJvRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  FOver := False;
  FHintColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FAutoSize := true;
  FWordWrap := true;
end;

destructor TJvRadioButton.Destroy;
begin
  FFontSave.Free;
  FHotFont.Free;
  inherited Destroy;
end;

procedure TJvRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if WordWrap then
    Params.Style := Params.Style or BS_MULTILINE or BS_TOP;
end;

procedure TJvRadioButton.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvRadioButton.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvRadioButton.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvRadioButton.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvRadioButton.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TJvRadioButton.SetAutoSize(Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    {$IFDEF COMPILER6_UP}
    inherited SetAutoSize(Value);
    {$ENDIF}
    FAutoSize := Value;
    if Value then WordWrap := false;
    CalcAutoSize;
  end;
end;


function TJvRadioButton.GetCanvas: TCanvas;
begin
  if FControlCanvas = nil then
  begin
    FControlCanvas := TControlCanvas.Create;
    FControlCanvas.Control := self;
  end;
  Result := FControlCanvas;
end;

function GetDefaultCheckBoxSize:TSize;
begin
  with TBitmap.Create do
  try
    Handle := LoadBitmap(0, PChar(32759));
    Result.cx := Width div 4;
    Result.cy := Height div 3;
  finally
    Free;
  end;
end;

procedure TJvRadioButton.CalcAutoSize;
var AWidth,AHeight:integer; ASize:TSize;
begin
  // (p3) TODO: find the Windows constants for width and height of checkbox and radiobutton icons
  if Parent = nil then Exit;
  ASize := GetDefaultCheckBoxSize;
  if AutoSize then
  begin
    with Canvas.TextExtent(Caption) do
    begin
      AWidth := cx + ASize.cx;
      if AWidth <= 18 then
        AWidth := ASize.cx;
      AHeight := cy + 4;
      if AHeight < ASize.cy then AHeight := ASize.cy;
      if Caption <> '' then
        Inc(AWidth, 4);
      ClientWidth := AWidth;
      ClientHeight := AHeight;
    end;
  end;
end;

procedure TJvRadioButton.CMFontchanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvRadioButton.CMTextchanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
end;

procedure TJvRadioButton.SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font,FHotTrackFontOptions);
  end;
end;

procedure TJvRadioButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if Value then AutoSize := false;
    RecreateWnd;
  end;
end;

end.

