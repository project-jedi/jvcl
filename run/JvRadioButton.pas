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
  JVCLVer, JvTypes;

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
    FAutoSize: Boolean;
    FCanvas: TControlCanvas;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FWordWrap: Boolean;
    procedure SetHotFont(const Value: TFont);
    function GetCanvas: TCanvas;
    procedure SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure SetAutoSize(Value: Boolean); {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    property Canvas: TCanvas read GetCanvas;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CalcAutoSize; virtual;
    procedure Loaded;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
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
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  FOver := False;
  FHintColor := clInfoBk;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FAutoSize := True;
  FWordWrap := True;
end;

destructor TJvRadioButton.Destroy;
begin
  FFontSave.Free;
  FHotFont.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
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

procedure TJvRadioButton.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    {$IFDEF COMPILER6_UP}
    inherited SetAutoSize(Value);
    {$ENDIF COMPILER6_UP}
    FAutoSize := Value;
    if Value then WordWrap := False;
    CalcAutoSize;
  end;
end;

function TJvRadioButton.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvRadioButton.CalcAutoSize;
const
  Flags: array [Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  AWidth, AHeight: Integer;
  ASize: TSize;
  R: TRect;
begin
  if Parent = nil then
    Exit;
  if AutoSize then
  begin
    ASize := GetDefaultCheckBoxSize;
    // add some spacing
    Inc(ASize.cy, 4);
    FCanvas.Font := Font;
    R := Rect(0, 0, ClientWidth, ClientHeight);
    // This is slower than GetTextExtentPoint but it does consider hotkeys
    if Caption <> '' then
    begin
      DrawText(FCanvas.Handle, PChar(Caption), Length(Caption), R, Flags[WordWrap] or DT_LEFT or DT_NOCLIP or DT_CALCRECT);
      AWidth := (R.Right - R.Left) + ASize.cx + 8;
      AHeight := R.Bottom - R.Top;
    end
    else
    begin
      AWidth := ASize.cx;
      AHeight := ASize.cy;
    end;
    if AWidth < ASize.cx then
      AWidth := ASize.cx;
    if AHeight < ASize.cy then
      AHeight := ASize.cy;
    ClientWidth := AWidth;
    ClientHeight := AHeight;
  end;
end;

procedure TJvRadioButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvRadioButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
end;

procedure TJvRadioButton.SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvRadioButton.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if Value then
      AutoSize := False;
    RecreateWnd;
  end;
end;

procedure TJvRadioButton.Loaded;
begin
  inherited Loaded;
  CalcAutoSize;
end;

end.

