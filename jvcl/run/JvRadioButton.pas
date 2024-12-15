{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRadioButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Robert Marquardt copied implementation of TJvCheckBox
Peter Thörnqvist- added LinkedControls property

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRadioButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvTypes, JvExStdCtrls, JvLinkedControls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvRadioButton = class(TJvExRadioButton)
  private
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FAutoSize: Boolean;
    FCanvas: TControlCanvas;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FLeftText: Boolean;
    FLinkedControls: TJvLinkedControls;
    FCheckingLinkedControls: Boolean;
    function GetCanvas: TCanvas;
    function GetReadOnly: Boolean;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLeftText(const Value: Boolean);
    procedure SetLinkedControls(const Value: TJvLinkedControls);
    procedure BMSetCheck(var Msg: TMessage); message BM_SETCHECK;
    function IsHotTrackFontStored: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure EnabledChanged;override;
    procedure SetAutoSize(Value: Boolean);  override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CalcAutoSize; virtual;
    procedure Loaded; override;

    procedure LinkedControlsChange(Sender: TObject);
    procedure CheckLinkedControls; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;

    property Canvas: TCanvas read GetCanvas;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HintColor;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions
      default DefaultTrackFontOptions;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    // show text to the left of the radio bullet
    property LeftText: Boolean read FLeftText write SetLeftText default False;
    // link the enabled state of other controls to the checked and/or enabled state of this control
    property LinkedControls: TJvLinkedControls read FLinkedControls write SetLinkedControls;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
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
  JvJVCLUtils;

//=== { TJvRadioButton } =====================================================

constructor TJvRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FAutoSize := True;
  FWordWrap := False;
  FAlignment := taLeftJustify;
  FLeftText := False;
  FLayout := tlCenter;
  FLinkedControls := TJvLinkedControls.Create(Self);
  FLinkedControls.OnChange := LinkedControlsChange;
end;

destructor TJvRadioButton.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  FreeAndNil(FLinkedControls);
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvRadioButton.Loaded;
begin
  inherited Loaded;
  CheckLinkedControls;
  CalcAutoSize;
end;

procedure TJvRadioButton.CreateParams(var Params: TCreateParams);
const
  cAlign: array [TAlignment] of Word = (BS_LEFT, BS_RIGHT, BS_CENTER);
  cLeftText: array [Boolean] of Word = (0, BS_RIGHTBUTTON);
  cLayout: array [TTextLayout] of Word = (BS_TOP, BS_VCENTER, BS_BOTTOM);
  cWordWrap: array [Boolean] of Word = (0, BS_MULTILINE);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or cAlign[Alignment] or cLayout[Layout] or
      cLeftText[LeftText] or cWordWrap[WordWrap];
end;

procedure TJvRadioButton.MouseEnter(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    inherited MouseEnter(AControl);
  end;
end;

procedure TJvRadioButton.MouseLeave(AControl: TControl);
begin
  if MouseOver then
  begin
    if FHotTrack then
      Font.Assign(FFontSave);
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvRadioButton.FontChanged;
begin
  inherited FontChanged;
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvRadioButton.TextChanged;
begin
  inherited TextChanged;
  CalcAutoSize;
end;

procedure TJvRadioButton.CalcAutoSize;
const
  Flags: array [Boolean] of Cardinal = (DT_SINGLELINE, DT_WORDBREAK);
var
  AWidth, AHeight: Integer;
  ASize: TSize;
  R: TRect;
begin
  if (Parent = nil) or not AutoSize or (csDestroying in ComponentState) or
    (csLoading in ComponentState) then
    Exit;
  ASize := GetDefaultCheckBoxSize;
  // add some spacing
  Inc(ASize.cy, 4);
  FCanvas.Font := Font;
  R := Rect(0, 0, ClientWidth, ClientHeight);
  // This is slower than GetTextExtentPoint but it does consider hotkeys
  if Caption <> '' then
  begin
    DrawText(FCanvas.Handle, PChar(Caption), Length(Caption), R,
      Flags[WordWrap] or DT_LEFT or DT_NOCLIP or DT_CALCRECT);
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

procedure TJvRadioButton.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvRadioButton.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    // inherited SetAutoSize(Value);
    FAutoSize := Value;
    if Value then
      WordWrap := False;
    CalcAutoSize;
  end;
end;

procedure TJvRadioButton.SetFocus;
var
  I: Integer;
  FocusLinkedControl: TControl;
begin
  inherited SetFocus;

  FocusLinkedControl := nil;
  I := 0;
  while (I < LinkedControls.Count) and not Assigned(FocusLinkedControl) do
  begin
    if (loForceFocus in LinkedControls[I].Options) and (LinkedControls[I].Control is TWinControl) then
      FocusLinkedControl := LinkedControls[I].Control;

    Inc(I);
  end;
  if Assigned(FocusLinkedControl) then
    TWinControl(FocusLinkedControl).SetFocus;
end;

function TJvRadioButton.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvRadioButton.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
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

procedure TJvRadioButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvRadioButton.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    RecreateWnd;
  end;
end;

procedure TJvRadioButton.SetReadOnly(const Value: Boolean);
begin
  ClicksDisabled := Value;
end;

procedure TJvRadioButton.SetLeftText(const Value: Boolean);
begin
  if FLeftText <> Value then
  begin
    FLeftText := Value;
    RecreateWnd;
  end;
end;

function TJvRadioButton.GetReadOnly: Boolean;
begin
  Result := ClicksDisabled;
end;

function TJvRadioButton.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(FHotTrackFont, Font, FHotTrackFontOptions);
end;

procedure TJvRadioButton.CheckLinkedControls;
var
  I: Integer;
begin
  if not FCheckingLinkedControls then // prevent an infinite recursion
  begin
    FCheckingLinkedControls := True;
    try
      if LinkedControls <> nil then
        for I := 0 to LinkedControls.Count - 1 do
          with LinkedControls[I] do
            if Control <> nil then
              Control.Enabled := CheckLinkControlEnabled(Self.Enabled, Self. Checked, Options);
    finally
      FCheckingLinkedControls := False;
    end;
  end;
end;

procedure TJvRadioButton.LinkedControlsChange(Sender: TObject);
begin
  CheckLinkedControls;
end;

procedure TJvRadioButton.SetLinkedControls(const Value: TJvLinkedControls);
begin
  FLinkedControls.Assign(Value);
end;

procedure TJvRadioButton.BMSetCheck(var Msg: TMessage);
begin
  inherited;
  CheckLinkedControls;
end;

procedure TJvRadioButton.EnabledChanged;
begin
  inherited EnabledChanged;
  CheckLinkedControls;
end;

procedure TJvRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(FLinkedControls) then
    LinkedControls.Notification(AComponent, Operation);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
