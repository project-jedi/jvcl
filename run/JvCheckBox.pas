{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Ain Valtin - ReadOnly, Alignment, Layout, RightButton
Robert Marquardt RightButton renamed to LeftText
Peter Thörnqvist- added LinkedControls property

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckBox;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  JvTypes, JvExStdCtrls, JvLinkedControls;

type
  TJvCheckBox = class(TJvExCheckBox)
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
    FReadOnly:Boolean;
    FLinkedControls: TJvLinkedControls;
    function GetCanvas: TCanvas;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetLeftText(const Value: Boolean);
    function GetLinkedControls: TJvLinkedControls;
    procedure SetLinkedControls(const Value: TJvLinkedControls);
    procedure ReadAssociated(Reader: TReader);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure EnabledChanged;override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure AdjustSize; override;
    procedure SetAutoSize(Value: Boolean); virtual;
    {$ENDIF VisualCLX}
    procedure UpdateProperties;
    procedure CalcAutoSize; virtual;
    procedure Loaded; override;
    procedure LinkedControlsChange(Sender: TObject);
    procedure CheckLinkedControls; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    {$IFDEF VCL}
    procedure BmSetCheck(var Msg: TMessage); message BM_SETCHECK;
    {$ENDIF VCL}
  {$IFDEF VisualCLX}
    procedure StateChanged(State: TToggleState); override;
  public
  {$ENDIF VisualCLX}
    procedure Toggle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    // link the enabled state of other controls to the checked and/or enabled state of this control
    property LinkedControls: TJvLinkedControls read GetLinkedControls write SetLinkedControls;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HintColor;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions
      default DefaultTrackFontOptions;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    // show text to the left of the checkbox
    property LeftText: Boolean read FLeftText write SetLeftText default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  SysUtils,
  JvJCLUtils, JvJVCLUtils;

constructor TJvCheckBox.Create(AOwner: TComponent);
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
  FReadOnly := False;
  FLinkedControls := TJvLinkedControls.Create(Self);
  FLinkedControls.OnChange := LinkedControlsChange;
end;

destructor TJvCheckBox.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  FreeAndNil(FLinkedControls);
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvCheckBox.Loaded;
begin
  inherited Loaded;
  CheckLinkedControls;
  CalcAutoSize;
end;

{$IFDEF VCL}
procedure TJvCheckBox.CreateParams(var Params: TCreateParams);
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
{$ENDIF VCL}

procedure TJvCheckBox.UpdateProperties;
begin
  {$IFDEF VCL}
  RecreateWnd;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  RecreateWidget;
  {$ENDIF VisualCLX}
end;

{$IFDEF VisualCLX}

(*
procedure TJvCheckBox.UpdateProperties;
begin
 { TODO:
  (ahuser) Missing features in CLX. If we implement it we must write a paint
           function and do all drawing ourself.

  Alignment
  LeftText
  Layout
  WordWrap
 }
end;
*)

procedure TJvCheckBox.AdjustSize;
begin
  inherited AdjustSize;
  CalcAutoSize;
end;

{$ENDIF VisualCLX}

procedure TJvCheckBox.MouseEnter(AControl: TControl);
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

procedure TJvCheckBox.MouseLeave(AControl: TControl);
begin
  if MouseOver then
  begin
    if FHotTrack then
      Font.Assign(FFontSave);
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvCheckBox.FontChanged;
begin
  inherited FontChanged;
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCheckBox.TextChanged;
begin
  inherited TextChanged;
  CalcAutoSize;
end;

procedure TJvCheckBox.CalcAutoSize;
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
    DrawText(FCanvas, Caption, -1, R,
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

procedure TJvCheckBox.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCheckBox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    {$IFDEF VCL}
    //inherited SetAutoSize(Value);
    {$ENDIF VCL}
    FAutoSize := Value;
    if Value then
      WordWrap := False;
    CalcAutoSize;
  end;
end;

function TJvCheckBox.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TJvCheckBox.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, FHotTrackFontOptions);
  end;
end;

procedure TJvCheckBox.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if Value then
      AutoSize := False;
    UpdateProperties;
  end;
end;

procedure TJvCheckBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    UpdateProperties;
  end;
end;

procedure TJvCheckBox.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    UpdateProperties;
  end;
end;

procedure TJvCheckBox.SetLeftText(const Value: Boolean);
begin
  if FLeftText <> Value then
  begin
    FLeftText := Value;
    UpdateProperties;
  end;
end;

function TJvCheckBox.GetLinkedControls: TJvLinkedControls;
begin
  Result := FLinkedControls;
end;

procedure TJvCheckBox.SetLinkedControls(const Value: TJvLinkedControls);
begin
  FLinkedControls.Assign(Value);
end;

procedure TJvCheckBox.CheckLinkedControls;
var
  I: Integer;
begin
  if LinkedControls <> nil then
    for I := 0 to LinkedControls.Count - 1 do
      with LinkedControls[I] do
        if Control <> nil then
          Control.Enabled := CheckLinkControlEnabled(Self.Enabled, Self.Checked, Options);

end;

procedure TJvCheckBox.LinkedControlsChange(Sender: TObject);
begin
  CheckLinkedControls;
end;

procedure TJvCheckBox.ReadAssociated(Reader: TReader);
var
  C: TComponent;
begin
  if Owner <> nil then
    C := Owner.FindComponent(Reader.ReadIdent)
  else
    C := nil;
  if (C is TControl) and (LinkedControls <> nil) then
    LinkedControls.Add.Control := TControl(C);
end;

procedure TJvCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Associated', ReadAssociated, nil, False);
end;

{$IFDEF VCL}
procedure TJvCheckBox.BmSetCheck(var Msg: TMessage);
begin
  if not ReadOnly then
  begin
    inherited;
    CheckLinkedControls;
  end;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvCheckBox.StateChanged(State: TToggleState);
begin
  if not ReadOnly then
  begin
    inherited StateChanged(State);
    CheckLinkedControls;
  end;
end;
{$ENDIF VisualCLX}

procedure TJvCheckBox.EnabledChanged;
begin
  inherited EnabledChanged;
  CheckLinkedControls;
end;

procedure TJvCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(FLinkedControls) and not (csDestroying in ComponentState) then
    LinkedControls.Notification(AComponent, Operation);
end;

procedure TJvCheckBox.Toggle;
begin
  if not ReadOnly then
  begin
    inherited;
    CheckLinkedControls;
  end;
end;

end.

