{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Ain Valtin - ReadOnly, Alignment, Layout, RightButton
Robert Marquardt RightButton renamed to LeftText

Last Modified: 2003-12-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCheckBox;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  {$ELSE}
  QGraphics, QControls, QForms, QStdCtrls,
  {$ENDIF VCL}
  JVCLVer, JvTypes;

type
  TJvCheckBox = class(TCheckBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    {$IFDEF VCL}
    FOnCtl3DChanged: TNotifyEvent;
    {$ENDIF VCL}
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FAutoSize: Boolean;
    FAssociated: TControl;
    FCanvas: TControlCanvas;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FLeftText: Boolean;
    function GetCanvas: TCanvas;
    function GetReadOnly: Boolean;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetAssociated(const Value: TControl);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetLeftText(const Value: Boolean);
    {$IFDEF VCL}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    {$ENDIF VCL}
  protected
    {$IFDEF VCL}
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure ParentColorChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure Ctl3DChanged; dynamic;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    {$ENDIF VisualCLX}
    procedure SetAutoSize(Value: Boolean); {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalcAutoSize; virtual;
    procedure Loaded; override;
    procedure Toggle; override;
    procedure Click; override;
    procedure SetChecked(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Associated: TControl read FAssociated write SetAssociated;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFOntOptions read FHotTrackFontOptions write SetHotTrackFontOptions
      default DefaultTrackFontOptions;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    // show text to the left of the checkbox
    property LeftText: Boolean read FLeftText write SetLeftText default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    {$IFDEF VCL}
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    {$ENDIF VCL}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

uses
  JvJVCLUtils;

constructor TJvCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  FHintColor := clInfoBk;
  FOver := False;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FAutoSize := True;
  FWordWrap := False;
  FAlignment := taLeftJustify;
  FLeftText := False;
  FLayout := tlCenter;
end;

destructor TJvCheckBox.Destroy;
begin
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvCheckBox.Loaded;
begin
  inherited Loaded;
  if Assigned(FAssociated) then
    Associated.Enabled := Checked;
  CalcAutoSize;
end;

procedure TJvCheckBox.Toggle;
begin
  if not ReadOnly then
  begin
    inherited Toggle;
    if Assigned(FAssociated) then
      FAssociated.Enabled := Checked;
  end;
end;

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

{$IFDEF VCL}

procedure TJvCheckBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  Ctl3DChanged;
end;

procedure TJvCheckBox.Ctl3DChanged;
begin
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvCheckBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;
{$ENDIF VCL}

procedure TJvCheckBox.MouseEnter(AControl: TControl);
begin
  {$IFDEF VisualCLX}
  inherited MouseEnter(AControl);
  {$ENDIF VisualCLX}
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotTrackFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{$IFDEF VCL}
procedure TJvCheckBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;
{$ENDIF VCL}

procedure TJvCheckBox.MouseLeave(AControl: TControl);
begin
  {$IFDEF VisualCLX}
  inherited MouseLeave(AControl);
  {$ENDIF VisualCLX}
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFDEF VCL}
procedure TJvCheckBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  ParentColorChanged;
end;
{$ENDIF VCL}

procedure TJvCheckBox.ParentColorChanged;
begin
  {$IFDEF VisualCLX}
  inherited ParentColorChanged;
  {$ENDIF VisualCLX}
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{$IFDEF VCL}
procedure TJvCheckBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  FontChanged;
end;
{$ENDIF VCL}

procedure TJvCheckBox.FontChanged;
begin
  {$IFDEF VisualCLX}
  inherited FontChanged;
  {$ENDIF VisualCLX}
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

{$IFDEF VCL}
procedure TJvCheckBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  TextChanged;
end;
{$ENDIF VCL}

procedure TJvCheckBox.TextChanged;
begin
  {$IFDEF VisualCLX}
  inherited TextChanged;
  {$ENDIF VisualCLX}
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

procedure TJvCheckBox.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCheckBox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    {$IFDEF COMPILER6_UP}
    // inherited SetAutoSize(Value);
    {$ENDIF COMPILER6_UP}
    FAutoSize := Value;
    if Value then
      WordWrap := False;
    CalcAutoSize;
  end;
end;

procedure TJvCheckBox.SetAssociated(const Value: TControl);
begin
  if FAssociated <> Self then
  begin
    FAssociated := Value;
    if Assigned(FAssociated) then
      FAssociated.Enabled := Checked;
  end;
end;

procedure TJvCheckBox.SetChecked(Value: Boolean);
begin
  inherited SetChecked(Value);
  if Assigned(FAssociated) then
    FAssociated.Enabled := Value;
end;

procedure TJvCheckBox.Click;
begin
  inherited Click;
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

procedure TJvCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Associated) then
    Associated := nil;
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
    RecreateWnd;
  end;
end;

procedure TJvCheckBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvCheckBox.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    RecreateWnd;
  end;
end;

procedure TJvCheckBox.SetReadOnly(const Value: Boolean);
begin
  ClicksDisabled := Value;
end;

procedure TJvCheckBox.SetLeftText(const Value: Boolean);
begin
  if FLeftText <> Value then
  begin
    FLeftText := Value;
    RecreateWnd;
  end;
end;

function TJvCheckBox.GetReadOnly: Boolean;
begin
  Result := ClicksDisabled;
end;

end.

