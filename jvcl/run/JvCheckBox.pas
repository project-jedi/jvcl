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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckBox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  JvTypes, JvExStdCtrls, JvLinkedControls, JvDataSourceIntf;

const
  DefaultValueChecked = '1';
  DefaultValueUnchecked = '0';

type
  TJvCheckBox = class;

  TJvCheckBoxDataConnector = class(TJvFieldDataConnector)
  private
    FCheckBox: TJvCheckBox;
    FValueChecked: string;
    FValueUnchecked: string;
    procedure SetValueChecked(const Value: string);
    procedure SetValueUnchecked(const Value: string);
    function IsValueCheckedStored: Boolean;
    function IsValueUncheckedStored: Boolean;
  protected
    procedure UpdateData; override;
    procedure RecordChanged; override;
  public
    constructor Create(ACheckBox: TJvCheckBox);
    procedure Assign(Source: TPersistent); override;
  published
    property ValueChecked: string read FValueChecked write SetValueChecked stored IsValueCheckedStored;
    property ValueUnchecked: string read FValueUnchecked write SetValueUnchecked stored IsValueUncheckedStored;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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
    FDataConnector: TJvCheckBoxDataConnector;
    FCheckingLinkedControls: Boolean;
    function GetCanvas: TCanvas;
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetLeftText(const Value: Boolean);
    procedure SetLinkedControls(const Value: TJvLinkedControls);
    procedure ReadAssociated(Reader: TReader);
    procedure SetDataConnector(const Value: TJvCheckBoxDataConnector);
    function IsHotTrackFontStored: Boolean;
    procedure SetClientSize(W, H: Integer);
  protected
    function CreateDataConnector: TJvCheckBoxDataConnector; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure TextChanged; override;
    procedure FontChanged; override;
    procedure EnabledChanged;override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure UpdateProperties;
    procedure CalcAutoSize; virtual;
    procedure Loaded; override;
    procedure LinkedControlsChange(Sender: TObject);
    procedure CheckLinkedControls; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure BmSetCheck(var Msg: TMessage); message BM_SETCHECK;
    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Toggle; override;
    procedure SetFocus; override;

    property Canvas: TCanvas read GetCanvas;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    // link the enabled state of other controls to the checked and/or enabled state of this control
    property LinkedControls: TJvLinkedControls read FLinkedControls write SetLinkedControls;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HintColor;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotTrackFont write SetHotTrackFont stored IsHotTrackFontStored;
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

    property DataConnector: TJvCheckBoxDataConnector read FDataConnector write SetDataConnector;
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
  SysUtils,
  JvJCLUtils, JvJVCLUtils;

//=== { TJvCheckBoxDataConnector } ===========================================

constructor TJvCheckBoxDataConnector.Create(ACheckBox: TJvCheckBox);
begin
  inherited Create;
  FCheckBox := ACheckBox;
  FValueChecked := DefaultValueChecked;
  FValueUnchecked := DefaultValueUnchecked;
end;

function TJvCheckBoxDataConnector.IsValueCheckedStored: Boolean;
begin
  Result := FValueChecked <> DefaultValueChecked;
end;

function TJvCheckBoxDataConnector.IsValueUncheckedStored: Boolean;
begin
  Result := FValueUnchecked <> DefaultValueUnchecked;
end;

procedure TJvCheckBoxDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvCheckBoxDataConnector then
  begin
    FValueChecked := TJvCheckBoxDataConnector(Source).ValueChecked;
    FValueUnchecked := TJvCheckBoxDataConnector(Source).ValueUnchecked;
    Reset;
  end;
end;

procedure TJvCheckBoxDataConnector.RecordChanged;
begin
  if Field.IsValid and (ValueChecked <> '') and (ValueUnchecked <> '') then
  begin
    if not (csDesigning in FCheckBox.ComponentState) then
      FCheckBox.ReadOnly := not Field.CanModify;
    if not Field.IsNull then
      FCheckBox.Checked := AnsiCompareText(Field.AsString, ValueUnchecked) <> 0
    else
      FCheckBox.State := cbGrayed;
  end
  else
  begin
    FCheckBox.State := cbGrayed;
    if not (csDesigning in FCheckBox.ComponentState) then
      FCheckBox.ReadOnly := True;
  end;
end;

procedure TJvCheckBoxDataConnector.UpdateData;
begin
  if Field.CanModify and Field.IsValid and (ValueChecked <> '') and (ValueUnchecked <> '') then
  begin
    if FCheckBox.Checked then
      Field.AsString := ValueChecked
    else
      Field.AsString := ValueUnchecked;
  end;
end;

procedure TJvCheckBoxDataConnector.SetValueChecked(const Value: string);
begin
  if Value <> FValueChecked then
  begin
    FValueChecked := Value;
    Reset;
  end;
end;

procedure TJvCheckBoxDataConnector.SetValueUnchecked(const Value: string);
begin
  if Value <> FValueUnchecked then
  begin
    FValueUnchecked := Value;
    Reset;
  end;
end;

//=== { TJvCheckBox } ========================================================

constructor TJvCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataConnector := CreateDataConnector;
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
  FDataConnector.Free;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvCheckBox.Loaded;
begin
  inherited Loaded;
  CheckLinkedControls;
  CalcAutoSize;
  DataConnector.Reset;
end;

function TJvCheckBox.CreateDataConnector: TJvCheckBoxDataConnector;
begin
  Result := TJvCheckBoxDataConnector.Create(Self);
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

procedure TJvCheckBox.UpdateProperties;
begin
  RecreateWnd;
end;

procedure TJvCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #8, ' ':
      DataConnector.Modify;
    #27:
      DataConnector.Reset;
  end;
end;

procedure TJvCheckBox.DoExit;
begin
  try
    DataConnector.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited DoExit;
end;

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

procedure TJvCheckBox.SetClientSize(W, H: Integer);
var
  Client: TRect;
begin
  Client := GetClientRect;
  SetBounds(Left, Top, Width - Client.Right + W, Height - Client.Bottom + H);
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
  // This is slower than GetTextExtentPoint but it does consider hotkeys
  if Caption <> '' then
  begin
    R := ClientRect;
    DrawText(FCanvas, Caption, -1, R, Flags[WordWrap] or DT_LEFT or DT_NOCLIP or DT_CALCRECT);
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
  SetClientSize(AWidth, AHeight);
end;

procedure TJvCheckBox.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;

procedure TJvCheckBox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    //inherited SetAutoSize(Value);
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

function TJvCheckBox.IsHotTrackFontStored: Boolean;
begin
  Result := IsHotTrackFontDfmStored(HotTrackFont, Font, HotTrackFontOptions);
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

procedure TJvCheckBox.SetLinkedControls(const Value: TJvLinkedControls);
begin
  FLinkedControls.Assign(Value);
end;

procedure TJvCheckBox.CheckLinkedControls;
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
              Control.Enabled := CheckLinkControlEnabled(Self.Enabled, Self.Checked, Options);
    finally
      FCheckingLinkedControls := False;
    end;
  end;
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

procedure TJvCheckBox.SetDataConnector(const Value: TJvCheckBoxDataConnector);
begin
  if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

type
  TWinControlAccess = class(TWinControl);

procedure TJvCheckBox.SetFocus;
var
  I: Integer;
  FocusLinkedControl: TJvLinkedControl;
  FocusTargetControl: TControl;
begin
  inherited SetFocus;

  // We want to transfer our own focus either to our children or to
  // the first focus accepting sibling, depending on the direction
  // that the user asked for.
  if GetKeyState(VK_SHIFT) >= 0 then
  begin
    for I := 0 to LinkedControls.Count - 1 do
    begin
      FocusLinkedControl := LinkedControls[I];
      if loForceFocus in FocusLinkedControl.Options then
      begin
        FocusTargetControl := FocusLinkedControl.Control;
        if (FocusTargetControl is TWinControl) and TWinControl(FocusTargetControl).CanFocus then
        begin
          TWinControl(FocusTargetControl).SetFocus;
          Break; // found the new focus owner
        end;
      end;
    end;
  end
  else
  if LinkedControls.Count > 0 then
    TWinControlAccess(Parent).SelectNext(Self, False, True);
end;

procedure TJvCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Associated', ReadAssociated, nil, False);
end;

procedure TJvCheckBox.BmSetCheck(var Msg: TMessage);
begin
//  if not ReadOnly then
//  begin
    inherited;
    CheckLinkedControls;
//  end;
end;

procedure TJvCheckBox.EnabledChanged;
begin
  inherited EnabledChanged;
  CheckLinkedControls;
end;

procedure TJvCheckBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(FLinkedControls) then
    LinkedControls.Notification(AComponent, Operation);
end;

procedure TJvCheckBox.Toggle;
begin
  if not ReadOnly then
  begin
    inherited;
    CheckLinkedControls;
    if not (csLoading in ComponentState) then
      DataConnector.Modify;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
