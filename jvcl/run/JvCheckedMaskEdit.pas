{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckedMaskEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen att lucatec dott com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A simple TCustomMaskEdit descendant with an optional checkbox control in front
  of the text area.

Known Issues:
 - BiDi support (checkbox should probably be on the right for RTL)
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCheckedMaskEdit;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls,
  JvMaskEdit;

type
  TJvCustomCheckedMaskEdit = class(TJvCustomMaskEdit)
  private
    FCheck: TCheckBox;
    { (rb) JvBaseEdits.pas name: FFormatting }
    FInternalChange: Boolean;
    FOnCheckClick: TNotifyEvent;
    {$IFDEF VisualCLX}
    FOnEnabledChanged: TNotifyEvent;
    {$ENDIF VisualCLX}
    procedure CheckClick(Sender: TObject);
    function GetShowCheckBox: Boolean;
  protected
    procedure DoCheckClick; dynamic;
    {$IFDEF VCL}
    procedure DoKillFocusEvent(const ANextControl: TWinControl); override;
    {$ENDIF VCL}
    procedure EnabledChanged; override;

    function GetChecked: Boolean; virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetShowCheckbox(const AValue: Boolean); virtual;

    procedure GetInternalMargins(var ALeft, ARight: Integer); override;

    {$IFDEF VCL}
    procedure UpdateControls; override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure ColorChanged; override;
    {$ENDIF VisualCLX}
    procedure Change; override;
    procedure BeginInternalChange;
    procedure EndInternalChange;
    function InternalChanging: Boolean;
  protected
    property AutoSize default False;
    property Checked: Boolean read GetChecked write SetChecked;
    property ShowCheckBox: Boolean read GetShowCheckBox write SetShowCheckbox default False;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
    {$IFDEF VisualCLX}
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvCheckedMaskEdit = class(TJvCustomCheckedMaskEdit)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property ButtonWidth;
    property CharCase;
    property Checked;
    property ClipboardCommands;
    property ClickKey;
    property Color;
    property Constraints;
    property DisabledColor;
    property DisabledTextColor;
    property GroupIndex;
    {$IFDEF VCL}
    {property BiDiMode;}
    property Caret;
    property DragCursor;
    property DragKind;
    property Flat;
    property HotTrack;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    {property ParentBiDiMode;}
    property PasswordChar;
    property ProtectPassword;
    property OnKillFocus;
    property OnSetFocus;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property DirectInput;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property HideSelection;
    property HintColor;
    property ImageIndex;
    property ImageKind;
    property Images;
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckBox;
    property Text;
    property TabOrder;
    {property TabStop;} { (rb) Why disabled?}
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnabledChanged;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, Forms,
  JvTypes, JvResources, JvThemes;

//=== { TJvCustomCheckedMaskEdit } ===========================================

{Begin/EndInternalChange and InternalChanging implement a simple locking
 mechanism to prevent change processing and display updates during internal
 operations. If descendants require nested calls to Begin/EndInternalChange they
 should override these methods to implement a better suited mechanism,
 e.g. a lock counter.}

procedure TJvCustomCheckedMaskEdit.BeginInternalChange;
begin
  if FInternalChange then
    raise EJVCLException.CreateRes(@RsEBeginUnsupportedNestedCall);
  FInternalChange := True;
end;

procedure TJvCustomCheckedMaskEdit.Change;
begin
  {Overridden to suppress change handling during internal operations. If
   descendants override Change again it is their responsibility to repeat the
   check for InternalChanging.}
  if not InternalChanging then
    inherited Change;
end;

procedure TJvCustomCheckedMaskEdit.CheckClick(Sender: TObject);
begin
  // call SetChecked to allow descendants to validate the new value:
  Checked := FCheck.Checked;
  DoCheckClick;
end;

{$IFDEF VisualCLX}
procedure TJvCustomCheckedMaskEdit.ColorChanged;
begin
  inherited;
  if Assigned(FCheck) then
    FCheck.Color := Color;
end;
{$ENDIF VisualCLX}

constructor TJvCustomCheckedMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheck := nil;
  FInternalChange := False;

  AutoSize := False;
  Height := 21;
  { (rb) ?? }
  TabStop := True;
end;

destructor TJvCustomCheckedMaskEdit.Destroy;
begin
  if ShowCheckBox then
    FCheck.OnClick := nil;
  inherited Destroy;
end;

procedure TJvCustomCheckedMaskEdit.DoCheckClick;
begin
  if Assigned(FOnCheckClick) then
    FOnCheckClick(Self);
end;

{$IFDEF VCL}
procedure TJvCustomCheckedMaskEdit.DoKillFocusEvent(const ANextControl: TWinControl);
begin
  if ANextControl <> FCheck then
    inherited DoKillFocusEvent(ANextControl);
end;
{$ENDIF VCL}

procedure TJvCustomCheckedMaskEdit.EnabledChanged;
begin
  { propagate to child controls: }
  if ShowCheckBox then
    FCheck.Enabled := Self.Enabled;
  inherited EnabledChanged;
  {$IFDEF VisualCLX}
  if Assigned(FOnEnabledChanged) then
     FOnEnabledChanged(Self);
  {$ENDIF VisualCLX}
end;

procedure TJvCustomCheckedMaskEdit.EndInternalChange;
begin
  { TODO : if this assertion ever fails, it's time to switch to a counted locking scheme }
  if not FInternalChange then
    raise EJVCLException.CreateRes(@RsEEndUnsupportedNestedCall);
  FInternalChange := False;
end;

function TJvCustomCheckedMaskEdit.GetChecked: Boolean;
begin
  if ShowCheckBox then
    Result := FCheck.Checked
  else
    Result := False; // should this really be the default?
end;

procedure TJvCustomCheckedMaskEdit.GetInternalMargins( var ALeft, ARight: Integer);
begin
  {This gets called by UpdateMargins and should be overridden by descendants
   that add additional child controls.}

  inherited GetInternalMargins(ALeft, ARight);

  if ShowCheckBox then
  begin
    ALeft := FCheck.Left + FCheck.Width;
    {$IFDEF VCL}
    // ensure the text starts 2 points from the checkbox edge
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      ALeft := ALeft + 1;
    {$ENDIF JVCLThemesEnabled}
    if BorderStyle = bsNone then
      ALeft := ALeft + 1
    else
    if not Ctl3D then
      ALeft := ALeft - 1;
    {$ENDIF VCL}
  end;
end;

function TJvCustomCheckedMaskEdit.GetShowCheckBox: Boolean;
begin
  Result := Assigned(FCheck);
end;

function TJvCustomCheckedMaskEdit.InternalChanging: Boolean;
begin
  Result := FInternalChange;
end;

procedure TJvCustomCheckedMaskEdit.SetChecked(const AValue: Boolean);
begin
  if ShowCheckBox and (FCheck.Checked <> AValue) then
  begin
    FCheck.Checked := AValue;
    Change;
  end;
  {TODO : Maybe Checked should be accessible even without the checkbox.
          The value could be cached in a state field and applied when the
          checkbox is instantiated.}
end;

procedure TJvCustomCheckedMaskEdit.SetShowCheckbox(const AValue: Boolean);
begin
  {The checkbox will only get instantiated when ShowCheckBox is set to True;
   setting it to false frees the checkbox.}
  if ShowCheckBox <> AValue then
  begin
    if AValue then
    begin
      FCheck := TCheckBox.Create(Self);
      with FCheck do
      begin
        {$IFDEF VCL}
        Parent := Self;
        // Align := alLeft;
        if HotTrack then
          Left := 1;
        Top := 1;
        Height := Self.ClientHeight - 2;
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        Parent := Self.ClientArea;
        Height := Self.ClientArea.Height;
        Color := Self.Color;
        Left := 3;
        {$ENDIF VisualCLX}
        Width := 15;
        Anchors := [akLeft, akTop, akBottom];
        Alignment := taLeftJustify;
        TabStop := False;
        OnClick := CheckClick;
        Visible := True;
        Enabled := Self.Enabled;
      end;
    end
    else
      FreeAndNil(FCheck);

    UpdateControls;
    UpdateMargins;
    Repaint;
  end;
end;

{$IFDEF VCL}

procedure TJvCustomCheckedMaskEdit.UpdateControls;
begin
  { delay until Loaded }
  if csLoading in ComponentState then
    Exit;

  inherited UpdateControls;

  { propagate to child controls: }
  if ShowCheckBox then
  begin
    FCheck.Ctl3D := Self.Ctl3D;

    { Adjust layout quirks:
      We want to place the checkbox 2 points from the edge

                        BorderStyle
                     bsNone  bsSingle
      Ctl3d   Yes:      0       0
              No :      0       1
    }
    if not Self.Ctl3D and (Self.BorderStyle = bsSingle) then
      FCheck.Left := 1
    else
      FCheck.Left := 0;
  end;
end;

procedure TJvCustomCheckedMaskEdit.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if (Msg.Result = HTCLIENT) and ShowCheckBox and not (csDesigning in ComponentState) then
  begin
    P := Point(FCheck.Left + FCheck.Width, FCheck.Top);
    Windows.ClientToScreen(FCheck.Handle, P);
    if Msg.XPos < P.X then
      Msg.Result := HTBORDER; {HTCAPTION;}
  end;
end;

{$ENDIF VCL}

end.
