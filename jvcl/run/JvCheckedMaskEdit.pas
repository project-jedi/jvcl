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
located at http://jvcl.delphi-jedi.org

Description:
  A simple TCustomMaskEdit descendant with an optional checkbox control in front
  of the text area.

Known Issues:
 - BiDi support (checkbox should probably be on the right for RTL)
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckedMaskEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, StdCtrls, Types,
  JvMaskEdit;

type
  TJvCustomCheckedMaskEdit = class(TJvCustomMaskEdit)
  private
    FCheck: TCheckBox;
    { (rb) JvBaseEdits.pas name: FFormatting }
    FInternalChange: Boolean;
    FOnCheckClick: TNotifyEvent;
    procedure CheckClick(Sender: TObject);
    function GetShowCheckBox: Boolean;
  protected
    procedure DoCheckClick; dynamic;
    procedure DoKillFocus(const ANextControl: TWinControl); override;
    procedure EnabledChanged; override;

    function GetChecked: Boolean; virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetShowCheckbox(const AValue: Boolean); virtual;

    procedure GetInternalMargins(var ALeft, ARight: Integer); override;

    procedure UpdateControls; override;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure Change; override;
    procedure BeginInternalChange;
    procedure EndInternalChange;
    function InternalChanging: Boolean;
  protected
    property AutoSize default False;
    property Checked: Boolean read GetChecked write SetChecked;
    property ShowCheckBox: Boolean read GetShowCheckBox write SetShowCheckbox default False;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCheckedMaskEdit = class(TJvCustomCheckedMaskEdit)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
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
    property ParentFlat;
    property PasswordChar;
    property ProtectPassword;
    property OnKillFocus;
    property OnSetFocus;
    property OnEndDock;
    property OnStartDock;
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
    property ShowButton;
    property ShowCheckBox;
    property ShowHint;
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

{ TODO -oahuser -cCLX : Isn't that also something for VCL? }


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

procedure TJvCustomCheckedMaskEdit.DoKillFocus(const ANextControl: TWinControl);
begin
  if ANextControl <> FCheck then
    inherited DoKillFocus(ANextControl);
end;

procedure TJvCustomCheckedMaskEdit.EnabledChanged;
begin
  { propagate to child controls: }
  if ShowCheckBox then
    FCheck.Enabled := Self.Enabled;
  inherited EnabledChanged;
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
    // ensure the text starts 2 points from the checkbox edge
    {$IFDEF JVCLThemesEnabled}
    if StyleServices.Enabled then
      ALeft := ALeft + 1;
    {$ENDIF JVCLThemesEnabled}
    if BorderStyle = bsNone then
      ALeft := ALeft + 1
    else
    if not Ctl3D then
      ALeft := ALeft - 1;
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
      FCheck.Parent := Self;
      // FCheck.Align := alLeft;
      if HotTrack then
        FCheck.Left := 1;
      FCheck.Top := 1;
      FCheck.Height := ClientHeight - 2;
      FCheck.Width := 15;
      FCheck.Anchors := [akLeft, akTop, akBottom];
      FCheck.Alignment := taLeftJustify;
      FCheck.TabStop := False;
      FCheck.OnClick := CheckClick;
      FCheck.Visible := True;
      FCheck.Enabled := Enabled;
    end
    else
      FreeAndNil(FCheck);

    UpdateControls;
    UpdateMargins;
    Repaint;
  end;
end;



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



{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.