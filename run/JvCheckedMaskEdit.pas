{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckedMaskEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen@lucatec.com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-12-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A simple TCustomMaskEdit descendant with an optional checkbox control in front
 of the text area.

 Known issues / not (yet) implemented features:

 - BiDi support (checkbox should probably be on the right for RTL)
}

unit JvCheckedMaskEdit;

interface

uses
  Classes, Controls, Graphics, StdCtrls, Messages, Mask,
  JvMaskEdit;

type
  TJvCustomCheckedMaskEdit = class(TJvCustomMaskEdit)
  private
    FCheck: TCheckBox;
    FInternalChange: Boolean;
    FOnCheckClick: TNotifyEvent;
    procedure CheckClick(Sender: TObject);
    function GetShowCheckbox: Boolean;
  protected
    procedure DoCheckClick; dynamic;
    procedure DoCtl3DChanged; override;
    procedure DoEnabledChanged; override;
    procedure DoKillFocus(const ANextControl: TWinControl); override;

    function GetChecked: Boolean; virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetShowCheckbox(const AValue: Boolean); virtual;

    procedure GetInternalMargins(var ALeft, ARight: Integer); virtual;
    procedure UpdateControls; dynamic;

    procedure CreateParams(var AParams: TCreateParams); override;
    procedure CreateWnd; override;

    procedure Change; override;
    procedure Resize; override;

    procedure BeginInternalChange;
    procedure EndInternalChange;
    function InternalChanging: Boolean;
  protected
    property Checked: Boolean read GetChecked write SetChecked;
    property ShowCheckbox: Boolean read GetShowCheckbox write SetShowCheckbox default False;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvCheckedMaskEdit = class(TJvCustomCheckedMaskEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize default False;
    property BorderStyle;
    property Caret;
    property CharCase;
    property Checked;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property Cursor;
    property Ctl3D;
    property DisabledColor;
    property DisabledTextColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GroupIndex;
    property HintColor;
    property HotTrack;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ProtectPassword;
    property ReadOnly;
    property ShowHint;
    property ShowCheckbox;
    property Text;
    property TabOrder;
    property Visible;

    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnCtl3DChanged;
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
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnSetFocus;
    property OnStartDrag;
  end;

implementation

uses
  Windows, Forms, SysUtils,
  JvTypes;

constructor TJvCustomCheckedMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCheck := nil;
  FInternalChange := False;

  AutoSize := False;
  Height := 21;
  TabStop := True;
end;

procedure TJvCustomCheckedMaskEdit.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  {ensure the child controls do not overlap}
  AParams.Style := AParams.Style or WS_CLIPCHILDREN;
end;

procedure TJvCustomCheckedMaskEdit.CreateWnd;
begin
  inherited CreateWnd;
  UpdateControls;
end;

destructor TJvCustomCheckedMaskEdit.Destroy;
begin
  if ShowCheckbox then
    FCheck.OnClick := nil;
  inherited Destroy;
end;

function TJvCustomCheckedMaskEdit.GetChecked: Boolean;
begin
  if ShowCheckbox then
    Result := FCheck.Checked
  else
    Result := False; //should this really be the default?
end;

procedure TJvCustomCheckedMaskEdit.SetChecked(const AValue: Boolean);
begin
  if ShowCheckbox and (FCheck.Checked <> AValue) then
  begin
    FCheck.Checked := AValue;
    Change;
  end;
  {TODO : Maybe Checked should be accessible even without the checkbox.
          The value could be cached in a state field and applied when the
          checkbox is instantiated.}
end;

function TJvCustomCheckedMaskEdit.GetShowCheckbox: Boolean;
begin
  Result := Assigned(FCheck);
end;

procedure TJvCustomCheckedMaskEdit.SetShowCheckbox(const AValue: Boolean);
begin
  {The checkbox will only get instantiated when ShowCheckbox is set to True;
   setting it to false frees the checkbox.}
  if ShowCheckbox <> AValue then
  begin
    if AValue then
    begin
      FCheck := TCheckBox.Create(Self);
      with FCheck do
      begin
        Parent := Self;
//        Align:= alLeft;
        if HotTrack then
          Left := 1;
        Top := 1;
        Width := 15;
        Height := Self.ClientHeight - 2;
        Anchors := [akLeft, akTop, akBottom];
        Alignment := taLeftJustify;
        TabStop := False;
        OnClick := CheckClick;
        Visible := True;
      end;
    end
    else
      FreeAndNil(FCheck);
  end;
  UpdateControls;
end;

procedure TJvCustomCheckedMaskEdit.UpdateControls;
var
  lLeft, lRight: Integer;
begin
  {UpdateControls gets called whenever the layout of child controls changes.
   It uses GetInternalMargins to determine the left and right margins of the
   actual text area.}
  lLeft := 0;
  lRight := 0;
  GetInternalMargins(lLeft, lRight);
  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN, MakeLong(lLeft, lRight));
end;

procedure TJvCustomCheckedMaskEdit.GetInternalMargins( var ALeft, ARight: Integer);
begin
  {This gets called by UpodateControls and should be overridden by descendants
   that add additional child controls.}
  if ShowCheckBox then
    ALeft := FCheck.Left + FCheck.Width;
end;

procedure TJvCustomCheckedMaskEdit.Change;
begin
  {Overridden to suppress change handling during internal operations. If
   descendants override Change again it is their responsibility to repeat the
   check for InternalChanging.}
  if not InternalChanging then
    inherited Change;
end;

procedure TJvCustomCheckedMaskEdit.Resize;
begin
  inherited Resize;
  UpdateControls;
end;


{Begin/EndInternalChange and InternalChanging implement a simple locking
 mechanism to prevent change processing and display updates during internal
 operations. If descendants require nested calls to Begin/EndInternalChange they
 should override these methods to implement a better suited mechanism,
 e.g. a lock counter.}

procedure TJvCustomCheckedMaskEdit.BeginInternalChange;
begin
  if FInternalChange then
    raise EJVCLException.Create('TJvCustomCheckedMaskEdit.BeginInternalChange: Unsupported nested call!');
  FInternalChange := True;
end;

procedure TJvCustomCheckedMaskEdit.EndInternalChange;
begin
  { TODO : if this assertion ever fails, it's time to switch to a counted locking scheme }
  if not FInternalChange then
    raise EJVCLException.Create('TJvCustomCheckedMaskEdit.EndInternalChange: Unsupported nested call!');
  FInternalChange := False;
end;

function TJvCustomCheckedMaskEdit.InternalChanging: Boolean;
begin
  Result := FInternalChange;
end;

procedure TJvCustomCheckedMaskEdit.CheckClick(Sender: TObject);
begin
  // call SetChecked to allow descendants to validate the new value:
  Checked := FCheck.Checked;
  DoCheckClick;
end;

procedure TJvCustomCheckedMaskEdit.DoCheckClick;
begin
  if Assigned(OnCheckClick) then
    OnCheckClick(Self);
end;

procedure TJvCustomCheckedMaskEdit.DoCtl3DChanged;
begin
  inherited DoCtl3DChanged;
  { propagate to child conrols: }
  if ShowCheckbox then
  begin
    FCheck.Ctl3D := Self.Ctl3D;
    // adjust layout quirks:
    if Self.Ctl3D then
      FCheck.Left := 0
    else
      FCheck.Left := 1;
  end;
  UpdateControls;
end;

procedure TJvCustomCheckedMaskEdit.DoEnabledChanged;
begin
  { propagate to child controls: }
  if ShowCheckbox then
    FCheck.Enabled := Self.Enabled;
  inherited DoEnabledChanged;
end;

procedure TJvCustomCheckedMaskEdit.DoKillFocus(const ANextControl: TWinControl);
begin
  if ANextControl <> FCheck then
    inherited DoKillFocus(ANextControl);
end;

end.
