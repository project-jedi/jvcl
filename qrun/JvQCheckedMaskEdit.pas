{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQCheckedMaskEdit;

interface

uses
  QWindows, QMessages, QForms, SysUtils, Classes, QControls, Types, QGraphics, QStdCtrls, 
  QComboEdits, JvQExComboEdits, QComCtrlsEx, 
  QMask, JvQMaskEdit;

type  
  TJvCustomCheckedMaskEdit = class(TJvExCustomComboMaskEdit) 
  private
    FCheck: TCheckBox;
    FInternalChange: Boolean;
    FOnCheckClick: TNotifyEvent; 
    FOnEnabledChanged: TNotifyEvent; 
    procedure CheckClick(Sender: TObject);
    function GetShowCheckBox: Boolean; 
  protected
    procedure DoCheckClick; dynamic; 
    procedure EnabledChanged; override;

    function GetChecked: Boolean; virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetShowCheckBox(const AValue: Boolean); virtual;

    procedure GetInternalMargins(var ALeft, ARight: Integer); virtual;
    procedure UpdateControls; dynamic;
  
    procedure CreateWidget; override;
    procedure ColorChanged; override; 
    procedure Change; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure BeginInternalChange;
    procedure EndInternalChange;
    function InternalChanging: Boolean;
  protected
    property Checked: Boolean read GetChecked write SetChecked;
    property ShowCheckBox: Boolean read GetShowCheckBox write SetShowCheckBox default False;
    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick; 
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;


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
    property CharCase;
    property Checked;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property Cursor;
//    property DisabledColor;
//    property DisabledTextColor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HintColor;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckBox;
    property Text;
    property TabOrder;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCheckClick;
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
  JvQTypes, JvQResources;

constructor TJvCustomCheckedMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheck := nil;
  FInternalChange := False;

  AutoSize := False;
  Height := 21;
  TabStop := True;
end;

destructor TJvCustomCheckedMaskEdit.Destroy;
begin
  if ShowCheckBox then
    FCheck.OnClick := nil;
  inherited Destroy;
end;




procedure TJvCustomCheckedMaskEdit.CreateWidget;
begin
  inherited CreateWidget;
  UpdateControls;
end;

procedure TJvCustomCheckedMaskEdit.ColorChanged;
begin
  inherited;
  if assigned(FCheck) then
    FCheck.Color := Color;
end;


function TJvCustomCheckedMaskEdit.GetChecked: Boolean;
begin
  if ShowCheckBox then
    Result := FCheck.Checked
  else
    Result := False; // should this really be the default?
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

function TJvCustomCheckedMaskEdit.GetShowCheckBox: Boolean;
begin
  Result := Assigned(FCheck);
end;

procedure TJvCustomCheckedMaskEdit.SetShowCheckBox(const AValue: Boolean);
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
        Parent := Self.ClientArea;
        Align := alLeft; 
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
  LLeft, LRight: Integer; 
  Loc: TRect; 
begin
  {UpdateControls gets called whenever the layout of child controls changes.
   It uses GetInternalMargins to determine the left and right margins of the
   actual text area.}
  LLeft := 0;
  LRight := 0;
  GetInternalMargins(LLeft, LRight);  
  SetRect(Loc, LLeft, 0, ClientWidth - LRight, ClientHeight);
  SetEditorRect(@Loc); 
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

procedure TJvCustomCheckedMaskEdit.Loaded;
begin
  inherited Loaded;
  UpdateControls;
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
    raise EJVCLException.CreateRes(@RsEBeginUnsupportedNestedCall);
  FInternalChange := True;
end;

procedure TJvCustomCheckedMaskEdit.EndInternalChange;
begin
  { TODO : if this assertion ever fails, it's time to switch to a counted locking scheme }
  if not FInternalChange then
    raise EJVCLException.CreateRes(@RsEEndUnsupportedNestedCall);
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
  if Assigned(FOnCheckClick) then
    FOnCheckClick(Self);
end;



procedure TJvCustomCheckedMaskEdit.EnabledChanged;
begin
  { propagate to child controls: }
  if ShowCheckBox then
    FCheck.Enabled := Self.Enabled;
  inherited EnabledChanged;
  if assigned(FOnEnabledChanged) then
     FOnEnabledChanged(self);
end;

(*
procedure TJvCustomCheckedMaskEdit.DoKillFocusEvent(const ANextControl: TWinControl);
begin
  if ANextControl <> FCheck then
    inherited DoKillFocusEvent(ANextControl);
end;
*)
end.
