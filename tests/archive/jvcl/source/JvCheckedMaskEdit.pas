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

Last Modified: 2002-10-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JVCL.INC}

{ A simple TCustomMaskEdit descendant with an optional checkbox control in front
 of the text area.

 Known issues / not (yet) implemented features:

 - BiDi support (checkbox should probably be on the right for RTL)
}

unit JvCheckedMaskEdit;

interface

uses
  Classes,
  Controls,
  Graphics,
  StdCtrls,
  Messages,
  Mask,
  JvTypes, 
  JVCLVer;


type
  TJvCustomCheckedMaskEdit = class(TCustomMaskEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCheck: TCheckBox;
    FEntering: Boolean;
    FHintColor: TColor;
    FHotTrack: Boolean;
    FInternalChange: Boolean;
    FLeaving: Boolean;
    FMouseOver: Boolean;
    FSavedHintColor: TColor;

    FOnCheckClick: TNotifyEvent;
    FOnCtl3DChanged: TNotiFyEvent;
    FOnEnabledChanged: TNotifyEvent;
    FOnGetFocus: TJvFocusChangeEvent;
    FOnLoseFocus: TJvFocusChangeEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;

    procedure CheckClick(Sender: TObject);

    function GetShowCheckbox: Boolean;
    procedure SetHotTrack(const AValue: Boolean);

    procedure CMCtl3DChanged(var AMessage: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var AMessage: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var AMessage: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMessage: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var AMessage: TMessage);
      message CM_PARENTCOLORCHANGED;
    procedure WMKillFocus(var AMessage: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var AMessage: TMessage); message WM_SETFOCUS;
  protected
    procedure DoCheckClick; dynamic;
    procedure DoCtl3DChanged; virtual;
    procedure DoEnabledChanged; virtual;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure DoParentColorChanged; dynamic;
    procedure GetFocus(const APreviousControl: TWinControl);dynamic;
    procedure LoseFocus(const AFocusControl: TWinControl); virtual;

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

    function Entering: Boolean;
    function Leaving: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  protected
    property HintColor: TColor read FHintColor write FHintColor;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;

    property Checked: Boolean read GetChecked write SetChecked;
    property ShowCheckbox: Boolean read GetShowCheckbox write SetShowCheckbox;

    property OnCheckClick: TNotifyEvent read FOnCheckClick write FOnCheckClick;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged
      write FOnCtl3DChanged;
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged
      write FOnEnabledChanged;
    property OnGetFocus: TJvFocusChangeEvent read FOnGetFocus write FOnGetFocus;
    property OnLoseFocus: TJvFocusChangeEvent read FOnLoseFocus
      write FOnLoseFocus;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChanged: TNotifyEvent read FOnParentColorChanged
      write FOnParentColorChanged;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL
      stored False;
  end;

  TJvCheckedMaskEdit = class(TJvCustomCheckedMaskEdit)
  published
    property Anchors;
    property AutoSelect;
    property AutoSize default False;
    property BorderStyle;
    property CharCase;
    property Checked;
    property Color;
    property Constraints;
    property Cursor;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HintColor default clInfoBk;
    property HotTrack default False;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckbox default False;
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
    property OnGetFocus;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoseFocus;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChanged;
    property OnStartDrag;
  end;

implementation

uses
  Windows,
  Forms,
  SysUtils;

{ TLucaCheckedMaskEdit }

constructor TJvCustomCheckedMaskEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCheck := NIL;
  FInternalChange := False;
  FEntering := False;
  FLeaving := False;

  FHotTrack := False;
  FHintColor := clInfoBk;
  FMouseOver := False;

  AutoSize := False;
  Height := 21;
  TabStop := True;
end;

procedure TJvCustomCheckedMaskEdit.CreateParams(var AParams: TCreateParams);
begin
  inherited;
  {ensure the child controls do not overlap}
  AParams.Style := AParams.Style or WS_CLIPCHILDREN;
end;

procedure TJvCustomCheckedMaskEdit.CreateWnd;
begin
  inherited;
  UpdateControls;
end;

destructor TJvCustomCheckedMaskEdit.Destroy;
begin
  if(ShowCheckbox) then
    FCheck.OnClick := NIL;
  inherited;
end;

function TJvCustomCheckedMaskEdit.GetChecked: Boolean;
begin
  if(ShowCheckbox) then
    result := FCheck.Checked
  else
    result := False; //should this really be the default?
end;

procedure TJvCustomCheckedMaskEdit.SetChecked(const AValue: Boolean);
begin
  if(ShowCheckbox) and(FCheck.Checked <> AValue) then
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
  result := Assigned(FCheck);
end;

procedure TJvCustomCheckedMaskEdit.SetShowCheckbox(const AValue: Boolean);
begin
  {The checkbox will only get instantiated when ShowCheckbox is set to True;
   setting it to false frees the checkbox.}
  if(ShowCheckbox <> AValue) then
  begin
    if(AValue) then
    begin
      FCheck := TCheckbox.Create(Self);
      with(FCheck) do
      begin
        Parent := Self;
//        Align:= alLeft;
        if(HotTrack) then
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
    begin
      FreeAndNil(FCheck);
    end;
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
  if(ShowCheckBox) then
    ALeft := FCheck.Left + FCheck.Width;
end;

procedure TJvCustomCheckedMaskEdit.Change;
begin
  {Overridden to suppress change handling during internal operations. If
   descendants override Change again it is their responsibility to repeat the
   check for InternalChanging.}
  if(InternalChanging) then
    Exit;

  inherited;
end;

procedure TJvCustomCheckedMaskEdit.Resize;
begin
  inherited;
  UpdateControls;
end;


{Begin/EndInternalChange and InternalChanging implement a simple locking
 mechanism to prevent change processing and display updates during internal
 operations. If descendants require nested calls to Begin/EndInternalChange they
 should override these methods to implement a better suited mechanism,
 e.g. a lock counter.}

procedure TJvCustomCheckedMaskEdit.BeginInternalChange;
begin
  FInternalChange := True;
end;

procedure TJvCustomCheckedMaskEdit.EndInternalChange;
begin
  { TODO : if this assertion ever fails, it's time to switch to a counted locking scheme }
  Assert( FInternalChange, 'Unsupported nested calls to Begin/EndInternalChange!');
  FInternalChange := False;
end;

function TJvCustomCheckedMaskEdit.InternalChanging: Boolean;
begin
  result := FInternalChange;
end;

procedure TJvCustomCheckedMaskEdit.CheckClick(Sender: TObject);
begin
  //call SetChecked to allow descendants to validate the new value:
  Checked := FCheck.Checked;
  DoCheckClick;
end;

procedure TJvCustomCheckedMaskEdit.DoCheckClick;
begin
  if(Assigned(OnCheckClick)) then
    OnCheckClick(Self);
end;

procedure TJvCustomCheckedMaskEdit.CMCtl3DChanged(var AMessage: TMessage);
begin
  inherited;

  {propagate to child conrols:}
  if(ShowCheckbox) then
  begin
    FCheck.Ctl3D := Self.Ctl3D;
    //adjust layout quirks:
    if(Self.Ctl3d) then
      FCheck.Left := 0
    else
      FCheck.Left := 1;
  end;

  DoCtl3DChanged;

  UpdateControls;
end;

procedure TJvCustomCheckedMaskEdit.DoCtl3DChanged;
begin
  if(Assigned(OnCtl3DChanged)) then
    OnCtl3DChanged(Self);
end;

procedure TJvCustomCheckedMaskEdit.SetHotTrack(const AValue: Boolean);
begin
  FHotTrack := AValue;
  if(AValue) then
    Ctl3d := False;
end;

procedure TJvCustomCheckedMaskEdit.CMEnabledChanged(
  var AMessage: TMessage);
begin
  inherited;

  {propagate to child controls:}
  if( ShowCheckbox) then
    FCheck.Enabled := Self.Enabled;

  DoEnabledChanged;
end;

procedure TJvCustomCheckedMaskEdit.DoEnabledChanged;
begin
  if(Assigned(OnEnabledChanged)) then
    OnEnabledChanged(Self);
end;

procedure TJvCustomCheckedMaskEdit.CMMouseEnter(var AMessage: TMessage);
begin
  inherited;
  if(not FMouseOver) then
  begin
    FSavedHintColor := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then Exit;
    Application.HintColor := FHintColor;
    if(HotTrack) then
      Ctl3d := True;
    FMouseOver := True;
  end;
  DoMouseEnter;
end;

procedure TJvCustomCheckedMaskEdit.DoMouseEnter;
begin
  if(Assigned(OnMouseEnter)) then
    OnMouseEnter(Self);
end;

procedure TJvCustomCheckedMaskEdit.CMMouseLeave(var AMessage: TMessage);
begin
  if(FMouseOver) then
  begin
    Application.HintColor := FSavedHintColor;
    if(HotTrack) then
      Ctl3d := False;
    FMouseOver := False;
  end;
  inherited;
  DoMouseLeave;
end;

procedure TJvCustomCheckedMaskEdit.DoMouseLeave;
begin
  if(Assigned(OnMouseLeave)) then
    OnMouseLeave(Self);
end;

procedure TJvCustomCheckedMaskEdit.CMParentColorChanged(var AMessage: TMessage);
begin
  inherited;
  DoParentColorChanged;
end;

procedure TJvCustomCheckedMaskEdit.DoParentColorChanged;
begin
  if(Assigned(OnParentColorChanged)) then
    OnParentColorChanged(Self);
end;

procedure TJvCustomCheckedMaskEdit.WMKillFocus(var AMessage: TMessage);
begin
  FLeaving := True;
  try
    inherited;
    LoseFocus(FindControl(AMessage.WParam));
  finally
    FLeaving := False;
  end;
end;

function TJvCustomCheckedMaskEdit.Leaving: Boolean;
begin
  result := FLeaving;
end;

procedure TJvCustomCheckedMaskEdit.LoseFocus(const AFocusControl: TWinControl);
begin
  if(AFocusControl <> FCheck) then
  begin
    if(Assigned(OnLoseFocus)) then
      OnLoseFocus(Self, AFocusControl);
  end;
end;

procedure TJvCustomCheckedMaskEdit.WMSetFocus(var AMessage: TMessage);
begin
  FEntering := True;
  try
    inherited;
    GetFocus(FindControl(AMessage.WParam));
  finally
    FEntering := False;
  end;
end;

function TJvCustomCheckedMaskEdit.Entering: Boolean;
begin
  result := FEntering;
end;

procedure TJvCustomCheckedMaskEdit.GetFocus(
  const APreviousControl: TWinControl);
begin
  if(Assigned(OnGetFocus)) then
    OnGetFocus(Self, APreviousControl)
end;

end.
