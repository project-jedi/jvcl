{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvYesNoPicker.pas, released November 2000.

The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2001 Anthony Steele.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvYesNoPicker;

{ AFS 16 Nov 2K
  Component to pick yes/no
  with layout and style as requested for silverfish wizards
  ie:
   - caption on the left
   - yes & no on the same line on the right
   - optional description extra text on the right of this
   - usually a bevel below
   = '?' help button on the far right
}

interface

uses ExtCtrls, StdCtrls, Classes, Controls, Buttons;

Type TCustomYesNoPicker = class(TCustomPanel)
  private
    fcCaption: TLabel;
    fcDescription: TLabel;

    fcYes, fcNo: TRadioButton;
    fcUnderBevel: TBevel;
    fcHelp: TSpeedButton;

    fiRadioXPosition: integer;
    fbHasHelpButton: Boolean;

    fcOnClick: TNotifyEvent;
    fcOnHelpClick: TNotifyEvent;

    procedure DoRadioClick(Sender: TObject);

    { property methods }
    function GetCaption: String;
    function GetHasUnderBevel: Boolean;
    function GetBlank: Boolean;
    function GetYesChecked: Boolean;
    function GetNoChecked: Boolean;
    procedure SetCaption(const psValue: String);
    procedure SetHasUnderBevel(const pbValue: Boolean);
    procedure SetBlank(const pbValue: Boolean);
    procedure SetYesChecked(const pbValue: Boolean);
    procedure SetRadioXPosition(const piValue: integer);
    function GetHasValue: boolean;
    procedure SetNoChecked(const pbValue: Boolean);
    procedure SetHasHelpButton(const pbValue: Boolean);
    function GetDescription: String;
    procedure SetDescription(const psValue: String);
  protected
    procedure DoHelpClick(Sender: TObject); virtual;

    procedure SetEnabled(Value: Boolean); override;


    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;
    procedure Resize; override;

    { set to yes }
    property YesChecked: Boolean read GetYesChecked Write SetYesChecked;
    { not set at all }
    property Blank: Boolean read GetBlank write SetBlank;

    { inverses of the above - set to no, set set anything }
    property NoChecked: Boolean read GetNoChecked write SetNoChecked;
    property HasValue: boolean read GetHasValue;

    property RadioXPosition: integer read fiRadioXPosition write SetRadioXPosition;

    property Caption: String read GetCaption write SetCaption;
    property HasUnderBevel: Boolean read GetHasUnderBevel write SetHasUnderBevel;
    property HasHelpButton: Boolean read fbHasHelpButton write SetHasHelpButton;

    property Description: String read GetDescription write SetDescription;
    { events }
    property OnClick: TNotifyEvent read fcOnClick write fcOnClick;
    property OnHelpClick: TNotifyEvent read fcOnHelpClick write fcOnHelpClick;

end;

type

  TJvYesNoPicker = class(TCustomYesNoPicker)
  public
    { panel properties }
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    { new properties }
    property YesChecked;
    property NoChecked;
    property Blank;
    property RadioXPosition;
    property Caption;
    property HasUnderBevel;
    property HasHelpButton;
    property Description;

    property OnClick;
    property OnHelpClick;
end;

implementation

uses
  { delphi } SysUtils, Dialogs,
  { local } JvComponentFunctions;


constructor TCustomYesNoPicker.Create(AOwner: TComponent);
begin
  inherited;

  fcOnClick := nil;
  fcOnHelpClick := nil;

  { change inherited properties }
  // no border
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csAcceptsControls]; // this is not a container

  { make components }
  fcCaption := TLabel.Create(self);
  fcCaption.Parent := Self;

  fcDescription := TLabel.Create(self);
  fcDescription.Parent := Self;

  fcYes := TRadioButton.Create(self);
  fcYes.Parent := Self;
  fcYes.Caption := '&Yes';
  fcYes.OnClick := DoRadioClick;

  fcNo := TRadioButton.Create(self);
  fcNo.Parent := Self;
  fcNo.Caption := '&No';
  fcNo.OnClick := DoRadioClick;

  fcUnderBevel := TBevel.Create(self);
  fcUnderBevel.Parent := Self;
  fcUnderBevel.Shape := bsBottomLine;

  fcHelp := TSpeedButton.Create(self);
  fcHelp.Parent := self;
  fcHelp.Caption := '?';
  fcHelp.OnClick := DoHelpClick;
  fcHelp.Flat := True;

  // default value
  fiRadioXPosition := 50;
end;

destructor TCustomYesNoPicker.Destroy;
begin
  FreeAndNil(fcCaption);
  FreeAndNil(fcDescription);
  FreeAndNil(fcYes);
  FreeAndNil(fcNo);
  FreeAndNil(fcUnderBevel);

  inherited;
end;


procedure TCustomYesNoPicker.Resize;
const
  RADIO_WIDTH: integer = 45;
  BEVEL_HEIGHT = 2;
  GUI_PAD = 2;
var
  liRadioX, liCaptionEnd: integer;
begin
  inherited;

  fcCaption.Left := GUI_PAD;
  CenterHeight(fcCaption, Self);
  fcCaption.Visible := True;

  CenterHeight(fcYes, Self);
  CenterHeight(fcNo, Self);
  fcYes.Width := RADIO_WIDTH;
  fcNo.Width := RADIO_WIDTH;

  liRadioX := RadioXPosition;
  liCaptionEnd := Canvas.TextWidth(fcCaption.Caption) + fcCaption.Left + GUI_PAD;
  if liRadioX < liCaptionEnd then
    liRadioX := liCaptionEnd;

  if (liRadioX <= 0) or ((liRadioX  + fcYes.Width + fcNo.Width) > ClientWidth) then
    liRadioX := ClientWidth div 2;

  fcYes.Left := liRadioX;
  fcYes.Visible := True;

  fcNo.Left := fcYes.Left + fcYes.Width + GUI_PAD;
  fcNo.Visible := True;

  fcDescription.Left := fcNo.Left + fcNo.Width + GUI_PAD;
  CenterHeight(fcDescription, Self);
  fcDescription.Visible := True;

  { bevel at the bottom }
  if HasUnderBevel then
  begin
    fcUnderBevel.Left := 0;
    fcUnderBevel.Width := ClientWidth;
    fcUnderBevel.Height := BEVEL_HEIGHT;
    fcUnderBevel.Top := CLientHeight - BEVEL_HEIGHT;
  end
  else
    fcUnderBevel.Visible := False;

  { help button to the right }
  if HasHelpButton then
  begin
    CenterHeight(fcHelp, Self);
    fcHelp.Left := ClientWidth - fcHelp.Width;
  end
  else
    fcHelp.Visible := False;
end;

procedure TCustomYesNoPicker.SetFocus;
var
  lbYes, lbBlank: Boolean;
  lcCtrl: TRadioButton;
  lcOnClick: TNotifyEvent;
begin
  inherited;

  lbBlank := Blank;
  lbYes := YesChecked;

  if lbYes or lbBlank then
    lcCtrl := fcYes
  else
    lcCtrl := fcNo;
  { hack so that the onclick is not called for a setfocus}
  lcOnClick := fcOnClick;
  fcOnClick := nil;
  if lcCtrl.CanFocus then
    lcCtrl.SetFocus;

  { if it was blank.. }
  if lbBlank then
    Blank := True;
  fcOnClick := lcOnClick;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TCustomYesNoPicker.DoRadioClick(Sender: TObject);
begin
  if Assigned(fcOnClick) then
    fcOnClick(self);
end;

procedure TCustomYesNoPicker.DoHelpClick(Sender: TObject);
begin
  // trigger the event
  if Assigned(fcOnHelpClick) then
    fcOnHelpClick(self);

  //override for more. Silverfish-specific stuff will have to come in here
end;

{-------------------------------------------------------------------------------
  layout properties }

function TCustomYesNoPicker.GetCaption: String;
begin
  Result := fcCaption.Caption;
end;

function TCustomYesNoPicker.GetHasUnderBevel: Boolean;
begin
  Result := fcUnderBevel.Visible;
end;


procedure TCustomYesNoPicker.SetCaption(const psValue: String);
begin
  Assert(fcCaption <> nil);

  fcCaption.Caption := psValue;
  Resize;
end;

procedure TCustomYesNoPicker.SetHasUnderBevel(const pbValue: Boolean);
begin
  Assert(fcUnderBevel <> nil);

  fcUnderBevel.Visible := pbValue;
end;

procedure TCustomYesNoPicker.SetHasHelpButton(const pbValue: Boolean);
begin
  fbHasHelpButton := pbValue;
  fcHelp.Visible := fbHasHelpButton;
end;


procedure TCustomYesNoPicker.SetRadioXPosition(const piValue: integer);
begin
  if fiRadioXPosition <> piValue then
  begin
    fiRadioXPosition := piValue;
    Resize;
  end;
end;

function TCustomYesNoPicker.GetDescription: String;
begin
  Result := fcDescription.Caption;
end;

procedure TCustomYesNoPicker.SetDescription(const psValue: String);
begin
  Assert(fcDescription <> nil);
  fcDescription.Caption := psValue;
  Resize;
end;

{--------  state properties }

function TCustomYesNoPicker.GetBlank: Boolean;
begin
  Result := (not fcYes.Checked) and (Not fcNo.Checked);
end;

function TCustomYesNoPicker.GetHasValue: boolean;
begin
  { not blank }
  Result := (fcYes.Checked) or (fcNo.Checked);
end;

function TCustomYesNoPicker.GetYesChecked: Boolean;
begin
  Result := fcYes.Checked;
end;

function TCustomYesNoPicker.GetNoChecked: Boolean;
begin
  Result := fcNo.Checked;
end;

procedure TCustomYesNoPicker.SetBlank(const pbValue: Boolean);
begin
  Assert(fcYes <> nil);
  Assert(fcNo <> nil);


  if pbValue then
  begin
    { make blank }
    fcYes.Checked := False;
    fcNo.Checked := False;
  end
  else
  begin
    { unblank - ie turn one on }
    if (not fcYes.Checked) and (not fcNo.Checked) then
      fcYes.Checked := True;
  end;
end;

procedure TCustomYesNoPicker.SetYesChecked(const pbValue: Boolean);
begin
  Assert(fcYes <> nil);
  Assert(fcNo <> nil);

  fcYes.Checked := pbValue;
  fcNo.Checked := not pbValue;
end;

procedure TCustomYesNoPicker.SetNoChecked(const pbValue: Boolean);
begin
  Assert(fcYes <> nil);
  Assert(fcNo <> nil);

  fcNo.Checked := pbValue;
  fcYes.Checked := not pbValue;
end;

procedure TCustomYesNoPicker.SetEnabled(Value: Boolean);
begin
  inherited;
  fcNo.Enabled := Value;
  fcYes.Enabled := Value;
end;

procedure TCustomYesNoPicker.Loaded;
begin
  inherited;
  Resize;
end;

end.
