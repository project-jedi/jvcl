{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvButton;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Menus, Controls,
  Forms, JVCLVer;

type
  TJvButton = class(TButton)
  private
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FDropMenu: TPopupMenu;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetHotFont(const Value: TFont);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  public
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property DropDownMenu: TPopupMenu read FDropMenu write FDropMenu;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;

    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FonMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FonMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FonCtl3DChanged write FonCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FonParentColorChanged write FonParentColorChanged;
  end;

implementation

{**************************************************}

constructor TJvButton.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clInfoBk;
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
end;

{**************************************************}

destructor TJvButton.Destroy;
begin
  FHotFont.Free;
  FFontSave.Free;
  inherited;
end;

{**************************************************}

procedure TJvButton.Click;
begin
  inherited;
  if FDropMenu <> nil then
  begin
    FDropMenu.Popup(GetClientOrigin.x, GetClientOrigin.y + Height);
    Perform(CM_MOUSELEAVE, 0, 0);
  end;
end;

{**************************************************}

procedure TJvButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_MULTILINE;
end;

{**************************************************}

procedure TJvButton.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FonCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvButton.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvButton.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvButton.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvButton.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

end.
