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

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCheckBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JVCLVer, JvTypes;

type
  TJvCheckBox = class(TCheckBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FAutoSize: Boolean;
    FAssociated: TControl;
    FControlCanvas: TControlCanvas;
    FHotTrackFontOptions: TJvTrackFOntOptions;
    procedure SetHotFont(const Value: TFont);
    procedure SetAssociated(const Value: TControl);
    function GetCanvas: TCanvas;
    procedure SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
  protected
    property Canvas: TCanvas read GetCanvas;
    procedure SetAutoSize(Value: Boolean);{$IFDEF COMPILER6_UP} override;{$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CalcAutoSize;virtual;
  public
    procedure Loaded; override;
    procedure Toggle; override;
    procedure Click; override;
    procedure SetChecked(Value: Boolean); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Associated: TControl read FAssociated write SetAssociated;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotTrackFontOptions: TJvTrackFOntOptions read FHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;

    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FonMouseEnter;
    property OnMouseLeave: TNotifyEvent read FonMouseLeave write FonMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FonCtl3DChanged write FonCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FonParentColorChanged write FonParentColorChanged;
  end;

implementation
uses
  JvJVCLUtils;
  
{**************************************************}

constructor TJvCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  FColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FAutoSize := true;
end;

destructor TJvCheckBox.Destroy;
begin
  FHotFont.Free;
  FFontSave.Free;
  FControlCanvas.Free;
  inherited Destroy;
end;

procedure TJvCheckBox.Toggle;
begin
  inherited Toggle;
  if Assigned(FAssociated) then
    FAssociated.Enabled := Checked;
end;

procedure TJvCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_MULTILINE or BS_TOP;
end;

procedure TJvCheckBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCheckBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCheckBox.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
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

procedure TJvCheckBox.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
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

procedure TJvCheckBox.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TJvCheckBox.Loaded;
begin
  inherited;
  if Assigned(Associated) then
    Associated.Enabled := Checked;
end;

procedure TJvCheckBox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    {$IFDEF COMPILER6_UP}
    inherited SetAutoSize(Value);
    {$ENDIF}
    FAutoSize := Value;
    CalcAutoSize;
  end;
end;

procedure TJvCheckBox.SetAssociated(const Value: TControl);
begin
  if FAssociated <> self then
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
  inherited;
  if (Operation = opRemove) and (AComponent = Associated) then
    Associated := nil;
end;

function TJvCheckBox.GetCanvas: TCanvas;
begin
  if FControlCanvas = nil then
  begin
    FControlCanvas := TControlCanvas.Create;
    FControlCanvas.Control := self;
  end;
  Result := FControlCanvas;
end;

procedure TJvCheckBox.CMTextchanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
end;

procedure TJvCheckBox.CMFontchanged(var Message: TMessage);
begin
  inherited;
  CalcAutoSize;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCheckBox.CalcAutoSize;
var AWidth,AHeight:integer;
begin
  // (p3) TODO: find the Windows constants for width and height of checkbox and radiobutton icons
  if AutoSize then
  begin
    with Canvas.TextExtent(Caption) do
    begin
      AWidth := cx + 18;
      if AWidth < 14 then
        AWidth := 14;
      AHeight := cy + 4;
      if AHeight < 14 then AHeight := 14;
      if Caption <> '' then
        Inc(AWidth,4);
      ClientWidth := AWidth;
      ClientHeight := AHeight;
    end;
  end;
end;

procedure TJvCheckBox.SetHotTrackFontOptions(const Value: TJvTrackFOntOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font,FHotTrackFontOptions);
  end;
end;

end.

