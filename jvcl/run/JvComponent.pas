{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponent.PAS, released on 2000-09-22.

The Initial Developer of the Original Code is Joe Doe .
Portions created by Joe Doe are Copyright (C) 1999 Joe Doe.
Portions created by XXXX Corp. are Copyright (C) 1998, 1999 XXXX Corp.
All Rights Reserved.

Contributor(s): Andreas Hausladen <Andreas.Hausladen@gmx.de> (XP Theming)

Last Modified: 2003-12-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvComponent;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, StdCtrls, Controls, ExtCtrls, Forms, CheckLst, ComCtrls,
  {$ELSE}
  Types, QTypes, QStdCtrls,  QExtCtrls, QControls, QForms, QCheckLst, QComCtrls,
  {$ENDIF VCL}
  {$IFDEF USE_DXGETTEXT}
  gnugettext,
  {$ENDIF USE_DXGETTEXT}
  JVCLVer, JvTypes;

type
  TJvClipboardCommand = (caCopy, caCut, caPaste, caUndo);
  TJvClipboardCommands = set of TJvClipboardCommand;

  TJvComponent = class(TComponent)
  private
    FAboutJVCL: TJVCLAboutInfo;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvGraphicControl = class(TGraphicControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
  {$IFDEF VisualCLX}
    FText: TCaption; // TControl does not save the Caption property
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
  {$ENDIF VisualCLX}
  {$IFDEF VCL}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
  protected
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    procedure ColorChanged; dynamic;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCustomTreeView = class(TCustomTreeView)
  private
    FAboutJVCL: TJVCLAboutInfo;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCustomPanel = class(TCustomPanel)
  private
    FAboutJVCL: TJVCLAboutInfo;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCustomControl = class(TCustomControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    {$IFDEF VCL}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    {$ENDIF VCL}
    {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    {$ENDIF JVCLThemesEnabledD56}
  protected
    {$IFDEF VCL}
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    procedure ColorChanged; dynamic;
    {$ENDIF VCL}
    {$IFDEF JVCLThemesEnabledD56}
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
    {$ENDIF JVCLThemesEnabledD56}
    {$IFDEF VCL}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$ENDIF VCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvWinControl = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    {$IFDEF VCL}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    {$ENDIF VCL}
    {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    {$ENDIF JVCLThemesEnabledD56}
  protected
    {$IFDEF VCL}
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    procedure ColorChanged; dynamic;
    {$ENDIF VCL}
    {$IFDEF JVCLThemesEnabledD56}
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
    {$ENDIF JVCLThemesEnabledD56}
    {$IFDEF VCL}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$ENDIF VCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvForm = class(TForm)
  private
    FAboutJVCL: TJVCLAboutInfo;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean; virtual;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$IFDEF USE_DXGETTEXT}
  public
    constructor Create(AOwner: TComponent); override;
  {$ENDIF USE_DXGETTEXT}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

uses
  JvThemes;

//=== TJvCustomPanel =========================================================

{$IFDEF JVCLThemesEnabledD56}

function TJvCustomPanel.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvCustomPanel.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

{$ENDIF JVCLThemesEnabledD56}

//=== TJvCustomControl =======================================================

{$IFDEF JVCLThemesEnabledD56}

function TJvCustomControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvCustomControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

{$ENDIF JVCLThemesEnabledD56}

{$IFDEF VCL}

procedure TJvCustomControl.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    MouseEnter(Self);
end;

procedure TJvCustomControl.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    MouseLeave(Self);
end;

procedure TJvCustomControl.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  ColorChanged;
end;

procedure TJvCustomControl.MouseEnter(Control: TControl);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomControl.MouseLeave(Control: TControl);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomControl.ColorChanged;
begin
  // do nothing
end;

{$ENDIF VCL}

//=== TJvWinControl ==========================================================

{$IFDEF JVCLThemesEnabledD56}

function TJvWinControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvWinControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

{$ENDIF JVCLThemesEnabledD56}

{$IFDEF VCL}

procedure TJvWinControl.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJvWinControl.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJvWinControl.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  ColorChanged;
end;

procedure TJvWinControl.MouseEnter(Control: TControl);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvWinControl.MouseLeave(Control: TControl);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvWinControl.ColorChanged;
begin
  // do nothing
end;

{$ENDIF VCL}

//=== TJvForm ================================================================

{$IFDEF JVCLThemesEnabledD56}

function TJvForm.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvForm.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

{$ENDIF JVCLThemesEnabledD56}

{$IFDEF USE_DXGETTEXT}
constructor TJvForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TranslateComponent(Self);
end;
{$ENDIF USE_DXGETTEXT}


//=== TJvGraphicControl ======================================================

{$IFDEF VisualCLX}
function TJvGraphicControl.GetText: TCaption;
begin
  Result := FText;
end;

procedure TJvGraphicControl.SetText(const Value: TCaption);
begin
  if Value <> FText then
  begin
    FText := Value;
    TextChanged;
  end;
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}

procedure TJvGraphicControl.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJvGraphicControl.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJvGraphicControl.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  ColorChanged;
end;

procedure TJvGraphicControl.MouseEnter(Control: TControl);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvGraphicControl.MouseLeave(Control: TControl);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvGraphicControl.ColorChanged;
begin
  // do nothing
end;

{$ENDIF VCL}

end.
