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

Contributor(s): ______________________________________.

Last Modified: 2003-10-10

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
  StdCtrls, Controls, ExtCtrls, Forms, CheckLst, ComCtrls,
  {$ELSE}
  QStdCtrls,  QExtCtrls, QControls, QForms, QCheckLst, QComCtrls, Types,
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
    function GetParentBackground: Boolean;
    procedure SetParentBackground(const Value: Boolean);
  protected
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCustomControl = class(TCustomControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
  {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    procedure SetParentBackground(const Value: Boolean);
  protected
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvWinControl = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
  {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    procedure SetParentBackground(const Value: Boolean);
  protected
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvForm = class(TForm)
  private
  {$IFDEF JVCLThemesEnabledD56}
    function GetParentBackground: Boolean;
    procedure SetParentBackground(const Value: Boolean);
  protected
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  // (rom) this has to be removed if gettext is GPL
  {$IFDEF USE_DXGETTEXT}
  public
    constructor Create(AOwner: TComponent); override;
  {$ENDIF USE_DXGETTEXT}
  end;

implementation

{$IFDEF JVCLThemesEnabledD56}

uses
  JvThemes;

function TJvCustomPanel.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvCustomPanel.SetParentBackground(const Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

function TJvCustomControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvCustomControl.SetParentBackground(const Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

function TJvWinControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvWinControl.SetParentBackground(const Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;

function TJvForm.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvForm.SetParentBackground(const Value: Boolean);
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
{$ENDIF}

end.
