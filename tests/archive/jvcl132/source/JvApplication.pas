{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvApplication.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvApplication;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ActnList, JvTypes, JvComponent;

type
  TJvApplication = class(TJvComponent)
  private
    FException: TExceptionEvent;
    FIdle: TIdleEvent;
    FHelp: THelpEvent;
    FShowHint: TShowHintEvent;
    FActionExecute: TActionEvent;
    FActionUpdate: TActionEvent;
    FActivate: TNotifyEvent;
    FDeactivate: TNotifyEvent;
    FMinimize: TNotifyEvent;
    FRestore: TNotifyEvent;
    FHint: TNotifyEvent;
    FMessage: TMessageEvent;
    FShortcut: TShortCutEvent;

    procedure SetHelp(Value: THelpEvent);
    procedure SetIdle(Value: TIdleEvent);
    procedure SetException(Value: TExceptionEvent);
    procedure SetShowHint(Value: TShowHintEvent);
    procedure SetActionExecute(Value: TActionEvent);
    procedure SetActionUpdate(Value: TActionEvent);
    procedure SetActivate(Value: TNotifyEvent);
    procedure SetDeactivate(Value: TNotifyEvent);
    procedure SetMinimize(Value: TNotifyEvent);
    procedure SetOnHint(Value: TNotifyEvent);
    procedure SetRestore(Value: TNotifyEvent);
    procedure SetMessage(Value: TMessageEvent);
    procedure SetShortcut(Value: TShortCutEvent);

    function GetHint: string;
    procedure SetHint(Value: string);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetHidePause: Integer;
    procedure SetHidePause(Value: Integer);
    function GetHintPause: Integer;
    procedure SetHintPause(Value: Integer);
    function GetHintShort: Boolean;
    procedure SetHintShort(Value: Boolean);
    function GetShort: Integer;
    procedure SetShort(Value: Integer);
    function GetShow: Boolean;
    procedure SetShow(Value: Boolean);
    function GetTitle: string;
    procedure SetTitle(Value: string);
  protected
  public
    property Title: string read GetTitle write SetTitle stored False;
  published
    property Hint: string read GetHint write SetHint;
    property HintColor: TColor read GetColor write SetColor;
    property HintHidePause: Integer read GetHidePause write SetHidePause;
    property HintPause: Integer read GetHintPause write SetHintPause;
    property HintShortcuts: Boolean read GetHintShort write SetHintShort;
    property HintShortPause: Integer read GetShort write SetShort;
    property ShowHints: Boolean read GetShow write SetShow;
    property OnException: TExceptionEvent read FException write SetException;
    property OnIdle: TIdleEvent read FIdle write SetIdle;
    property OnHelp: THelpEvent read FHelp write SetHelp;
    property OnShowHint: TShowHintEvent read FShowHint write SetShowHint;
    property OnActionExecute: TActionEvent read FACtionExecute write SetActionExecute;
    property OnActionUpdate: TActionEvent read FActionUpdate write SetActionUpdate;
    property OnActivate: TNotifyEvent read FActivate write SetActivate;
    property OnDeactivate: TNotifyEvent read FDeactivate write SetDeactivate;
    property OnMessage: TMessageEvent read FMessage write SetMessage;
    property OnMinimize: TNotifyEvent read FMinimize write SetMinimize;
    property OnRestore: TNotifyEvent read FRestore write SetRestore;
    property OnShortcut: TShortCutEvent read FShortcut write SetShortcut;
    property OnHint: TNotifyEvent read FHint write SetOnHint;
  end;

implementation

{*****************************************************}

procedure TJvApplication.SetException(Value: TExceptionEvent);
begin
  FException := Value;
  Application.OnException := Value;
end;

{*****************************************************}

procedure TJvApplication.SetIdle(Value: TIdleEvent);
begin
  FIdle := Value;
  Application.OnIdle := Value;
end;

{*****************************************************}

procedure TJvApplication.SetHelp(Value: THelpEvent);
begin
  FHelp := Value;
  Application.OnHelp := Value;
end;

{*****************************************************}

procedure TJvApplication.SetShowHint(Value: TShowHintEvent);
begin
  FShowHint := Value;
  Application.OnShowHint := Value;
end;

{*****************************************************}

procedure TJvApplication.SetActionExecute(Value: TActionEvent);
begin
  FActionExecute := Value;
  Application.OnActionExecute := Value;
end;

{*****************************************************}

procedure TJvApplication.SetActionUpdate(Value: TActionEvent);
begin
  FActionUpdate := Value;
  Application.OnActionUpdate := Value;
end;

{*****************************************************}

procedure TJvApplication.SetActivate(Value: TNotifyEvent);
begin
  FActivate := Value;
  Application.OnActivate := Value;
end;

{*****************************************************}

procedure TJvApplication.SetDeactivate(Value: TNotifyEvent);
begin
  FDeactivate := Value;
  Application.OnDeactivate := Value;
end;

{*****************************************************}

procedure TJvApplication.SetMinimize(Value: TNotifyEvent);
begin
  FMinimize := Value;
  Application.OnMinimize := Value;
end;

{*****************************************************}

procedure TJvApplication.SetOnHint(Value: TNotifyEvent);
begin
  FHint := Value;
  Application.OnHint := Value;
end;

{*****************************************************}

procedure TJvApplication.SetRestore(Value: TNotifyEvent);
begin
  FRestore := Value;
  Application.OnRestore := Value;
end;

{*****************************************************}

procedure TJvApplication.SetMessage(Value: TMessageEvent);
begin
  FMessage := Value;
  Application.OnMessage := Value;
end;

{*****************************************************}

procedure TJvApplication.SetShortcut(Value: TShortCutEvent);
begin
  FShortcut := Value;
  Application.OnShortcut := Value;
end;

{*****************************************************}

function TJvApplication.GetHint: string;
begin
  Result := Application.Hint;
end;

{*****************************************************}

procedure TJvApplication.SetHint(Value: string);
begin
  Application.Hint := Value;
end;

{*****************************************************}

function TJvApplication.GetColor: TColor;
begin
  Result := Application.HintColor;
end;
{*****************************************************}

procedure TJvApplication.SetColor(Value: TColor);
begin
  Application.HintColor := Value;
end;

{*****************************************************}

function TJvApplication.GetHidePause: Integer;
begin
  Result := Application.HintHidePause;
end;

{*****************************************************}

procedure TJvApplication.SetHidePause(Value: Integer);
begin
  Application.HintHidePause := Value;
end;

{*****************************************************}

function TJvApplication.GetHintPause: Integer;
begin
  Result := Application.HintPause;
end;

{*****************************************************}

procedure TJvApplication.SetHintPause(Value: Integer);
begin
  Application.HintPause := Value;
end;

{*****************************************************}

function TJvApplication.GetHintShort: Boolean;
begin
  Result := Application.HintShortCuts;
end;

{*****************************************************}

procedure TJvApplication.SetHintShort(Value: Boolean);
begin
  Application.HintShortCuts := Value;
end;

{*****************************************************}

function TJvApplication.GetShort: Integer;
begin
  Result := Application.HintShortPause;
end;

{*****************************************************}

procedure TJvApplication.SetShort(Value: Integer);
begin
  Application.HintShortPause := Value;
end;

{*****************************************************}

function TJvApplication.GetShow: Boolean;
begin
  Result := Application.ShowHint;
end;

{*****************************************************}

procedure TJvApplication.SetShow(Value: Boolean);
begin
  Application.ShowHint := Value;
end;

{*****************************************************}

function TJvApplication.GetTitle: string;
begin
  Result := Application.Title;
end;

{*****************************************************}

procedure TJvApplication.SetTitle(Value: string);
begin
  Application.Title := Value;
end;

end.
