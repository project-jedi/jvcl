{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTipsOfDay.PAS, released on 2001-02-28.

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

unit JvTipsOfDay;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  JvFormTips, JvBaseDlg, JvTypes, JvButtonPersistent;

type
  TJvTipsOfDay = class(TJvCommonDialogP)
  private
    FSCaption: string;
    FTitle: string;
    FDid: string;
    FColor: TColor;
    FHints: TStringList;
    FFont: TFont;
    FClose: TJvButtonPersistent;
    FNext: TJvButtonPersistent;
    procedure SetStrings(const Value: TStringList);
    procedure SetFont(const Value: TFont);
  protected
    procedure Loaded; override;
    function GetRegKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetShowHintOnStartup: Boolean;
    procedure SetShowHintOnStartup(Value: Boolean);
    procedure ManuallyExecute;
    procedure Execute; override;
  published
    property Hints: TStringList read FHints write SetStrings;
    property Title: string read FTitle write FTitle;
    property TextShowOnStartup: string read FSCaption write FSCaption;
    property TextDidYouKnow: string read FDid write FDid;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor default clInfoBk;
    property ShowHintsOnStartUp:boolean read GetShowHintOnStartup write SetShowHintOnStartup;

    property ButtonNext: TJvButtonPersistent read FNext write FNext;
    property ButtonClose: TJvButtonPersistent read FClose write FClose;
  end;

implementation

resourcestring

  RC_KeyStartup = 'Software\JEDI-VCL\TipsStartup';
  RC_TipsTitle = '&Show Tips on startup';
  RC_CloseCaption = '&Close';
  RC_TipsTitle2 = 'Tips and Tricks';
  RC_TipsLabel1 = 'Did you know...';
  RC_NextCaption = '&Next Tip';

  {**************************************************}

constructor TJvTipsOfDay.Create(AOwner: TComponent);
begin
  inherited;
  FClose := TJvButtonPersistent.Create;
  FNext := TJvButtonPersistent.Create;
  FClose.Caption := RC_CloseCaption;
  FNext.Caption := RC_NextCaption;

  FHints := TStringList.Create;
  FSCaption := RC_TipsTitle;
  FTitle := RC_TipsTitle2;
  FDid := RC_TipsLabel1;
  FColor := clInfoBk;
  FFont := TFont.Create;
end;

{**************************************************}

destructor TJvTipsOfDay.Destroy;
begin
  FClose.Free;
  FNext.Free;
  FFont.Free;
  FHints.Free;
  inherited;
end;

{**************************************************}

procedure TJvTipsOfDay.Execute;
begin
  ManuallyExecute;
end;

{**************************************************}

function TJvTipsOfDay.GetShowHintOnStartup: Boolean;
begin
  Result := True;
  with TRegistry.Create do
  begin
    OpenKey(RC_KeyStartup, True);
    if ValueExists(GetRegKey) then
      Result := ReadBool(GetRegKey);
    Free;
  end;
end;

{**************************************************}

procedure TJvTipsOfDay.ManuallyExecute;
begin
  with TFormTip.Create(Application) do
  begin
    Caption := FTitle;
    did.Caption := FDid;
    did.Color := FColor;
    tip.Color := FColor;
    pnlTip.Color := FColor;
    tip.Font.Assign(FFont);
    ShowTips.Caption := FSCaption;

    FNext.AssignTo(BuSpeedButton1);
    FClose.AssignTo(BuSpeedButton2);

    if FHints.count > 0 then
    begin
      it := Random(FHints.Count);
      if FHints[it] = '' then
        it := 0;
      tip.Caption := FHints[it];
      hints.Assign(FHints);
      ShowModal;
    end;

    SetShowHintOnStartup(showtips.Checked);
    Free;
  end;
end;

{**************************************************}

procedure TJvTipsOfDay.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{**************************************************}

procedure TJvTipsOfDay.SetShowHintOnStartup(Value: Boolean);
begin
  with TRegistry.Create do
  begin
    OpenKey(RC_KeyStartup, True);
    WriteBool(GetRegKey, Value);
    Free;
  end;
end;

{**************************************************}

procedure TJvTipsOfDay.SetStrings(const Value: TStringList);
begin
  FHints.Assign(Value);
end;

{**************************************************}

procedure TJvTipsOfDay.Loaded;
begin
  inherited;
  if GetShowHintOnStartup and not (csDesigning in ComponentState) then
    ManuallyExecute;
end;

{**************************************************}

function TJvTipsOfDay.GetRegKey: string;
begin
  Result := Application.Name + '_' + Name;
end;

end.
