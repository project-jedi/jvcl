{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTipWin.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ A tipwindow component. }
unit JvTipWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvComponent;

type
  TJvTipWindow = class(TJvComponent)
  private
    { Private declarations }
    FCaption: string;
    FShowTips: boolean;
    FLines: TStrings;
    FTipFont: TFont;
    FTitleFont: TFont;
    FLineCount: integer;
    procedure SetLines(Value: TStrings);
    procedure SetTipFont(Value: TFont);
    procedure SetTitleFont(Value: TFont);
    procedure GetTipsEvent(Sender: TObject; var Tips: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure LoadFromFile(aFilename: string);
    procedure SaveToFile(aFilename: string);
  published
    { Published declarations }
    property Caption: string read FCaption write FCaption;
    property ShowTips: boolean read FShowTips write FShowTips;
    property Tips: TStrings read FLines write SetLines;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property TipFont: TFont read FTipFont write SetTipFont;
  end;

implementation

uses
  JvTipsDlg;

constructor TJvTipWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTipFont := TFont.Create;
  FTitleFont := TFont.Create;
  FTipFont.Name := 'Arial';
  FTipFont.Size := 8;
  FTipFont.Style := [];
  FTitleFont.Name := 'Arial';
  FTitleFont.Size := 14;
  FTitleFont.Style := [fsBold];
  FLines := TStringList.Create;
  FLineCount := 0;
end;

destructor TJvTipWindow.Destroy;
begin
  FLines.Free;
  FTipFont.Free;
  FTitleFont.Free;
  inherited Destroy;
end;

function TJvTipWindow.Execute: Boolean;
begin
  Result := TJvTipsFrm.ShowTips(Caption, ShowTips, TitleFont, TipFont, GetTipsEvent);
  ShowTips := Result;
end;

procedure TJvTipWindow.GetTipsEvent(Sender: TObject; var Tips: string);
begin
  if FLineCount >= FLines.Count then
    FLineCount := 0;
  if FLines.Count > 0 then
    Tips := FLines[FLineCount];
  Inc(FLineCount);
end;

procedure TJvTipWindow.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TJvTipWindow.LoadFromFile(aFilename: string);
begin
  if Length(aFilename) = 0 then
    with TOpenDialog.Create(nil) do
    begin
      if Execute then
        Tips.LoadFromFile(Filename);
      Free;
    end
  else if FileExists(aFilename) then
    Tips.LoadFromFile(aFilename);
end;

procedure TJvTipWindow.SaveToFile(aFilename: string);
begin
  if Length(aFilename) = 0 then
    with TSaveDialog.Create(nil) do
    begin
      if Execute then
        Tips.SaveToFile(Filename);
      Free;
    end
  else
    Tips.SaveToFile(aFilename);
end;

procedure TJvTipWindow.SetTipFont(Value: TFont);
begin
  FTipFont.Assign(Value);
end;

procedure TJvTipWindow.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

end.

