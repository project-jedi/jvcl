{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MainForm.pas, released on 2007-02-06.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2007 Olivier Sannier.
All Rights Reserved.

Contributor(s): None to date.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Demonstrates the usage of TJvSimScope

Known Issues:
-----------------------------------------------------------------------------}
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvSimScope, StdCtrls, JvCPUUsage;

type
  TfrmMain = class(TForm)
    jssCPU: TJvSimScope;
    jssRandom: TJvSimScope;
    lblCPUDetails1: TLabel;
    btnActivateDeactivateCPU: TButton;
    lblCPUDetails2: TLabel;
    lblRandomDetails1: TLabel;
    btnActivateDeactivateRandom: TButton;
    btnAdjustMax: TButton;
    Label1: TLabel;
    lblWelcome: TLabel;
    procedure btnActivateDeactivateCPUClick(Sender: TObject);
    procedure jssCPUUpdate(Sender: TObject);
    procedure jssRandomUpdate(Sender: TObject);
    procedure btnActivateDeactivateRandomClick(Sender: TObject);
    procedure btnAdjustMaxClick(Sender: TObject);
  private
    FCPUUsage: TJvCPUUsage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnActivateDeactivateCPUClick(Sender: TObject);
begin
  jssCPU.Active := not jssCPU.Active;
  if jssCPU.Active then
    btnActivateDeactivateCPU.Caption := 'Deactivate'
  else
    btnActivateDeactivateCPU.Caption := 'Activate';
end;

procedure TfrmMain.btnActivateDeactivateRandomClick(Sender: TObject);
begin
  jssRandom.Active := not jssRandom.Active;
  if jssRandom.Active then
    btnActivateDeactivateRandom.Caption := 'Deactivate'
  else
    btnActivateDeactivateRandom.Caption := 'Activate';
end;

procedure TfrmMain.btnAdjustMaxClick(Sender: TObject);
var
  I: Integer;
  LineMax: Integer;
begin
  // We check all values of line number 1 to see if there is one that is greater
  // than the current max value of the scope. If so, we change the Maximum value
  // to demonstrate how redrawing is done and how past values were kept to allow
  // redrawing with a new scale.
  LineMax := jssRandom.Minimum;
  for I := 0 to jssRandom.Lines[1].Values.Count - 1 do
  begin
    if jssRandom.Lines[1].Values[I] > LineMax then
      LineMax := jssRandom.Lines[1].Values[I];
  end;
  
  if LineMax > jssRandom.Maximum then
    jssRandom.Maximum := LineMax;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCPUUsage := TJvCPUUsage.Create(Self);
end;

destructor TfrmMain.Destroy;
begin
  FCPUUsage.Free;

  inherited Destroy;
end;

procedure TfrmMain.jssCPUUpdate(Sender: TObject);
begin
  jssCPU.Lines[0].Position := Round(FCPUUsage.Usage);
end;

procedure TfrmMain.jssRandomUpdate(Sender: TObject);
begin
  jssRandom.Lines[0].Position := Random(200) - 100;
  jssRandom.Lines[1].Position := Random(200);  // this one will eventually go out of scope
end;

end.