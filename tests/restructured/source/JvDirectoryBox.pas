{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirectoryBox.PAS, released on 2001-02-28.

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

unit JvDirectoryBox;

{$OBJEXPORTALL On}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvEdit, JvTypes, JvCustomBox, JvSelectDirectory, FileCtrl;

type
{ TODO -opeter3 : Rewrite to not depend on FileCtrl? }
  TJvDirectoryOptions = class(TPersistent)
  private
    FTitle: string;
    FHelpContext: Integer;
    FInitialDir: string;
    FOnChanged: TNotifyEvent;
    FOptions: TSelectDirOpts;
    FClassic: Boolean;
    procedure SetHelpContext(const Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetOptions(const Value: TSelectDirOpts);
    procedure SetTitle(const Value: string);
    procedure SetClassic(const Value: Boolean);
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Changed;
  public
    constructor Create;
  published
    property ClassicDialog: Boolean read FClassic write SetClassic default True;
    property Title: string read FTitle write SetTitle;
    property InitialDir: string read FInitialDir write SetInitialDir;
    property Options: TSelectDirOpts read FOptions write SetOptions;
    property HelpContext: Integer read FHelpContext write SetHelpContext;
  end;

  TJvDirectoryBox = class(TJvCustomBox)
  private
    FChoose: TJvSelectDirectory;
    FOnOpened: TOnOpened;
    FOnOpenCanceled: TOnOpenCanceled;
    FDialogOptions: TJvDirectoryOptions;
    function GetFileName: string;
    procedure SetFileName(Value: string);
  protected
    procedure BtnClick(Sender: TObject); override;
    procedure DlgOptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Directory: string read GetFileName write SetFileName;
    property OnOpened: TOnOpened read FOnOpened write FOnOpened;
    property OnOpenCanceled: TOnOpenCanceled read FOnOpenCanceled write FOnOpenCanceled;
    property DialogOptions: TJvDirectoryOptions read FDialogOptions write FDialogOptions;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvDirectoryBox
///////////////////////////////////////////////////////////

constructor TJvDirectoryBox.Create(AOwner: TComponent);
begin
  inherited;
  Button.Glyph.LoadFromResourceName(HInstance, 'FOLD');
  FChoose := TJvSelectDirectory.Create(Self);
  FDialogOptions := TJvDirectoryOptions.Create;
  FDialogOptions.OnChanged := DlgOptionsChanged;
end;

{*****************************************************}

destructor TJvDirectoryBox.Destroy;
begin
  FChoose.Free;
  FDialogOptions.Free;
  inherited;
end;

{*****************************************************}

procedure TJvDirectoryBox.SetFileName(Value: string);
begin
  Edit.Text := Value;
end;

{*****************************************************}

function TJvDirectoryBox.GetFileName: string;
begin
  Result := Edit.Text;
end;

{*****************************************************}

procedure TJvDirectoryBox.BtnClick(Sender: TObject);
begin
  if FChoose.Execute then
  begin
    Edit.Text := FChoose.Directory;
    if Assigned(FOnOpened) then
      FOnOpened(Self, Edit.Text);
  end
  else if Assigned(FOnOpenCanceled) then
    FOnOpenCanceled(Self);
end;

{*****************************************************}

procedure TJvDirectoryBox.DlgOptionsChanged(Sender: TObject);
begin
  FChoose.ClassicDialog := FDialogOptions.ClassicDialog;
  FChoose.Title := FDialogOptions.Title;
  //mb  FChoose.StartDir := FDialogOptions.InitialDir;
  FChoose.Options := FDialogOptions.Options;
  FChoose.HelpContext := FDialogOptions.HelpContext;
end;

///////////////////////////////////////////////////////////
// TJvDirectoryOptions
///////////////////////////////////////////////////////////

procedure TJvDirectoryOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{*****************************************************}

constructor TJvDirectoryOptions.Create;
begin
  FClassic := True;
end;

{*****************************************************}

procedure TJvDirectoryOptions.SetClassic(const Value: Boolean);
begin
  FClassic := Value;
  Changed;
end;

{*****************************************************}

procedure TJvDirectoryOptions.SetHelpContext(const Value: Integer);
begin
  FHelpContext := Value;
  Changed;
end;

{*****************************************************}

procedure TJvDirectoryOptions.SetInitialDir(const Value: string);
begin
  FInitialDir := Value;
  Changed;
end;

{*****************************************************}

procedure TJvDirectoryOptions.SetOptions(const Value: TSelectDirOpts);
begin
  FOptions := Value;
  Changed;
end;

{*****************************************************}

procedure TJvDirectoryOptions.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

end.
